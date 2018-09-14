;;; gdb.el --- GDB frontend -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Gonçalo Santos

;; Author: Gonçalo Santos (aka. weirdNox)
;; Keywords: lisp gdb
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'comint)
(require 'gdb-module (concat default-directory "gdb-module" module-file-suffix))

;; ------------------------------------------------------------------------------------------
;; User configurable variables
(defvar gdb-debug nil
  "List of debug symbols, which will enable different components.
Possible values are:
  - `gdb-debug-timings': show timings of some function calls
  - `gdb-debug-log-commands': show which GDB commands are sent
  - `gdb-debug-raw-input': send comint input as is
  - `gdb-debug-raw-output': print GDB/MI output to the messages buffer

This can also be set to t, which means that all debug components are active.")


;; ------------------------------------------------------------------------------------------
;; Private constants and variables
(defvar gdb--previous-executable nil
  "Previous executable path.")

(defconst gdb--available-contexts
  '(gdb--context-ignore
    gdb--context-initial-file
    gdb--context-breakpoint-insert
    gdb--context-thread-info
    gdb--context-frame-info  ;; DATA: Thread ID string
    gdb--context-disassemble ;; DATA: Disassemble buffer
    )
  "List of implemented token contexts.
Must be in the same order of the `token_context' enum in the
dynamic module.")

(defconst gdb--buffer-types
  '(gdb--comint
    gdb--inferior-io
    gdb--breakpoints
    gdb--threads
    gdb--frames
    gdb--disassembly
    gdb--registers
    gdb--locals
    gdb--expression-watcher)
  "List of available buffer types.")

(defconst gdb--keep-buffer-types '(gdb--comint gdb--inferior-io)
  "List of buffer types that should be kept after GDB is killed.")

(cl-defstruct gdb--frame thread-id addr func file line from)
(cl-defstruct gdb--thread target-id name state core frames)

(cl-defstruct gdb--breakpoint type disp enabled addr hits what file line)
(defconst gdb--available-breakpoint-types
  '(("Breakpoint" . "")
    ("Temporary Breakpoint" . "-t")
    ("Hardware Breakpoint" . "-h")
    ("Temporary Hardware Breakpoint" . "-t -h"))
  "Alist of (TYPE . FLAGS).
Both are strings. FLAGS are the flags to be passed to -break-insert in order to create a
breakpoint of TYPE.")

(cl-defstruct gdb--session
  frame process buffers source-window
  (buffer-types-to-update gdb--buffer-types) buffers-to-update
  current-frame threads breakpoints)
(defvar gdb--sessions nil
  "List of active sessions.")

(cl-defstruct gdb--buffer-info session type thread update-func data)
(defvar-local gdb--buffer-info nil
  "GDB related information related to each buffer.")

(defvar gdb--next-token 0
  "Next token value to be used for context matching.
This is shared among all sessions.")

(defvar gdb--token-contexts nil
  "Alist of tokens and contexts.
The alist has the format ((TOKEN . (TYPE . DATA)) ...).
This is shared among all sessions.")

(cl-defstruct gdb--instruction address function offset instruction)
(cl-defstruct gdb--source-instr-info file line instr-list)


;; ------------------------------------------------------------------------------------------
;; Session management
(defsubst gdb--infer-session ()
  (or (and (gdb--buffer-info-p gdb--buffer-info)
           (gdb--session-p (gdb--buffer-info-session gdb--buffer-info))
           (gdb--buffer-info-session gdb--buffer-info))
      (and (gdb--session-p (frame-parameter nil 'gdb--session))
           (frame-parameter nil 'gdb--session))))

(defun gdb--valid-session (session)
  "Returns t if SESSION is valid. Else, nil."
  (when (gdb--session-p session)
    (if (and (frame-live-p (gdb--session-frame session))
             (process-live-p (gdb--session-process session)))
        t
      (gdb-kill-session session)
      nil)))

(defmacro gdb--with-valid-session (&rest body)
  (declare (debug ([&optional stringp] body)))
  (let ((message (and (stringp (car body)) (car body))))
    (when message (setq body (cdr body)))
   `(let ((session (gdb--infer-session)))
     (if (gdb--valid-session session)
         (progn ,@body)
       ,(when message `(error "%s" ,message))))))

(defun gdb--update-buffer (buffer)
  (with-current-buffer buffer
    (let ((func (gdb--buffer-info-update-func gdb--buffer-info)))
      (cl-assert (fboundp func))
      (gdb--measure-time (concat "Calling " (symbol-name func)) (funcall func)))))

(defun gdb--update ()
  (gdb--with-valid-session
   (let ((inhibit-read-only t)
         (buffers-to-update (gdb--session-buffers-to-update session))
         (types-to-update   (gdb--session-buffer-types-to-update session)))
     (dolist (buffer (gdb--session-buffers session))
       (when (or (memq buffer buffers-to-update)
                 (memq (gdb--buffer-info-type (buffer-local-value 'gdb--buffer-info buffer))
                       types-to-update))
         (gdb--update-buffer buffer)))

     (setf (gdb--session-buffers-to-update session) nil
           (gdb--session-buffer-types-to-update session) nil))))


;; ------------------------------------------------------------------------------------------
;; Utilities
(defun gdb--debug-check (arg)
  "Check if debug ARG is enabled.
Type may be a symbol or a list of symbols and are checked against `gdb-debug'."
  (or (eq gdb-debug t)
      (and (listp arg) (cl-loop for type in arg when (memq type gdb-debug) return t))
      (and (symbolp arg) (memq arg gdb-debug))))

(defmacro gdb--debug-execute-body (debug-symbol &rest body)
  "Execute body when DEBUG-SYMBOL is in `gdb-debug'.
DEBUG-SYMBOL may be a symbol or a list of symbols."
  (declare (indent defun))
  `(when (gdb--debug-check ,debug-symbol) (progn ,@body)))

(defun gdb--escape-argument (string)
  "Return STRING quoted properly as an MI argument.
The string is enclosed in double quotes.
All embedded quotes, newlines, and backslashes are preceded with a backslash."
  (setq string (replace-regexp-in-string "\\([\"\\]\\)" "\\\\\\&" string t))
  (setq string (replace-regexp-in-string "\n" "\\n" string t t))
  (concat "\"" string "\""))

(defmacro gdb--measure-time (string &rest body)
  "Measure the time it takes to evaluate BODY."
  `(if (gdb--debug-check 'gdb-debug-timings)
       (let ((time (current-time))
             (result (progn ,@body)))
         (message "GDB TIME MEASUREMENT: %s - %.06fs" ,string (float-time (time-since time)))
         result)
     (progn ,@body)))


;; ------------------------------------------------------------------------------------------
;; Buffers
(defun gdb--get-buffer-with-type (session type)
  (cl-loop for buffer in (gdb--session-buffers session)
           when (eq (gdb--buffer-info-type (buffer-local-value 'gdb--buffer-info buffer)) type)
           return buffer
           finally return nil))

(defmacro gdb--simple-get-buffer (type update-func &rest body)
  "Simple buffer creator/fetcher, for buffers that should be unique in a session."
  (declare (indent defun) (debug (sexp body)))
  (unless (memq type gdb--buffer-types) (error "Type %s does not exist" (symbol-name type)))
  `(defun ,(intern (concat (symbol-name type) "-get-buffer")) (session)
     ,(concat "Creator and fetcher of buffer with type `" (symbol-name type) "'")
     (cond ((gdb--get-buffer-with-type session ',type))
           (t (let ((buffer (generate-new-buffer "*GDB-temp*")))
                (with-current-buffer buffer
                  ,@body
                  (setq gdb--buffer-info
                        (make-gdb--buffer-info :session session :type ',type :update-func #',update-func)))
                (push buffer (gdb--session-buffers session))
                buffer)))))

(defmacro gdb--rename-buffer (&optional specific-str)
  `(rename-buffer ,(concat "*GDB" (when specific-str (concat ": " specific-str)) "*") t))


;; ------------------------------------------------------------------------------------------
;; Frames and windows
(defun gdb--frame-name (&optional debuggee)
  "Return GDB frame name, possibly using DEBUGGEE file name."
  (let ((suffix (and (stringp debuggee) (file-executable-p debuggee)
                     (concat " - " (file-name-nondirectory debuggee)))))
    (concat "Emacs GDB" suffix)))

(defun gdb--create-frame (session)
  (let ((frame (make-frame `((fullscreen . maximized)
                             (gdb--session . ,session)
                             (name . ,(gdb--frame-name))))))
    (setf (gdb--session-frame session) frame)
    (add-hook 'delete-frame-functions #'gdb--handle-delete-frame)
    frame))

(defun gdb--handle-delete-frame (frame)
  (cl-loop for session in gdb--sessions
           when (eq (gdb--session-frame session) frame)
           return (gdb-kill-session session)))

(defun gdb--set-window-buffer (window buffer)
  (set-window-dedicated-p window nil)
  (set-window-buffer window buffer)
  (set-window-dedicated-p window t))

(defun gdb--setup-windows (session)
  (with-selected-frame (gdb--session-frame session)
    (delete-other-windows)
    (let* ((top-left (selected-window))
           (middle-left (split-window))
           (bottom-left (split-window middle-left))
           (top-right (split-window top-left nil t))
           (middle-right (split-window middle-left nil t))
           (bottom-right (split-window bottom-left nil t)))
      (balance-windows)
      (gdb--set-window-buffer top-left (gdb--comint-get-buffer session))
      (gdb--set-window-buffer middle-right (gdb--inferior-io-get-buffer session))
      (setf (gdb--session-source-window session) middle-left))))


;; ------------------------------------------------------------------------------------------
;; Comint buffer
(define-derived-mode gdb-comint-mode comint-mode "GDB Comint"
  "Major mode for interacting with GDB."
  (set-process-sentinel (get-buffer-process (current-buffer)) #'gdb--comint-sentinel)
  (setq-local comint-input-sender #'gdb--comint-sender)
  (setq-local comint-preoutput-filter-functions '(gdb--output-filter))

  (setq-local mode-line-process '(":%s"))

  (setq-local comint-prompt-read-only nil)
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-regexp "^(gdb)[ ]+")

  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp))

;; NOTE(nox): This buffer doesn't need a kill hook because it has a process sentinel
(gdb--simple-get-buffer gdb--comint ignore
  (gdb--rename-buffer "Comint")
  (make-comint-in-buffer "GDB" buffer "gdb" nil "-i=mi" "-nx")
  (gdb-comint-mode)
  (setf (gdb--session-process session) (get-buffer-process buffer)))

(defun gdb--comint-sender (process string)
  "Send user commands from comint."
  (if (gdb--debug-check 'gdb-debug-raw-input)
      (comint-simple-send process string)
    (comint-simple-send process (concat "-interpreter-exec console " (gdb--escape-argument string)))))

(defun gdb--output-filter (string)
  "Parse GDB/MI output."
  (gdb--debug-execute-body 'gdb-debug-raw-output (message "%s" string))
  (let ((output (gdb--measure-time "Handle MI Output" (gdb--handle-mi-output string))))
    (gdb--update)
    (gdb--debug-execute-body '(gdb-debug-timings gdb-debug-log-commands gdb-debug-raw-output)
      (message "--------------------"))
    output))

(defun gdb--comint-sentinel (process _)
  "Handle GDB comint process state changes."
  (when (or (not (buffer-name (process-buffer process)))
            (eq (process-status process) 'exit))
    (gdb-kill-session)))

(defun gdb--command (command &optional context force-stopped)
  "Execute GDB COMMAND.
If provided, the CONTEXT is assigned to a unique token, which
will be received, alongside the output, by the dynamic module,
and used to know what the context of that output was. When
FORCE-STOPPED is non-nil, ensure that exists at least one stopped
thread before running the command.

CONTEXT may be a cons (CONTEXT-TYPE . DATA), where DATA is
anything relevant for the context, or just CONTEXT-TYPE.
CONTEXT-TYPE must be a member of `gdb--available-contexts'."
  (gdb--with-valid-session
   "Could not run command because no session is available"
   (let ((process (gdb--session-process session))
         (context-type (or (and (consp context) (car context)) context))
         (threads (gdb--session-threads session))
         token stopped-thread-id-str)
     (when (memq context-type gdb--available-contexts)
       (setq token (number-to-string gdb--next-token)
             command (concat token command)
             gdb--next-token (1+ gdb--next-token))
       (when (not (consp context)) (setq context (cons context-type nil)))
       (push (cons token context) gdb--token-contexts))

     (when (and force-stopped (> (length threads) 0))
       (when (not (cl-loop for thread in threads
                           when (string= "stopped" (gdb--thread-state thread))
                           return t))
         (setq stopped-thread-id-str (number-to-string (caar threads)))
         (gdb--command (concat "-exec-interrupt --thread " stopped-thread-id-str) 'gdb--context-ignore)))

     (comint-simple-send process command)

     (when stopped-thread-id-str
       (gdb--command (concat "-exec-continue --thread " stopped-thread-id-str) 'gdb--context-ignore))
     (gdb--debug-execute-body 'gdb-debug-log-commands (message "Command %s" command)))))


;; ------------------------------------------------------------------------------------------
;; Inferior I/O buffer
(define-derived-mode gdb-inferior-io-mode comint-mode "Inferior I/O"
  "Major mode for interacting with the inferior."
  :syntax-table nil :abbrev-table nil)

(gdb--simple-get-buffer gdb--inferior-io ignore
  (gdb--rename-buffer "Inferior I/O")
  (gdb-inferior-io-mode)
  (gdb--inferior-io-initialization buffer))

(defun gdb--inferior-io-initialization (buffer)
  (let* ((inferior-process (get-buffer-process (make-comint-in-buffer "GDB inferior" buffer nil)))
         (tty (or (process-get inferior-process 'remote-tty)
                  (process-tty-name inferior-process))))
    (gdb--command (concat "-inferior-tty-set " tty) 'gdb--context-ignore)
    (set-process-sentinel inferior-process #'gdb--inferior-io-sentinel)
    (add-hook 'kill-buffer-hook #'gdb--inferior-io-killed nil t)))

(defun gdb--inferior-io-killed () (gdb-kill-session))

;; NOTE(nox): When the debuggee exits, Emacs gets an EIO error and stops listening to the
;; tty. This re-inits the buffer so everything works fine!
(defun gdb--inferior-io-sentinel (process _str)
  (when (eq (process-status process) 'failed)
    (let ((buffer (process-buffer process)))
      (delete-process process)
      (when buffer (gdb--inferior-io-initialization buffer)))))


;; ------------------------------------------------------------------------------------------
;; Module API
(defun gdb--extract-context (token-string)
  "Return the context-data cons assigned to TOKEN-STRING, deleting
it from the list."
  (let* ((context (assoc token-string gdb--token-contexts))
         data)
    (when context
      (setq gdb--token-contexts (delq context gdb--token-contexts)
            context (cdr context)
            data (cdr context)
            context (car context))

      (cons (cl-loop for test-context in gdb--available-contexts
                     with result = 1
                     if   (eq context test-context) return result
                     else do (setq result (1+ result))
                     finally return 0)
            data))))

;; ------------------------------------------------------------------------------------------
;; User commands
(defun gdb-kill-session (&optional session)
  (interactive)
  (setq session (or (and (gdb--session-p session) session) (gdb--infer-session)))
  (unless session (user-error "No session in the current context."))

  (setq gdb--sessions (delq session gdb--sessions))
  (when (= (length gdb--sessions) 0) (remove-hook 'delete-frame-functions #'gdb--handle-delete-frame))

  (when (frame-live-p (gdb--session-frame session)) (delete-frame (gdb--session-frame session)))

  (cl-loop for buffer in (gdb--session-buffers session)
           do (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (let ((type (gdb--buffer-info-type gdb--buffer-info)))
                    (setq gdb--buffer-info nil)
                    (when (eq type 'gdb--inferior-io) (delete-process (get-buffer-process buffer)))
                    (unless (memq type gdb--keep-buffer-types) (kill-buffer))))))

  (delete-process (gdb--session-process session)))

;;;###autoload
(defun gdb-create-session ()
  (interactive)
  (let* ((session (make-gdb--session))
         (frame (gdb--create-frame session)))
    (push session gdb--sessions)

    (with-selected-frame frame ;; NOTE(nox): In order to have a session available
      ;; NOTE(nox): Create essential buffers
      (gdb--comint-get-buffer session)
      (gdb--inferior-io-get-buffer session)

      ;; NOTE(nox): Essential settings
      (gdb--command "-gdb-set mi-async on" 'gdb--context-ignore)
      (gdb--command "-gdb-set non-stop on" 'gdb--context-ignore))

    (gdb--setup-windows session)
    session))

;;;###autoload
(defun gdb-executable ()
  "Start debugging an executable with GDB in a new frame."
  (interactive)
  (let ((debuggee-path (expand-file-name (read-file-name "Select executable to debug: " nil nil t
                                                         gdb--previous-executable 'file-executable-p)))
        (session (or (gdb--infer-session) (gdb-create-session))))
    (setq gdb--previous-executable debuggee-path)

    (with-selected-frame (gdb--session-frame session)
      (gdb--command (concat "-file-exec-and-symbols " debuggee-path) 'gdb--context-ignore)
      (set-frame-parameter nil 'name (gdb--frame-name debuggee-path)))))

(provide 'gdb)
;;; gdb.el ends here
