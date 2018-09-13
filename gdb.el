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
(require 'emacs-gdb-module (concat default-directory "emacs-gdb-module" module-file-suffix))

;; ------------------------------------------------------------------------------------------
;; User configurable variables
(defvar gdb-debug nil
  "List of debug symbols, which will enable different components.
Possible values are:
  - `gdb-debug-timings': show timings of some function calls
  - `gdb-debug-log-commands': show which GDB commands are sent
  - `gdb-debug-raw-input': send comint input as is
  - `gdb-debug-raw-output': print GDB output as is
  - `gdb-debug-all': the same as having every debug flag")


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
  id frame process buffers source-window
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
           (gdb--buffer-info-session gdb--buffer-info))
      (frame-parameter nil 'gdb--session)))

(defun gdb--valid-session (session)
  "Returns t if SESSION is valid. Else, nil."
  (if (and (gdb--session-p session)
           (frame-live-p (gdb--session-frame session))
           (process-live-p (gdb--session-process session)))
      t
    (gdb-kill-session session)
    nil))

(defmacro gdb--with-valid-session (&rest body)
  (declare (debug (body)))
  `(let ((session (gdb--infer-session)))
     (when (gdb--valid-session session)
       (progn ,@body))))

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
         (gdb--update-buffer buffer))))))


;; ------------------------------------------------------------------------------------------
;; Utilities
(defun gdb--frame-name (&optional debuggee)
  "Return GDB frame name, possibly using DEBUGGEE file name."
  (let ((suffix (and (stringp debuggee) (file-executable-p debuggee)
                     (concat " - " (file-name-nondirectory debuggee)))))
    (concat "Emacs GDB" suffix)))

(defun gdb--debug-check (arg)
  "Check if debug ARG is enabled.
Type may be a symbol or a list of symbols and are checked against `gdb-debug'."
  (or (memq 'all gdb-debug)
      (and (listp arg) (cl-loop for type in arg when (memq type gdb-debug) return t))
      (and (symbolp arg) (memq arg gdb-debug))))

(defmacro gdb--debug-execute-body (debug-symbol &rest body)
  "Execute body when DEBUG-SYMBOL or `gdb-debug-all' is in `gdb-debug'.
DEBUG-SYMBOL may be a symbol or a list of symbols."
  (when (gdb--debug-check (eval debug-symbol)) `(progn ,@body)))

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
;; Comint buffer
(define-derived-mode gdb-comint-mode comint-mode "GDB Comint"
  "Major mode for interacting with GDB."
  (gdb--with-valid-session
   (let ((process (gdb--session-process session)))
     (set-process-sentinel process #'gdb--comint-sentinel)
     (setq-local comint-input-sender #'gdb--comint-sender)
     (setq-local comint-preoutput-filter-functions '(gdb--output-filter))

     (setq-local mode-line-process '(":%s"))

     (setq-local comint-prompt-read-only t)
     (setq-local comint-use-prompt-regexp t)
     (setq-local comint-prompt-regexp "^(gdb) ")

     (setq-local paragraph-separate "\\'")
     (setq-local paragraph-start comint-prompt-regexp))

   (gdb--command "-gdb-set mi-async on" 'gdb--context-ignore)
   (gdb--command "-gdb-set non-stop on" 'gdb--context-ignore)))

(defun gdb--comint-create-buffer (session)
  "Create the comint buffer needed for a session."
  (let ((buffer (generate-new-buffer "*GDB*")))
    (make-comint-in-buffer "GDB" buffer "gdb" nil "-i=mi")

    (setf (gdb--session-process session) (get-buffer-process buffer))
    (push buffer (gdb--session-buffers session))
    (setq gdb--buffer-info (make-gdb--buffer-info :session session :type 'gdb--comint
                                                  :update-func #'ignore))

    (with-current-buffer buffer (gdb-comint-mode))))

(defun gdb--comint-sender (process string)
  "Send user commands from comint."
  (if (gdb--debug-check 'gdb-debug-raw-input)
      (comint-simple-send process string)
    (comint-simple-send process (concat "-interpreter-exec console " (gdb--escape-argument string)))))

(defun gdb--output-filter (string)
  "Parse GDB/MI output."
  (let ((output (gdb--measure-time "Handle MI Output" (gdb--handle-mi-output string))))
    (gdb--update)
    (gdb--debug-execute-body '(gdb-debug-timings gdb-debug-log-commands) (message "--------------------"))
    (if (gdb--debug-check 'gdb-debug-raw-output)
        (concat string "--------------------\n")
      output)))

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
   (let* ((process (gdb--get-comint-process))
          (context-type (or (when (consp context) (car context)) context))
          token stopped-thread-id-str)
     (when process
       (when (memq context-type gdb--available-contexts)
         (setq token (int-to-string gdb--next-token)
               command (concat token command)
               gdb--next-token (1+ gdb--next-token))
         (when (not (consp context)) (setq context (cons context-type nil)))
         (push (cons token context) gdb--token-contexts)))
     (when (and force-stopped (> (length gdb--threads) 0))
       (when (not (catch 'break
                    (dolist (thread gdb--threads)
                      (when (string= "stopped" (gdb--thread-state (cdr thread)))
                        (throw 'break t)))))
         (setq stopped-thread-id-str (int-to-string (car (car gdb--threads))))
         (gdb--command (concat "-exec-interrupt --thread " stopped-thread-id-str))))
     (comint-simple-send process command)
     (when stopped-thread-id-str
       (gdb--command (concat "-exec-continue --thread " stopped-thread-id-str)))
     (when (gdb--debug-check 'gdb-debug-log-commands) (message "Command %s" command)))))


;; ------------------------------------------------------------------------------------------
;; Frame setup
(defun gdb--create-frame (session)
  (let ((frame (make-frame `((fullscreen . maximized)
                             (gdb--session . ,session)
                             (name . ,(gdb--frame-name))))))
    (setf (gdb--session-frame session) frame)
    (add-hook 'delete-frame-functions #'gdb--handle-delete-frame)))

(defun gdb--handle-delete-frame (_)
  (let ((session (gdb--infer-session)))
    (when (gdb--session-p session)
      (gdb-kill-session session))))

(defun gdb--setup-windows (_session) ;; TODO
  )


;; ------------------------------------------------------------------------------------------
;; User commands
(defun gdb-kill-session (&optional session)
  (interactive)
  (setq session (or (and (gdb--session-p session) session)
                    (gdb--infer-session)))
  (unless (gdb--session-p session) (user-error "No session in the current context."))

  (setq gdb--sessions (delq session gdb--sessions))
  (when (= (length gdb--sessions) 0) (remove-hook 'delete-frame-functions 'gdb--handle-delete-frame))

  (cl-loop for buffer in (gdb--session-buffers session)
           do (with-current-buffer buffer
                (let ((type (gdb--buffer-info-type gdb--buffer-info)))
                  (setq gdb--buffer-info nil)
                  (unless (memq type gdb--keep-buffer-types) (kill-buffer))))))

;;;###autoload
(defun gdb-create-session ()
  (interactive)
  (let* ((session (make-gdb--session)))
    (gdb--create-frame session)
    (gdb--comint-create-buffer session)
    (gdb--setup-windows session)))

;;;###autoload
(defun gdb-executable ()
  "Start debugging an executable with GDB in a new frame."
  (interactive)
  (let ((debuggee-path (expand-file-name (read-file-name "Select executable to debug: " nil nil t
                                                         gdb--previous-executable 'file-executable-p)))
        (_session (gdb-create-session)))
    (setq gdb--previous-executable debuggee-path)))

(provide 'gdb)
;;; gdb.el ends here
