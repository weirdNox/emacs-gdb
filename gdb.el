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

(cl-defstruct gdb--thread id target-id name state frames core)
(cl-defstruct gdb--frame thread addr func file line from)

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
  threads current-frame
  breakpoints)
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
(defsubst gdb--infer-session (&optional only-from-buffer)
  (or (and (gdb--buffer-info-p gdb--buffer-info)
           (gdb--session-p (gdb--buffer-info-session gdb--buffer-info))
           (gdb--buffer-info-session gdb--buffer-info))
      (and (not only-from-buffer)
           (gdb--session-p (frame-parameter nil 'gdb--session))
           (frame-parameter nil 'gdb--session))))

(defun gdb--valid-session (session)
  "Returns t if SESSION is valid. Else, nil."
  (when (gdb--session-p session)
    (if (and (frame-live-p (gdb--session-frame session))
             (process-live-p (gdb--session-process session)))
        t
      (gdb--kill-session session)
      nil)))

(defmacro gdb--with-valid-session (&rest body)
  (declare (debug ([&optional stringp] body)))
  (let ((message (and (stringp (car body)) (car body))))
    (when message (setq body (cdr body)))
   `(let ((session (gdb--infer-session)))
     (if (gdb--valid-session session)
         (progn ,@body)
       ,(when message `(error "%s" ,message))))))

(defun gdb--kill-session (session)
  (when (and (gdb--session-p session) (memq session gdb--sessions))
    (setq gdb--sessions (delq session gdb--sessions))
    (when (= (length gdb--sessions) 0) (remove-hook 'delete-frame-functions #'gdb--handle-delete-frame))

    (when (frame-live-p (gdb--session-frame session))
      (set-frame-parameter (gdb--session-frame session) 'gdb--session nil)
      (when (> (length (frame-list)) 0)
        (delete-frame (gdb--session-frame session))))

    (delete-process (gdb--session-process session))
    (dolist (buffer (gdb--session-buffers session))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((type (gdb--buffer-info-type gdb--buffer-info)))
            (when (eq type 'gdb--inferior-io) (delete-process (get-buffer-process buffer)))
            (if (memq type gdb--keep-buffer-types)
                (setq gdb--buffer-info nil)
              (kill-buffer))))))))

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

(defsubst gdb--current-line () (string-to-number (format-mode-line "%l")))


;; ------------------------------------------------------------------------------------------
;; Tables
(defsubst gdb--pad-string (string padding) (format (concat "%" (number-to-string padding) "s") string))

(cl-defstruct gdb--table
  (column-sizes nil)
  (rows nil)
  (row-properties nil)
  (right-align nil))

(defun gdb--table-add-row (table row &optional properties)
  "Add ROW, a list of strings, to TABLE and recalculate column sizes.
When non-nil, PROPERTIES will be added to the whole row when
calling `gdb--table-string'."
  (let ((rows (gdb--table-rows table))
        (row-properties (gdb--table-row-properties table))
        (column-sizes (gdb--table-column-sizes table))
        (right-align (gdb--table-right-align table)))
    (when (not column-sizes)
      (setf (gdb--table-column-sizes table)
            (make-list (length row) 0)))
    (setf (gdb--table-rows table)
          (append rows (list row)))
    (setf (gdb--table-row-properties table)
          (append row-properties (list properties)))
    (setf (gdb--table-column-sizes table)
          (cl-mapcar (lambda (x s)
                       (let ((new-x
                              (max (abs x) (string-width (or s "")))))
                         (if right-align new-x (- new-x))))
                     (gdb--table-column-sizes table)
                     row))
    ;; Avoid trailing whitespace at eol
    (if (not (gdb--table-right-align table))
        (setcar (last (gdb--table-column-sizes table)) 0))))

(defun gdb--table-string (table &optional sep)
  "Return TABLE as a string with columns separated with SEP."
  (let ((column-sizes (gdb--table-column-sizes table)))
    (mapconcat
     'identity
     (cl-mapcar
      (lambda (row properties)
        (apply 'propertize
               (mapconcat 'identity (cl-mapcar (lambda (s x) (gdb--pad-string s x)) row column-sizes) sep)
               properties))
      (gdb--table-rows table)
      (gdb--table-row-properties table))
     "\n")))


;; ------------------------------------------------------------------------------------------
;; Buffers
(defun gdb--get-buffer-with-type (session type)
  (cl-loop for buffer in (gdb--session-buffers session)
           when (eq (gdb--buffer-info-type (buffer-local-value 'gdb--buffer-info buffer)) type)
           return buffer
           finally return nil))

(defmacro gdb--simple-get-buffer (type update-func &rest body)
  "Simple buffer creator/fetcher, for buffers that should be unique in a session."
  (declare (indent defun) (debug (sexp sexp body)))
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
  `(save-match-data
     (let ((old-name (buffer-name)))
       (string-match "[ ]+-[ ]+\\(.+\\)\\*\\(<[0-9]+>\\)?$" old-name)
       (rename-buffer (concat ,(concat "*GDB" (when specific-str (concat ": " specific-str)))
                              (when (match-string 1 old-name) (concat " - " (match-string 1 old-name)))
                              "*")
                      t))))

(defun gdb--rename-buffers-with-debuggee (debuggee-path)
  (let* ((debuggee-name (file-name-nondirectory debuggee-path))
         (replacement (concat " - " debuggee-name "*")))
    (dolist (buffer (gdb--session-buffers (gdb--infer-session)))
      (with-current-buffer buffer
        (rename-buffer (replace-regexp-in-string "\\([ ]+-.+\\)?\\*\\(<[0-9]+>\\)?$"
                                                 replacement (buffer-name) t)
                       t)))))

(defun gdb--important-buffer-kill-cleanup () (gdb--kill-session (gdb--infer-session t)))


;; ------------------------------------------------------------------------------------------
;; Frames and windows
(defun gdb--frame-name (&optional debuggee)
  "Return GDB frame name, possibly using DEBUGGEE file name."
  (let ((suffix (and (stringp debuggee) (file-executable-p debuggee)
                     (concat " - " (abbreviate-file-name debuggee)))))
    (concat "Emacs GDB" suffix)))

(defun gdb--create-frame (session)
  (let ((frame (make-frame `((fullscreen . maximized)
                             (gdb--session . ,session)
                             (name . ,(gdb--frame-name))))))
    (setf (gdb--session-frame session) frame)
    (add-hook 'delete-frame-functions #'gdb--handle-delete-frame)
    frame))

(defun gdb--handle-delete-frame (frame)
  (let ((session (frame-parameter frame 'gdb--session)))
    (when (gdb--session-p session) (gdb--kill-session session))))

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
           (_top-right (split-window top-left nil t))
           (middle-right (split-window middle-left nil t))
           (_bottom-right (split-window bottom-left nil t)))
      (balance-windows)
      (gdb--set-window-buffer top-left (gdb--comint-get-buffer session))
      (gdb--set-window-buffer middle-right (gdb--inferior-io-get-buffer session))
      (setf (gdb--session-source-window session) middle-left))))

(defun gdb--scroll-buffer-to-line (buffer line)
  (let ((windows (get-buffer-window-list buffer nil t)))
    (dolist (window windows)
      (with-selected-window window
        (goto-char (point-min))
        (forward-line (1- line))))))


;; ------------------------------------------------------------------------------------------
;; Comint buffer
(define-derived-mode gdb-comint-mode comint-mode "GDB Comint"
  "Major mode for interacting with GDB."
  (set-process-sentinel (get-buffer-process (current-buffer)) #'gdb--comint-sentinel)
  (setq-local comint-input-sender #'gdb--comint-sender)
  (setq-local comint-preoutput-filter-functions '(gdb--output-filter))

  (setq-local comint-prompt-read-only nil)
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-regexp "^(gdb)[ ]+")

  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp))

(gdb--simple-get-buffer gdb--comint ignore
  (gdb--rename-buffer "Comint")
  (make-comint-in-buffer "GDB" buffer "gdb" nil "-i=mi" "-nx")
  (gdb-comint-mode)
  (setf (gdb--session-process session) (get-buffer-process buffer))
  (add-hook 'kill-buffer-hook #'gdb--important-buffer-kill-cleanup nil t))

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

(defun gdb--comint-sentinel (process str)
  "Handle GDB comint process state changes."
  (let* ((buffer (process-buffer process)))
    (when (and (or (eq (process-status process) 'exit)
                   (string= str "killed\n"))
               buffer)
      (with-current-buffer buffer
        (gdb--kill-session (gdb--infer-session t))))))

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
    (add-hook 'kill-buffer-hook #'gdb--important-buffer-kill-cleanup nil t)))

;; NOTE(nox): When the debuggee exits, Emacs gets an EIO error and stops listening to the
;; tty. This re-inits the buffer so everything works fine!
(defun gdb--inferior-io-sentinel (process str)
  (let ((process-status (process-status process))
        (buffer (process-buffer process)))
    (cond ((eq process-status 'failed)
           (delete-process process)
           (when buffer (gdb--inferior-io-initialization buffer)))
          ((and (string= str "killed\n") buffer)
           (with-current-buffer buffer (gdb--kill-session (gdb--infer-session t)))))))


;; ------------------------------------------------------------------------------------------
;; Threads buffer
(define-derived-mode gdb--threads-mode nil "GDB Threads"
  (setq-local buffer-read-only t)
  (buffer-disable-undo))

(gdb--simple-get-buffer gdb--threads gdb--threads-update
  (gdb--rename-buffer "Threads")
  (gdb--threads-mode))

(defun gdb--threads-update ()
  (gdb--with-valid-session
   (let ((threads (gdb--session-threads session))
         (current-thread (gdb--frame-thread (gdb--session-current-frame session)))
         (cursor-on-thread (get-text-property (point) 'gdb--thread))
         (cursor-on-line (gdb--current-line))
         (table (make-gdb--table))
         (count 1) current-thread-line)
     (gdb--table-add-row table '("ID" "TgtID" "Name" "State" "Core" "Frame"))
     (dolist (thread threads)
       (let ((id-str (number-to-string (gdb--thread-id thread)))
             (target-id (gdb--thread-target-id thread))
             (name (gdb--thread-name thread))
             (state-display
              (if (string= (gdb--thread-state thread) "running")
                  (eval-when-compile (propertize "running" 'font-lock-face font-lock-string-face))
                (eval-when-compile (propertize "stopped" 'font-lock-face font-lock-warning-face))))
             (frame (gdb--threads-frame-string (car (gdb--thread-frames thread))))
             (core (gdb--thread-core thread)))
         (gdb--table-add-row table (list id-str target-id name state-display core frame) `(gdb--thread ,thread))
         (setq count (1+ count))
         (when (eq current-thread thread) (setq current-thread-line count))
         (when (eq cursor-on-thread thread) (setq cursor-on-line count))))

     (erase-buffer)
     (insert (gdb--table-string table " "))
     (gdb--scroll-buffer-to-line (current-buffer) cursor-on-line)

     (remove-overlays nil nil 'gdb--thread-indicator t)
     (when current-thread-line
       (gdb--place-symbol (current-buffer) current-thread-line '((type . thread-indicator)))))))

(defun gdb--threads-frame-string (frame)
  (cond
   (frame
    (let ((addr (gdb--frame-addr frame))
          (func (gdb--frame-func frame))
          (file (gdb--frame-file frame))
          (line (gdb--frame-line frame))
          (from (gdb--frame-from frame)))
      (when file (setq file (file-name-nondirectory file)))
      (concat "in " (propertize (or func "???") 'font-lock-face font-lock-function-name-face) " at " addr
              (or (and file line (concat " of " file ":" line))
                  (and from (concat " of " from))))))
   (t "No information")))


;; ------------------------------------------------------------------------------------------
;; Source buffers
(defun gdb--find-file (path)
  "Return the buffer of the file specified by PATH.
Create the buffer, if it wasn't already open."
  (when (and path (not (file-directory-p path)) (file-readable-p path))
    (find-file-noselect path t)))

(defun gdb--display-source-buffer (file line &optional no-mark)
  "Display buffer of the selected source."
  (gdb--with-valid-session
   (let ((buffer (gdb--find-file file))
         (window (gdb--session-source-window session)))
     (gdb--remove-all-symbols 'gdb--source-indicator t)
     (unless no-mark (gdb--place-symbol buffer line '((type . source-indicator))))

     (when (and (window-live-p window) buffer)
       (with-selected-window window
         (switch-to-buffer buffer)
         (if (display-images-p)
             (set-window-fringes nil 8)
           (set-window-margins nil 2))
         (goto-char (point-min))
         (forward-line (1- line))
         (recenter 3))))))


;; ------------------------------------------------------------------------------------------
;; Fringe symbols
(defun gdb--place-symbol (_buffer _line _data) ;; TODO
  ;; (when (and buffer line data)
  ;;   (with-current-buffer buffer
  ;;     (let* ((type (alist-get 'type data))
  ;;            (pos (line-beginning-position (1+ (- line (line-number-at-pos)))))
  ;;            (overlay (make-overlay pos pos buffer))
  ;;            (dummy-string (make-string 1 ?x))
  ;;            property)
  ;;       (overlay-put overlay 'gdb--indicator t)
  ;;       (overlay-put overlay (intern (concat "gdb--" (symbol-name type))) t)
  ;;       (cond ((eq type 'breakpoint-indicator)
  ;;              (let ((number (alist-get 'number data))
  ;;                    (enabled (alist-get 'enabled data)))
  ;;                (overlay-put overlay 'gdb--breakpoint-number number)
  ;;                (if (display-images-p)
  ;;                    (setq property `(left-fringe gdb--breakpoint ,(if enabled
  ;;                                                                      'gdb--breakpoint-enabled
  ;;                                                                    'gdb--breakpoint-disabled)))
  ;;                  (setq property `((margin left-margin) ,(if enabled "B" "b"))))))
  ;;             ((memq type '(source-indicator frame-indicator thread-indicator))
  ;;              (if (display-images-p)
  ;;                  (setq property '(left-fringe right-triangle))
  ;;                (setq property '((margin left-margin) "=>")))))
  ;;       (when (alist-get 'source data)
  ;;         (overlay-put overlay 'window (gdb--local gdb--source-window)))
  ;;       (put-text-property 0 1 'display property dummy-string)
  ;;       (overlay-put overlay 'before-string dummy-string))))
  )

(defun gdb--remove-all-symbols (type &optional source-files-only)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless (and source-files-only gdb--buffer-info)
        (remove-overlays nil nil type t)))))


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

(defun gdb--set-initial-file (file line-string)
   (gdb--display-source-buffer file (string-to-number line-string) t))


;; ------------------------------------------------------------------------------------------
;; User commands
(defun gdb-kill-session ()
  "Kill current GDB session."
  (interactive)
  (gdb--kill-session (gdb--infer-session)))

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
      (gdb--command "-file-list-exec-source-file" 'gdb--context-initial-file)
      (set-frame-parameter nil 'name (gdb--frame-name debuggee-path))
      (gdb--rename-buffers-with-debuggee debuggee-path))))

(provide 'gdb)
;;; gdb.el ends here
