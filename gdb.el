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
  - `timings': show timings of some function calls
  - `commands': show which GDB commands are sent
  - `raw-input': send comint input as is
  - `raw-output': print GDB/MI output to the messages buffer

This can also be set to t, which means that all debug components are active.")


;; ------------------------------------------------------------------------------------------
;; Private constants and variables
(defvar gdb--previous-executable nil
  "Previous executable path.")

(defconst gdb--available-contexts
  '(gdb--context-ignore
    gdb--context-initial-file
    gdb--context-thread-info
    gdb--context-frame-info      ;; Data: Thread
    gdb--context-breakpoint-insert
    gdb--context-get-variables   ;; Data: Frame
    gdb--context-create-variable
    gdb--context-disassemble     ;; Data: Disassemble buffer
    )
  "List of implemented token contexts.
Must be in the same order of the `token_context' enum in the
dynamic module.")

(defconst gdb--buffer-types
  '(gdb--comint
    gdb--inferior-io
    gdb--threads
    gdb--frames
    gdb--breakpoints
    gdb--variables
    gdb--watcher
    gdb--disassembly
    gdb--registers)
  "List of available buffer types.")

(defconst gdb--keep-buffer-types '(gdb--comint gdb--inferior-io)
  "List of buffer types that should be kept after GDB is killed.")

(cl-defstruct gdb--thread id target-id name state frames core)
(cl-defstruct gdb--frame thread level addr func file line from variables)
(cl-defstruct gdb--variable name type value)

(cl-defstruct gdb--breakpoint number type disp enabled addr hits what file line)
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
  threads selected-thread selected-frame
  breakpoints watched-variables)
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
;; Faces and bitmaps
(define-fringe-bitmap 'gdb--fringe-breakpoint "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")

(defface gdb--breakpoint-enabled
  '((t :foreground "red1" :weight bold))
  "Face for enabled breakpoint icon in fringe.")

(defface gdb--breakpoint-disabled
  '((((class color) (min-colors 88)) :foreground "gray70")
    (((class color) (min-colors 8) (background light)) :foreground "black")
    (((class color) (min-colors 8) (background dark)) :foreground "white")
    (((type tty) (class mono)) :inverse-video t)
    (t :background "gray"))
  "Face for disabled breakpoint icon in fringe.")

;; (defconst gdb--disassembly-font-lock-keywords
;;   '(;; 0xNNNNNNNN opcode
;;     ("^0x[[:xdigit:]]+[[:space:]]+\\(\\sw+\\)"
;;      (1 font-lock-keyword-face))
;;     ;; Hexadecimals
;;     ("0x[[:xdigit:]]+" . font-lock-constant-face)
;;     ;; Source lines
;;     ("^Line.*$" . font-lock-comment-face)
;;     ;; %register(at least i386)
;;     ("%\\sw+" . font-lock-variable-name-face)
;;     ;; <FunctionName+Number>
;;     ("<\\([^()[:space:]+]+\\)\\(([^>+]*)\\)?\\(\\+[0-9]+\\)?>"
;;      (1 font-lock-function-name-face)))
;;   "Font lock keywords used in `gdb--disassembly'.")

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

    (set-process-sentinel (gdb--session-process session) nil)
    (delete-process (gdb--session-process session))
    (dolist (buffer (gdb--session-buffers session))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((type (gdb--buffer-info-type gdb--buffer-info)))
            (when (eq type 'gdb--inferior-io)
              (set-process-sentinel (get-buffer-process buffer) nil)
              (delete-process (get-buffer-process buffer)))

            (if (memq type gdb--keep-buffer-types)
                (setq gdb--buffer-info nil)
              (kill-buffer))))))

    (gdb--remove-all-symbols session 'all)))

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

(defun gdb--get-thread-by-id (id)
  (gdb--with-valid-session
   (cl-loop for thread in (gdb--session-threads session)
            when (= (gdb--thread-id thread) id) return thread)))

(defun gdb--switch-to-thread (thread)
  "Unconditionally switch to _different_ THREAD. This will also switch to the top level frame.
THREAD may be nil, which means to remove the selected THREAD."
  (gdb--with-valid-session
   (unless (eq thread (gdb--session-selected-thread session))
     (setf (gdb--session-selected-thread session) thread)
     (gdb--switch-to-frame (and thread (car (gdb--thread-frames thread))))

     (with-current-buffer (gdb--get-buffer-with-type session 'gdb--threads)
       (remove-overlays nil nil 'gdb--thread-indicator t)
       (when thread
         (let ((pos (text-property-any (point-min) (point-max) 'gdb--thread thread)))
           (when pos
             (gdb--place-symbol session (current-buffer) (line-number-at-pos pos) '((type . thread-indicator)))))

         (message "Switched to thread %s." (gdb--thread-id thread))))

     (cl-pushnew 'gdb--frames (gdb--session-buffer-types-to-update session)))))

(defun gdb--switch-to-frame (frame)
  "Unconditionally switch to a _different_ FRAME.
When FRAME is in a different thread, switch to it."
  (gdb--with-valid-session
   (unless (eq frame (gdb--session-selected-frame session))
     (setf (gdb--session-selected-frame session) frame)

     (when frame (gdb--switch-to-thread (gdb--frame-thread frame)))

     (if (and frame (not (gdb--frame-variables frame)))
         (gdb--command "-stack-list-variables --simple-values" (cons 'gdb--context-get-variables frame) frame)
       (cl-pushnew 'gdb--variables (gdb--session-buffer-types-to-update session)))

     (let ((file (and frame (gdb--frame-file frame)))
           (line (and frame (gdb--frame-line frame))))
       (if (and file line)
           (gdb--display-source-buffer file line)
         (gdb--remove-all-symbols session 'gdb--source-indicator t))

       (with-current-buffer (gdb--get-buffer-with-type session 'gdb--frames)
         (remove-overlays nil nil 'gdb--frame-indicator t)
         (when frame
           (let ((pos (text-property-any (point-min) (point-max) 'gdb--frame frame)))
             (when pos
               (gdb--place-symbol session (current-buffer) (line-number-at-pos pos)
                                  '((type . frame-indicator)))))))))))

(defun gdb--conditional-switch (frame-or-thread &optional cause)
  "Conditionally switch to a _different_ FRAME-OR-THREAD depending on CAUSE.
This will _always_ switch when no thread is selected.

CAUSE should be a list of the following symbols:
- `running': Switch when selected thread is running and is different from THREAD
- `same-thread' (only for frames): Switch when same thread

When the thread is switched, the current frame will also be changed."
  (gdb--with-valid-session
   (cl-assert (or (gdb--thread-p frame-or-thread) (gdb--frame-p frame-or-thread) (not frame-or-thread)))
   (let* ((type (type-of frame-or-thread))
          (thread (if (eq type 'gdb--thread) frame-or-thread (gdb--frame-thread frame-or-thread)))
          (frame  (if (eq type 'gdb--frame)  frame-or-thread (car (gdb--thread-frames frame-or-thread))))
          (selected-thread (gdb--session-selected-thread session))
          (condition (or (not selected-thread)
                         (and (memq 'running cause)
                              (string= "running" (gdb--thread-state selected-thread)))
                         (and (eq type 'gdb--frame) (memq 'same-thread cause)
                              (eq thread selected-thread)))))
     (when condition (gdb--switch-to-frame frame)))))


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
  `(if (gdb--debug-check 'timings)
       (progn
         (message (concat "Starting measurement: " ,string))
         (let ((time (current-time))
               (result (progn ,@body)))
           (message "GDB TIME MEASUREMENT: %s - %.06fs" ,string (float-time (time-since time)))
           result))
     (progn ,@body)))

(defsubst gdb--current-line ()
  "Return an integer of the current line of point in the current buffer."
  (save-restriction (widen) (save-excursion (beginning-of-line)
                                            (1+ (count-lines 1 (point))))))

(defmacro gdb--update-struct (type struct &rest pairs)
  (declare (indent defun))
  `(progn ,@(cl-loop for (key val) in pairs
                     collect `(setf (,(intern (concat (symbol-name type) "-" (symbol-name key))) ,struct) ,val))))

(defun gdb--location-string (&optional func file line from addr)
  (when file (setq file (file-name-nondirectory file)))
  (concat "in " (propertize (or func "??") 'font-lock-face font-lock-function-name-face)
          (and addr (concat " at " addr))
          (or (and file line (format " of %s:%d" file line))
              (and from (concat " of " from)))))

(defun gdb--frame-location-string (frame &optional for-threads-view)
  (cond (frame (gdb--location-string (gdb--frame-func frame) (gdb--frame-file frame) (gdb--frame-line frame)
                                     (gdb--frame-from frame) (and for-threads-view (gdb--frame-addr frame))))
        (t "No information")))


;; ------------------------------------------------------------------------------------------
;; Tables
(defsubst gdb--pad-string (string padding) (format (concat "%" (number-to-string padding) "s") (or string "")))

(cl-defstruct gdb--table column-sizes rows row-properties right-align)

(defun gdb--table-add-row (table row &optional properties)
  "Add ROW, a list of strings, to TABLE and recalculate column sizes.
When non-nil, PROPERTIES will be added to the whole row when printing."
  (let ((right-align (gdb--table-right-align table)))
    (unless (gdb--table-column-sizes table)
      (setf (gdb--table-column-sizes table) (make-list (length row) 0)))

    (setf (gdb--table-rows           table) (append (gdb--table-rows           table) (list row)))
    (setf (gdb--table-row-properties table) (append (gdb--table-row-properties table) (list properties)))

    (setf (gdb--table-column-sizes table)
          (cl-loop for size in (gdb--table-column-sizes table) and string in row
                   collect (let ((new-size (max (abs size) (string-width (or string "")))))
                             (if right-align new-size (- new-size)))))

    (unless right-align (setcar (last (gdb--table-column-sizes table)) 0))))

(defun gdb--table-row-string (row column-sizes properties sep)
  "Return ROW as a string, with PROPERTIES and respective COLUMN-SIZES, separated by SEP."
  (apply #'propertize (cl-loop for string in row
                               and size   in column-sizes
                               and first = t then nil
                               unless first concat sep
                               concat (gdb--pad-string string size))
         properties))

(defun gdb--table-insert (table &optional with-header sep)
  "Erase buffer and insert TABLE with columns separated with SEP (space as default).
If WITH-HEADER is set, then the first row is used as header. The table contents _are modified_."
  (let ((column-sizes (gdb--table-column-sizes table))
        (sep (or sep " ")))
    (erase-buffer)

    (when with-header
      (setq-local header-line-format
                  (list " " (gdb--table-row-string (pop (gdb--table-rows table)) column-sizes
                                                   (pop (gdb--table-row-properties table)) sep))))

    (cl-loop for row        in (gdb--table-rows table)
             and properties in (gdb--table-row-properties table)
             and first = t then nil
             unless first do (insert "\n")
             do (insert (gdb--table-row-string row column-sizes properties sep)))))


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
           (top-right (split-window top-left nil t))
           (middle-right (split-window middle-left nil t))
           (bottom-right (split-window bottom-left nil t)))
      (balance-windows)
      (gdb--set-window-buffer top-left     (gdb--comint-get-buffer session))
      (gdb--set-window-buffer top-right    (gdb--frames-get-buffer session))
      (gdb--set-window-buffer middle-right (gdb--inferior-io-get-buffer session))
      (gdb--set-window-buffer bottom-left  (gdb--variables-get-buffer session))
      (gdb--set-window-buffer bottom-right (gdb--threads-get-buffer session))
      (setf (gdb--session-source-window session) middle-left))))

(defun gdb--scroll-buffer-to-line (buffer line)
  (dolist (window (get-buffer-window-list buffer nil t))
    (with-selected-window window
      (goto-char (point-min))
      (forward-line (1- line)))))


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
  (let ((process-connection-type nil))
    (make-comint-in-buffer "GDB" buffer "gdb" nil "-i=mi" "-nx"))
  (gdb-comint-mode)
  (setf (gdb--session-process session) (get-buffer-process buffer))
  (add-hook 'kill-buffer-hook #'gdb--important-buffer-kill-cleanup nil t))

(defun gdb--comint-sender (_process string)
  "Send user commands from comint."
  (if (gdb--debug-check 'raw-input)
      (gdb--command string nil)
    (gdb--command (concat "-interpreter-exec console " (gdb--escape-argument string)))))

(defun gdb--output-filter (string)
  "Parse GDB/MI output."
  (gdb--debug-execute-body 'raw-output (message "%s" string))
  (let ((output (gdb--measure-time "Handle MI Output" (gdb--handle-mi-output string))))
    (gdb--update)
    (gdb--debug-execute-body '(timings commands raw-output)
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

(defun gdb--command (command &optional context thread-or-frame force-stopped)
  "Execute COMMAND in GDB.
If provided, the CONTEXT is assigned to a unique token, which
will be received, alongside the output, by the dynamic module,
and used to know what the context of that output was. CONTEXT may
be a cons (CONTEXT-TYPE . DATA), where DATA is anything relevant
for the context, or just CONTEXT-TYPE. CONTEXT-TYPE must be a
member of `gdb--available-contexts'.

If THREAD-or-FRAME is:
  a thread/frame: the command will run on that thread/frame
      an integer: the command will run on the thread with that ID
               t: the command will run on the selected thread/frame, when available
             nil: the command will run without specifying any thread/frame

When FORCE-STOPPED is non-nil, ensure that exists at least one
stopped thread before running the command."
  (gdb--with-valid-session
   "Could not run command because no session is available"
   (let* ((command-parts (and command (split-string command)))
          (context-type (or (and (consp context) (car context)) context))
          (threads (gdb--session-threads session))
          (in-frame (cond ((eq (type-of thread-or-frame) 'gdb--frame) thread-or-frame)
                          ((eq (type-of thread-or-frame) 'gdb--thread) (car (gdb--thread-frames thread-or-frame)))
                          ((eq thread-or-frame t) (gdb--session-selected-frame session))))
          (in-thread (cond (in-frame (gdb--frame-thread in-frame))
                           ((eq (type-of thread-or-frame) 'gdb--thread) thread-or-frame)
                           ((integerp thread-or-frame) (or (gdb--get-thread-by-id thread-or-frame)
                                                           (make-gdb--thread :id thread-or-frame)))
                           ((eq thread-or-frame t) (gdb--session-selected-thread session))))
          token stopped-thread)
     (when (memq context-type gdb--available-contexts)
       (setq token (number-to-string gdb--next-token)
             gdb--next-token (1+ gdb--next-token))
       (when (not (consp context)) (setq context (cons context-type nil)))
       (push (cons token context) gdb--token-contexts))

     (when (and force-stopped threads (not (cl-loop for thread in threads
                                                    when (string= "stopped" (gdb--thread-state thread))
                                                    return t)))
       (setq stopped-thread (or in-thread (car threads)))
       (gdb--command "-exec-interrupt" 'gdb--context-ignore stopped-thread))

     (setq command (concat token (car command-parts)
                           (and in-thread (format " --thread %d" (gdb--thread-id   in-thread)))
                           (and in-frame  (format " --frame  %d" (gdb--frame-level in-frame)))
                           " " (mapconcat #'identity (cdr command-parts) " ")))
     (gdb--debug-execute-body 'commands (message "Command %s" command))
     (comint-simple-send (gdb--session-process session) command)

     (when stopped-thread (gdb--command "-exec-continue" 'gdb--context-ignore stopped-thread)))))

(defun gdb--infer-thread-or-frame ()
  ;; (when (not thread)
  ;;   (cond ((eq gdb--buffer-type 'gdb--threads)
  ;;          (setq thread (get-text-property (point) 'gdb--thread-id))
  ;;          (when thread (setq thread (int-to-string thread))))
  ;;         ((eq gdb--buffer-type 'gdb--frames)
  ;;          (setq thread (or gdb--thread-id (gdb--local gdb--thread-id)))
  ;;          (setq frame (get-text-property (point) 'gdb--frame-level))
  ;;          (when thread
  ;;            (setq thread (int-to-string thread))
  ;;            (when frame (setq frame (int-to-string frame)))))
  ;;         ((eq gdb--buffer-type 'gdb--disassembly) ;; TODO(nox): Do this when disassembly is done
  ;;          )
  ;;         (t (setq thread (gdb--local (and gdb--thread-id
  ;;                                          (int-to-string gdb--thread-id)))))))
  ;; (gdb--command (concat command
  ;;                       (and thread (concat " --thread " thread))
  ;;                       (and thread frame (concat " --frame " frame)))
  ;;               context)
  )


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
           (set-process-sentinel process nil)
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
         (selected-thread (gdb--session-selected-thread session))
         (cursor-on-thread (get-text-property (point) 'gdb--thread))
         (cursor-on-line   (gdb--current-line))
         (table (make-gdb--table))
         (count 1) selected-thread-line)
     (gdb--table-add-row table '("ID" "TgtID" "Name" "State" "Core" "Frame"))
     (dolist (thread threads)
       (let ((id-str (number-to-string (gdb--thread-id thread)))
             (target-id (gdb--thread-target-id thread))
             (name (gdb--thread-name thread))
             (state-display
              (if (string= (gdb--thread-state thread) "running")
                  (eval-when-compile (propertize "running" 'font-lock-face font-lock-string-face))
                (eval-when-compile (propertize "stopped" 'font-lock-face font-lock-warning-face))))
             (frame-str (gdb--frame-location-string (car (gdb--thread-frames thread)) t))
             (core (gdb--thread-core thread)))
         (gdb--table-add-row table (list id-str target-id name state-display core frame-str)
                             `(gdb--thread ,thread))
         (setq count (1+ count))
         (when (eq selected-thread thread) (setq selected-thread-line count))
         (when (eq cursor-on-thread thread) (setq cursor-on-line count))))

     (remove-overlays nil nil 'gdb--thread-indicator t)
     (gdb--table-insert table t)
     (gdb--scroll-buffer-to-line (current-buffer) cursor-on-line)

     (when selected-thread-line
       (gdb--place-symbol session (current-buffer) selected-thread-line '((type . thread-indicator)))))))


;; ------------------------------------------------------------------------------------------
;; Stack frames buffer
(define-derived-mode gdb--frames-mode nil "GDB Frames"
  (setq-local buffer-read-only t)
  (buffer-disable-undo))

(gdb--simple-get-buffer gdb--frames gdb--frames-update
  (gdb--rename-buffer "Stack Frames")
  (gdb--frames-mode))

(defun gdb--frames-update ()
  (gdb--with-valid-session
   (let* ((thread (gdb--session-selected-thread session))
          (frames (when thread (gdb--thread-frames thread)))
          (selected-frame (gdb--session-selected-frame session))
          (cursor-on-frame (get-text-property (point) 'gdb--frame))
          (cursor-on-line  (gdb--current-line))
          (table (make-gdb--table))
          (count 1) selected-frame-line)
     (gdb--table-add-row table '("Level" "Address" "Where"))
     (dolist (frame frames)
       (let ((level-str (number-to-string (gdb--frame-level frame)))
             (addr (gdb--frame-addr frame))
             (where (gdb--frame-location-string frame)))
         (gdb--table-add-row table (list level-str addr where) (list 'gdb--frame frame))
         (setq count (1+ count))
         (when (eq selected-frame frame)  (setq selected-frame-line count))
         (when (eq cursor-on-frame frame) (setq cursor-on-line count))))

     (remove-overlays nil nil 'gdb--frame-indicator t)
     (gdb--table-insert table t)
     (gdb--scroll-buffer-to-line (current-buffer) cursor-on-line)

     (when selected-frame-line
       (gdb--place-symbol session (current-buffer) selected-frame-line '((type . frame-indicator)))))))


;; ------------------------------------------------------------------------------------------
;; Breakpoints buffer
(define-derived-mode gdb--breakpoints-mode nil "GDB Breakpoints"
  (setq-local buffer-read-only t)
  (buffer-disable-undo))

(gdb--simple-get-buffer gdb--breakpoints gdb--breakpoints-update
  (gdb--rename-buffer "Breakpoints")
  (gdb--breakpoints-mode))

(defun gdb--breakpoints-update ()
  (gdb--with-valid-session
   (let ((breakpoints (gdb--session-breakpoints session))
         (table (make-gdb--table)))
     (gdb--table-add-row table '("Num" "Type" "Disp" "Enb" "Addr" "Hits" "What"))
     (dolist (breakpoint breakpoints)
       (let ((enabled-disp (if (gdb--breakpoint-enabled breakpoint)
                               (eval-when-compile (propertize "y" 'font-lock-face font-lock-warning-face))
                             (eval-when-compile (propertize "n" 'font-lock-face font-lock-comment-face)))))

         (gdb--table-add-row table (list (number-to-string (gdb--breakpoint-number breakpoint))
                                         (gdb--breakpoint-type   breakpoint) (gdb--breakpoint-disp   breakpoint)
                                         enabled-disp                        (gdb--breakpoint-addr   breakpoint)
                                         (gdb--breakpoint-hits   breakpoint) (gdb--breakpoint-what   breakpoint))
                             `(gdb--breakpoint ,breakpoint))))

     (gdb--table-insert table t))))


;; ------------------------------------------------------------------------------------------
;; Variables buffer
(define-derived-mode gdb--variables-mode nil "GDB Variables"
  (setq-local buffer-read-only t)
  (buffer-disable-undo))

(gdb--simple-get-buffer gdb--variables gdb--variables-update
  (gdb--rename-buffer "Variables")
  (gdb--variables-mode))

(defun gdb--variables-update ()
  (gdb--with-valid-session
   (let* ((frame (gdb--session-selected-frame session))
          (variables (and frame (gdb--frame-variables frame)))
          (table (make-gdb--table)))
     (gdb--table-add-row table '("Name" "Type" "Value"))
     (dolist (variable variables)
       (gdb--table-add-row
        table (list (propertize (gdb--variable-name  variable) 'face 'font-lock-variable-name-face)
                    (propertize (gdb--variable-type  variable) 'face 'font-lock-type-face)
                    (or         (gdb--variable-value variable) "<Composite type>"))))
     (gdb--table-insert table t))))


;; ------------------------------------------------------------------------------------------
;; Watcher buffers
(define-derived-mode gdb--watcher-mode nil "GDB Watcher"
  (setq-local buffer-read-only t)
  (buffer-disable-undo))

(gdb--simple-get-buffer gdb--watcher gdb--watcher-update
  (gdb--rename-buffer "Watcher")
  (gdb--watcher-mode))

(defun gdb--watcher-update ()
  (gdb--with-valid-session
   ))


;; ------------------------------------------------------------------------------------------
;; Source buffers
(defun gdb--find-file (path)
  "Return the buffer of the file specified by PATH.
Create the buffer, if it wasn't already open."
  (when (and path (not (file-directory-p path)) (file-readable-p path))
    (find-file-noselect path t)))

(defun gdb--complete-path (path)
  "Add TRAMP prefix to PATH returned from GDB output, if needed."
  (gdb--with-valid-session
   (when path
     (concat (file-remote-p (buffer-local-value 'default-directory (gdb--comint-get-buffer session)))
             path))))

(defun gdb--display-source-buffer (file line &optional no-mark)
  "Display buffer of the selected source."
  (gdb--with-valid-session
   (let ((buffer (gdb--find-file file))
         (window (gdb--session-source-window session)))
     (gdb--remove-all-symbols session 'gdb--source-indicator t)
     (unless no-mark (gdb--place-symbol session buffer line '((type . source-indicator)
                                                              (source . t))))

     (when (and (window-live-p window) buffer)
       (with-selected-window window
         (switch-to-buffer buffer)
         (if (display-images-p)
             (set-window-fringes nil 8)
           (set-window-margins nil 2))
         (goto-char (point-min))
         (forward-line (1- line))
         (recenter))))))


;; ------------------------------------------------------------------------------------------
;; Fringe symbols
(defun gdb--place-symbol (session buffer line data)
  (when (and (buffer-live-p buffer) line data)
    (with-current-buffer buffer
      (let* ((type (alist-get 'type data))
             (pos (line-beginning-position (1+ (- line (line-number-at-pos)))))
             (overlay (make-overlay pos pos buffer))
             (dummy-string (make-string 1 ?x))
             property)
        ;; NOTE(nox): Properties for housekeeping, session and type of symbol
        (overlay-put overlay 'gdb--indicator-session session)
        (overlay-put overlay (intern (concat "gdb--" (symbol-name type))) t)

        ;; NOTE(nox): Fringe spec: (left-fringe BITMAP [FACE])
        ;;            Margin spec: ((margin left-margin) STRING)
        (cond
         ((eq type 'breakpoint-indicator)
          (let ((breakpoint (alist-get 'breakpoint data))
                (enabled    (alist-get 'enabled    data)))
            (overlay-put overlay 'gdb--breakpoint breakpoint)
            (if (display-images-p)
                (setq property `(left-fringe gdb--fringe-breakpoint
                                             ,(if enabled 'gdb--breakpoint-enabled 'gdb--breakpoint-disabled)))
              (setq property `((margin left-margin) ,(if enabled "B" "b"))))))

         ((memq type '(source-indicator frame-indicator thread-indicator))
          (overlay-put overlay 'priority 10) ;; NOTE(nox): Above breakpoint symbols
          (if (display-images-p)
              (setq property '(left-fringe right-triangle compilation-warning))
            (setq property '((margin left-margin) "=>")))))

        (put-text-property 0 1 'display property dummy-string)
        (overlay-put overlay 'before-string dummy-string)

        (when (alist-get 'source data) (overlay-put overlay 'window (gdb--session-source-window session)))))))

(defun gdb--remove-all-symbols (session type &optional source-files-only)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless (and source-files-only gdb--buffer-info)
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (and (eq session (overlay-get ov 'gdb--indicator-session))
                     (or (eq type 'all) (overlay-get ov type)))
            (delete-overlay ov)))))))

(defun gdb--remove-breakpoint-symbol (breakpoint)
  (let ((file (gdb--breakpoint-file breakpoint))
        (line (gdb--breakpoint-line breakpoint)))
    (when (and file line)
      (with-current-buffer (gdb--find-file file)
        (remove-overlays nil nil 'gdb--breakpoint breakpoint)))))


;; ------------------------------------------------------------------------------------------
;; Module API
(defun gdb--extract-context (token-string)
  "Return the context-data cons assigned to TOKEN-STRING, deleting
it from the list."
  (let ((context (assoc token-string gdb--token-contexts))
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

(defun gdb--running (thread-id-str)
  (gdb--with-valid-session
   (let ((thread (gdb--get-thread-by-id (string-to-number thread-id-str)))
         (selected-thread (gdb--session-selected-thread session)))
     (when thread
       (setf (gdb--thread-state thread) "running"
             (gdb--thread-frames thread) nil)

       (when (eq thread selected-thread)
         (gdb--remove-all-symbols session 'gdb--source-indicator t)
         (cl-loop for thread in (gdb--session-threads session)
                  when (string= (gdb--thread-state thread) "stopped")
                  do (gdb--switch-to-thread thread) and return nil))

       (cl-pushnew 'gdb--threads (gdb--session-buffer-types-to-update session))
       (cl-pushnew 'gdb--frames  (gdb--session-buffer-types-to-update session))))))

(defun gdb--set-initial-file (file line-str)
  (gdb--display-source-buffer (gdb--complete-path file) (string-to-number line-str) t))

(defun gdb--get-thread-info (&optional id-str)
  (gdb--command (concat "-thread-info " id-str) 'gdb--context-thread-info))

(defun gdb--thread-exited (thread-id-str)
  (gdb--with-valid-session
   (let ((thread (gdb--get-thread-by-id (string-to-number thread-id-str))))
     (setf (gdb--session-threads session) (cl-delete thread (gdb--session-threads session) :test 'eq))

     (when (eq (gdb--session-selected-thread session) thread)
       (gdb--switch-to-thread (car (gdb--session-threads session))))

     (cl-pushnew 'gdb--threads (gdb--session-buffer-types-to-update session)))))

(defun gdb--update-thread (id-str target-id name state core)
  (gdb--with-valid-session
   (let* ((id (string-to-number id-str))
          (existing-thread (gdb--get-thread-by-id id))
          (thread (or existing-thread (make-gdb--thread))))

     (gdb--update-struct gdb--thread thread
       (id id) (target-id target-id) (name name) (state state) (core core))

     (unless existing-thread
       (setf (gdb--session-threads session) (append (gdb--session-threads session) (list thread))))

     (if (string= "stopped" state)
         (gdb--command "-stack-list-frames" (cons 'gdb--context-frame-info thread) thread)
       ;; NOTE(nox): Only update when it is running, otherwise it will update when the frame list
       ;; arrives.
       (cl-pushnew 'gdb--threads (gdb--session-buffer-types-to-update session))
       (gdb--conditional-switch thread '(no-selected-thread))))))

(defun gdb--add-frames-to-thread (thread &rest args)
  (gdb--with-valid-session
   (cl-loop for (level-str addr func file line-str from) in args
            do (push (make-gdb--frame
                      :thread thread :level (string-to-number level-str) :addr addr :func func  :from from
                      :file (gdb--complete-path file) :line (and line-str (string-to-number line-str)))
                     (gdb--thread-frames thread))
            finally do (setf (gdb--thread-frames thread) (nreverse (gdb--thread-frames thread))))

   (cl-pushnew 'gdb--threads (gdb--session-buffer-types-to-update session))
   (when (eq thread (gdb--session-selected-thread session))
     (cl-pushnew 'gdb--frames (gdb--session-buffer-types-to-update session)))

   (gdb--conditional-switch (car (gdb--thread-frames thread)) '(running same-thread))))

(defun gdb--breakpoint-changed (number-str type disp enabled-str addr func fullname line-str at
                                           pending thread cond times what)
  (gdb--with-valid-session
   (let* ((number (string-to-number number-str))
          (enabled (string= enabled-str "y"))
          (file (gdb--complete-path fullname))
          (line (and line-str (string-to-number line-str)))
          (existing-breakpoint (cl-loop for breakpoint in (gdb--session-breakpoints session)
                                        when (= number (gdb--breakpoint-number breakpoint))
                                        return breakpoint))
          (breakpoint (or existing-breakpoint (make-gdb--breakpoint))))

     (if existing-breakpoint
         (gdb--remove-breakpoint-symbol existing-breakpoint)
       (setf (gdb--session-breakpoints session) (append (gdb--session-breakpoints session) (list breakpoint))))

     (gdb--update-struct gdb--breakpoint breakpoint
       (number number) (type type) (disp disp) (addr addr) (hits times) (enabled )
       (what (concat (or what pending at (gdb--location-string func fullname line))
                     (and cond   (concat " if " cond))
                     (and thread (concat " on thread " thread))))
       (file file) (line line))

     (when (and file line)
       (gdb--place-symbol session (gdb--find-file file) line `((type . breakpoint-indicator)
                                                               (breakpoint . ,breakpoint)
                                                               (enabled . ,enabled)
                                                               (source . t))))

     (cl-pushnew 'gdb--breakpoints (gdb--session-buffer-types-to-update session)))))

(defun gdb--breakpoint-deleted (number)
  (gdb--with-valid-session
   (setq number (string-to-number number))
   (setf (gdb--session-breakpoints session)
         (cl-delete-if (lambda (breakpoint)
                         (when (= (gdb--breakpoint-number breakpoint) number)
                           (gdb--remove-breakpoint-symbol breakpoint)
                           t))
                       (gdb--session-breakpoints session)))
   (cl-pushnew 'gdb--breakpoints (gdb--session-buffer-types-to-update session))))

(defun gdb--add-variables-to-frame (frame &rest args)
  (gdb--with-valid-session
   (cl-loop for (name type value) in args
            do (push (make-gdb--variable :name name :type type :value value) (gdb--frame-variables frame)))
   (cl-pushnew 'gdb--variables (gdb--session-buffer-types-to-update session))))

;; (defun gdb--set-disassembly (buffer list with-source-info)
;;   (when (buffer-live-p buffer)
;;     (with-current-buffer buffer
;;       (setq-local gdb--disassembly-list list)
;;       (setq-local gdb--with-source-info with-source-info))
;;     (add-to-list 'gdb--buffers-to-update buffer)))


;; ------------------------------------------------------------------------------------------
;; User commands
(defun gdb-watch-expression ()
  (interactive)
  (gdb--with-valid-session
   (let ((expression (replace-regexp-in-string "\\(\\`[ \t\r\n]+\\|[ \t\r\n]+\\'\\)" ""
                                               (read-string "Expression: "))))
     (when (> (length expression) 0)
       (setq expression (replace-regexp-in-string "\n+" " " expression)
             expression (format "\"%s\"" (replace-regexp-in-string "[ \t\n]+" " " expression)))
       (gdb--command (concat "-var-create - * " expression) 'gdb--context-create-variable
                     (gdb--session-selected-thread session))))))

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
