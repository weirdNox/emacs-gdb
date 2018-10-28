;;; gdb.el --- GDB frontend -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Gonçalo Santos

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
  '(gdb--context-tty-set ;; Data (optional): Old TTY process
    gdb--context-initial-file
    gdb--context-thread-info
    gdb--context-frame-info ;; Data: Thread
    gdb--context-breakpoint-insert
    gdb--context-breakpoint-delete ;; Data: Breakpoint
    gdb--context-get-variables ;; Data: Frame
    gdb--context-watcher-create ;; Data: (Expression . WatcherToReplace)
    gdb--context-watcher-update
    gdb--context-watcher-list-children
    gdb--context-watcher-change-format ;; Data: Watcher
    gdb--context-registers-list-names ;; Data: Thread
    gdb--context-registers-get-changed ;; Data: (FormatString . Thread)
    gdb--context-registers-update ;; Data: Thread
    gdb--context-disassemble ;; Data: Disassemble buffer
    gdb--context-persist-thread
    gdb--context-get-data ;; Data: Result name string
    )
  "List of implemented token contexts.
Must be in the same order of the `token_context' enum in the
dynamic module.")

(eval-and-compile
  (defconst gdb--buffer-types
    '(gdb--comint
      gdb--inferior-io
      gdb--threads
      gdb--frames
      gdb--breakpoints
      gdb--variables
      gdb--watchers
      gdb--registers
      gdb--disassembly)
    "List of available buffer types."))

(defconst gdb--keep-buffer-types '(gdb--comint gdb--inferior-io)
  "List of buffer types that should be kept after GDB is killed.")

(cl-defstruct gdb--thread
  id target-id name state frames core
  (registers-tick most-negative-fixnum) (registers (make-hash-table :size 250)))

(cl-defstruct gdb--frame thread level addr func file line from variables)

(cl-defstruct gdb--breakpoint
  number type disp enabled addr hits ignore-count what thread
  pending condition file gdb-fullname line func overlay)
(defconst gdb--available-breakpoint-types
  '(("Breakpoint" . "")
    ("Temporary Breakpoint" . "-t ")
    ("Hardware Breakpoint" . "-h ")
    ("Temporary Hardware Breakpoint" . "-t -h "))
  "Alist of (TYPE . FLAGS).
Both are strings. FLAGS are the flags to be passed to -break-insert in order to create a
breakpoint of TYPE.")

(cl-defstruct gdb--variable name type value)
(cl-defstruct gdb--watcher  name expr type value thread parent children-count children open flag)
(cl-defstruct gdb--register number name value tick)

(cl-defstruct gdb--session
  frame process buffers source-window debuggee-path debuggee-args
  buffer-types-to-update buffers-to-update
  threads selected-thread persist-thread selected-frame
  breakpoints
  (watchers-tick most-negative-fixnum) (watchers (make-hash-table :test 'equal)) root-watchers)
(defvar gdb--sessions nil
  "List of active sessions.")

(cl-defstruct gdb--buffer-info session type thread update-func)
(defvar-local gdb--buffer-info nil
  "GDB related information related to each buffer.")
(put 'gdb--buffer-info 'permanent-local t)

(defvar gdb--next-token 0
  "Next token value to be used for context matching.
This is shared among all sessions.")

(defvar gdb--token-contexts nil
  "Alist of tokens and contexts.
The alist has the format ((TOKEN . (TYPE . DATA)) ...).
This is shared among all sessions.")

(cl-defstruct gdb--instruction address function offset instruction)
(cl-defstruct gdb--source-instr-info file line instr-list)

(defvar gdb--inhibit-display-source nil)
(defvar gdb--open-buffer-new-frame  nil)
(defvar gdb--data nil)

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
    (when (= (length gdb--sessions) 0)
      (remove-hook 'delete-frame-functions #'gdb--handle-delete-frame)
      (gdb-keys-mode -1))

    (cl-loop for frame in (frame-list)
             when (eq (frame-parameter frame 'gdb--session) session)
             do (unless (eq frame (gdb--session-frame session))
                  (delete-frame frame)))

    (when (frame-live-p (gdb--session-frame session))
      (set-frame-parameter (gdb--session-frame session) 'gdb--session nil)
      (when (> (length (frame-list)) 0) (delete-frame (gdb--session-frame session))))

    (set-process-sentinel (gdb--session-process session) nil)
    (delete-process (gdb--session-process session))

    (dolist (buffer (gdb--session-buffers session))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (if gdb--buffer-info
              (let ((type (gdb--buffer-info-type gdb--buffer-info)))
                (when (eq type 'gdb--inferior-io)
                  (let ((proc (get-buffer-process buffer)))
                    (when proc
                      (set-process-sentinel proc nil)
                      (delete-process (get-buffer-process buffer)))))

                (if (memq type gdb--keep-buffer-types)
                    (setq gdb--buffer-info nil)
                  (kill-buffer)))
            (kill-buffer)))))

    (gdb--remove-all-symbols session 'all)))


;; ------------------------------------------------------------------------------------------
;; Threads and frames
(defun gdb--get-thread-by-id (id)
  (gdb--with-valid-session
   (when id
     (cl-loop for thread in (gdb--session-threads session)
              when (= (gdb--thread-id thread) id) return thread))))

(defun gdb--switch-to-thread (thread)
  "Unconditionally switch to _different_ THREAD. This will also switch to the most relevant frame.
THREAD may be nil, which means to remove the selected THREAD."
  (gdb--with-valid-session
   (unless (eq thread (gdb--session-selected-thread session))
     (setf (gdb--session-selected-thread session) thread)
     (gdb--switch-to-frame (gdb--best-frame-to-switch-to thread))

     (let ((buffer (gdb--get-buffer-with-type session 'gdb--threads)) pos)
       (when buffer
         (with-current-buffer buffer
           (remove-overlays nil nil 'gdb--thread-indicator t)
           (when thread
             (when (setq pos (text-property-any (point-min) (point-max) 'gdb--thread thread))
               (gdb--place-symbol session (current-buffer) (line-number-at-pos pos)
                                  '((type . thread-indicator))))))))

     (when thread
       (gdb--command (format "-thread-select %d" (gdb--thread-id thread)))
       (message "Switched to thread %d." (gdb--thread-id thread)))

     (cl-pushnew 'gdb--frames    (gdb--session-buffer-types-to-update session))
     (cl-pushnew 'gdb--registers (gdb--session-buffer-types-to-update session)))))

(defun gdb--best-frame-to-switch-to (thread)
  "Return the most relevant frame to switch to in THREAD's frames."
  (when thread
    (let ((fallback (car (gdb--thread-frames thread)))
          runner-up)
      (or (cl-loop for frame in (gdb--thread-frames thread)
                   when (and (gdb--frame-file frame) (gdb--frame-line frame)) return frame
                   when (gdb--frame-file frame) do (setq runner-up frame))
          runner-up fallback))))

(defun gdb--switch-to-frame (frame)
  "Unconditionally switch to a _different_ FRAME.
When FRAME is in a different thread, switch to it."
  (gdb--with-valid-session
   (unless (eq frame (gdb--session-selected-frame session))
     (setf (gdb--session-selected-frame session) frame)

     (when frame
       (gdb--switch-to-thread (gdb--frame-thread frame))
       (gdb--command "-var-update --all-values *" 'gdb--context-watcher-update frame))

     (if (and frame (not (gdb--frame-variables frame)))
         (gdb--command "-stack-list-variables --simple-values" (cons 'gdb--context-get-variables frame) frame)
       (cl-pushnew 'gdb--variables (gdb--session-buffer-types-to-update session)))

     (gdb--display-source-buffer)

     (let ((buffer (gdb--get-buffer-with-type session 'gdb--frames)) pos)
       (when buffer
         (with-current-buffer buffer
           (remove-overlays nil nil 'gdb--frame-indicator t)
           (when frame
             (when (setq pos (text-property-any (point-min) (point-max) 'gdb--frame frame))
               (gdb--place-symbol session (current-buffer) (line-number-at-pos pos)
                                  '((type . frame-indicator)))))))))))

(defun gdb--switch (frame-or-thread)
  "Unconditionally switch to a _different_ FRAME-OR-THREAD."
  (gdb--with-valid-session
   (cl-assert (or (gdb--thread-p frame-or-thread) (gdb--frame-p frame-or-thread)))
   (let* ((type (type-of frame-or-thread))
          (thread (if (eq type 'gdb--thread) frame-or-thread (gdb--frame-thread frame-or-thread)))
          (frame  (if (eq type 'gdb--frame)  frame-or-thread (gdb--best-frame-to-switch-to frame-or-thread))))
     (if frame
         (gdb--switch-to-frame frame)
       (gdb--switch-to-thread thread)))))

(defun gdb--conditional-switch (frame-or-thread &optional cause)
  "Conditionally switch to a _different_ FRAME-OR-THREAD depending on CAUSE.
This will _always_ switch when no thread is selected.

CAUSE should be a list of the following symbols:
- `running': Switch when selected thread is running and is different from THREAD
- `same-thread' (only for frames): Switch when same thread

When the thread is switched, the current frame will also be changed."
  (gdb--with-valid-session
   (cl-assert (or (gdb--thread-p frame-or-thread) (gdb--frame-p frame-or-thread)))
   (let* ((type (type-of frame-or-thread))
          (thread (if (eq type 'gdb--thread) frame-or-thread (gdb--frame-thread frame-or-thread)))
          (frame  (if (eq type 'gdb--frame)  frame-or-thread (gdb--best-frame-to-switch-to frame-or-thread)))
          (selected-thread (gdb--session-selected-thread session))
          (condition (or (not selected-thread)
                         (and (memq 'running cause)
                              (string= "running" (gdb--thread-state selected-thread)))
                         (and (eq type 'gdb--frame) (memq 'same-thread cause)
                              (eq thread selected-thread)))))
     (when condition (if frame
                         (gdb--switch-to-frame frame)
                       (gdb--switch-to-thread thread))))))

(defun gdb--ask-for-thread (&optional default)
  (gdb--with-valid-session
   (when (gdb--session-threads session)
     (let* ((default-string nil)
            (collection (cl-loop for thread in (gdb--session-threads session) with display
                                 do (setq display (format "%d: %s" (gdb--thread-id thread)
                                                          (gdb--thread-target-id thread)))
                                 collect (cons display thread)
                                 when (eq thread default) do (setq default-string display))))
       (push (cons "NONE" nil) collection)
       (unless default-string (setq default-string (caar collection)))
       (cdr (assoc (completing-read "Thread: " collection nil t nil nil default-string)
                   collection))))))


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

(defun gdb--read-line (prompt &optional default)
  "Read a line of user input with PROMPT and DEFAULT value.
The string is trimmed and all the spaces (including newlines) are converted into a single space."
  (let ((result (replace-regexp-in-string
                 "\\(\\`[ \t\r\n\v\f]+\\|[ \t\r\n\v\f]+\\'\\)" "" (read-string prompt default))))
    (and (> (length result) 0) (replace-regexp-in-string "[ \t\r\n\v\f]+" " " result))))

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

(defsubst gdb--stn (str) (and (stringp str) (string-to-number str)))
(defsubst gdb--nts (num) (and (numberp num) (number-to-string num)))
(defsubst gdb--add-face (string face) (when string (propertize string 'face face)))

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

(defun gdb--append-to-buffer (buffer string)
  (when (buffer-live-p buffer)
    (let* ((windows (get-buffer-window-list buffer nil t))
           windows-to-move return-pos)
      (with-current-buffer buffer
        (dolist (window windows)
          (when (= (window-point window) (point-max))
            (push window windows-to-move)))

        (unless (= (point) (point-max))
          (setq return-pos (point))
          (goto-char (point-max)))
        (insert string)
        (when return-pos (goto-char return-pos))

        (dolist (window windows-to-move)
          (set-window-point window (point-max)))))))

(defun gdb--get-data (command key)
  "Synchronously retrieve result KEY of COMMAND."
  (gdb--with-valid-session
   (setq gdb--data nil)
   (gdb--command command (cons 'gdb--context-get-data key))
   (while (not gdb--data) (accept-process-output (gdb--session-process session) 0.5))
   (when (stringp gdb--data) gdb--data)))


;; ------------------------------------------------------------------------------------------
;; Tables
(cl-defstruct gdb--table header rows (num-rows 0) column-sizes target-line)
(cl-defstruct gdb--table-row table columns properties level has-children)

(defsubst gdb--pad-string (string padding) (format (concat "%" (number-to-string padding) "s") (or string "")))

(defun gdb--table-update-column-sizes (table columns &optional level has-children)
  "Update TABLE column sizes to include new COLUMNS.
LEVEL should be an integer specifying the indentation level."
  (unless (gdb--table-column-sizes table)
    (setf (gdb--table-column-sizes table) (make-list (length columns) 0)))

  (setf (gdb--table-column-sizes table)
        (cl-loop for string in columns
                 and size in (gdb--table-column-sizes table)
                 and first = t then nil
                 collect (- (max (abs size) (+ (string-width (or string ""))
                                               (* (or (and first level) 0) 4)
                                               (or (and first has-children 4) 0)))))))

(defun gdb--table-add-header (table columns)
  "Set TABLE header to COLUMNS, a list of strings, and recalculate column sizes."
  (gdb--table-update-column-sizes table columns)
  (setf (gdb--table-header table) columns))

(defsubst gdb--get-table-from-table-or-parent (table-or-parent)
  "TABLE-OR-PARENT should be a table or a table row, which, in the latter case, will be made the parent of
the inserted row. Returns table."
  (cond ((eq (type-of table-or-parent) 'gdb--table)     table-or-parent)
        ((eq (type-of table-or-parent) 'gdb--table-row) (gdb--table-row-table table-or-parent))
        (t   (error "Unexpected table-or-argument type."))))

(defun gdb--table-add-row (table-or-parent columns &optional properties has-children)
  "Add a row of COLUMNS, a list of strings, to TABLE-OR-PARENT and recalculate column sizes.
When non-nil, PROPERTIES will be added to the whole row when printing.
TABLE-OR-PARENT should be a table or a table row, which, in the latter case, will be made the parent of
the inserted row.
HAS-CHILDREN should be t when this node has children."
  (let* ((table (gdb--get-table-from-table-or-parent table-or-parent))
         (parent (and (eq  (type-of table-or-parent) 'gdb--table-row) table-or-parent))
         (level (or (and parent (1+ (gdb--table-row-level parent))) 0))

         (row (make-gdb--table-row :table table :columns columns :properties properties :level level
                                   :has-children has-children)))

    (gdb--table-update-column-sizes table columns level has-children)
    (setf (gdb--table-rows table) (append (gdb--table-rows table) (list row))
          (gdb--table-num-rows table) (1+ (gdb--table-num-rows table)))

    (when parent (setf (gdb--table-row-has-children parent) 'open))

    row))

(defun gdb--table-row-string (columns column-sizes sep &optional with-newline properties level has-children)
  (apply #'propertize (cl-loop for string in columns
                               and size   in column-sizes
                               and first = t then nil
                               unless first concat sep into result
                               concat (gdb--pad-string
                                       (concat (and first (make-string (* (or level 0) 4) ? ))
                                               (and first (cond ((eq has-children t)     "[+] ")
                                                                ((eq has-children 'open) "[-] ")))
                                               string)
                                       size)
                               into result
                               finally return (concat result (and with-newline "\n")))
         properties))

(defun gdb--table-insert (table &optional sep)
  "Erase buffer and insert TABLE with columns separated with SEP (space as default).
If WITH-HEADER is set, then the first row is used as header."
  (let ((column-sizes (gdb--table-column-sizes table))
        (sep (or sep " ")))
    (erase-buffer)

    (when (gdb--table-header table)
      (setq-local header-line-format
                  (list " " (gdb--table-row-string (gdb--table-header table) column-sizes sep))))

    (cl-loop for row in (gdb--table-rows table)
             for row-number from 1 with insert-newline = t
             when (= row-number (gdb--table-num-rows table)) do (setq insert-newline nil)
             do (insert (gdb--table-row-string (gdb--table-row-columns    row) column-sizes sep insert-newline
                                               (gdb--table-row-properties row) (gdb--table-row-level row)
                                               (gdb--table-row-has-children row))))

    (when (gdb--table-target-line table)
      (gdb--scroll-buffer-to-line (current-buffer) (gdb--table-target-line table)))))


;; ------------------------------------------------------------------------------------------
;; Buffers
(defun gdb--get-buffer-with-type (session type)
  (cl-loop for buffer in (gdb--session-buffers session)
           when (let ((buffer-info (buffer-local-value 'gdb--buffer-info buffer)))
                  (and buffer-info (eq (gdb--buffer-info-type buffer-info) type)))
           return buffer
           finally return nil))

(defmacro gdb--simple-get-buffer (type update-func name important &rest body)
  "Simple buffer creator/fetcher, for buffers that should be unique in a session."
  (declare (indent defun) (debug (sexp sexp body)))
  (unless (memq type gdb--buffer-types) (error "Type %s does not exist" (symbol-name type)))
  `(defun ,(intern (concat (symbol-name type) "-get-buffer")) (session)
     ,(concat "Creator and fetcher of buffer with type `" (symbol-name type) "'")
     (cond ((gdb--get-buffer-with-type session ',type))
           (t (let ((buffer (generate-new-buffer "*GDB Temporary Name*")))
                (with-current-buffer buffer
                  (gdb--rename-buffer ,name (gdb--session-debuggee-path session))
                  ,@body
                  (setq gdb--buffer-info (make-gdb--buffer-info :session session :type ',type
                                                                :update-func #',update-func))
                  ,(if important
                       '(add-hook 'kill-buffer-hook #'gdb--important-buffer-kill-cleanup nil t)
                     '(add-hook 'kill-buffer-hook #'gdb--buffer-kill-cleanup nil t)))
                (push buffer (gdb--session-buffers session))
                (gdb--update-buffer buffer)
                buffer)))))

(defun gdb--update-buffer (buffer)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (func (gdb--buffer-info-update-func gdb--buffer-info)))
      (cl-assert (fboundp func))
      (gdb--measure-time (concat "Calling " (symbol-name func)) (funcall func)))))

(defun gdb--update ()
  (gdb--with-valid-session
   (let ((buffers-to-update (gdb--session-buffers-to-update session))
         (types-to-update   (gdb--session-buffer-types-to-update session)))
     (dolist (buffer (gdb--session-buffers session))
       (let ((buffer-info (buffer-local-value 'gdb--buffer-info buffer)))
         (if buffer-info
             (when (or (memq buffer buffers-to-update) (memq (gdb--buffer-info-type buffer-info) types-to-update))
               (gdb--update-buffer buffer))
           (kill-buffer buffer))))

     (setf (gdb--session-buffers-to-update session) nil
           (gdb--session-buffer-types-to-update session) nil))))

(defmacro gdb--rename-buffer (&optional specific-str debuggee-path)
  `(save-match-data
     (rename-buffer (concat ,(concat "*GDB" (when specific-str (concat ": " specific-str)))
                            (when ,debuggee-path (concat " - " (file-name-nondirectory ,debuggee-path)))
                            "*")
                    t)))

(defun gdb--rename-buffers-with-debuggee (debuggee-path)
  (let* ((debuggee-name (file-name-nondirectory debuggee-path))
         (replacement (concat " - " debuggee-name "*")))
    (dolist (buffer (gdb--session-buffers (gdb--infer-session)))
      (with-current-buffer buffer
        (rename-buffer (replace-regexp-in-string "\\([ ]+-.+\\)?\\*\\(<[0-9]+>\\)?$" replacement (buffer-name) t)
                       t)))))

(defun gdb--important-buffer-kill-cleanup () (gdb--kill-session (gdb--infer-session t)))

(defun gdb--buffer-kill-cleanup ()
  (gdb--with-valid-session
   (setf (gdb--session-buffers session) (cl-delete (current-buffer) (gdb--session-buffers session) :test 'eq))))

(defsubst gdb--is-buffer-type (type)
  (and gdb--buffer-info (eq (gdb--buffer-info-type gdb--buffer-info) type)))


;; ------------------------------------------------------------------------------------------
;; Frames and windows
(defun gdb--frame-name (session)
  "Return GDB frame name for SESSION, possibly using debuggee file name."
  (let* ((debuggee (gdb--session-debuggee-path session))
         (suffix (and (stringp debuggee)
                      (file-executable-p debuggee)
                      (concat " - " (abbreviate-file-name debuggee)))))
    (concat "Emacs GDB" suffix)))

(defun gdb--create-frame (session)
  (let ((frame (make-frame `((fullscreen . maximized)
                             (gdb--session . ,session)
                             (name . ,(gdb--frame-name session))))))
    (setf (gdb--session-frame session) frame)
    (add-hook 'delete-frame-functions #'gdb--handle-delete-frame)
    frame))

(defun gdb--handle-delete-frame (frame)
  (let ((session (frame-parameter frame 'gdb--session)))
    (when (and (gdb--session-p session)
               (eq frame (gdb--session-frame session)))
      (gdb--kill-session session))))

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
      (gdb--set-window-buffer top-right    (gdb--threads-get-buffer session))
      (gdb--set-window-buffer middle-left  (gdb--breakpoints-get-buffer session))
      (gdb--set-window-buffer middle-right (gdb--frames-get-buffer session))
      (gdb--set-window-buffer bottom-left  (gdb--watchers-get-buffer session))
      (gdb--set-window-buffer bottom-right (gdb--variables-get-buffer session))
      (setf (gdb--session-source-window session) top-left))))

(defun gdb--scroll-buffer-to-line (buffer line)
  (with-current-buffer buffer
    (goto-char (point-min))
    (forward-line (1- line)))

  (dolist (window (get-buffer-window-list buffer nil t))
    (with-selected-window window
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun gdb--scroll-buffer-to-last-line (buffer)
  (with-current-buffer buffer
    (forward-line most-positive-fixnum)
    (beginning-of-line))

  (dolist (window (get-buffer-window-list buffer nil t))
    (with-selected-window window
      (forward-line most-positive-fixnum)
      (beginning-of-line))))


;; ------------------------------------------------------------------------------------------
;; Comint buffer
(define-derived-mode gdb-comint-mode comint-mode "GDB Comint"
  "Major mode for interacting with GDB."
  (setq-local comint-input-sender #'gdb--comint-sender)
  (setq-local comint-preoutput-filter-functions '(gdb--output-filter)))

(gdb--simple-get-buffer gdb--comint ignore "Comint" t
  (gdb-comint-mode)
  (let ((process-connection-type nil)) (make-comint-in-buffer "GDB" buffer "gdb" nil "-i=mi" "-nx"))

  (let ((proc (get-buffer-process buffer)))
    (set-process-sentinel proc #'gdb--comint-sentinel)
    (setf (gdb--session-process session) proc)))

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

(defun gdb--command (command &optional context thread-or-frame force-stopped add-ampersand)
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
stopped thread before running the command. If FORCE-STOPPED is
'no-resume, don't resume the stopped thread."
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
       (gdb--command "-exec-interrupt" nil stopped-thread))

     (setq command (concat token (car command-parts)
                           (and in-thread (format " --thread %d" (gdb--thread-id   in-thread)))
                           (and in-frame  (format " --frame  %d" (gdb--frame-level in-frame)))
                           " " (mapconcat #'identity (cdr command-parts) " ")))
     (gdb--debug-execute-body 'commands (message "Command %s" command))
     (process-send-string (gdb--session-process session) (concat command (and add-ampersand "&")
                                                                 "\n"))

     (when (and stopped-thread (not (eq force-stopped 'no-resume)))
       (gdb--command "-exec-continue" nil stopped-thread)))))


;; ------------------------------------------------------------------------------------------
;; Infer from point
(defun gdb--infer-thread-or-frame (&optional not-selected)
  (gdb--with-valid-session
   (let* ((buffer-info gdb--buffer-info)
          (buffer-type (and buffer-info (gdb--buffer-info-type buffer-info)))
          result)
     (when buffer-type
       (cond ((eq buffer-type 'gdb--threads)
              (setq result (get-text-property (line-beginning-position) 'gdb--thread)))
             ((eq buffer-type 'gdb--frames)
              (setq result (get-text-property (line-beginning-position) 'gdb--frame)))))
     (or result (and (not not-selected) (gdb--session-selected-thread session))))))

(defun gdb--infer-thread (&optional not-selected)
  (let ((thread-or-frame (gdb--infer-thread-or-frame not-selected)))
    (if (eq (type-of thread-or-frame) 'gdb--frame)
        (gdb--frame-thread thread-or-frame)
      thread-or-frame)))

(defun gdb--point-location ()
  "Return a GDB-readable location of the point, in a source file or special buffer."
  (cond ((buffer-file-name) (format "%s:%d" (buffer-file-name) (gdb--current-line)))))

(defun gdb--infer-breakpoint ()
  (gdb--with-valid-session
   (cond ((buffer-file-name)
          (let ((file (buffer-file-name))
                (line (gdb--current-line)))
            (cl-loop for breakpoint in (gdb--session-breakpoints session)
                     when (and (gdb--breakpoint-file breakpoint) (gdb--breakpoint-line breakpoint)
                               (string= file (gdb--breakpoint-file breakpoint))
                               (=       line (gdb--breakpoint-line breakpoint)))
                     return breakpoint)))
         ((gdb--is-buffer-type 'gdb--breakpoints)
          (get-text-property (line-beginning-position) 'gdb--breakpoint)))))

(defun gdb--infer-breakpoint-location (breakpoint)
  (when breakpoint
    (cond ((gdb--breakpoint-pending breakpoint))
          ((and (gdb--breakpoint-gdb-fullname breakpoint) (gdb--breakpoint-line breakpoint))
           (format "%s:%d" (gdb--breakpoint-gdb-fullname breakpoint) (gdb--breakpoint-line breakpoint)))
          ((gdb--breakpoint-func breakpoint)
           (concat (and (gdb--breakpoint-gdb-fullname breakpoint)
                        (concat (gdb--breakpoint-gdb-fullname breakpoint) ":"))
                   (gdb--breakpoint-func breakpoint))))))


;; ------------------------------------------------------------------------------------------
;; Inferior I/O buffer
(define-derived-mode gdb-inferior-io-mode comint-mode "Inferior I/O"
  "Major mode for interacting with the inferior."
  :syntax-table nil :abbrev-table nil)

(gdb--simple-get-buffer gdb--inferior-io ignore "Inferior I/O" t
  (gdb-inferior-io-mode)
  (gdb--inferior-io-initialization))

(defun gdb--inferior-io-initialization (&optional old-proc)
  (gdb--with-valid-session
   (let ((buffer (current-buffer))
         inferior-process tty)
     (when old-proc (set-process-buffer old-proc nil))

     (save-excursion
       (setq inferior-process (get-buffer-process (make-comint-in-buffer "GDB inferior" buffer nil))
             tty (or (process-get inferior-process 'remote-tty) (process-tty-name inferior-process))))

     (gdb--command (concat "-inferior-tty-set " tty) (cons 'gdb--context-tty-set old-proc))

     (set-process-sentinel inferior-process #'gdb--inferior-io-sentinel))))

(defun gdb--inferior-io-sentinel (proc str)
  (let ((proc-status (process-status proc))
        (buffer (process-buffer proc)))
    (cond ((eq proc-status 'failed)
           ;; NOTE(nox): When the debuggee exits, Emacs gets an EIO error and stops listening to the tty.
           ;; This re-inits the buffer so everything works fine!
           (set-process-sentinel proc nil)
           (if buffer
               (with-current-buffer buffer
                 (gdb--append-to-buffer buffer "\n---------- END ----------\n\n")
                 (gdb--inferior-io-initialization proc))
             (delete-process proc)))

          ((and (string= str "killed\n") buffer)
           (with-current-buffer buffer (gdb--kill-session (gdb--infer-session t)))))))


;; ------------------------------------------------------------------------------------------
;; Threads buffer
(defvar gdb-threads-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p")   #'previous-line)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "SPC") #'gdb-select)
    (define-key map (kbd "c")   #'gdb-continue)
    (define-key map (kbd "s")   #'gdb-stop)
    map))

(define-derived-mode gdb-threads-mode nil "GDB Threads"
  (setq-local buffer-read-only t)
  (buffer-disable-undo))

(gdb--simple-get-buffer gdb--threads gdb--threads-update "Threads" nil
  (gdb-threads-mode))

(defun gdb--threads-update ()
  (gdb--with-valid-session
   (let ((threads (gdb--session-threads session))
         (selected-thread (gdb--session-selected-thread session))
         (cursor-on-thread (get-text-property (line-beginning-position) 'gdb--thread))
         (table (make-gdb--table :target-line (gdb--current-line)))
         selected-thread-line)
     (gdb--table-add-header table '("ID" "TgtID" "Name" "State" "Core" "Frame"))
     (dolist (thread threads)
       (let ((id-str (number-to-string (gdb--thread-id thread)))
             (target-id (gdb--thread-target-id thread))
             (name (gdb--thread-name thread))
             (state-display
              (if (string= (gdb--thread-state thread) "running")
                  (eval-when-compile (propertize "running" 'font-lock-face font-lock-string-face))
                (eval-when-compile (propertize "stopped" 'font-lock-face font-lock-warning-face))))
             (core (gdb--thread-core thread))
             (frame-str (gdb--frame-location-string (car (gdb--thread-frames thread)) t)))
         (gdb--table-add-row table (list id-str target-id name state-display core frame-str)
                             `(gdb--thread ,thread))
         (when (eq selected-thread thread) (setq selected-thread-line (gdb--table-num-rows table)))
         (when (eq cursor-on-thread thread) (setf (gdb--table-target-line table) (gdb--table-num-rows table)))))

     (remove-overlays nil nil 'gdb--thread-indicator t)
     (gdb--table-insert table)

     (when selected-thread-line
       (gdb--place-symbol session (current-buffer) selected-thread-line '((type . thread-indicator)))))))


;; ------------------------------------------------------------------------------------------
;; Stack frames buffer
(defvar gdb-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p")   #'previous-line)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "SPC") #'gdb-select)
    (define-key map (kbd "c")   #'gdb-continue)
    map))

(define-derived-mode gdb-frames-mode nil "GDB Frames"
  (setq-local buffer-read-only t)
  (buffer-disable-undo))

(gdb--simple-get-buffer gdb--frames gdb--frames-update "Stack Frames" nil
  (gdb-frames-mode))

(defun gdb--frames-update ()
  (gdb--with-valid-session
   (let* ((thread (gdb--session-selected-thread session))
          (frames (when thread (gdb--thread-frames thread)))
          (selected-frame (gdb--session-selected-frame session))
          (cursor-on-frame (get-text-property (line-beginning-position) 'gdb--frame))
          (table (make-gdb--table :target-line (gdb--current-line)))
          selected-frame-line)
     (gdb--table-add-header table '("Level" "Address" "Where"))
     (dolist (frame frames)
       (let ((level-str (number-to-string (gdb--frame-level frame)))
             (addr (gdb--frame-addr frame))
             (where (gdb--frame-location-string frame)))
         (gdb--table-add-row table (list level-str addr where) (list 'gdb--frame frame))

         (when (eq selected-frame frame) (setq selected-frame-line (gdb--table-num-rows table)))
         (when (eq cursor-on-frame frame) (setf (gdb--table-target-line table) (gdb--table-num-rows table)))))

     (remove-overlays nil nil 'gdb--frame-indicator t)
     (gdb--table-insert table)

     (when selected-frame-line
       (gdb--place-symbol session (current-buffer) selected-frame-line '((type . frame-indicator)))))))


;; ------------------------------------------------------------------------------------------
;; Breakpoints buffer
(defvar gdb-breakpoints-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "d") #'gdb-delete-breakpoint)
    (define-key map (kbd "<backspace>") #'gdb-delete-breakpoint)
    map))

(define-derived-mode gdb-breakpoints-mode nil "GDB Breakpoints"
  (setq-local buffer-read-only t)
  (buffer-disable-undo))

(gdb--simple-get-buffer gdb--breakpoints gdb--breakpoints-update "Breakpoints" nil
  (gdb-breakpoints-mode))

(defun gdb--breakpoints-update ()
  (gdb--with-valid-session
   (let ((breakpoints (gdb--session-breakpoints session))
         (cursor-on-breakpoint (get-text-property (line-beginning-position) 'gdb--breakpoint))
         (table (make-gdb--table :target-line (gdb--current-line))))
     (gdb--table-add-header table '("Num" "Type" "Disp" "Enb" "Addr" "Hits" "Ign" "What"))
     (dolist (breakpoint breakpoints)
       (let ((enabled-disp (if (gdb--breakpoint-enabled breakpoint)
                               (eval-when-compile (propertize "y" 'font-lock-face font-lock-warning-face))
                             (eval-when-compile (propertize "n" 'font-lock-face font-lock-comment-face)))))

         (gdb--table-add-row table (list (number-to-string (gdb--breakpoint-number breakpoint))
                                         (gdb--breakpoint-type   breakpoint) (gdb--breakpoint-disp   breakpoint)
                                         enabled-disp                        (gdb--breakpoint-addr   breakpoint)
                                         (gdb--breakpoint-hits   breakpoint)
                                         (gdb--nts (gdb--breakpoint-ignore-count breakpoint))
                                         (gdb--breakpoint-what   breakpoint))
                             `(gdb--breakpoint ,breakpoint))

         (when (eq cursor-on-breakpoint breakpoint)
           (setf (gdb--table-target-line table) (gdb--table-num-rows table)))))

     (gdb--table-insert table))))


;; ------------------------------------------------------------------------------------------
;; Variables buffer
(defvar gdb-variables-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd          "p") #'previous-line)
    (define-key map (kbd          "n") #'next-line)
    (define-key map (kbd        "RET") #'gdb-create-watcher-from-variable)
    (define-key map (kbd   "<return>") #'gdb-create-watcher-from-variable)
    (define-key map (kbd      "S-RET") #'gdb-create-watcher-from-variable-ask)
    (define-key map (kbd "<S-return>") #'gdb-create-watcher-from-variable-ask)
    map))

(define-derived-mode gdb-variables-mode nil "GDB Variables"
  (setq-local buffer-read-only t)
  (buffer-disable-undo))

(gdb--simple-get-buffer gdb--variables gdb--variables-update "Variables" nil
  (gdb-variables-mode))

(defun gdb--variables-update ()
  (gdb--with-valid-session
   (let* ((frame (gdb--session-selected-frame session))
          (variables (and frame (gdb--frame-variables frame)))
          (table (make-gdb--table :target-line (gdb--current-line))))
     (gdb--table-add-header table '("Name" "Type" "Value"))
     (dolist (variable variables)
       (gdb--table-add-row
        table (list (propertize (gdb--variable-name  variable) 'face 'font-lock-variable-name-face)
                    (propertize (gdb--variable-type  variable) 'face 'font-lock-type-face)
                    (or         (gdb--variable-value variable) "<Composite type>"))
        (list 'gdb--var variable)))
     (gdb--table-insert table))))


;; ------------------------------------------------------------------------------------------
;; Watcher buffer
(defvar gdb-watchers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd        "p") #'previous-line)
    (define-key map (kbd        "n") #'next-line)
    (define-key map (kbd        "a") #'gdb-watcher-add-expression)
    (define-key map (kbd      "RET") #'gdb-watcher-assign)
    (define-key map (kbd "<return>") #'gdb-watcher-assign)
    (define-key map (kbd        "e") #'gdb-watcher-edit-expression)
    (define-key map (kbd        "f") #'gdb-watcher-change-format)
    (define-key map (kbd        "d") #'gdb-watcher-duplicate)
    (define-key map (kbd      "SPC") #'gdb-watcher-toggle)
    (define-key map (kbd      "TAB") #'gdb-watcher-toggle)
    (define-key map (kbd    "<tab>") #'gdb-watcher-toggle)
    (define-key map (kbd "<delete>") #'gdb-watcher-delete)
    map))

(define-derived-mode gdb-watchers-mode nil "GDB Watchers"
  (setq-local buffer-read-only t)
  (buffer-disable-undo))

(gdb--simple-get-buffer gdb--watchers gdb--watchers-update "Watchers" nil
  (gdb-watchers-mode))

(defun gdb--watcher-draw (table-or-parent watcher tick watcher-under-cursor)
  (let* ((out-of-scope (eq (gdb--watcher-flag watcher) 'out-of-scope))
         (row (gdb--table-add-row
               table-or-parent
               (list (gdb--add-face (gdb--watcher-expr watcher)  'font-lock-variable-name-face)
                     (gdb--add-face (gdb--watcher-type watcher)  'font-lock-type-face)
                     (if out-of-scope
                         (eval-when-compile (propertize "Out of scope" 'face 'font-lock-comment-face))
                       (gdb--add-face (gdb--watcher-value watcher)
                                      (and (eq (gdb--watcher-flag watcher) tick) 'error))))
               (list 'gdb--watcher watcher)
               (and (not out-of-scope) (> (gdb--watcher-children-count watcher) 0))))
         (children (gdb--watcher-children watcher)))

    (when (eq watcher watcher-under-cursor)
      (let ((table (gdb--get-table-from-table-or-parent table-or-parent)))
        (setf (gdb--table-target-line table) (gdb--table-num-rows table))))

    (when (and (not out-of-scope) (gdb--watcher-open watcher))
      (cl-loop for child in children
               do (gdb--watcher-draw row child tick watcher-under-cursor)))))

(defun gdb--watchers-update ()
  (gdb--with-valid-session
   (let ((table (make-gdb--table :target-line (gdb--current-line))))
     (gdb--table-add-header table '("" "Type" "Value"))
     (let ((watcher-under-cursor (get-text-property (line-beginning-position) 'gdb--watcher))
           (tick (gdb--session-watchers-tick session)))
       (cl-loop for watcher in (gdb--session-root-watchers session)
                do (gdb--watcher-draw table watcher tick watcher-under-cursor))
       (gdb--table-insert table)))))

(defun gdb--watchers-remove-records (session list)
  "Removes every reference to the watchers in LIST and their children."
  (cl-loop for watcher in list
           do (gdb--watchers-remove-records session (gdb--watcher-children watcher))
           do (remhash (gdb--watcher-name watcher) (gdb--session-watchers session))
           unless (gdb--watcher-parent watcher) do (setf (gdb--session-root-watchers session)
                                                         (delq watcher (gdb--session-root-watchers session)))))


;; ------------------------------------------------------------------------------------------
;; Registers buffer
(defvar gdb-registers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd          "p") #'previous-line)
    (define-key map (kbd          "n") #'next-line)
    ;; TODO(nox): Create the analogous commands or change these
    ;; (define-key map (kbd        "RET") #'gdb-create-watcher-from-variable)
    ;; (define-key map (kbd   "<return>") #'gdb-create-watcher-from-variable)
    ;; (define-key map (kbd      "S-RET") #'gdb-create-watcher-from-variable-ask)
    ;; (define-key map (kbd "<S-return>") #'gdb-create-watcher-from-variable-ask)
    map))

(define-derived-mode gdb-registers-mode nil "GDB Registers"
  (setq-local buffer-read-only t)
  (buffer-disable-undo))

(gdb--simple-get-buffer gdb--registers gdb--registers-update "Registers" nil
  (gdb-watchers-mode))

(defun gdb--registers-update ()
  (gdb--with-valid-session
   (let ((table (make-gdb--table :target-line (gdb--current-line)))
         (thread (gdb--session-selected-thread session)))
     (gdb--table-add-header table '("Name" "Value"))
     (when thread
       (cl-loop with tick = (gdb--thread-registers-tick thread)
                for reg being the hash-values of (gdb--thread-registers thread)
                for reg-tick = (gdb--register-tick reg)
                do (gdb--table-add-row
                    table (list (propertize (gdb--register-name  reg) 'face 'font-lock-variable-name-face)
                                (gdb--add-face (gdb--register-value reg)
                                               (when (and reg-tick (= tick reg-tick))
                                                 'error)))
                    (list 'gdb--register reg))))
     (gdb--table-insert table))))

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
   (when path (concat (file-remote-p (buffer-local-value 'default-directory (gdb--comint-get-buffer session)))
                      path))))

(defun gdb--display-source-buffer (&optional override-file override-line)
  "Display buffer of the selected source, and mark the current line.
The source file and line are fetched from the selected frame, unless OVERRIDE-FILE and OVERRIDE-LINE are set,
in which case those will be used.
OVERRIDE-LINE may also be `no-mark', which forces it to not mark any line."
  (gdb--with-valid-session
   (gdb--remove-all-symbols session 'gdb--source-indicator t)

   (let* ((frame (gdb--session-selected-frame session))
          (file (cond (override-file)
                      (frame (gdb--frame-file frame))))
          (line (cond ((and (not (eq override-line 'no-mark)) override-line))
                      (frame (gdb--frame-line frame))))
          (buffer (and file (gdb--find-file file)))
          (window (gdb--session-source-window session)))

     (unless (window-valid-p window)
       (let ((new-frame (make-frame `((gdb--session . ,session)
                                      (name . ,(gdb--frame-name session))
                                      (unsplittable . t)))))
         (x-focus-frame new-frame)
         (setq window (setf (gdb--session-source-window session) (frame-first-window new-frame)))
         (set-window-buffer window (gdb--comint-get-buffer session))))

     (when (and (not gdb--inhibit-display-source) buffer)
       (with-selected-window window
         (set-window-dedicated-p window nil)
         (switch-to-buffer buffer)

         (if (display-images-p)
             (set-window-fringes nil 8)
           (set-window-margins nil 2))

         (when line
           (goto-char (point-min))
           (forward-line (1- line))
           (recenter)))

       (gdb--place-symbol session buffer line '((type . source-indicator)))))))


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
            (setf (gdb--breakpoint-overlay breakpoint) overlay)
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

        (when (eq type 'source-indicator) (overlay-put overlay 'window (gdb--session-source-window session)))))))

(defun gdb--remove-all-symbols (session type &optional source-files-only)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless (and source-files-only gdb--buffer-info)
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (and (eq session (overlay-get ov 'gdb--indicator-session))
                     (or (eq type 'all) (overlay-get ov type)))
            (delete-overlay ov)))))))

(defun gdb--breakpoint-remove-symbol (breakpoint)
  (let ((overlay (gdb--breakpoint-overlay breakpoint)))
    (when overlay (delete-overlay overlay))))


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

(defun gdb--log-error (err) (message "GDB Error: %s" err))

(defun gdb--running (thread-id-str)
  (gdb--with-valid-session
   (let ((thread (gdb--get-thread-by-id (string-to-number thread-id-str)))
         (selected-thread (gdb--session-selected-thread session)))
     (when thread
       (setf (gdb--thread-state thread) "running"
             (gdb--thread-frames thread) nil)

       (when (eq thread selected-thread)
         (gdb--remove-all-symbols session 'gdb--source-indicator t)
         (unless (gdb--session-persist-thread session)
           (cl-loop for thread in (gdb--session-threads session)
                    when (string= (gdb--thread-state thread) "stopped")
                    do (gdb--switch-to-thread thread) and return nil))
         (setf (gdb--session-persist-thread session) nil))

       (cl-pushnew 'gdb--threads (gdb--session-buffer-types-to-update session))
       (cl-pushnew 'gdb--frames  (gdb--session-buffer-types-to-update session))))))

(defun gdb--set-initial-file (file)
  (gdb--display-source-buffer (gdb--complete-path file) 'no-mark))

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
       (gdb--command "-data-list-register-names" (cons 'gdb--context-registers-list-names thread))
       (setf (gdb--session-threads session) (append (gdb--session-threads session) (list thread))))

     (cond
      ((string= "stopped" state)
       ;; NOTE(nox): Don't update right now, because it will update when the frame list arrives
       (gdb--command "-stack-list-frames" (cons 'gdb--context-frame-info thread) thread)
       (gdb--command "-var-update --all-values *" 'gdb--context-watcher-update thread)

       ;; TODO(): FORMATS!!!!!
       (cond ((= (gdb--thread-registers-tick thread) most-negative-fixnum)
              (gdb--command "-data-list-changed-registers" nil thread) ;; NOTE(nox): Ignored
              (gdb--command "-data-list-register-values --skip-unavailable x"
                            (cons 'gdb--context-registers-update thread) thread))
             (t
              (gdb--command "-data-list-changed-registers"
                            (cons 'gdb--context-registers-get-changed (cons "x" thread)) thread))))

      (t
       (cl-pushnew 'gdb--threads (gdb--session-buffer-types-to-update session))
       (gdb--conditional-switch thread '(no-selected-thread)))))))

(defun gdb--add-frames-to-thread (thread &rest args)
  (gdb--with-valid-session
   (setf (gdb--thread-frames thread)
         (cl-loop for (level-str addr func file line-str from) in args
                  collect (make-gdb--frame
                           :thread thread :level (string-to-number level-str) :addr addr :func func  :from from
                           :file (gdb--complete-path file) :line (gdb--stn line-str))))

   (cl-pushnew 'gdb--threads (gdb--session-buffer-types-to-update session))
   (when (eq thread (gdb--session-selected-thread session))
     (cl-pushnew 'gdb--frames (gdb--session-buffer-types-to-update session)))

   (gdb--conditional-switch (gdb--best-frame-to-switch-to thread) '(running same-thread))))

(defun gdb--breakpoint-changed (number-str type disp enabled-str addr func fullname line-str at
                                           pending thread cond times ignore-count what)
  (gdb--with-valid-session
   (let* ((number (string-to-number number-str))
          (enabled (string= enabled-str "y"))
          (file (gdb--complete-path fullname))
          (line (gdb--stn line-str))
          (existing-breakpoint (cl-loop for breakpoint in (gdb--session-breakpoints session)
                                        when (= number (gdb--breakpoint-number breakpoint))
                                        return breakpoint))
          (breakpoint (or existing-breakpoint (make-gdb--breakpoint))))

     (if existing-breakpoint
         (gdb--breakpoint-remove-symbol existing-breakpoint)
       (setf (gdb--session-breakpoints session) (append (gdb--session-breakpoints session) (list breakpoint))))

     (gdb--update-struct gdb--breakpoint breakpoint
       (number number) (type type) (disp disp) (addr addr) (hits times)
       (ignore-count (gdb--stn ignore-count)) (enabled enabled) (condition cond)
       (thread (gdb--get-thread-by-id (gdb--stn thread))) (pending pending)
       (file file) (line line) (gdb-fullname fullname) (func func)
       (what (concat (or what pending at (gdb--location-string func fullname line))
                     (and cond   (concat " if " cond))
                     (and thread (concat " on thread " thread)))))

     (gdb--place-symbol session (gdb--find-file file) line `((type . breakpoint-indicator)
                                                             (breakpoint . ,breakpoint)
                                                             (enabled . ,enabled)
                                                             (source . t)))

     (cl-pushnew 'gdb--breakpoints (gdb--session-buffer-types-to-update session)))))

(defun gdb--breakpoint-deleted (arg) ;; NOTE(nox): ARG may be ID number or breakpoint object
  (gdb--with-valid-session
   (let ((number (and (eq (type-of arg) 'string) (string-to-number arg))))
     (setf (gdb--session-breakpoints session)
           (cl-delete-if (lambda (breakpoint)
                           (when (cond (number (= (gdb--breakpoint-number breakpoint) number))
                                       ((eq arg breakpoint)))
                             (gdb--breakpoint-remove-symbol breakpoint)
                             t))
                         (gdb--session-breakpoints session))))
   (cl-pushnew 'gdb--breakpoints (gdb--session-buffer-types-to-update session))))

(defun gdb--add-variables-to-frame (frame &rest args)
  (gdb--with-valid-session
   (cl-loop for (name type value) in args
            do (push (make-gdb--variable :name name :type type :value value) (gdb--frame-variables frame)))
   (cl-pushnew 'gdb--variables (gdb--session-buffer-types-to-update session))))

(defun gdb--new-watcher-info (data name num-child value type thread-id)
  (gdb--with-valid-session
   (let* ((expr (car data))
          (watcher (make-gdb--watcher :name name :expr expr :type type :value value
                                      :thread (gdb--get-thread-by-id (gdb--stn thread-id))
                                      :children-count (or (gdb--stn num-child) 0)))
          (to-replace (cdr data)))
     (if to-replace
         (progn (setf (gdb--session-root-watchers session)
                      (cl-nsubstitute watcher to-replace (gdb--session-root-watchers session)))
                (gdb--command (concat "-var-delete " (gdb--watcher-name to-replace)))
                (gdb--watchers-remove-records session (list to-replace)))
       (setf (gdb--session-root-watchers session)
             (append (gdb--session-root-watchers session) (list watcher))))

     (puthash name watcher (gdb--session-watchers session))
     (cl-pushnew 'gdb--watchers (gdb--session-buffer-types-to-update session)))))

(defun gdb--watcher-update-info (&rest args)
  (gdb--with-valid-session
   (let ((tick (1+ (gdb--session-watchers-tick session))))
     (setf (gdb--session-watchers-tick session) tick)
     (cl-loop for (name value in-scope type-changed new-type new-children-count) in args
              do
              (let ((watcher (gethash name (gdb--session-watchers session))))
                (cond ((string= in-scope "true")
                       (when (string= type-changed "true")
                         ;; NOTE(nox): Children are automatically deleted
                         (gdb--watchers-remove-records session (gdb--watcher-children watcher))
                         (setf (gdb--watcher-type watcher) new-type
                               (gdb--watcher-children watcher) nil
                               (gdb--watcher-children-count watcher) (gdb--stn new-children-count)
                               (gdb--watcher-open watcher) nil))
                       (setf (gdb--watcher-value watcher) value
                             (gdb--watcher-flag  watcher) tick))

                      ((string= in-scope "false")
                       (setf (gdb--watcher-value watcher) nil
                             (gdb--watcher-flag  watcher) 'out-of-scope))

                      (t ;; NOTE(nox): Invalid
                       (gdb--command (concat "-var-delete " name))
                       (gdb--watchers-remove-records session (list watcher)))))
              finally
              ;; NOTE(nox): We need to update even if nothing changed, in order to remove the modified
              ;; highlight that could be here from the previous time
              (cl-pushnew 'gdb--watchers (gdb--session-buffer-types-to-update session))))))

(defun gdb--watcher-add-children (parent &rest children)
  (gdb--with-valid-session
   (setf (gdb--watcher-children-count parent) (length children))
   (cl-loop for (name expr num-children value type thread-id) in children
            do
            (let ((watcher (make-gdb--watcher
                            :name name :expr expr :type type :value value :parent parent
                            :thread (gdb--get-thread-by-id (gdb--stn thread-id))
                            :children-count (or (gdb--stn num-children) 0))))
              (puthash name watcher (gdb--session-watchers session))
              (push watcher (gdb--watcher-children parent)))
            finally (setf (gdb--watcher-children parent) (nreverse (gdb--watcher-children parent))))
   (cl-pushnew 'gdb--watchers (gdb--session-buffer-types-to-update session))))

(defun gdb--watcher-format-change (watcher new-value)
  (gdb--with-valid-session
   (setf (gdb--watcher-value watcher) new-value)
   (cl-pushnew 'gdb--watchers (gdb--session-buffer-types-to-update session))))

(defun gdb--set-register-names (thread &rest names)
  (gdb--with-valid-session
   (let ((registers (gdb--thread-registers thread)))
     (cl-loop for name in names
              for num from 0
              unless (string= name "")
              do (puthash num (make-gdb--register :number num :name name) registers)))))

(defun gdb--update-registers (thread &rest pairs)
  (gdb--with-valid-session
   (let ((registers (gdb--thread-registers      thread))
         (tick (1+  (gdb--thread-registers-tick thread))))
     (setf (gdb--thread-registers-tick thread) tick)
     (cl-loop for (num-str . value) in pairs
              for num = (string-to-number num-str)
              for reg = (gethash num registers)
              when reg do (setf (gdb--register-value reg) value
                                (gdb--register-tick  reg) tick))
     (cl-pushnew 'gdb--registers (gdb--session-buffer-types-to-update session)))))

;; (defun gdb--set-disassembly (buffer list with-source-info)
;;   (when (buffer-live-p buffer)
;;     (with-current-buffer buffer
;;       (setq-local gdb--disassembly-list list)
;;       (setq-local gdb--with-source-info with-source-info))
;;     (add-to-list 'gdb--buffers-to-update buffer)))

(defun gdb--persist-thread ()
  (gdb--with-valid-session (setf (gdb--session-persist-thread session) t)))

(defun gdb--set-data (result) (setq gdb--data (or result 'no-data)))

;; ------------------------------------------------------------------------------------------
;; Global minor mode
(define-minor-mode gdb-keys-mode
  "This mode enables global keybindings to interact with GDB."
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd    "<f5>") #'gdb-run-or-continue)
            (define-key map (kbd  "<C-f5>") #'gdb-start)
            (define-key map (kbd  "<S-f5>") #'gdb-kill)
            (define-key map (kbd    "<f6>") #'gdb-stop)
            (define-key map (kbd    "<f8>") #'gdb-watcher-add-expression)
            (define-key map (kbd    "<f9>") #'gdb-toggle-breakpoint)
            (define-key map (kbd   "<f10>") #'gdb-next)
            (define-key map (kbd "<M-f10>") #'gdb-next-instruction)
            (define-key map (kbd   "<f11>") #'gdb-step)
            (define-key map (kbd "<M-f11>") #'gdb-step-instruction)
            (define-key map (kbd "<C-f10>") #'gdb-until)
            (define-key map (kbd "<C-f11>") #'gdb-advance)
            (define-key map (kbd "<S-f11>") #'gdb-finish)
            (define-key map (kbd   "<f12>") #'gdb-switch-buffer/body)
            map))


;; ------------------------------------------------------------------------------------------
;; User commands
(defun gdb-watcher-add-expression (&optional default watcher-to-replace)
  (interactive)
  (gdb--with-valid-session
   (unless (gdb--session-selected-frame session) (user-error "No selected frame"))

   (let ((expression (cond ((consp default) (car default))
                           (t (gdb--read-line "Expression: " default)))))
     (when (and expression (not (and watcher-to-replace (string= expression default))))
       (gdb--command (format "-var-create - @ \"%s\"" expression)
                     (cons 'gdb--context-watcher-create (cons expression watcher-to-replace))
                     (gdb--session-selected-frame session))))))

(defun gdb-watcher-toggle ()
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--watchers)
     (let ((watcher (get-text-property (line-beginning-position) 'gdb--watcher)))
       (when (and watcher (> (gdb--watcher-children-count watcher) 0))
         (when (and (setf (gdb--watcher-open watcher) (not (gdb--watcher-open watcher)))
                    (not  (gdb--watcher-children watcher)))
           (gdb--command (concat "-var-list-children --simple-values " (gdb--watcher-name watcher))
                         (cons 'gdb--context-watcher-list-children watcher)))

         (cl-pushnew 'gdb--watchers (gdb--session-buffer-types-to-update session))
         (gdb--update))))))

(defun gdb-watcher-assign ()
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--watchers)
     (let ((frame (gdb--session-selected-frame session))
           (watcher (get-text-property (line-beginning-position) 'gdb--watcher)))
       (unless frame   (user-error "No frame is selected"))
       (unless watcher (user-error "No leaf watcher under cursor"))
       (unless (string= (gdb--get-data (concat "-var-show-attributes " (gdb--watcher-name watcher)) "attr")
                        "editable")
         (user-error "This watcher is not editable"))

       (gdb--command (format "-var-assign %s %s"
                             (gdb--watcher-name watcher)
                             (gdb--escape-argument (gdb--read-line "Value: " (gdb--watcher-value watcher)))))
       (gdb--command "-var-update --all-values *" 'gdb--context-watcher-update frame)))))

(defun gdb-watcher-edit-expression ()
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--watchers)
     (let ((watcher (get-text-property (line-beginning-position) 'gdb--watcher)))
       (when (or (not watcher) (gdb--watcher-parent watcher)) (user-error "No root watcher under cursor"))
       (gdb-watcher-add-expression
        (gdb--get-data (concat "-var-info-path-expression " (gdb--watcher-name watcher)) "path_expr")
        watcher)))))

(defun gdb-watcher-duplicate ()
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--watchers)
     (let ((watcher (get-text-property (line-beginning-position) 'gdb--watcher)))
       (unless watcher (user-error "No watcher under cursor"))
       (gdb-watcher-add-expression (gdb--get-data (concat "-var-info-path-expression " (gdb--watcher-name watcher))
                                                  "path_expr"))))))

(defun gdb-watcher-change-format ()
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--watchers)
     (let ((watcher (get-text-property (line-beginning-position) 'gdb--watcher)))
       (unless watcher (user-error "No watcher under cursor"))
       (gdb--command (format "-var-set-format %s %s" (gdb--watcher-name watcher)
                             (downcase (completing-read "Format: " '("Natural" "Binary" "Octal" "Decimal"
                                                                     "Hexadecimal" "Zero-Hexadecimal"))))
                     (cons 'gdb--context-watcher-change-format watcher))))))

(defun gdb-watcher-delete ()
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--watchers)
     (let ((watcher (get-text-property (line-beginning-position) 'gdb--watcher)))
       (when (or (not watcher) (gdb--watcher-parent watcher)) (user-error "No root watcher under cursor"))
       (gdb--command (concat "-var-delete " (gdb--watcher-name watcher)))
       (gdb--watchers-remove-records session (list watcher))
       (cl-pushnew 'gdb--watchers (gdb--session-buffer-types-to-update session))
       (gdb--update)))))

(defun gdb-create-watcher-from-variable (arg)
  (interactive "P")
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--variables)
     (let ((var (get-text-property (line-beginning-position) 'gdb--var)))
       (unless var (user-error "No variable selected"))
       (gdb-watcher-add-expression (if arg (gdb--variable-name var) (cons (gdb--variable-name var) nil)))
       (accept-process-output (gdb--session-process session) 0.5)
       (gdb--scroll-buffer-to-last-line (gdb--watchers-get-buffer session))))))

(defun gdb-create-watcher-from-variable-ask ()
  (interactive)
  (gdb-create-watcher-from-variable t))

(defun gdb-run (&optional arg break-main)
  "Start execution of the inferior from the beginning.
If ARG is non-nil, ask for program arguments (they will be used for subsequent runs).
If BREAK-MAIN is non-nil, break at main."
  (interactive "P")
  (gdb--with-valid-session
   (when (gdb-kill) (sit-for 0.05))
   (when arg
     (let ((arguments (read-string "Arguments: " (gdb--session-debuggee-args session))))
       (setf (gdb--session-debuggee-args session) arguments)
       (gdb--command (concat "-exec-arguments " arguments))))
   (gdb--command (concat "-exec-run" (and break-main " --start")))))

(defun gdb-continue (&optional arg)
  "If ARG is nil, try to resume threads in this order:
  - Inferred thread if it is stopped
  - Selected thread if it is stopped
  - All threads

If ARG is non-nil, resume all threads unconditionally."
  (interactive "P")
  (gdb--with-valid-session
   (let* ((inferred-thread (gdb--infer-thread 'not-selected))
          (selected-thread (gdb--session-selected-thread session))
          (thread-to-resume
           (unless arg
             (cond
              ((and inferred-thread (string= (gdb--thread-state inferred-thread) "stopped")) inferred-thread)
              ((and selected-thread (string= (gdb--thread-state selected-thread) "stopped")) selected-thread)))))

     (if (or arg (not thread-to-resume))
         (gdb--command "-exec-continue --all")
       (gdb--command "-exec-continue" nil thread-to-resume)))))

(defun gdb-run-or-continue (&optional arg)
  "When the inferior is not running, start it. Else, run `gdb-continue', which see."
  (interactive "P")
  (gdb--with-valid-session
   (if (gdb--session-threads session) (gdb-continue) (gdb-run arg))))

(defun gdb-start (&optional arg)
  "Start execution of the inferior from the beginning, stopping at the start of the inferior's main subprogram.
If ARG is non-nil, ask for program arguments."
  (interactive "P")
  (gdb-run arg t))

(defun gdb-stop (&optional arg)
  "If ARG is nil, try to stop threads in this order:
  - Inferred thread if it is running
  - Selected thread if it is running
  - All threads

If ARG is non-nil, stop all threads unconditionally."
  (interactive "P")
  (gdb--with-valid-session
   (let* ((inferred-thread (gdb--infer-thread 'not-selected))
          (selected-thread (gdb--session-selected-thread session))
          (thread-to-stop
           (unless arg
             (cond
              ((and inferred-thread (not (string= (gdb--thread-state inferred-thread) "stopped"))) inferred-thread)
              ((and selected-thread (not (string= (gdb--thread-state selected-thread) "stopped"))) selected-thread)))))

     (if (or arg (not thread-to-stop))
         (gdb--command "-exec-interrupt --all")
       (gdb--command "-exec-interrupt" nil thread-to-stop)))))

(defun gdb-kill ()
  "Kill inferior process."
  (interactive)
  (gdb--with-valid-session
   (when (gdb--session-threads session)
     (gdb--command "kill" nil nil 'no-resume)
     t)))

(defun gdb-select ()
  "Select inferred frame or thread."
  (interactive)
  (gdb--with-valid-session
   (let ((thread-or-frame (gdb--infer-thread-or-frame 'not-selected)))
     (when thread-or-frame
       (gdb--switch thread-or-frame)
       (gdb--update)))))

(defun gdb-next ()
  (interactive)
  (gdb--with-valid-session (gdb--command "-exec-next" 'gdb--context-persist-thread t)))

(defun gdb-next-instruction ()
  (interactive)
  (gdb--with-valid-session (gdb--command "-exec-next-instruction" 'gdb--context-persist-thread t)))

(defun gdb-step ()
  (interactive)
  (gdb--with-valid-session (gdb--command "-exec-step" 'gdb--context-persist-thread t)))

(defun gdb-step-instruction ()
  (interactive)
  (gdb--with-valid-session (gdb--command "-exec-step-instruction" 'gdb--context-persist-thread t)))

(defun gdb-until (&optional arg advance)
  "Run until inferred location. With non-nil ARG, prompt for location.
If ADVANCE is nil, then only run until code inside the same frame."
  (interactive "P")
  (gdb--with-valid-session
   (let ((location (gdb--point-location))
         (gdb--inhibit-display-source t))
     (when arg (setq location (gdb--read-line (if advance "Advance until: " "Until: ") location)))

     (when location
       (unless (gdb--session-threads session) (gdb-run nil t) (sit-for 0.5))
       ;; NOTE(nox): These commands are used because:
       ;; - `-exec-until' was blocking everything
       ;; - `advance' doesn't have an alternative
       (gdb--command (concat (if advance "advance " "until ") (gdb--escape-argument location))
                     'gdb--context-persist-thread nil nil t)))))

(defun gdb-advance (&optional arg)
  "Check `gdb-until'. This will make it run until code in any frame."
  (interactive "P")
  (gdb-until arg t))

(defun gdb-finish ()
  "Run until the end of the current function."
  (interactive)
  (gdb--with-valid-session (gdb--command "-exec-finish" 'gdb--context-persist-thread t)))

(defun gdb-jump (&optional arg)
  "Jump to inferred location. With non-nil ARG, prompt for location."
  (interactive)
  (gdb--with-valid-session
   (let ((location (gdb--point-location)))
     (when arg (setq location (gdb--read-line "Location: " location)))
     (when location (gdb--command (concat "-exec-jump " (gdb--escape-argument location)))))))

(defun gdb--switch-buffer (buffer-fun)
  (gdb--with-valid-session
   (let ((buffer (funcall buffer-fun session))
         (window (selected-window)))
     (if gdb--open-buffer-new-frame
         (let ((frame (make-frame `((gdb--session . ,session)
                                    (name . ,(gdb--frame-name session))
                                    (unsplittable . t)))))
           (x-focus-frame frame)
           (setq window (frame-first-window frame)))
       (when (eq window (gdb--session-source-window session))
         (setf (gdb--session-source-window session) nil)))
     (gdb--set-window-buffer window buffer))))

(defhydra gdb-switch-buffer
  (:hint nil :foreign-keys warn :exit t :body-pre (gdb--with-valid-session
                                                   "No session available!"
                                                   (setq gdb--open-buffer-new-frame nil)))
  "
Show GDB buffer in %s(if gdb--open-buffer-new-frame \"new frame\" \"selected window\") [_<f12>_]
_g_db shell     |  _t_hreads  |  _b_reakpoints  |  _v_ariables  |  _r_egisters | _s_ource buffer
_i_nferior i/o  |  _f_rames   |  ^ ^            |  _w_atcher    |  ^ ^         |
"
  ("g" (gdb--switch-buffer 'gdb--comint-get-buffer))
  ("i" (gdb--switch-buffer 'gdb--inferior-io-get-buffer))
  ("t" (gdb--switch-buffer 'gdb--threads-get-buffer))
  ("f" (gdb--switch-buffer 'gdb--frames-get-buffer))
  ("b" (gdb--switch-buffer 'gdb--breakpoints-get-buffer))
  ("v" (gdb--switch-buffer 'gdb--variables-get-buffer))
  ("w" (gdb--switch-buffer 'gdb--watchers-get-buffer))
  ("r" (gdb--switch-buffer 'gdb--registers-get-buffer))
  ("s" (progn (setf (gdb--session-source-window (gdb--infer-session))
                    (unless gdb--open-buffer-new-frame (selected-window)))
              (gdb--display-source-buffer)))
  ("<f12>" (setq gdb--open-buffer-new-frame (not gdb--open-buffer-new-frame)) :exit nil))

(defun gdb-toggle-breakpoint (&optional arg)
  "Toggle breakpoint in the current location.
When ARG is non-nil, prompt for additional breakpoint settings.
If ARG is `dprintf' create a dprintf breakpoint instead."
  (interactive "P")
  (gdb--with-valid-session
   (let ((breakpoint-on-point (gdb--infer-breakpoint))
         (location (gdb--point-location))
         (dprintf (eq arg 'dprintf))
         type thread ignore-count condition format-args location-input)
     (when (and (not location) breakpoint-on-point)
       (setq location (gdb--infer-breakpoint-location breakpoint-on-point)))

     (when arg
       (unless (string= (or location "")
                        (setq location-input (read-string (concat "Location: ") location)))
         (setq location location-input
               breakpoint-on-point nil))

       (when breakpoint-on-point
         (setq thread       (gdb--breakpoint-thread       breakpoint-on-point)
               ignore-count (gdb--breakpoint-ignore-count breakpoint-on-point)
               condition    (gdb--breakpoint-condition    breakpoint-on-point)))

       (setq
        type (cdr (assoc-string (completing-read (concat "Type of " (if dprintf "dprintf" "breakpoint") ":")
                                                 gdb--available-breakpoint-types nil t nil nil
                                                 (caar gdb--available-breakpoint-types))
                                gdb--available-breakpoint-types))
        thread (gdb--ask-for-thread thread)
        ignore-count (read-number "Ignore count: " (or ignore-count 0))
        condition (gdb--read-line "Condition: " condition))

       (when dprintf (setq format-args (gdb--read-line "Format and args: "))))

     (when breakpoint-on-point
       (gdb--command (format "-break-delete %d" (gdb--breakpoint-number breakpoint-on-point))
                     (cons 'gdb--context-breakpoint-delete breakpoint-on-point)))

     (when (and location (or arg (not breakpoint-on-point)))
       (gdb--command (concat "-" (if dprintf "dprintf" "break") "-insert "
                             (and (> (length condition) 0) (concat "-c " (gdb--escape-argument condition) " "))
                             (and thread (format "-p %d " (gdb--thread-id thread)))
                             (and ignore-count (> ignore-count 0) (format "-i %d " ignore-count))
                             type "-f " (gdb--escape-argument location)
                             (when dprintf (concat " " format-args)))
                     'gdb--context-breakpoint-insert nil t)))))

(defun gdb-toggle-dprintf ()
  (interactive)
  (gdb-toggle-breakpoint 'dprintf))

(defun gdb-delete-breakpoint ()
  (interactive)
  (gdb--with-valid-session
   (let ((breakpoint-on-point (gdb--infer-breakpoint)))
     (when breakpoint-on-point
       (gdb--command (format "-break-delete %d" (gdb--breakpoint-number breakpoint-on-point))
                     (cons 'gdb--context-breakpoint-delete breakpoint-on-point))))))

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
      (gdb--command "-gdb-set mi-async on")
      (gdb--command "-gdb-set non-stop on"))

    (gdb--setup-windows session)
    (gdb-keys-mode)

    session))

;;;###autoload
(defun gdb-executable ()
  "Start debugging an executable with GDB in a new frame."
  (interactive)
  (let ((debuggee-path (expand-file-name (read-file-name "Select executable to debug: " nil nil t
                                                         gdb--previous-executable 'file-executable-p)))
        (session (or (gdb--infer-session) (gdb-create-session))))
    (setq gdb--previous-executable debuggee-path)
    (setf (gdb--session-debuggee-path session) debuggee-path)

    (with-selected-frame (gdb--session-frame session)
      (gdb--command (concat "-file-exec-and-symbols " debuggee-path))
      (gdb--command "-file-list-exec-source-file" 'gdb--context-initial-file)
      (set-frame-parameter nil 'name (gdb--frame-name session))
      (gdb--rename-buffers-with-debuggee debuggee-path))))

(provide 'gdb)
;;; gdb.el ends here
