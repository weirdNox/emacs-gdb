;;; emacs-gdb.el --- GDB frontend                     -*- lexical-binding: t; -*-

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

;;; Commentary:

;; This will be a replacement for GDB mode.

;;; Code:
(require 'cl-lib)
(require 'comint)
(add-to-list 'load-path (expand-file-name "."))
(require 'emacs-gdb-module)

;; ------------------------------------------------------------------------------------------
;; User variables
(defvar gdb-debug nil
  "List of debug symbols, which will enable different components.
Possible values are:
  - `gdb-debug-timings': show timings of some function calls
  - `gdb-debug-log-commands': show which GDB commands are sent
  - `gdb-debug-raw-input': send comint input as is
  - `gdb-debug-raw-output': print GDB output as is
  - `gdb-debug-all': the same as having every debug flag")

(defvar gdb-disassembly-show-offset nil
  "Whether or not to show function offset in the disassembly buffer.")

;; ------------------------------------------------------------------------------------------
;; Private variables and constants
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

(defconst gdb--buffers-to-keep '(gdb--comint gdb--inferior-io)
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
  (next-token 0) token-contexts ;; (TOKEN . (CONTEXT . DATA))
  current-frame threads
  breakpoints)
(defvar gdb--sessions nil
  "List of gdb--session.")

(cl-defstruct gdb--instruction address function offset instruction)
(cl-defstruct gdb--source-instr-info file line instr-list)

(cl-defstruct gdb--buffer-info session type thread update-defun data)
(defvar-local gdb--buffer-info nil
  "GDB related information related to each buffer.")

;; ------------------------------------------------------------------------------------------
;; Faces and bitmaps
(define-fringe-bitmap 'gdb--fringe-breakpoint "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")
(define-fringe-bitmap 'gdb--fringe-arrow "\xe0\x90\x88\x84\x84\x88\x90\xe0")

(defface gdb--breakpoint-enabled
  '((t :foreground "red1"
       :weight bold))
  "Face for enabled breakpoint icon in fringe.")

(defface gdb--breakpoint-disabled
  '((((class color) (min-colors 88)) :foreground "grey70")
    (((class color) (min-colors 8) (background light)) :foreground "black")
    (((class color) (min-colors 8) (background dark)) :foreground "white")
    (((type tty) (class mono)) :inverse-video t)
    (t :background "gray"))
  "Face for disabled breakpoint icon in fringe.")

(defconst gdb--disassembly-font-lock-keywords
  '(;; 0xNNNNNNNN opcode
    ("^0x[[:xdigit:]]+[[:space:]]+\\(\\sw+\\)"
     (1 font-lock-keyword-face))
    ;; Hexadecimals
    ("0x[[:xdigit:]]+" . font-lock-constant-face)
    ;; Source lines
    ("^Line.*$" . font-lock-comment-face)
    ;; %register(at least i386)
    ("%\\sw+" . font-lock-variable-name-face)
    ;; <FunctionName+Number>
    ("<\\([^()[:space:]+]+\\)\\(([^>+]*)\\)?\\(\\+[0-9]+\\)?>"
     (1 font-lock-function-name-face)))
  "Font lock keywords used in `gdb--disassembly'.")

;; ------------------------------------------------------------------------------------------
;; GDB comint buffer


(fset 'gdb--comint-update 'ignore)

(defun gdb--comint-sender (process string)
  "Send user commands from comint."
  (if (gdb--debug-check 'gdb-debug-raw-input)
      (comint-simple-send process string)
    (comint-simple-send process (concat "-interpreter-exec console "
                                        (gdb--escape-argument string)))))

(defun gdb--output-filter (string)
  "Parse GDB/MI output."
  (let ((output (gdb--measure-time "Handle MI Output" (gdb--handle-mi-output string))))
    (gdb--update)
    (gdb--debug-execute-body '(gdb-debug-timings gdb-debug-log-commands) (message "--------------------"))
    (if (gdb--debug-check 'gdb-debug-raw-output)
        (concat string "--------------------\n")
      output)))

(defun gdb--comint-sentinel (process str)
  "Handle GDB comint process state changes."
  (when (or (not (buffer-name (process-buffer process)))
            (eq (process-status process) 'exit))
    (gdb-kill)))

(defun gdb--get-comint-process ()
  "Return GDB process."
  (and gdb--comint-buffer (buffer-live-p gdb--comint-buffer) (get-buffer-process gdb--comint-buffer)))

(defmacro gdb--local (&rest body)
  "Execute BODY inside GDB comint buffer."
  `(when (buffer-live-p gdb--comint-buffer)
     (with-current-buffer gdb--comint-buffer
       ,@body)))

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
  (gdb--local
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

(defun gdb--local-command (command &optional context thread frame)
  "Execute GDB COMMAND in a specific THREAD and FRAME.
That means it will use the arguments, if provided. If not, it
will try to infer a thread and frame from the current buffer, for
example, the thread under point in the threads buffer. If the
point has no contextual thread, it will use the
`gdb--thread-id' when set in the comint buffer.

See `gdb--command' for the meaning of CONTEXT."
  (when (not thread)
    (cond ((eq gdb--buffer-type 'gdb--threads)
           (setq thread (get-text-property (point) 'gdb--thread-id))
           (when thread (setq thread (int-to-string thread))))
          ((eq gdb--buffer-type 'gdb--frames)
           (setq thread (or gdb--thread-id (gdb--local gdb--thread-id)))
           (setq frame (get-text-property (point) 'gdb--frame-level))
           (when thread
             (setq thread (int-to-string thread))
             (when frame (setq frame (int-to-string frame)))))
          ((eq gdb--buffer-type 'gdb--disassembly) ;; TODO(nox): Do this when disassembly is done
           )
          (t (setq thread (gdb--local (and gdb--thread-id
                                           (int-to-string gdb--thread-id)))))))
  (gdb--command (concat command
                        (and thread (concat " --thread " thread))
                        (and thread frame (concat " --frame " frame)))
                context))

;; ------------------------------------------------------------------------------------------
;; Inferior I/O buffer
(defun gdb--inferior-io-init ()
  (gdb--rename-buffer "Inferior I/O")
  (let* ((inferior-process (get-buffer-process
                            (make-comint-in-buffer "GDB inferior" (current-buffer) nil)))
         (tty (or (process-get inferior-process 'remote-tty)
                  (process-tty-name inferior-process))))
    (gdb--command (concat "-inferior-tty-set " tty) 'gdb--context-ignore)
    (set-process-sentinel inferior-process 'gdb--inferior-io-sentinel)
    (add-hook 'kill-buffer-hook 'gdb--inferior-io-killed nil t)))

(fset 'gdb--inferior-io-update 'ignore)

(defun gdb--inferior-io-killed ()
  (with-current-buffer gdb--comint-buffer (comint-interrupt-subjob))
  (gdb--command "-exec-abort" 'gdb--context-ignore))

;; NOTE(nox): When the debuggee exits, Emacs gets an EIO error and stops listening to the
;; tty. This re-inits the buffer so everything works fine!
(defun gdb--inferior-io-sentinel (process str)
  (when (eq (process-status process) 'failed)
    (let ((buffer (process-buffer process)))
      (delete-process process)
      (if buffer
          (with-current-buffer buffer
            (gdb--inferior-io-init))))))

;; ------------------------------------------------------------------------------------------
;; Breakpoints buffer
(defun gdb--breakpoints-init ()
  (gdb--init-buffer "Breakpoints"))

(defun gdb--breakpoints-update ()
  (gdb--remove-all-symbols 'gdb--breakpoint-indicator t)
  (let ((breakpoints (gdb--local gdb--breakpoints))
        (table (make-gdb--table)))
    (gdb--table-add-row table '("Num" "Type" "Disp" "Enb" "Addr" "Hits" "What"))
    (dolist (breakpoint-cons breakpoints)
      (let* ((number (int-to-string (car breakpoint-cons)))
             (breakpoint (cdr breakpoint-cons))
             (type (gdb--breakpoint-type breakpoint))
             (disp (gdb--breakpoint-disp breakpoint))
             (enabled (gdb--breakpoint-enabled breakpoint))
             (enabled-display
              (if enabled
                  (eval-when-compile (propertize "y" 'font-lock-face font-lock-warning-face))
                (eval-when-compile (propertize "n" 'font-lock-face font-lock-comment-face))))
             (addr (gdb--breakpoint-addr breakpoint))
             (hits (gdb--breakpoint-hits breakpoint))
             (what (gdb--breakpoint-what breakpoint))
             (file (gdb--breakpoint-file breakpoint))
             (line (gdb--breakpoint-line breakpoint)))
        (gdb--table-add-row table (list number type disp enabled-display addr hits what)
                            `(gdb--breakpoint-number ,number))
        (gdb--place-symbol (gdb--find-file (gdb--complete-path file))
                           (when line (string-to-number line))
                           `((type . breakpoint-indicator) (number . ,number)
                             (enabled . ,enabled) (source . t))))
      ;; TODO(nox): Place on disassembly buffers
      )
    (let ((line (gdb--current-line)))
      (erase-buffer)
      (insert (gdb--table-string table " "))
      (gdb--scroll-buffer-to-line (current-buffer) line))))

;; ------------------------------------------------------------------------------------------
;; Threads buffer
(defun gdb--threads-init ()
  (gdb--init-buffer "Threads"))

(defun gdb--threads-update ()
  (let ((threads (gdb--local gdb--threads))
        (table (make-gdb--table))
        (count 1)
        (current-thread (gdb--local gdb--thread-id))
        current-thread-line)
    (gdb--table-add-row table '("ID" "TgtID" "Name" "State" "Core" "Frame"))
    (dolist (thread-cons threads)
      (let* ((id (car thread-cons))
             (id-str (int-to-string id))
             (thread (cdr thread-cons))
             (target-id (gdb--thread-target-id thread))
             (name (gdb--thread-name thread))
             (state (gdb--thread-state thread))
             (state-display
              (if (string= state "running")
                  (eval-when-compile (propertize "running" 'font-lock-face font-lock-string-face))
                (eval-when-compile (propertize "stopped" 'font-lock-face font-lock-warning-face))))
             (core (gdb--thread-core thread))
             (frame (gdb--threads-frame-string (car (gdb--thread-frames thread)))))
        (gdb--table-add-row table (list id-str target-id name state-display core frame)
                            `(gdb--thread-id ,id))
        (setq count (1+ count))
        (when (eq current-thread id) (setq current-thread-line count))))
    (let ((line (gdb--current-line)))
      (erase-buffer)
      (insert (gdb--table-string table " "))
      (gdb--scroll-buffer-to-line (current-buffer) line))
    (remove-overlays nil nil 'gdb--thread-indicator t)
    (when current-thread-line
      (gdb--place-symbol (current-buffer) current-thread-line '((type . thread-indicator))))))

(defun gdb--threads-frame-string (frame)
  (if frame
      (progn
        (setq frame (cdr frame))
        (let ((addr (gdb--frame-addr frame))
              (func (gdb--frame-func frame))
              (file (gdb--frame-file frame))
              (line (gdb--frame-line frame))
              (from (gdb--frame-from frame)))
          (when file (setq file (file-name-nondirectory file)))
          (concat
           "in " (propertize (or func "???") 'font-lock-face font-lock-function-name-face)
           " at " addr
           (or (and file line (concat " of " file ":" line))
               (and from (concat " of " from))))))
    "No information"))

;; ------------------------------------------------------------------------------------------
;; Frames buffer
(defun gdb--frames-init ()
  (gdb--init-buffer "Stack frames"))

(defun gdb--frames-update ()
  (let* ((thread-id (or gdb--thread-id (gdb--local gdb--thread-id)))
         (thread (when thread-id (gdb--local (alist-get thread-id gdb--threads))))
         (frames (when thread (gdb--thread-frames thread)))
         (table (make-gdb--table)))
    (gdb--table-add-row table '("Level" "Address" "Where"))
    (dolist (frame-cons frames)
      (let* ((level (car frame-cons))
             (level-str (int-to-string level))
             (frame (cdr frame-cons))
             (addr (gdb--frame-addr frame))
             (func (gdb--frame-func frame))
             (file (gdb--frame-file frame))
             (line (gdb--frame-line frame))
             (from (gdb--frame-from frame))
             (where (concat
                     "in " (propertize (or func "???") 'font-lock-face
                                       font-lock-function-name-face)
                     (or (and file line (concat " of " (file-name-nondirectory file)
                                                ":" line))
                         (and from (concat " of " from))))))
        (gdb--table-add-row table (list level-str addr where) (list 'gdb--frame-level level))))
    (let ((line (gdb--current-line)))
      (erase-buffer)
      (insert (gdb--table-string table " "))
      (gdb--scroll-buffer-to-line (current-buffer) line))
    (remove-overlays nil nil 'gdb--frame-indicator t)
    (when (and thread-id frames (eq thread-id (gdb--local gdb--thread-id)))
      (gdb--switch-to-frame 0))))

;; ------------------------------------------------------------------------------------------
;; Disassembly buffer
(defun gdb--disassembly-init ()
  (gdb--init-buffer "Disassembly")
  (set (make-local-variable 'font-lock-defaults)
       '(gdb--disassembly-font-lock-keywords)))

(defun gdb--disassembly-update ()
  (let ((list (when (boundp 'gdb--disassembly-list) gdb--disassembly-list))
        (with-source-info (when (boundp 'gdb--with-source-info) gdb--with-source-info))
        should-enable-font-lock begin end)
    (gdb--measure-time
     "Disassembly: Reverse main list" (setq list (nreverse list)))
    (gdb--measure-time
     "Disassembly: Set begin addr"
     (if with-source-info
         (let* ((info (car list))
                (instr-list (when info (gdb--source-instr-info-instr-list info)))
                ;; NOTE(nox): We need last because the list is not in order
                (first-instr (car (last instr-list))))
           (when first-instr (setq begin (gdb--instruction-address first-instr))))
       (let* ((first-instr (car list)))
         (when first-instr (setq begin (gdb--instruction-address first-instr))))))
    (gdb--measure-time
     "Disassembly: Create contents string"
     (when font-lock-mode
       (setq should-enable-font-lock t)
       (font-lock-mode 0))
     (erase-buffer)
     (if with-source-info
         (dolist (info list)
           (let* ((file (gdb--source-instr-info-file info))
                  (line (gdb--source-instr-info-line info))
                  (line-contents (gdb--get-line file (and line (string-to-number line)) t))
                  (instr-list (nreverse (gdb--source-instr-info-instr-list info))))
             (when instr-list
               (setq end (gdb--instruction-address (car (last instr-list)))))
             (setf (gdb--source-instr-info-instr-list info) instr-list)
             (when line-contents
               (insert (format "%-20s %s\n" (concat "Line " line) line-contents)))
             (dolist (instr instr-list)
               (insert (if gdb-disassembly-show-offset
                           (format "%-20s %-50s %s\n"
                                   (gdb--instruction-address instr)
                                   (gdb--instruction-instruction instr)
                                   (concat "<" (gdb--instruction-function instr) "+"
                                           (gdb--instruction-offset instr) ">"))
                         (format "%-20s %s\n"
                                 (gdb--instruction-address instr)
                                 (gdb--instruction-instruction instr)))))))
       (when list
         (setq end (gdb--instruction-address (car (last list)))))
       (dolist (instr list)
         (insert (if gdb-disassembly-show-offset
                     (format "%-20s %-50s %s\n"
                             (gdb--instruction-address instr)
                             (gdb--instruction-instruction instr)
                             (concat "<" (gdb--instruction-function instr) "+"
                                     (gdb--instruction-offset instr) ">"))
                   (format "%-20s %s\n"
                           (gdb--instruction-address instr)
                           (gdb--instruction-instruction instr))))))
     (when should-enable-font-lock
       (font-lock-mode 1)))
    (gdb--measure-time
     "Disassembly: Misc"
     (setq-local gdb--begin-address (gdb--convert-hex-to-number begin))
     (setq-local gdb--end-address (gdb--convert-hex-to-number end))
     (gdb--scroll-buffer-to-line (current-buffer) 1)
     (when (and gdb--begin-address gdb--end-address)
       (dolist (buffer-iter (buffer-list))
         (unless (eq buffer-iter (current-buffer))
           (let (iter-type iter-begin iter-end)
             (with-current-buffer buffer-iter
               (when (eq gdb--buffer-type 'gdb--disassembly)
                 (setq iter-type gdb--type
                       iter-begin (when (boundp 'gdb--begin-address) gdb--begin-address)
                       iter-end (when (boundp 'gdb--end-address) gdb--end-address))))
             (when (and (eq gdb--type iter-type)
                        (eq gdb--begin-address iter-begin)
                        (eq gdb--end-address iter-end))))))))))

(defun gdb--disassembly-fetch-info ()
  (let (file line addr)
    (cond ((eq gdb--type 'thread)
           (let ((thread-obj (gdb--local (alist-get gdb--thread-id gdb--threads)))
                 frame identifier)
             (when thread-obj
               (setq frame (cdr (car (gdb--thread-frames thread-obj))))
               (when frame
                 (setq file (gdb--frame-file frame)
                       line (gdb--frame-line frame)
                       addr (gdb--frame-addr frame))))))
          ((eq gdb--type 'function)
           (message "Not yet implemented"))
          ((eq gdb--type 'address)
           (message "Not yet implemented")))
    (cond ((and file line)
           (gdb--command (concat "-data-disassemble -f " file " -l " line
                                 " -- 4")
                         `(gdb--context-disassemble . ,(current-buffer))))
          (addr
           (gdb--command (concat "-data-disassemble -s " addr " -e "
                                 (gdb--escape-argument (concat addr "+2000"))
                                 " -- 4")
                         `(gdb--context-disassemble . ,(current-buffer)))))))

(defun gdb--convert-hex-to-number (hex-string)
  (if (not (and hex-string (stringp hex-string)))
      -1
    (setq hex-string (substring hex-string 2))
    (string-to-number hex-string 16)))

;; ------------------------------------------------------------------------------------------
;; Registers buffer
(defun gdb--registers-init ()
  (gdb--init-buffer "Registers"))

(defun gdb--registers-update ()
  )

;; ------------------------------------------------------------------------------------------
;; Locals buffer
(defun gdb--locals-init ()
  (gdb--init-buffer "Locals"))

(defun gdb--locals-update ()
  )

;; ------------------------------------------------------------------------------------------
;; Expression watcher buffer
(defun gdb--expression-watcher-init ()
  (gdb--init-buffer "Expression watcher"))

(defun gdb--expression-watcher-update ()
  )

;; ------------------------------------------------------------------------------------------
;; Utility functions
(defun gdb--init-buffer (buffer-name)
  "Buffer initialization for GDB buffers."
  (kill-all-local-variables)
  (setq-local buffer-read-only t)
  (buffer-disable-undo)
  (gdb--rename-buffer buffer-name))

(defun gdb--rename-buffer (&optional specific)
  "Rename special GDB buffer, using SPECIFIC when provided."
  (rename-buffer (concat "*GDB" (when (and specific (not (string= "" specific)))
                                  (concat " - " specific))
                         "*")
                 t))

(defun gdb--get-buffer (buffer-type &optional parameters)
  "Get existing GDB buffer of a specific BUFFER-TYPE."
  (let (symbol)
    (catch 'found
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (eq gdb--buffer-type buffer-type)
                     (catch 'break
                       (dolist (parameter parameters)
                         (setq
                          symbol (intern (concat "gdb--" (symbol-name (car parameter)))))
                         (when (not (and (boundp symbol)
                                         (equal (symbol-value symbol) (cdr parameter))))
                           (throw 'break nil)))
                       t))
            (throw 'found buffer)))))))

(defun gdb--get-buffer-create (buffer-type &optional parameters force-creation)
  "Get GDB buffer of a specific BUFFER-TYPE, creating it, if needed.
When creating, assign `gdb--buffer-type' to BUFFER-TYPE.
BUFFER-TYPE must be a member of `gdb--buffer-types'.

TODO parameters"
  (when (memq buffer-type gdb--buffer-types)
    (let ((buffer (when (not force-creation) (gdb--get-buffer buffer-type parameters)))
          (init-symbol (intern (concat (symbol-name buffer-type) "-init"))))
      (when (not buffer) (setq buffer (generate-new-buffer "*gdb-temp*")))
      (with-current-buffer buffer
        (when (not (eq gdb--buffer-status t))
          (when (eq gdb--buffer-status 'dead)
            (goto-char (point-max))
            (insert "\n\n--------- New GDB session ---------\n"))
          (funcall init-symbol)
          (setq gdb--buffer-status t
                gdb--buffer-type buffer-type)
          (dolist (parameter parameters)
            (set (make-local-variable
                  (intern (concat "gdb--" (symbol-name (car parameter)))))
                 (cdr parameter)))))
      buffer)))

(defun gdb--update ()
  (gdb--local
   (let ((inhibit-read-only t)
         (buffers-to-update gdb--buffers-to-update)
         (types-to-update gdb--buffer-types-to-update)
         symbol)
     (dolist (buffer (buffer-list))
       (with-current-buffer buffer
         (when (memq gdb--buffer-type types-to-update)
           (setq symbol (intern (concat (symbol-name gdb--buffer-type) "-update")))
           (gdb--measure-time (concat "Calling " (symbol-name symbol)) (funcall symbol)))))
     (dolist (buffer buffers-to-update)
       (when (and (buffer-live-p buffer) (not (memq (buffer-local-value 'gdb--buffer-type buffer)
                                                    types-to-update)))
         (with-current-buffer buffer
           (setq symbol (intern (concat (symbol-name gdb--buffer-type) "-update")))
           (gdb--measure-time (concat "Calling " (symbol-name symbol)) (funcall symbol)))))
     (setq gdb--buffers-to-update nil
           gdb--buffer-types-to-update nil))))

(defun gdb--setup-windows ()
  (delete-other-windows)
  (let* ((top-left (selected-window))
         (middle-left (split-window))
         (bottom-left (split-window middle-left))
         (top-right (split-window top-left nil t))
         (middle-right (split-window middle-left nil t))
         (bottom-right (split-window bottom-left nil t)))
    (balance-windows)
    (gdb--set-window-buffer top-left (gdb--get-buffer-create 'gdb--comint))
    (gdb--set-window-buffer top-right (gdb--get-buffer-create 'gdb--frames))
    (gdb--set-window-buffer middle-right (gdb--get-buffer-create 'gdb--inferior-io))
    (gdb--set-window-buffer bottom-left (gdb--get-buffer-create 'gdb--threads))
    (gdb--set-window-buffer bottom-right (gdb--get-buffer-create 'gdb--breakpoints))
    (gdb--local (setq gdb--source-window middle-left))
    (gdb--display-source-buffer)))

(defun gdb--set-window-buffer (window buffer)
  (set-window-dedicated-p window nil)
  (set-window-buffer window buffer)
  (set-window-dedicated-p window t))

(defun gdb--display-source-buffer (&optional no-mark)
  "Display buffer of the selected source."
  (gdb--local
   (let ((buffer (gdb--find-file gdb--selected-file))
         (line gdb--selected-line)
         (window (gdb--local gdb--source-window)))
     (gdb--remove-all-symbols 'gdb--source-indicator t)
     (unless no-mark
       (gdb--place-symbol buffer line '((type . source-indicator))))
     (when (and (window-live-p window) buffer)
       (with-selected-window window
         (switch-to-buffer buffer)
         (if (display-images-p)
             (set-window-fringes nil 8)
           (set-window-margins nil 2))
         (goto-char (point-min))
         (forward-line (1- line))
         (recenter 3))))))

(defun gdb--switch-to-thread (thread-id &optional always)
  (gdb--local
   (let ((selected-thread (and gdb--thread-id (alist-get gdb--thread-id gdb--threads))))
     (when (or always (not selected-thread)
               (and (not (eq thread-id gdb--thread-id))
                    (not (string= "stopped" (gdb--thread-state selected-thread)))))
       (setq gdb--thread-id thread-id)
       (when thread-id
         (add-to-list 'gdb--buffer-types-to-update 'gdb--frames)
         (gdb--update)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (eq gdb--buffer-type 'gdb--threads)
               (remove-overlays nil nil 'gdb--thread-indicator t)
               (let ((pos (text-property-any (point-min) (point-max)
                                             'gdb--thread-id thread-id)))
                 (when pos
                   (gdb--place-symbol buffer (line-number-at-pos pos)
                                      '((type . thread-indicator))))))))
         (message "Switched to thread %s." thread-id))))))

(defun gdb--switch-to-frame (frame-level)
  (let* ((thread-obj (gdb--local (alist-get gdb--thread-id gdb--threads)))
         (frame-obj (when thread-obj (alist-get frame-level (gdb--thread-frames thread-obj))))
         (file (when frame-obj (gdb--complete-path (gdb--frame-file frame-obj))))
         (line (when frame-obj (gdb--frame-line frame-obj))))
    (if (and file line)
        (progn
          (gdb--local
           (setq gdb--selected-file file
                 gdb--selected-line (string-to-number line))
           (gdb--display-source-buffer)))
      (gdb--remove-all-symbols 'gdb--source-indicator t))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq gdb--buffer-type 'gdb--frames)
          (remove-overlays nil nil 'gdb--frame-indicator t)
          (when (or (not gdb--thread-id)
                    (eq gdb--thread-id thread-id))
            (gdb--place-symbol buffer (+ 2 frame-level) '((type . frame-indicator)))))))))

(defun gdb--complete-path (path)
  "Add TRAMP prefix to PATH returned from GDB output, if needed."
  (concat (gdb--local (file-remote-p default-directory)) path))

(defun gdb--find-file (path)
  "Return the buffer of the file specified by PATH.
Create the buffer, if it wasn't already open."
  (when (and path (not (file-directory-p path)) (file-readable-p path))
    (with-current-buffer (find-file-noselect path t)
      (setq gdb--source-buffer t)
      (current-buffer))))

(defun gdb--get-line (path line &optional trim)
  "TODO"
  (when (and path (> line 0))
    (let ((buffer (gdb--find-file (gdb--complete-path path))))
      (when buffer
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- line))
            (let ((string (thing-at-point 'line)))
              (if trim
                  (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" "" string)
                string))))))))

(defun gdb--current-line ()
  "Return an integer of the current line of point in the current
buffer."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun gdb--scroll-buffer-to-line (buffer line)
  (let ((windows (get-buffer-window-list buffer nil t)))
    (dolist (window windows)
      (with-selected-window window
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun gdb--point-location ()
  "Return a GDB-readable location of the point, in a source file
or in a special GDB buffer (eg. disassembly buffer)."
  (cond ((eq gdb--buffer-type 'gdb--disassembly) ;; TODO(nox): Do this when disassembly is done
         )
        ((buffer-file-name)
         (concat (buffer-file-name) ":" (int-to-string (gdb--current-line))))))

(defun gdb--place-symbol (buffer line data)
  (when (and buffer line data)
    (with-current-buffer buffer
      (let* ((type (alist-get 'type data))
             (pos (line-beginning-position (1+ (- line (line-number-at-pos)))))
             (overlay (make-overlay pos pos buffer))
             (dummy-string (make-string 1 ?x))
             property)
        (overlay-put overlay 'gdb--indicator t)
        (overlay-put overlay (intern (concat "gdb--" (symbol-name type))) t)
        (cond ((eq type 'breakpoint-indicator)
               (let ((number (alist-get 'number data))
                     (enabled (alist-get 'enabled data)))
                 (overlay-put overlay 'gdb--breakpoint-number number)
                 (if (display-images-p)
                     (setq property `(left-fringe gdb--breakpoint ,(if enabled
                                                                       'gdb--breakpoint-enabled
                                                                     'gdb--breakpoint-disabled)))
                   (setq property `((margin left-margin) ,(if enabled "B" "b"))))))
              ((memq type '(source-indicator frame-indicator thread-indicator))
               (if (display-images-p)
                   (setq property '(left-fringe right-triangle))
                 (setq property '((margin left-margin) "=>")))))
        (when (alist-get 'source data)
          (overlay-put overlay 'window (gdb--local gdb--source-window)))
        (put-text-property 0 1 'display property dummy-string)
        (overlay-put overlay 'before-string dummy-string)))))

(defun gdb--remove-all-symbols (type &optional source-files-only)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or gdb--source-buffer (and (not source-files-only) gdb--buffer-type))
        (remove-overlays nil nil type t)))))

(defun gdb--ask-for-thread (&optional default-id with-all)
  (gdb--local
   (let (collection default-string)
     (when with-all (push '("All" . nil) collection))
     (dolist (thread gdb--threads (setq collection (nreverse collection)))
       (let* ((id (car thread))
              (obj (cdr thread))
              (target-id (gdb--thread-target-id obj))
              (display (concat (int-to-string id) ": " target-id)))
         (push (cons display id) collection)
         (when (eq id default-id) (setq default-string display))))
     (cdr (assoc (completing-read "Thread: " collection nil t nil nil default-string)
                 collection)))))

(defun gdb--debug-check (debug-symbol)
  "Will return t when DEBUG-SYMBOL or `gdb-debug-all' is in `gdb-debug'.
Otherwise, return `nil'. DEBUG-SYMBOL may be a symbol or a list
of symbols."
  (or (memq 'gdb-debug-all gdb-debug)
      (if (listp debug-symbol)
          (catch 'break
            (dolist (symbol debug-symbol)
              (when (memq symbol gdb-debug) (throw 'break t))))
        (memq debug-symbol gdb-debug))))

(defmacro gdb--debug-execute-body (debug-symbol &rest body)
  "Execute body when DEBUG-SYMBOL or `gdb-debug-all' is in `gdb-debug'.
DEBUG-SYMBOL may be a symbol or a list of symbols."
  (when (gdb--debug-check (eval debug-symbol))
    `(progn ,@body)))

(defmacro gdb--measure-time (string &rest body)
  "Measure the time it takes to evaluate BODY."
  (if (gdb--debug-check 'gdb-debug-timings)
      `(let ((time (current-time))
             (result (progn ,@body)))
         (message "%s: %.06fs" ,string (float-time (time-since time)))
         result)
    `(progn ,@body)))

;; ------------------------------------------------------------------------------------------
;; Functions to be called by the dynamic module
(defun gdb--extract-context (token-string)
  "Return the context-data cons assigned to TOKEN-STRING, deleting
it from the list."
  (gdb--local
   (let* ((context (assoc token-string gdb--token-contexts))
          (result 1)
          data)
     (when context
       (setq gdb--token-contexts (delq context gdb--token-contexts)
             context (cdr context)
             data (cdr context)
             context (car context))
       (cons (catch 'found
               (dolist (test-context gdb--available-contexts)
                 (when (eq context test-context) (throw 'found result))
                 (setq result (1+ result)))
               0)
             data)))))

(defun gdb--done ())

(defun gdb--error ())

(defun gdb--running (thread-id)
  (setq thread-id (string-to-number thread-id))
  (gdb--local
   (let ((thread (alist-get thread-id gdb--threads)))
     (when thread
       (setf (gdb--thread-state thread) "running")
       (setf (gdb--thread-frames thread) nil)
       (add-to-list 'gdb--buffer-types-to-update 'gdb--threads)
       (add-to-list 'gdb--buffer-types-to-update 'gdb--frames)))
   (when (eq thread-id gdb--thread-id) (gdb--remove-all-symbols 'gdb--source-indicator t))))

(defun gdb--stopped (thread-id)
  (gdb--switch-to-thread (and thread-id (string-to-number thread-id))))

(defun gdb--set-initial-file (file line-string)
  (gdb--local (setq gdb--selected-file (gdb--complete-path file)
                    gdb--selected-line (string-to-number line-string)))
  (gdb--display-source-buffer t))

(defun gdb--breakpoint-changed (number type disp enabled addr func fullname line at
                                       pending thread cond times what)
  (gdb--local
   (let* ((number (string-to-number number))
          (breakpoint
           (make-gdb--breakpoint
            :type (or type "")
            :disp (or disp "")
            :enabled (string= enabled "y")
            :addr (or addr "")
            :hits (or times "")
            :what (concat (or what pending at
                              (concat "in "
                                      (propertize (or func "???")
                                                  'font-lock-face font-lock-function-name-face)
                                      (and fullname line (concat " at "
                                                                 (file-name-nondirectory fullname)
                                                                 ":"
                                                                 line))))
                          (and cond (concat " if " cond))
                          (and thread (concat " on thread " thread)))
            :file fullname
            :line line))
          (existing (assq number gdb--breakpoints)))
     (if existing
         (setf (alist-get number gdb--breakpoints) breakpoint)
       (add-to-list 'gdb--breakpoints `(,number . ,breakpoint) t))
     (add-to-list 'gdb--buffer-types-to-update 'gdb--breakpoints))))

(defun gdb--breakpoint-deleted (number)
  (gdb--local
   (setq number (string-to-number number))
   (setq gdb--breakpoints (assq-delete-all number gdb--breakpoints))
   (add-to-list 'gdb--buffer-types-to-update 'gdb--breakpoints)))

(defun gdb--get-thread-info (&optional id-str)
  (gdb--command (concat "-thread-info " id-str) 'gdb--context-thread-info))

(defun gdb--thread-exited (thread-id)
  (gdb--local
   (setq thread-id (string-to-number thread-id)
         gdb--threads (assq-delete-all thread-id gdb--threads))
   (when (eq thread-id gdb--thread-id)
     (gdb--switch-to-thread (car (car gdb--threads)) t))
   (add-to-list 'gdb--buffer-types-to-update 'gdb--threads))
  (add-to-list 'gdb--buffer-types-to-update 'gdb--frames))

(defun gdb--update-thread (id-str target-id name state core)
  (gdb--local
   (let* ((id (string-to-number id-str))
          (thread
           (make-gdb--thread
            :target-id target-id
            :name (or name "")
            :state state
            :core (or core "")))
          (existing (assq id gdb--threads)))
     (if existing
         (setf (alist-get id gdb--threads) thread)
       (add-to-list 'gdb--threads `(,id . ,thread) t))
     (when (not gdb--thread-id) (gdb--switch-to-thread id t))
     (if (string= "stopped" state)
         (gdb--command (concat "-stack-list-frames --thread " id-str)
                       (cons 'gdb--context-frame-info id-str))
       ;; NOTE(nox): Only update when it is running, otherwise it will update when the
       ;; frame list arrives.
       (add-to-list 'gdb--buffer-types-to-update 'gdb--threads)))))

(defun gdb--clear-thread-frames (thread-id)
  (gdb--local
   (setf (gdb--thread-frames (alist-get (string-to-number thread-id) gdb--threads)) nil)))

(defun gdb--add-frame-to-thread (thread-id level addr func file line from)
  (gdb--local
   (push (cons (string-to-number level)
               (make-gdb--frame :addr addr :func func :file file :line line :from from))
         (gdb--thread-frames (alist-get (string-to-number thread-id) gdb--threads)))))

(defun gdb--finalize-thread-frames (thread-id)
  (gdb--local
   (setq thread-id (string-to-number thread-id))
   (let* ((thread (alist-get thread-id gdb--threads))
          (frames (gdb--thread-frames thread)))
     (setf (gdb--thread-frames (alist-get thread-id gdb--threads))
           (nreverse frames)))
   (add-to-list 'gdb--buffer-types-to-update 'gdb--threads)
   (add-to-list 'gdb--buffer-types-to-update 'gdb--frames)))

(defun gdb--set-disassembly (buffer list with-source-info)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local gdb--disassembly-list list)
      (setq-local gdb--with-source-info with-source-info))
    (add-to-list 'gdb--buffers-to-update buffer)))

;; ------------------------------------------------------------------------------------------
;; Everything enclosed in here was adapted from gdb-mi.el
(defun gdb--escape-argument (string)
  "Return STRING quoted properly as an MI argument.
The string is enclosed in double quotes.
All embedded quotes, newlines, and backslashes are preceded with a backslash."
  (setq string (replace-regexp-in-string "\\([\"\\]\\)" "\\\\\\&" string))
  (setq string (replace-regexp-in-string "\n" "\\n" string t t))
  (concat "\"" string "\""))

(defun gdb--pad-string (string padding)
  (format (concat "%" (number-to-string padding) "s") string))

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
               (mapconcat 'identity
                          (cl-mapcar (lambda (s x) (gdb--pad-string s x))
                                     row column-sizes)
                          sep)
               properties))
      (gdb--table-rows table)
      (gdb--table-row-properties table))
     "\n")))

;; ------------------------------------------------------------------------------------------
;; User commands
;; TODO(nox): Implement reverse debugging
;; TODO(nox): Should we use a toggle like "Instruction mode" where, if set, `gdb-next'
;; works instruction-wise or have a separate command `gdb-nexti'?
;; TODO(nox): Need to think carefully about `-exec-return', this _does not_ execute the
;; inferior, so there's no *stopped notif. Issue a `-stack-list-frames' for every stopped
;; thread after doing this? Maybe...
(defun gdb-run (arg)
  "Start execution of the inferior from the beginning.
If ARG is non-nil, stop at the start of the inferior's main subprogram."
  (interactive "P")
  (gdb--command (concat "-exec-run" (and arg " --start"))))

(defun gdb-continue (arg)
  "TODO"
  (interactive "P")
  (if arg
      (gdb--command "-exec-continue --all")
    (gdb--local-command "-exec-continue")))

(defun gdb-interrupt (arg)
  "Interrupt inferred thread, unless ARG is non-nil, in which case
it will interrupt all threads."
  (interactive "P")
  (if arg
      (gdb--command "-exec-interrupt --all")
    (gdb--local-command "-exec-interrupt")))

(defun gdb-next ()
  "TODO"
  (interactive)
  (gdb--local-command "-exec-next"))

(defun gdb-step ()
  "TODO"
  (interactive)
  (gdb--local-command "-exec-step"))

(defun gdb-finish ()
  "TODO"
  (interactive)
  (gdb--local-command "-exec-finish"))

(defun gdb-select (arg)
  (interactive "P")
  (let (thread-id frame-level)
    (cond ((eq gdb--buffer-type 'gdb--threads)
           (setq thread-id (get-text-property (point) 'gdb--thread-id)))
          ((eq gdb--buffer-type 'gdb--frames)
           (setq thread-id (or gdb--thread-id (gdb--local gdb--thread-id)))
           (setq frame-level (get-text-property (point) 'gdb--frame-level))))
    (when (or arg (not thread-id))
      (let (collection default thread-obj frames)
        (setq thread-id (gdb--ask-for-thread thread-id))
        (setq thread-obj (gdb--local (alist-get thread-id gdb--threads))
              collection nil
              default nil)
        (when thread-obj (setq frames (gdb--thread-frames thread-obj)))
        (dolist (frame frames (setq collection (nreverse collection)))
          (let* ((iterator-level (car frame))
                 (display (concat (int-to-string iterator-level) " "
                                  (gdb--threads-frame-string frame))))
            (push (cons display iterator-level) collection)
            (when (eq iterator-level frame-level) (setq default display))))
        (setq frame-level (cdr (assoc (completing-read "Frame: " collection nil t nil nil default)
                                      collection)))))
    (when thread-id (gdb--switch-to-thread thread-id t))
    (when frame-level (gdb--switch-to-frame frame-level))))

(defun gdb-break (arg)
  "TODO"
  (interactive "P")
  (let ((location (gdb--point-location))
        (type "")
        (thread nil)
        (condition "")
        command)
    (when (or arg (not location))
      (setq type (cdr
                  (assoc
                   (completing-read "Type of breakpoint: " gdb--available-breakpoint-types nil
                                    t nil nil "Breakpoint")
                   gdb--available-breakpoint-types)))
      (setq location (read-from-minibuffer "Location: " location))
      (setq thread (gdb--ask-for-thread nil t))
      (setq condition (read-from-minibuffer "Condition: ")))
    (gdb--command (concat "-break-insert -f " type
                          (and (> (length condition) 0) (concat " -c " (gdb--escape-argument
                                                                        condition)))
                          (when thread (concat " -p " (int-to-string thread)))
                          " " location)
                  'gdb--context-breakpoint-insert
                  t)))

(defun gdb-delete-breakpoint ()
  "TODO"
  (interactive)
  (let* (number-string
         (file (buffer-file-name))
         (line (int-to-string (gdb--current-line))))
    (cond ((eq gdb--buffer-type 'gdb--breakpoints)
           (setq number-string (get-text-property (point) 'gdb--breakpoint-number)))
          ((eq gdb--buffer-type 'gdb--disassembly)
           ;; TODO(nox): Do this when the disassembly window is ready
           )
          (file
           (catch 'found
             (dolist (breakpoint (gdb--local gdb--breakpoints))
               (when (and (string= file (gdb--breakpoint-file (cdr breakpoint)))
                          (string= line (gdb--breakpoint-line (cdr breakpoint))))
                 (setq number-string (int-to-string (car breakpoint)))
                 (throw 'found t))))))
    (when (not number-string)
      (let (collection)
        (dolist (breakpoint (gdb--local gdb--breakpoints) (setq collection (nreverse collection)))
          (push (cons (concat (int-to-string (car breakpoint))
                              " " (gdb--breakpoint-what (cdr breakpoint)))
                      (int-to-string (car breakpoint)))
                collection))
        (setq number-string (cdr (assoc (completing-read "Which one? " collection nil
                                                         t)
                                        collection)))))
    (when number-string
      (gdb--breakpoint-deleted number-string)
      (gdb--command (concat "-break-delete " number-string) 'gdb--context-ignore))))

(defun gdb-disassemble (arg)
  (interactive "P")
  (let (type thread-id file line begin-address end-address buffer)
    (cond ((eq gdb--buffer-type 'gdb--threads) (setq type "Thread"))
          ((eq gdb--buffer-type 'gdb--frames) (setq type "Function"))
          ((buffer-file-name) (setq type "Function")))
    (when (or arg (not type))
      (setq type (completing-read "What? " '("Thread" "Function" "Address") nil t nil nil
                                  type)))
    (cond ((string= type "Thread")
           (when (eq gdb--buffer-type 'gdb--threads)
             (setq thread-id (get-text-property (point) 'gdb--thread-id)))
           (when (or arg (not thread-id))
             (setq thread-id (gdb--ask-for-thread thread-id)))
           (setq buffer (gdb--get-buffer-create
                         'gdb--disassembly `((type . thread)
                                             (thread-id . ,thread-id)))))
          ((string= type "Function")
           ;; TODO(nox): This can't be like this, the same function may be referred to by
           ;; different line numbers... :/ Or, as it is static, it doesn't matter?? Or
           ;; maybe check the begin/end addresses, after grabbing all information, and if
           ;; it already exists, kill the buffer? Maybe it is not worth the time..
           `((type . function)
             (file . ,file)
             (line . ,line)))
          ((string= type "Address")
           `((type . address)
             (begin-address . ,begin-address)
             (end-address . ,end-address))))
    (when buffer
      (with-selected-frame (make-frame '((name . "Emacs GDB - Disassembly")))
        (switch-to-buffer buffer)
        (gdb--disassembly-fetch-info)))))

(defun gdb-kill (&optional frame)
  "Kill GDB, and delete frame if there are more visible frames.
When called interactively:
 - If there are more frames, this will first delete the frame,
     which will call this function again and proceed to kill GDB.
 - Else, this will kill GDB."
  (interactive)
  (let* ((no-arg (not frame))
         (gdb-frame (gdb--local gdb--frame))
         (frame-live (frame-live-p gdb-frame))
         (more-frames (> (length (visible-frame-list)) 1))
         (should-delete-frame (and no-arg frame-live more-frames))
         (should-cleanup (or no-arg (eq frame gdb-frame))))
    (if should-delete-frame
        (delete-frame gdb-frame t) ; Only delete frame when running command, this
                                        ; function will be called again
      (when should-cleanup
        (let ((process (gdb--get-comint-process))
              (inferior-process (get-process "GDB inferior")))
          (when process (kill-process process))
          (when inferior-process (delete-process inferior-process)))
        (setq delete-frame-functions (remove 'gdb-kill delete-frame-functions))
        (gdb--remove-all-symbols 'gdb--indicator)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (cond (gdb--buffer-type
                   (if (memq gdb--buffer-type gdb--buffers-to-keep)
                       (setq gdb--buffer-status 'dead)
                     (kill-buffer)))
                  (gdb--source-buffer
                   (setq gdb--source-buffer nil)))))))))





(provide 'emacs-gdb)
;;; emacs-gdb.el ends here
