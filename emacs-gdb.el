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
(add-to-list 'load-path (expand-file-name "."))
(require 'emacs-gdb-module)
(require 'comint)
(require 'cl-lib)

(defvar gdb-debug nil
  "If non `nil', print raw GDB/MI output and accept any
  command.")

(defvar gdb--last-debuggee nil
  "Last executable to be ran by GDB.")

(defvar gdb--last-args nil
  "Last args to be passed to GDB.")

(defvar gdb--comint-buffer nil
  "Buffer where the GDB process runs.")

(defvar-local gdb--frame nil
  "The Emacs frame where GDB runs.")

(defvar-local gdb--source-window nil
  "Window where GDB shows its source files.")

(defvar-local gdb--main-thread nil
  "Number of the main selected thread.")

(defvar-local gdb--main-frame nil
  "Number of the main selected thread frame.")

(defvar-local gdb--selected-file ""
  "Name of the selected file for the main selected thread.")

(defvar-local gdb--selected-line 0
  "Number of the selected line for the main selected thread.")

(define-fringe-bitmap 'gdb--arrow "\xe0\x90\x88\x84\x84\x88\x90\xe0")

(defvar-local gdb--next-token 0
  "Next token for a GDB command.")

(defconst gdb--available-contexts
  '(gdb--context-ignore
    gdb--context-initial-file
    gdb--context-breakpoint-insert
    gdb--context-thread-info
    gdb--context-frame-info)
  "List of implemented token contexts.
Must be in the same order of the `token_context' enum in the
dynamic module.")

(defvar-local gdb--token-contexts '()
  "Alist of (TOKEN . (CONTEXT . DATA)) pairs.
TOKEN must be the decimal representation of a token as a string
and CONTEXT must be a member of `gdb--available-contexts'. DATA
may be any type of data, and will be CONTEXT specific.")

(cl-defstruct gdb--breakpoint type disp enabled addr hits what file line)
(defvar-local gdb--breakpoints nil
  "Alist of (NUMBER . BREAKPOINT) pairs.
NUMBER is an integer and BREAKPOINT is a `gdb--breakpoint'.")

(defconst gdb--available-breakpoint-types
  '(("Breakpoint" . "")
    ("Temporary Breakpoint" . "-t")
    ("Hardware Breakpoint" . "-h")
    ("Temporary Hardware Breakpoint" . "-t -h"))
  "Alist of (TYPE . FLAGS).
Both are strings. FLAGS are the flags to be passed to
-break-insert in order to create a breakpoint of TYPE.")

(define-fringe-bitmap 'gdb--breakpoint "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")

(defface gdb--breakpoint-enabled
  '((t
     :foreground "red1"
     :weight bold))
  "Face for enabled breakpoint icon in fringe.")

(defface gdb--breakpoint-disabled
  '((((class color) (min-colors 88)) :foreground "grey70")
    (((class color) (min-colors 8) (background light))
     :foreground "black")
    (((class color) (min-colors 8) (background dark))
     :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face for disabled breakpoint icon in fringe.")

(cl-defstruct gdb--frame level addr func file line from)
(cl-defstruct gdb--thread target-id name state core frames)
(defvar-local gdb--threads nil
  "Alist of (ID . THREAD) pairs.
ID is an integer and thread is a `gdb--thread'.")

(defvar-local gdb--source-buffer nil
  "Specifies whether or not this buffer is a source file buffer.")

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

(defconst gdb--buffers-to-keep
  '(gdb--comint
    gdb--inferior-io)
  "List of buffer types that should be kept after GDB is killed.")

(defvar-local gdb--buffer-type nil
  "Type of current GDB buffer.")

(defvar-local gdb--thread-number nil
  "Number of the thread associated with the current GDB buffer.")

(defvar-local gdb--buffer-status nil
  "Status of a GDB buffer.
Possible values are:
  - `nil', if never initialized
  - `dead', if was initialized previously, but GDB was killed afterwards
  - `t', if initialized")

(defvar-local gdb--buffers-to-update gdb--buffer-types
  "List of buffers types to redraw. Used when new information is
  available.")

(defun gdb--init-buffer (buffer-name)
  "Buffer initialization for GDB buffers."
  (kill-all-local-variables)
  (setq-local buffer-read-only t)
  (buffer-disable-undo)
  (gdb--rename-buffer buffer-name))

;; NOTE(nox): GDB comint buffer
(defun gdb--comint-init ()
  (kill-all-local-variables)
  (let* ((debuggee gdb--last-debuggee)
         (extra-args gdb--last-args)
         (debuggee-name (file-name-nondirectory debuggee))
         (debuggee-path (file-name-directory debuggee))
         (debuggee-exists (and (not (string= "" debuggee-name)) (file-executable-p debuggee)))
         (switches (split-string extra-args))
         (frame-name (concat "Emacs GDB" (when debuggee-exists
                                           (concat " - Debugging " debuggee-name)))))
    (gdb--rename-buffer debuggee-name)
    (setq gdb--comint-buffer (current-buffer))
    ;; TODO(nox): We should rename this if file is changed inside GDB, no? Is there a
    ;; way??
    (modify-frame-parameters nil `((name . ,frame-name)))
    (push "-i=mi" switches)
    (when debuggee-exists
      (cd debuggee-path)
      (push debuggee switches))
    (apply 'make-comint-in-buffer "GDB" (current-buffer) "gdb" nil switches)
    (setq-local comint-use-prompt-regexp t)
    (setq-local comint-prompt-regexp "^(gdb) ")
    (setq-local comint-prompt-read-only nil)
    (setq-local mode-line-process '(":%s"))
    (setq-local paragraph-separate "\\'")
    (setq-local paragraph-start comint-prompt-regexp)
    (setq-local comint-input-sender 'gdb--comint-sender)
    (setq-local comint-preoutput-filter-functions '(gdb--output-filter))
    (set-process-sentinel (get-buffer-process (current-buffer)) 'gdb--comint-sentinel)
    (setq gdb--frame (selected-frame))
    (gdb--command "-gdb-set mi-async on" 'gdb--context-ignore)
    ;; TODO(nox): Add customization for these settings?
    (gdb--command "-gdb-set non-stop on" 'gdb--context-ignore)
    (gdb--command "-file-list-exec-source-file" 'gdb--context-initial-file)))

(fset 'gdb--comint-update 'ignore)

(defun gdb--comint-sender (process string)
  "Send user commands from comint."
  (if gdb-debug
      (comint-simple-send process string)
    (comint-simple-send process (concat "-interpreter-exec console "
                                        (gdb--escape-argument string)))))

(defun gdb--output-filter (string)
  "Parse GDB/MI output."
  (let ((output (gdb--handle-mi-output string)))
    (gdb--update)
    (if gdb-debug
        string
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

(defun gdb--command (command &optional context)
  "Execute GDB COMMAND.
If provided, the CONTEXT is assigned to a unique token, which
will be received, alongside the output, by the dynamic module,
and used to know what the context of that output was.

CONTEXT may be a cons (CONTEXT-TYPE . DATA), where DATA is
anything relevant for the context, or just CONTEXT-TYPE.
CONTEXT-TYPE must be a member of `gdb--available-contexts'."
  (let* ((process (gdb--get-comint-process))
         (context-type (or (when (consp context) (car context)) context))
         token)
    (when process
      (when (memq context-type gdb--available-contexts)
        (gdb--local
         (setq token (int-to-string gdb--next-token)
               command (concat token command)
               gdb--next-token (1+ gdb--next-token))
         (when (not (consp context)) (setq context (cons context-type nil)))
         (push (cons token context) gdb--token-contexts)))
      (comint-simple-send process command))))

(defun gdb--local-command (command &optional context thread frame)
  "Execute GDB COMMAND in a specific THREAD and FRAME.
That means it will use the arguments, if provided. If not, it
will try to infer a thread and frame from the current buffer, for
example, the thread under point in the threads buffer. If the
point has no contextual thread, it will use the
`gdb--main-thread' and `gdb--main-frame', when set.

See `gdb--command' for the meaning of CONTEXT."
  (when (not thread)
    (cond ((eq gdb--buffer-type 'gdb--threads)
           (setq thread (get-text-property (point) 'gdb--thread-id)))
          ((eq gdb--buffer-type 'gdb--frames) ;; TODO(nox): Do this when frames is done
           )
          ((eq gdb--buffer-type 'gdb--disassembly) ;; TODO(nox): Do this when disassembly is done
           )
          (t (setq thread (gdb--local (and gdb--main-thread
                                           (int-to-string gdb--main-thread)))
                   frame (gdb--local (and gdb--main-frame
                                          (int-to-string gdb--main-frame)))))))
  (gdb--command (concat command
                        (and thread (concat " --thread " thread))
                        (and frame (concat " --frame " frame)))
                context))

;; NOTE(nox): Inferior I/O buffer
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

;; NOTE(nox): Breakpoints buffer
(defun gdb--breakpoints-init ()
  (gdb--init-buffer "Breakpoints"))

(defun gdb--breakpoints-update ()
  (gdb--remove-all-breakpoint-symbols)
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
        (gdb--place-breakpoint-symbol (gdb--find-file file) line t number enabled))
      ;; TODO(nox): Place on disassembly buffers
      )
    (erase-buffer)
    (insert (gdb--table-string table " "))))

(defun gdb--place-breakpoint-symbol (buffer line is-source-buffer breakpoint-number enabled)
  (when (and buffer line)
    (with-current-buffer buffer
      (let* ((pos (line-beginning-position (1+ (- (string-to-int line) (line-number-at-pos)))))
             (overlay (make-overlay pos pos buffer))
             (dummy-string "x")
             property)
        (if (display-images-p)
            (setq property `(left-fringe gdb--breakpoint ,(if enabled
                                                              'gdb--breakpoint-enabled
                                                            'gdb--breakpoint-disabled)))
          (setq property `((margin left-margin) ,(if enabled "B" "b"))))
        (put-text-property 0 1 'display property dummy-string)
        (when is-source-buffer
          (overlay-put overlay 'window (gdb--local gdb--source-window)))
        (overlay-put overlay 'gdb--breakpoint t)
        (overlay-put overlay 'gdb--breakpoint-number breakpoint-number)
        (overlay-put overlay 'before-string dummy-string)))))

(defun gdb--remove-all-breakpoint-symbols ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond (gdb--source-buffer
             (remove-overlays nil nil 'gdb--breakpoint t))))))

;; NOTE(nox): Threads buffer
(defun gdb--threads-init ()
  (gdb--init-buffer "Threads"))

(defun gdb--threads-update ()
  (let ((threads (gdb--local gdb--threads))
        (table (make-gdb--table)))
    (gdb--table-add-row table '("ID" "TgtID" "Name" "State" "Core" "Frame"))
    (dolist (thread-cons threads)
      (let* ((id (int-to-string (car thread-cons)))
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
        (gdb--table-add-row table (list id target-id name state-display core frame)
                            `(gdb--thread-id ,id))))
    (erase-buffer)
    (insert (gdb--table-string table " "))))

(defun gdb--threads-frame-string (frame)
  (if frame
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
             (and from (concat " of " from)))))
    "No information"))

;; NOTE(nox): Frames buffer
(defun gdb--frames-init ()
  (gdb--init-buffer "Stack frames"))

(defun gdb--frames-update ()
  (let* ((thread (or gdb--thread-number (gdb--local gdb--main-thread)))
         (frames (and thread
                      (gdb--local (gdb--thread-frames (alist-get thread gdb--threads)))))
         (table (make-gdb--table)))
    (gdb--table-add-row table '("Level" "Address" "Where"))
    (dolist (frame frames)
      (let* ((level (gdb--frame-level frame))
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
        (gdb--table-add-row table (list level addr where))))
    (erase-buffer)
    (insert (gdb--table-string table " "))))

;; NOTE(nox): Disassembly buffer
(defun gdb--disassembly-init ()
  (gdb--init-buffer "Stack frames"))

(defun gdb--disassembly-update ()
  )

;; NOTE(nox): Registers buffer
(defun gdb--registers-init ()
  (gdb--init-buffer "Registers"))

(defun gdb--registers-update ()
  )

;; NOTE(nox): Locals buffer
(defun gdb--locals-init ()
  (gdb--init-buffer "Locals"))

(defun gdb--locals-update ()
  )

;; NOTE(nox): Expression watcher buffer
(defun gdb--expression-watcher-init ()
  (gdb--init-buffer "Expression watcher"))

(defun gdb--expression-watcher-update ()
  )

(defun gdb--get-buffer (buffer-type &optional thread)
  "Get existing GDB buffer of a specific BUFFER-TYPE and THREAD,
when provided."
  (catch 'found
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq gdb--buffer-type buffer-type)
                   (or (not thread)
                       (eq gdb--thread-number thread)))
          (throw 'found buffer))))))

(defun gdb--get-buffer-create (buffer-type &optional thread)
  "Get GDB buffer of a specific BUFFER-TYPE (and THREAD, if specified), creating it, if needed.

When creating, assign `gdb--buffer-type' to BUFFER-TYPE and
`gdb--thread-number' to THREAD (if provided).

BUFFER-TYPE must be a member of `gdb--buffer-types'."
  (when (memq buffer-type gdb--buffer-types)
    (let ((buffer (gdb--get-buffer buffer-type thread))
          (init-symbol (intern (concat (symbol-name buffer-type) "-init"))))
      (when (not buffer) (setq buffer (generate-new-buffer "*gdb-temp*")))
      (with-current-buffer buffer
        (when (not (eq gdb--buffer-status t))
          (when (eq gdb--buffer-status 'dead)
            (goto-char (point-max))
            (insert "\n\n--------- New GDB session ---------\n"))
          (funcall init-symbol)
          (setq gdb--buffer-status t)
          (setq gdb--buffer-type buffer-type)
          (setq gdb--thread-number thread)))
      buffer)))

(defun gdb--rename-buffer (&optional specific)
  "Rename special GDB buffer, using SPECIFIC when provided."
  (rename-buffer (concat "*GDB"
                         (when (and specific (not (string= "" specific)))
                           (concat " - " specific))
                         "*")
                 t))

(defun gdb--complete-path (path)
  "Add TRAMP prefix to PATH returned from GDB output, if needed."
  (concat (gdb--local (file-remote-p default-directory)) path))

(defun gdb--find-file (path)
  "Return the buffer of the file specified by PATH.
Create the buffer, if it wasn't already open."
  (when (and path (not (file-directory-p path)) (file-readable-p path))
    (with-current-buffer (find-file-noselect path)
      (setq gdb--source-buffer t)
      (current-buffer))))

(defun gdb--display-source-buffer ()
  "Display buffer of the selected source."
  (gdb--local
   (let ((buffer (gdb--find-file gdb--selected-file))
         (line gdb--selected-line)
         (window (gdb--local gdb--source-window)))
     (when (and (window-live-p window) buffer)
       (with-selected-window window
         (switch-to-buffer buffer)
         (if (display-images-p)
             (set-window-fringes nil 8)
           (set-window-margins nil 2))
         (goto-char (point-min))
         (forward-line (1- line))
         (recenter 2))))))

(defun gdb--set-window-buffer (window buffer)
  (set-window-dedicated-p window nil)
  (set-window-buffer window buffer)
  (set-window-dedicated-p window t))

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

(defun gdb--current-line ()
  "Return an integer of the current line of point in the current
buffer."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun gdb--point-location ()
  "Return a GDB-readable location of the point, in a source file
or in a special GDB buffer (eg. disassembly buffer)."
  (cond ((eq gdb--buffer-type 'gdb--disassembly) ;; TODO(nox): Do this when disassembly is done
         )
        ((buffer-file-name)
         (concat (buffer-file-name) ":" (int-to-string (gdb--current-line))))))

(defun gdb--update ()
  (let ((types-to-update (gdb--local gdb--buffers-to-update))
        (inhibit-read-only t))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (memq gdb--buffer-type types-to-update)
          (funcall (intern (concat (symbol-name gdb--buffer-type) "-update")))))))
  (gdb--local (setq gdb--buffers-to-update nil)))

;; NOTE(nox): Functions to be called by the dynamic module
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
  (gdb--get-thread-info thread-id)
  ;; TODO(nox): Handle selected thread
  )

(defun gdb--stopped (thread-id)
  (gdb--local
   (when (or (not gdb--main-thread)
             (and
              (not (eq (string-to-int thread-id) gdb--main-thread))
              (not (string= "stopped"
                            (gdb--thread-state (alist-get
                                                gdb--main-thread
                                                gdb--threads))))))
     (setq gdb--main-thread (string-to-int thread-id))
     (message "Switched to thread %s." thread-id))
   (gdb--command (concat "-stack-list-frames --thread " thread-id)
                 (cons 'gdb--context-frame-info thread-id))))

(defun gdb--set-initial-file (file line-string)
  (gdb--local (setq gdb--selected-file (gdb--complete-path file)
                    gdb--selected-line (string-to-int line-string)))
  (gdb--display-source-buffer))

(defun gdb--breakpoint-changed (number type disp enabled addr func fullname line at
                                       pending thread cond times what)
  (gdb--local
   (let* ((number (string-to-int number))
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
     (add-to-list 'gdb--buffers-to-update 'gdb--breakpoints))))

(defun gdb--breakpoint-deleted (number)
  (gdb--local
   (setq number (string-to-int number))
   (setq gdb--breakpoints (assq-delete-all number gdb--breakpoints))
   (add-to-list 'gdb--buffers-to-update 'gdb--breakpoints)))

(defun gdb--get-thread-info (&optional id)
  (gdb--command (concat "-thread-info " id) 'gdb--context-thread-info))

(defun gdb--thread-exited (id)
  (gdb--local
   (setq id (string-to-int id))
   (setq gdb--threads (assq-delete-all id gdb--threads))
   (when (eq id gdb--main-thread)
     (setq gdb--main-thread (car (car gdb--threads))))
   (add-to-list 'gdb--buffers-to-update 'gdb--threads)))

(defun gdb--update-thread (id target-id name state core)
  (gdb--local
   (let* ((id (string-to-int id))
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
     (when (not gdb--main-thread)
       (setq gdb--main-thread id))
     (add-to-list 'gdb--buffers-to-update 'gdb--threads))))

(defun gdb--clear-thread-frames (thread-id)
  (gdb--local
   (setf (gdb--thread-frames (alist-get (string-to-int thread-id) gdb--threads)) nil)))

(defun gdb--add-frame-to-thread (thread-id level addr func file line from)
  (gdb--local
   (push (make-gdb--frame :level level :addr addr :func func
                          :file file :line line :from from)
         (gdb--thread-frames (alist-get (string-to-int thread-id) gdb--threads)))))

(defun gdb--finalize-thread-frames (thread-id)
  (gdb--local
   (setq thread-id (string-to-int thread-id))
   (let* ((thread (alist-get thread-id gdb--threads))
          (frames (gdb--thread-frames thread)))
     (setf (gdb--thread-frames (alist-get thread-id gdb--threads))
           (nreverse frames)))
   (add-to-list 'gdb--buffers-to-update 'gdb--frames)))

;; ----------------------------------------------------------------------
;; NOTE(nox): Everything enclosed in here was adapted from gdb-mi.el
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
;; ----------------------------------------------------------------------

;; NOTE(nox): User commands
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
  (gdb--command "-exec-run" (and arg " --start")))

(defun gdb-continue (arg)
  "TODO"
  (interactive "P")
  (if arg
      (gdb--command "-exec-continue --all")
    (gdb--local-command "-exec-continue")))

(defun gdb-interrupt (arg)
  "Interrupt inferred tread, unless ARG is non-nil, in which case
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

(defun gdb-break (arg)
  "TODO"
  (interactive "P")
  (let ((location (gdb--point-location))
        (type "")
        (thread "")
        (condition "")
        command)
    (when (or arg (not location))
      (setq type (cdr
                  (assoc
                   (completing-read "Type of breakpoint: " gdb--available-breakpoint-types nil
                                    t nil nil "Breakpoint")
                   gdb--available-breakpoint-types)))
      (setq location (read-from-minibuffer "Location: " location))
      (let ((threads (gdb--local gdb--threads))
            thread-alist)
        (push '("All" . nil) thread-alist)
        (dolist (thread threads (setq thread-alist (nreverse thread-alist)))
          (let* ((thread-id (int-to-string (car thread)))
                 (thread-obj (cdr thread))
                 (target-id (gdb--thread-target-id thread-obj)))
            (push (cons (concat thread-id ": " target-id) thread-id) thread-alist)))
        (setq thread (cdr (assoc (completing-read "Thread: " thread-alist nil t nil nil "All")
                                 thread-alist))))
      (setq condition (read-from-minibuffer "Condition: ")))
    (gdb--command (concat "-break-insert -f " type
                          (and (> (length condition) 0) (concat " -c " (gdb--escape-argument
                                                                        condition)))
                          (and (> (length thread) 0) (concat " -p " thread))
                          " " location)
                  'gdb--context-breakpoint-insert)))

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
        (gdb--remove-all-breakpoint-symbols)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (cond (gdb--buffer-type
                   (if (memq gdb--buffer-type gdb--buffers-to-keep)
                       (setq gdb--buffer-status 'dead)
                     (kill-buffer)))
                  (gdb--source-buffer
                   (setq gdb--source-buffer nil)))))))))

;;;###autoload
(defun gdb (arg)
  "Start GDB in a new frame, or switch to existing GDB session
TODO(nox): Complete this"
  (interactive "P")
  (let* ((this-frame (equal arg '(16)))
         (stop-or-specify (or this-frame (equal arg '(4)))))
    (if stop-or-specify (gdb-kill))
    (let* ((frame (gdb--local gdb--frame))
           (frame-live (and (not stop-or-specify) (frame-live-p frame)))
           (gdb-running (and (not stop-or-specify) (gdb--get-comint-process))))
      (cond ((and gdb-running frame-live)
             (with-selected-frame frame (gdb--setup-windows)))
            ((and gdb-running (not frame-live))
             (gdb-kill)
             (error "Frame was dead and process was still running - this should never happen."))
            (t
             (let* ((debuggee
                     (or (unless stop-or-specify gdb--last-debuggee)
                         (and (y-or-n-p "Do you want to debug an executable?")
                              (expand-file-name (read-file-name
                                                 "Select file to debug: "
                                                 nil gdb--last-debuggee t gdb--last-debuggee
                                                 'file-executable-p)))
                         ""))
                    (extra-args (or (unless stop-or-specify gdb--last-args)
                                    (read-string "Extra arguments: " gdb--last-args))))
               (setq gdb--last-debuggee debuggee)
               (setq gdb--last-args extra-args)
               (if this-frame
                   (setq frame (selected-frame))
                 (unless frame-live (setq frame (make-frame '((fullscreen . maximized))))))
               (add-to-list 'delete-frame-functions 'gdb-kill)
               (with-selected-frame frame (gdb--setup-windows)))))))
  (select-frame-set-input-focus (gdb--local gdb--frame)))

(provide 'emacs-gdb)
;;; emacs-gdb.el ends here
