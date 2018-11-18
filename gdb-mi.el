;;; gdb-mi.el --- GDB Graphical Interface -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Gonçalo Santos

;; Author: Gonçalo Santos (weirdNox @ GitHub)
;; Homepage: https://github.com/weirdNox/emacs-gdb
;; Keywords: lisp gdb mi debugger graphical interface
;; Package-Requires: ((emacs "26.1") (hydra "0.14.0"))
;; Version: 0.1

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

;; This package provides a graphical user interface to GDB.

;;; Code:
;; ------------------------------------------------------------------------------------------
;; Package and related things
(require 'cl-lib)
(require 'comint)
(require 'hydra)

(eval-and-compile
  (unless (bound-and-true-p module-file-suffix)
    (error "Dynamic modules are NOT supported in your build of Emacs")))

(let* ((default-directory (file-name-directory (or load-file-name default-directory)))
       (required-module (concat "gdb-module" module-file-suffix))
       (path-to-module  (concat default-directory required-module)))

  (if (file-exists-p required-module)
      (require 'gdb-module path-to-module)

    (message "Compiling GDB dynamic module...")
    (with-current-buffer (compile (concat "make -k " required-module))
      (add-hook 'compilation-finish-functions
                (lambda (_buffer _status) (require 'gdb-module path-to-module)) nil t))))

(declare-function gdb--handle-mi-output "ext:gdb-module")

(defun gdb--clear-old-customization-vars ()
  (setplist 'gdb-buffers  nil)
  (setplist 'gdb-non-stop nil))
(eval-after-load 'cus-load #'gdb--clear-old-customization-vars)

;; ------------------------------------------------------------------------------------------
;; User configurable variables
(defgroup gdb nil
  "A GDB graphical interface"
  :group 'tools
  :prefix  "gdb-"
  :version "26.1")

(defcustom gdb-enable-global-keybindings t
  "If non-nil, enable `gdb-keys-mode' which provides global keybindings while there are open sessions."
  :group 'gdb
  :type 'boolean
  :version "26.1")

(defcustom gdb-window-setup-function #'gdb--setup-windows
  "Function to setup the windows and buffers in the main frame.
If you want to write your own, use `gdb--setup-windows' as inspiration!"
  :group 'gdb
  :type 'function
  :version "26.1")

(defcustom gdb-watchers-hide-access-specifiers t
  "When non-nil, this will hide the access specifiers in the watchers.
This can be changed in a debugging session with the command `gdb-watchers-toggle-access-specifiers'."
  :group 'gdb
  :type  'boolean
  :version "26.1")

(defcustom gdb-ignore-gdbinit t
  "When non-nil, use the flag \"-nx\" which makes GDB ignore .gdbinit."
  :group 'gdb
  :type  'boolean
  :version "26.1")

(defcustom gdb-debug nil
  "List of debug symbols, which will enable different components.
Possible values are:
  - `timings': show timings of some function calls
  - `commands': print to the messages buffer which GDB commands are sent
  - `raw-input': send comint input as is
  - `raw-output': print GDB/MI output to the messages buffer

This can also be set to t, which means that all debug components are active."
  :group 'gdb
  :type  '(choice boolean
                  (set (const :tag "Timings of some function calls" timings)
                       (const :tag "Print, to the messages buffer, which GDB commands are sent" commands)
                       (const :tag "Send comint input as is" raw-input)
                       (const :tag "Print GDB/MI output to the messages buffer" raw-output)))
  :version "26.1")


;; ------------------------------------------------------------------------------------------
;; Faces and bitmaps
(when (display-images-p)
  (define-fringe-bitmap 'gdb--fringe-breakpoint "\x3c\x7e\xff\xff\xff\xff\x7e\x3c"))

(defgroup gdb-faces nil
  "Faces in the GDB graphical interface"
  :group 'gdb
  :version "26.1"
  :prefix "gdb-")

(defface gdb-breakpoint-enabled-face
  '((t :foreground "red1" :weight bold))
  "Face for enabled breakpoint icon in fringe."
  :group 'gdb-faces
  :version "26.1")

(defface gdb-breakpoint-disabled-face
  '((((class color) (min-colors 88)) :foreground "gray70")
    (((class color) (min-colors 8) (background light)) :foreground "black")
    (((class color) (min-colors 8) (background dark)) :foreground "white")
    (((type tty) (class mono)) :inverse-video t)
    (t :background "gray"))
  "Face for disabled breakpoint icon in fringe."
  :group 'gdb-faces
  :version "26.1")

(defface gdb-function-face '((t :inherit font-lock-function-name-face))
  "Face for highlighting function names."
  :group 'gdb-faces
  :version "26.1")

(defface gdb-constant-face '((t :inherit font-lock-constant-face))
  "Face for highlighting constant values."
  :group 'gdb-faces
  :version "26.1")

(defface gdb-variable-face '((t :inherit font-lock-variable-name-face))
  "Face for highlighting variable and register names in watcher, variables and registers buffers.")

(defface gdb-type-face '((t :inherit font-lock-type-face))
  "Face for highlighting types names in watcher and variables buffers.")

(defface gdb-modified-face '((t :inherit error))
  "Face for highlighting modified values in watcher and register buffers."
  :group 'gdb-faces
  :version "26.1")

(defface gdb-disassembly-src-face '((t :inherit font-lock-string-face))
  "Face for highlighting the source code in disassembly buffers."
  :group 'gdb-faces
  :version "26.1")

(defface gdb-disassembly-line-indicator-face '((t :inherit font-lock-type-face))
  "Face for highlighting the source code line number in disassembly buffers."
  :group 'gdb-faces
  :version "26.1")

(defface gdb-opcode-face '((t :inherit font-lock-keyword-face))
  "Face for highlighting opcodes in disassembly buffers."
  :group 'gdb-faces
  :version "26.1")

(defface gdb-raw-opcode-face '((t :inherit font-lock-comment-face))
  "Face for highlighting raw opcodes in disassembly buffers."
  :group 'gdb-faces
  :version "26.1")

(eval-and-compile
  (defface gdb-running-face '((t :inherit font-lock-string-face))
    "Face for highlighting the \"running\" keyword."
    :group 'gdb-faces
    :version "26.1")

  (defface gdb-stopped-face '((t :inherit font-lock-warning-face))
    "Face for highlighting the \"stopped\" keyword."
    :group 'gdb-faces
    :version "26.1")

  (defface gdb-out-of-scope-face '((t :inherit font-lock-comment-face))
    "Face for highlighting \"Out of scope\" in watcher buffers."
    :group 'gdb-faces
    :version "26.1")

  (defface gdb-watcher-hold-face '((t :inherit error))
    "Face for highlighting \"HOLD\" in watcher buffers."
    :group 'gdb-faces
    :version "26.1")

  (defface gdb-y-face '((t :inherit font-lock-warning-face))
    "Face for highlighting the enabled symbol \"y\" in breakpoint buffers."
    :group 'gdb-faces
    :version "26.1")

  (defface gdb-n-face '((t :inherit font-lock-comment-face))
    "Face for highlighting the disabled symbol \"n\" in breakpoint buffers."
    :group 'gdb-faces
    :version "26.1"))


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
    gdb--context-breakpoint-enable-disable ;; Data: (Breakpoint . NewState)
    gdb--context-breakpoint-delete ;; Data: Breakpoint
    gdb--context-get-variables ;; Data: Frame
    gdb--context-watcher-create ;; Data: [Expression WatcherToReplace StackDepth]
    gdb--context-watcher-update ;; Data: ShouldHighlight
    gdb--context-watcher-list-children
    gdb--context-watcher-change-format ;; Data: Watcher
    gdb--context-registers-list-names ;; Data: Thread
    gdb--context-registers-get-changed ;; Data: (FormatString . Thread)
    gdb--context-registers-update ;; Data: Thread
    gdb--context-disassemble ;; Data: BufferData
    gdb--context-persist-thread
    gdb--context-get-data ;; Data: Result name string
    gdb--context-get-console-data
    gdb--context-ignore-errors
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

(cl-defstruct gdb--register number name value tick)

(cl-defstruct gdb--thread
  id target-id name state frames core
  (registers-tick most-negative-fixnum) registers (registers-format "N"))

(cl-defstruct gdb--variable name type value)
(cl-defstruct gdb--frame thread level addr func file line from variables)

(cl-defstruct gdb--breakpoint
  session number type disp enabled addr hits ignore-count what
  thread pending condition file gdb-fullname line func overlays)
(defconst gdb--available-breakpoint-types
  '(("Breakpoint" . "")
    ("Temporary Breakpoint" . "-t ")
    ("Hardware Breakpoint" . "-h ")
    ("Temporary Hardware Breakpoint" . "-t -h "))
  "Alist of (TYPE . FLAGS).
Both are strings. FLAGS are the flags to be passed to -break-insert in order to create a
breakpoint of TYPE.")

(cl-defstruct gdb--watcher  name expr type value parent children-count children open flag
              thread-id stack-depth)

(cl-defstruct gdb--session
  frame process buffers source-window debuggee-path debuggee-args
  buffer-types-to-update buffers-to-update
  threads selected-thread persist-thread selected-frame
  breakpoints
  (watchers-tick most-negative-fixnum) (watchers (make-hash-table :test 'equal)) root-watchers
  (hide-access-spec gdb-watchers-hide-access-specifiers))
(defvar gdb--sessions nil
  "List of active sessions.")
(defvar gdb--session nil
  "Let-bound chosen session.")

(cl-defstruct gdb--buffer-info session type thread update-func data)
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

(cl-defstruct gdb--disassembly-instr addr func offset instr opcodes)
(cl-defstruct gdb--disassembly-src   file line-str    instrs)
(cl-defstruct gdb--disassembly-data (mode 4) func list new widths) ;; NOTE(nox): widths - [addr opcode instr]

(defvar gdb--inhibit-display-source nil)
(defvar gdb--open-buffer-new-frame  nil)
(defvar gdb--data nil)
(defvar gdb--omit-console-output nil)


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

(defmacro gdb--current-line (&optional pos)
  "Return current line number. When POS is provided, count lines until POS."
  `(save-excursion ,(when pos `(goto-char ,pos))
                   (beginning-of-line)
                   (1+ (count-lines 1 (point)))))

(defsubst gdb--stn (str) (and (stringp str) (string-to-number str)))
(defsubst gdb--nts (num) (and (numberp num) (number-to-string num)))
(defsubst gdb--add-face (string face) (when string (propertize string 'face face)))

(defmacro gdb--update-struct (type struct &rest pairs)
  (declare (indent defun))
  `(progn ,@(cl-loop for (key val) in pairs
                     collect `(setf (,(intern (concat (symbol-name type) "-" (symbol-name key))) ,struct) ,val))))

(defun gdb--location-string (&optional func file line from addr)
  (when file (setq file (file-name-nondirectory file)))
  (concat "in " (propertize (or func "??") 'face 'gdb-function-face)
          (and addr (concat " at " (propertize addr 'face 'gdb-constant-face)))
          (or (and file line (format " of %s:%d" file line))
              (and from (concat " of " from)))))

(defun gdb--frame-location-string (frame &optional for-threads-view)
  (cond (frame (gdb--location-string (gdb--frame-func frame) (gdb--frame-file frame) (gdb--frame-line frame)
                                     (gdb--frame-from frame) (and for-threads-view   (gdb--frame-addr frame))))
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

(defsubst gdb--parse-address (addr-str)
  (and addr-str (string-to-number (substring addr-str 2) 16)))


;; ------------------------------------------------------------------------------------------
;; Session management
(defsubst gdb--infer-session (&optional only-from-buffer)
  (or (and (not only-from-buffer) gdb--session)
      (and (gdb--buffer-info-p gdb--buffer-info)
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
  (let ((message (and (stringp (car body)) (pop body))))
    `(let* ((session (gdb--infer-session))
            (gdb--session session))
       (if (gdb--valid-session session)
           (progn ,@body)
         ,(when message `(error "%s" ,message))))))

(defun gdb--session-name (session)
  (let ((debuggee (gdb--session-debuggee-path session)))
    (concat "Session " (if (stringp debuggee)
                           (concat "debugging " (abbreviate-file-name debuggee))
                         "without debuggee"))))

(defmacro gdb--after-choosing-session (&rest body)
  (declare (debug (body)))
  `(let* ((collection (cl-loop for session in gdb--sessions
                               collect (cons (gdb--session-name session) session)))
          (session (or (gdb--infer-session)
                       (if (= (length gdb--sessions) 1)
                           (car gdb--sessions)
                         (cdr (assoc-string (completing-read "Which session? " collection nil t nil nil)
                                            collection)))))
          (gdb--session session))
     (when (gdb--valid-session session)
       ,@body)))

(defun gdb--kill-session (session)
  (when (and (gdb--session-p session) (memq session gdb--sessions))
    (setq gdb--sessions (delq session gdb--sessions))
    (when (= (length gdb--sessions) 0)
      (remove-hook 'delete-frame-functions #'gdb--handle-delete-frame)
      (remove-hook 'window-configuration-change-hook #'gdb--rename-frame)
      (gdb-keys-mode -1))

    (unless (cl-loop for frame in (frame-list)
                     when (and (not (eq (frame-parameter frame 'gdb--session) session))
                               (frame-visible-p frame) (not (frame-parent frame))
                               (not (frame-parameter frame 'delete-before)))
                     return t)
      (save-buffers-kill-emacs)
      (make-frame))

    (cl-loop for frame in (frame-list)
             when (and (eq (frame-parameter frame 'gdb--session) session)
                       (not (eq frame (gdb--session-frame session))))
             do (delete-frame frame))

    (when (frame-live-p (gdb--session-frame session))
      (set-frame-parameter (gdb--session-frame session) 'gdb--session nil)
      (delete-frame (gdb--session-frame session)))

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

(defun gdb--get-data (command key &rest args-to-command)
  "Synchronously retrieve result KEY of COMMAND.
ARGS-TO-COMMAND are passed to `gdb--command', after the context."
  (gdb--with-valid-session
   (setq gdb--data nil)
   (apply 'gdb--command command (cons 'gdb--context-get-data key) args-to-command)
   (while (not gdb--data) (accept-process-output (gdb--session-process session) 0.5))
   (when (stringp gdb--data) gdb--data)))

(defun gdb--get-console-data (command &rest args-to-command)
  "Synchronously retrieve output from command. Each line will turn into a string in a list.
ARGS-TO-COMMAND are passed to `gdb--command', after the context."
  (gdb--with-valid-session
   (setq gdb--data nil
         gdb--omit-console-output 'to-data)
   (apply 'gdb--command command 'gdb--context-get-console-data args-to-command)
   (while (or (not gdb--data) (user-ptrp gdb--data))
     (accept-process-output (gdb--session-process session) 0.5))
   (when (listp gdb--data) gdb--data)))


;; ------------------------------------------------------------------------------------------
;; Files
(defun gdb--find-file (path)
  "Return the buffer of the file specified by PATH.
Create the buffer, if it wasn't already open."
  (when (and path (not (file-directory-p path)) (file-readable-p path))
    (find-file-noselect path t)))

(defun gdb--get-line (path line &optional trim)
  (when (and path line (> line 0))
    (let ((buffer (gdb--find-file path)))
      (when buffer
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- line))
            (let ((string (thing-at-point 'line)))
              (if trim
                  (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" "" string)
                string))))))))

(defun gdb--complete-path (path)
  "Add TRAMP prefix to PATH returned from GDB output, if needed."
  (gdb--with-valid-session
   (when path (concat (file-remote-p (buffer-local-value 'default-directory (gdb--comint-get-buffer session)))
                      path))))

(defsubst gdb--local-path (complete-path)
  "Returns path local to the machine it is in (without TRAMP prefix)."
  (or (file-remote-p complete-path 'localname) complete-path))


;; ------------------------------------------------------------------------------------------
;; Fringe symbols
(defun gdb--place-symbol (session buffer line-or-pos data)
  "Place fringe symbol from SESSION on BUFFER.
LINE-OR-POS must be a line number or (pos).
DATA is an alist and must at least include `type'."
  (when (and (buffer-live-p buffer) line-or-pos data)
    (with-current-buffer buffer
      (let* ((type (alist-get 'type data))
             (pos (cond ((consp line-or-pos) (save-excursion (goto-char (car line-or-pos))
                                                             (line-beginning-position)))
                        ((line-beginning-position (1+ (- line-or-pos (gdb--current-line)))))))
             (overlay (make-overlay pos pos buffer))
             (dummy-string (make-string 1 ?x))
             property)
        ;; NOTE(nox): Properties for housekeeping, session and type of symbol
        (overlay-put overlay 'gdb--session session)
        (overlay-put overlay 'gdb--type    type)

        ;; NOTE(nox): Fringe spec: (left-fringe BITMAP [FACE])
        ;;            Margin spec: ((margin left-margin) STRING)
        (cond
         ((eq type 'breakpoint-indicator)
          (let ((breakpoint (alist-get 'breakpoint data))
                (enabled    (alist-get 'enabled    data)))
            (push overlay (gdb--breakpoint-overlays breakpoint))
            (overlay-put overlay 'gdb--breakpoint breakpoint)

            (if (display-images-p)
                (setq property
                      `(left-fringe gdb--fringe-breakpoint
                                    ,(if enabled 'gdb-breakpoint-enabled-face 'gdb-breakpoint-disabled-face)))
              (setq property `((margin left-margin) ,(if enabled "B" "b"))))))

         ((memq type '(source-indicator frame-indicator thread-indicator disassembly-indicator))
          (overlay-put overlay 'priority 10) ;; NOTE(nox): Above breakpoint symbols
          (if (display-images-p)
              (setq property '(left-fringe right-triangle compilation-warning))
            (setq property '((margin left-margin) "=>")))))

        (put-text-property 0 1 'display property dummy-string)
        (overlay-put overlay 'before-string dummy-string)

        (when (eq type 'source-indicator)
          (overlay-put overlay 'window (gdb--session-source-window session)))))))

(defsubst gdb--remove-symbols-in-curr-buffer (type)
  (remove-overlays nil nil 'gdb--type type))

(defun gdb--remove-all-symbols (session type &optional source-files-only)
  "Remove all symbols from SESSION, with type TYPE.
When SOURCE-FILES-ONLY is non-nil, only remove them from buffer
that are not created by GDB."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless (and source-files-only gdb--buffer-info)
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (and (eq session (overlay-get ov 'gdb--session))
                     (or (memq type (list 'all (overlay-get ov 'gdb--type)))))
            (delete-overlay ov)))))))

(defun gdb--place-breakpoint-in-disassembly (buffer breakpoint)
  (when buffer
    (with-current-buffer buffer
      (gdb--with-valid-session
       (let ((pos (text-property-any (point-min) (point-max) 'gdb--addr-num (gdb--parse-address
                                                                             (gdb--breakpoint-addr breakpoint)))))
         (when pos (gdb--place-symbol session buffer (cons pos nil)
                                      `((type . breakpoint-indicator)
                                        (breakpoint . ,breakpoint)
                                        (enabled . ,(gdb--breakpoint-enabled breakpoint))))))))))

(defun gdb--place-breakpoint (session breakpoint)
  (gdb--place-symbol session (gdb--find-file (gdb--breakpoint-file breakpoint)) (gdb--breakpoint-line breakpoint)
                     `((type . breakpoint-indicator)
                       (breakpoint . ,breakpoint)
                       (enabled . ,(gdb--breakpoint-enabled breakpoint))))

  (gdb--place-breakpoint-in-disassembly (gdb--get-buffer-with-type session 'gdb--disassembly) breakpoint))

(defun gdb--breakpoint-remove-symbol (breakpoint)
  (dolist (overlay (gdb--breakpoint-overlays breakpoint)) (delete-overlay overlay))
  (setf (gdb--breakpoint-overlays breakpoint) nil))


;; ------------------------------------------------------------------------------------------
;; Threads and frames
(defun gdb--get-thread-by-id (id)
  (gdb--with-valid-session
   (when id
     (cl-loop for thread in (gdb--session-threads session)
              when (= (gdb--thread-id thread) id) return thread))))

(defun gdb--best-frame-to-switch-to (thread)
  "Return the most relevant frame to switch to in THREAD's frames."
  (when thread
    (let ((fallback (car (gdb--thread-frames thread))))
      (or (unless (gdb--disassembly-is-visible)
            (cl-loop for frame in (gdb--thread-frames thread)
                     when (and (gdb--frame-file frame) (gdb--frame-line frame)) return frame))
          fallback))))

;; NOTE(nox): Called from the dynamic module
(defun gdb--switch-to-thread (thread &optional auto)
  "Unconditionally switch to _different_ THREAD. This will also switch to the most relevant frame.
THREAD may be nil, which means to remove the selected THREAD.
THREAD may also be an ID string."
  (gdb--with-valid-session
   (when (stringp thread) (setq thread (gdb--get-thread-by-id (string-to-number thread))))

   (unless (eq thread (gdb--session-selected-thread session))
     (setf (gdb--session-selected-thread session) thread)

     (when thread
       (gdb--command (format "-thread-select %d" (gdb--thread-id thread)) 'gdb--context-ignore-errors)
       (message "Switching to thread %d." (gdb--thread-id thread)))

     (gdb--switch-to-frame (gdb--best-frame-to-switch-to thread) auto)

     (let ((buffer (gdb--get-buffer-with-type session 'gdb--threads)) pos)
       (when buffer
         (with-current-buffer buffer
           (gdb--remove-symbols-in-curr-buffer 'thread-indicator)
           (when thread
             (when (setq pos (text-property-any (point-min) (point-max) 'gdb--thread thread))
               (gdb--place-symbol session (current-buffer) (gdb--current-line pos)
                                  '((type . thread-indicator))))))))

     (cl-pushnew 'gdb--frames    (gdb--session-buffer-types-to-update session))
     (cl-pushnew 'gdb--registers (gdb--session-buffer-types-to-update session)))))

;; NOTE(nox): Called from the dynamic module
(defun gdb--switch-to-frame (frame &optional auto)
  "Unconditionally switch to a _different_ FRAME.
When FRAME is in a different thread, switch to it.
FRAME may also be (ThreadIdString . LevelString).
When FRAME is `deselect-frame', then deselect the current frame but keep the selected thread."
  (gdb--with-valid-session
   (when (consp frame)
     (let ((thread (gdb--get-thread-by-id (string-to-number (car frame))))
           (level  (string-to-number (cdr frame))))
       (setq frame (cl-loop for frame in (gdb--thread-frames thread)
                            when (= (gdb--frame-level frame) level) return frame))))

   (let ((keep-thread (eq frame 'deselect-frame)))
     (setq frame (if keep-thread nil frame))

     (unless (eq frame (gdb--session-selected-frame session))
       (setf (gdb--session-selected-frame session) frame)
       (unless keep-thread (gdb--switch-to-thread (and frame (gdb--frame-thread frame)) auto))

       (when frame
         (gdb--command (format "-stack-select-frame %d" (gdb--frame-level frame)))
         (gdb--command "-var-update --all-values *" (cons 'gdb--context-watcher-update auto) frame))

       (if (and frame (not (gdb--frame-variables frame)))
           (gdb--command "-stack-list-variables --simple-values" (cons 'gdb--context-get-variables frame) frame)
         (cl-pushnew 'gdb--variables (gdb--session-buffer-types-to-update session)))

       (gdb--disassembly-fetch frame)
       (gdb--display-source-buffer)

       (let ((buffer (gdb--get-buffer-with-type session 'gdb--frames)) pos)
         (when buffer
           (with-current-buffer buffer
             (gdb--remove-symbols-in-curr-buffer 'frame-indicator)
             (when frame
               (when (setq pos (text-property-any (point-min) (point-max) 'gdb--frame frame))
                 (gdb--place-symbol session (current-buffer) (gdb--current-line pos)
                                    '((type . frame-indicator))))))))))))

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
                         (gdb--switch-to-frame frame t)
                       (gdb--switch-to-thread thread t))))))

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
;; Tables
(cl-defstruct gdb--table header rows (num-rows 0) column-sizes target-line start-line)
(cl-defstruct gdb--table-row table columns properties level has-children children-func)

(eval-when-compile
  (defvar gdb-table-mouse-map
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map t)
      (define-key map (kbd "<mouse-1>") #'gdb-table-mouse-toggle)
      map)))

(defsubst gdb--pad-string (string padding)
  (if (= padding 0)
      string
    (format (concat "%" (number-to-string padding) "s") (or string ""))))

(defun gdb--table-update-column-sizes (table columns &optional level has-children)
  "Update TABLE column sizes to include new COLUMNS.
LEVEL should be an integer specifying the indentation level."
  (unless (gdb--table-column-sizes table)
    (setf (gdb--table-column-sizes table) (make-list (length columns) 0)))

  (setf (gdb--table-column-sizes table)
        (cl-loop
         with len = (length (gdb--table-column-sizes table))
         for string in columns
         and size in (gdb--table-column-sizes table)
         and first = t then nil
         and count from 1
         when (= count len) collect 0
         else collect (- (max (abs size) (+ (string-width (or string ""))
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

(defun gdb--table-add-row (table-or-parent columns &optional properties has-children children-func)
  "Add a row of COLUMNS, a list of strings, to TABLE-OR-PARENT and recalculate column sizes.
When non-nil, PROPERTIES will be added to the whole row when printing.
TABLE-OR-PARENT should be a table or a table row, which, in the latter case, will be made the parent of
the inserted row.
HAS-CHILDREN should be t when this node has children."
  (let* ((table (gdb--get-table-from-table-or-parent table-or-parent))
         (parent (and (eq  (type-of table-or-parent) 'gdb--table-row) table-or-parent))
         (level (or (and parent (1+ (gdb--table-row-level parent))) 0))

         (row (make-gdb--table-row :table table :columns columns :properties properties :level level
                                   :has-children has-children :children-func children-func)))

    (gdb--table-update-column-sizes table columns level has-children)
    (setf (gdb--table-rows table) (append (gdb--table-rows table) (list row))
          (gdb--table-num-rows table) (1+ (gdb--table-num-rows table)))

    (when parent (setf (gdb--table-row-has-children parent) 'open))

    row))

(defun gdb--table-row-string (columns column-sizes sep &optional with-newline properties level has-children
                                      children-func)
  (apply #'propertize (cl-loop
                       for string in columns
                       and size   in column-sizes
                       and first = t then nil
                       unless first concat sep into result
                       concat (gdb--pad-string
                               (concat (and first (make-string (* (or level 0) 4) ? ))
                                       (and first (cond ((eq has-children t)
                                                         (eval-when-compile
                                                           (propertize "[+] " 'keymap gdb-table-mouse-map)))
                                                        ((eq has-children 'open)
                                                         (eval-when-compile
                                                           (propertize "[-] " 'keymap gdb-table-mouse-map)))))
                                       string)
                               size)
                       into result
                       finally return (concat result (and with-newline "\n")))
         (append properties (when (functionp children-func) (list 'gdb--table-fetch-func children-func)))))

(defun gdb--table-insert (table &optional sep)
  "Erase buffer and insert TABLE with columns separated with SEP (space as default)."
  (let ((column-sizes (gdb--table-column-sizes table))
        (sep (or sep " ")))
    (erase-buffer)

    (when (gdb--table-header table)
      (setq-local header-line-format
                  (list (if (display-images-p) " " "  ")
                        (gdb--table-row-string (gdb--table-header table) column-sizes sep))))

    (cl-loop for row in (gdb--table-rows table)
             for row-number from 1 with insert-newline = t
             when (= row-number (gdb--table-num-rows table)) do (setq insert-newline nil)
             do (insert (gdb--table-row-string (gdb--table-row-columns    row) column-sizes sep insert-newline
                                               (gdb--table-row-properties row) (gdb--table-row-level row)
                                               (gdb--table-row-has-children row)
                                               (gdb--table-row-children-func row))))

    (let ((buffer (current-buffer))
          (start-line  (gdb--table-start-line  table))
          (target-line (gdb--table-target-line table)))
      (when start-line
        (gdb--scroll-buffer-to-last-line buffer 'bottom)
        (gdb--scroll-buffer-to-line buffer (+ start-line scroll-margin) 'top-if-invisible))

      (when target-line
        (gdb--scroll-buffer-to-line buffer target-line)))))


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
                (push buffer (gdb--session-buffers session))
                (with-current-buffer buffer
                  (gdb--rename-buffer ,name (gdb--session-debuggee-path session))
                  (setq gdb--buffer-info (make-gdb--buffer-info :session session :type ',type
                                                                :update-func #',update-func))
                  ,@body
                  ,(if important
                       '(add-hook 'kill-buffer-hook #'gdb--important-buffer-kill-cleanup nil t)
                     '(add-hook 'kill-buffer-hook #'gdb--buffer-kill-cleanup nil t))

                  (if (display-images-p)
                      (setq left-fringe-width 8)
                    (setq left-margin-width 2)))

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

(defmacro gdb--buffer-get-data (&optional buffer)
  (if buffer
      `(let ((buffer ,buffer) info)
         (when buffer
           (setq info (buffer-local-value 'gdb--buffer-info buffer))
           (and  info (gdb--buffer-info-data info))))
    `(and gdb--buffer-info (gdb--buffer-info-data gdb--buffer-info))))


;; ------------------------------------------------------------------------------------------
;; Frames and windows
(defsubst gdb--name-for-window (buffer src first)
  (if first
      (if src "GDB Source" (buffer-local-value 'mode-name buffer))
    (concat " & " (if src "Source" (substring (buffer-local-value 'mode-name buffer) 3)))))

(defun gdb--rename-frame (&optional frame)
  "Rename FRAME, possibly using debuggee file name and buffer type."
  (setq frame (or frame (selected-frame)))
  (let ((session (frame-parameter frame 'gdb--session)))
    (when session
      (let* ((is-main (eq frame (gdb--session-frame session)))
             (src-window (gdb--session-source-window session))
             (prefix (or (and (not is-main)
                              (cl-loop with first = t
                                       for window in (window-list frame)
                                       for buffer =  (window-buffer window)
                                       for src    =  (eq src-window window)
                                       if (or src (buffer-local-value 'gdb--buffer-info buffer))
                                       concat (gdb--name-for-window buffer src first)
                                       and do (setq first nil)))
                         "GDB"))
             (debuggee (gdb--session-debuggee-path session))
             (suffix (and (stringp debuggee)
                          (file-executable-p debuggee)
                          (concat " - " (abbreviate-file-name debuggee)))))
        (set-frame-parameter frame 'name (concat prefix suffix))))))

(defun gdb--create-frame (session)
  (let ((frame (make-frame `((fullscreen . maximized)
                             (gdb--session . ,session)))))
    (setf (gdb--session-frame session) frame)
    (select-frame-set-input-focus frame)
    (gdb--rename-frame frame)
    frame))

(defun gdb--create-frame-for-buffer (session buffer)
  "Create a new frame with BUFFER. Return the frame's window."
  (let* ((new-frame (make-frame `((gdb--session . ,session))))
         (window (frame-first-window new-frame)))
    (select-frame-set-input-focus new-frame)
    (set-window-buffer window buffer)
    window))

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
      (gdb--display-source-buffer top-left))))

(defun gdb--switch-buffer (buffer-fun)
  (gdb--with-valid-session
   (let ((buffer (funcall buffer-fun session))
         (window (selected-window)))
     (if gdb--open-buffer-new-frame
         (let ((frame (make-frame `((gdb--session . ,session)))))
           (select-frame-set-input-focus frame)
           (setq window (frame-first-window frame)))

       (when (eq window (gdb--session-source-window session))
         (setf (gdb--session-source-window session) nil)))

     (let ((frame (window-frame window)))
       (unless (eq (frame-parameter frame 'gdb--session) session)
         (user-error "This frame does not belong to GDB"))

       (gdb--set-window-buffer window buffer)))))

(defun gdb--scroll-buffer-to-line (buffer line &optional where)
  (with-current-buffer buffer
    (goto-char (point-min))
    (beginning-of-line)
    (forward-line (1- line)))

  (dolist (window (get-buffer-window-list buffer nil t))
    (with-selected-window window
      (goto-char (point-min))
      (forward-line (1- line))
      (beginning-of-line)
      (let (recenter-redisplay)
        (cond ((eq where 'top)    (recenter 0))
              ((eq where 'center) (recenter))
              ((eq where 'bottom) (recenter -1))
              ((eq where 'top-if-invisible)
               (unless (pos-visible-in-window-p (point))
                 (recenter 0))))))))

(defun gdb--scroll-buffer-to-last-line (buffer &optional where)
  (with-current-buffer buffer
    (forward-line most-positive-fixnum)
    (beginning-of-line))

  (dolist (window (get-buffer-window-list buffer nil t))
    (with-selected-window window
      (forward-line most-positive-fixnum)
      (beginning-of-line)
      (let (recenter-redisplay)
        (cond ((eq where 'top)    (recenter 0))
              ((eq where 'center) (recenter))
              ((eq where 'bottom) (recenter -1)))))))


;; ------------------------------------------------------------------------------------------
;; Comint buffer
(define-derived-mode gdb-comint-mode comint-mode "GDB Comint"
  "Major mode for interacting with GDB."
  (setq-local comint-input-sender #'gdb--comint-sender)
  (setq-local comint-preoutput-filter-functions '(gdb--output-filter))
  (setq-local completion-at-point-functions '(gdb--comint-completion-at-point)))

(gdb--simple-get-buffer gdb--comint ignore "Comint" t
  (setq gdb--omit-console-output nil)
  (gdb-comint-mode)
  (let ((process-connection-type nil))
    (make-comint-in-buffer "GDB" buffer "gdb" nil "-i=mi"
                           (if gdb-ignore-gdbinit "-nx" "")))

  (let ((proc (get-buffer-process buffer)))
    (set-process-sentinel proc #'gdb--comint-sentinel)
    (setf (gdb--session-process session) proc)))

(defun gdb--comint-sender (process string)
  "Send user commands from comint."
  (if (gdb--debug-check 'raw-input)
      (gdb--command string nil)
    (let* ((use-default (= (length string) 0))
           (cmd (if use-default (or (gdb--buffer-get-data) "") string))
           comint-preoutput-filter-functions)
      (if use-default
          (progn (delete-char -1)
                 (comint-output-filter process (concat cmd "\n")))
        (setf (gdb--buffer-info-data gdb--buffer-info) cmd))

      (setq gdb--omit-console-output nil)
      (gdb--command (concat "-interpreter-exec console " (gdb--escape-argument cmd))))))

(defun gdb--output-filter (string)
  "Parse GDB/MI output."
  (gdb--debug-execute-body 'raw-output (message "%s" string))
  (let* ((gc-cons-threshold most-positive-fixnum)
         (output (gdb--measure-time "Handle MI Output" (gdb--handle-mi-output string))))
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

(defun gdb--comint-completion-at-point ()
  (let* ((beg (comint-line-beginning-position)) (end (point-max))
         (str (buffer-substring-no-properties beg end))
         (relevant-index (string-match "\\( [^ .*]+\\|\\(\\.\\|\\*\\)[^ .]*\\)$" str)))
    (unless (= beg end)
      (when relevant-index
        (setq relevant-index (1+ relevant-index)
              beg (+ beg relevant-index)))

      (list beg end (completion-table-dynamic
                     (lambda (_)
                       (let ((raw-list (gdb--get-console-data (concat "complete " str))))
                         (cl-loop for item in raw-list
                                  collect (substring item relevant-index)))))))))


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
  (cond ((buffer-file-name) (format "%s:%d" (buffer-file-name) (gdb--current-line)))
        ((gdb--is-buffer-type 'gdb--disassembly)
         (let* ((instr (get-text-property (line-beginning-position) 'gdb--instr))
                (addr  (and instr (gdb--disassembly-instr-addr instr))))
           (when addr (format "*%s" addr))))))

(defun gdb--infer-breakpoint (&optional session)
  (cond ((or (buffer-file-name)
             (gdb--is-buffer-type 'gdb--disassembly))
         (let ((pos (line-beginning-position)))
           (cl-loop for  overlay   in (overlays-in (1- pos) (1+ pos))
                    for  breakpoint = (overlay-get overlay 'gdb--breakpoint)
                    when (and breakpoint (or (not session) (eq session (gdb--breakpoint-session breakpoint))))
                    return breakpoint)))
        ((gdb--is-buffer-type 'gdb--breakpoints)
         (get-text-property (line-beginning-position) 'gdb--breakpoint))))

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

(defconst gdb--tramp-retry-file "/tmp/gdb-tramp-retry")

(defun gdb--inferior-tramp-sender (process string)
  (let* ((initial-dir (buffer-local-value 'default-directory (process-buffer process)))
         (temp-file (concat (file-remote-p initial-dir) gdb--tramp-retry-file)))
    (process-send-string process (concat string "\n"))
    (cl-letf (((symbol-function 'message) (lambda (&rest _))))
      (cl-loop for try from 1 to 3
               for contents = (with-temp-buffer (insert-file-contents temp-file) (buffer-string))
               if   (string= contents "") return nil
               else do (progn (write-region "" nil temp-file)
                              (process-send-string process contents))
               finally do (write-region "" nil temp-file)))))

(defun gdb--inferior-tramp-output-filter (string)
  (if (string= string "&\"warning: GDB: Failed to set controlling terminal: Operation not permitted\\n\"\n")
      ""
    string))

(defun gdb--inferior-io-initialization (&optional old-proc)
  (gdb--with-valid-session
   (let* ((default-directory (buffer-local-value 'default-directory
                                                 (gdb--get-buffer-with-type session 'gdb--comint)))
          (remote (file-remote-p default-directory))
          (buffer (current-buffer))
          inferior-process tty)
     (when old-proc (set-process-buffer old-proc nil))

     (save-excursion
       (setq inferior-process (get-buffer-process
                               (apply #'make-comint-in-buffer
                                      "GDB inferior" buffer (and remote "/bin/sh") nil
                                      (and remote (list "-c" (concat "cat >> " gdb--tramp-retry-file)))))
             tty (or (process-get inferior-process 'remote-tty) (process-tty-name inferior-process))))

     (gdb--command (concat "-inferior-tty-set " tty) (cons 'gdb--context-tty-set old-proc))

     (set-process-sentinel inferior-process #'gdb--inferior-io-sentinel)

     (when remote
       (write-region "" nil (concat remote gdb--tramp-retry-file))
       (setq-local comint-input-sender #'gdb--inferior-tramp-sender)
       (setq-local comint-preoutput-filter-functions '(gdb--inferior-tramp-output-filter))))))

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
    (suppress-keymap map t)
    (define-key map (kbd "p")   #'previous-line)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "SPC") #'gdb-select)
    (define-key map (kbd "c")   #'gdb-continue)
    (define-key map (kbd "s")   #'gdb-stop)
    map))

(define-derived-mode gdb-threads-mode nil "GDB Threads"
  (setq-local buffer-read-only t)
  (buffer-disable-undo)
  (font-lock-mode -1))

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
                  (eval-when-compile (propertize "running" 'face 'gdb-running-face))
                (eval-when-compile (propertize "stopped" 'face 'gdb-stopped-face))))
             (core (gdb--thread-core thread))
             (frame-str (gdb--frame-location-string (car (gdb--thread-frames thread)) t)))
         (gdb--table-add-row table (list id-str target-id name state-display core frame-str)
                             `(gdb--thread ,thread))
         (when (eq selected-thread thread) (setq selected-thread-line (gdb--table-num-rows table)))
         (when (eq cursor-on-thread thread) (setf (gdb--table-target-line table) (gdb--table-num-rows table)))))

     (gdb--remove-symbols-in-curr-buffer 'thread-indicator)
     (gdb--table-insert table)

     (when selected-thread-line
       (gdb--place-symbol session (current-buffer) selected-thread-line '((type . thread-indicator)))))))


;; ------------------------------------------------------------------------------------------
;; Stack frames buffer
(defvar gdb-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "p")   #'previous-line)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "SPC") #'gdb-select)
    (define-key map (kbd "c")   #'gdb-continue)
    map))

(define-derived-mode gdb-frames-mode nil "GDB Frames"
  (setq-local buffer-read-only t)
  (buffer-disable-undo)
  (font-lock-mode -1))

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
             (addr (gdb--add-face (gdb--frame-addr frame) 'gdb-constant-face))
             (where (gdb--frame-location-string frame)))
         (gdb--table-add-row table (list level-str addr where) (list 'gdb--frame frame))

         (when (eq selected-frame frame) (setq selected-frame-line (gdb--table-num-rows table)))
         (when (eq cursor-on-frame frame) (setf (gdb--table-target-line table) (gdb--table-num-rows table)))))

     (gdb--remove-symbols-in-curr-buffer 'frame-indicator)
     (gdb--table-insert table)

     (when selected-frame-line
       (gdb--place-symbol session (current-buffer) selected-frame-line '((type . frame-indicator)))))))


;; ------------------------------------------------------------------------------------------
;; Breakpoints buffer
(defvar gdb-breakpoints-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd        "p") #'previous-line)
    (define-key map (kbd        "n") #'next-line)
    (define-key map (kbd        "d") #'gdb-delete-breakpoint)
    (define-key map (kbd      "DEL") #'gdb-delete-breakpoint)
    (define-key map (kbd "<delete>") #'gdb-delete-breakpoint)
    (define-key map (kbd      "SPC") #'gdb-breakpoint-enable-disable)
    map))

(define-derived-mode gdb-breakpoints-mode nil "GDB Breakpoints"
  (setq-local buffer-read-only t)
  (buffer-disable-undo)
  (font-lock-mode -1))

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
                               (eval-when-compile (propertize "y" 'face 'gdb-y-face))
                             (eval-when-compile (propertize "n" 'face 'gdb-n-face)))))

         (gdb--table-add-row table (list (number-to-string (gdb--breakpoint-number breakpoint))
                                         (gdb--breakpoint-type breakpoint)
                                         (gdb--breakpoint-disp breakpoint)
                                         enabled-disp
                                         (gdb--add-face (gdb--breakpoint-addr breakpoint) 'gdb-constant-face)
                                         (gdb--breakpoint-hits breakpoint)
                                         (gdb--nts (gdb--breakpoint-ignore-count breakpoint))
                                         (gdb--breakpoint-what breakpoint))
                             `(gdb--breakpoint ,breakpoint))

         (when (eq cursor-on-breakpoint breakpoint)
           (setf (gdb--table-target-line table) (gdb--table-num-rows table)))))

     (gdb--table-insert table))))


;; ------------------------------------------------------------------------------------------
;; Variables buffer
(defvar gdb-variables-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd     "p") #'previous-line)
    (define-key map (kbd     "n") #'next-line)
    (define-key map (kbd   "SPC") #'gdb-create-watcher-from)
    (define-key map (kbd "S-SPC") #'gdb-create-watcher-from-ask)
    (define-key map (kbd   "RET") #'gdb-create-watcher-from-switch)
    (define-key map (kbd "S-RET") #'gdb-create-watcher-from-switch-ask)
    map))

(define-derived-mode gdb-variables-mode nil "GDB Variables"
  (setq-local buffer-read-only t)
  (buffer-disable-undo)
  (font-lock-mode -1))

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
        table (list (propertize (gdb--variable-name  variable) 'face 'gdb-variable-face)
                    (propertize (gdb--variable-type  variable) 'face 'gdb-type-face)
                    (or         (gdb--variable-value variable) "<Composite type>"))
        (list 'gdb--var variable)))
     (gdb--table-insert table))))


;; ------------------------------------------------------------------------------------------
;; Watchers buffer
(defvar gdb-watchers-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd        "p") #'previous-line)
    (define-key map (kbd        "n") #'next-line)
    (define-key map (kbd        "a") #'gdb-watcher-add)
    (define-key map (kbd      "RET") #'gdb-watcher-assign)
    (define-key map (kbd        "e") #'gdb-watcher-edit-expression)
    (define-key map (kbd        "f") #'gdb-watcher-change-format)
    (define-key map (kbd        "d") #'gdb-watcher-duplicate)
    (define-key map (kbd        "h") #'gdb-watcher-toggle-hold-frame)
    (define-key map (kbd        "t") #'gdb-watchers-toggle-access-specifiers)
    (define-key map (kbd      "SPC") #'gdb-table-toggle)
    (define-key map (kbd      "TAB") #'gdb-table-toggle)
    (define-key map (kbd      "DEL") #'gdb-watcher-delete)
    (define-key map (kbd "<delete>") #'gdb-watcher-delete)
    map))

(define-derived-mode gdb-watchers-mode nil "GDB Watchers"
  (setq-local buffer-read-only t)
  (buffer-disable-undo)
  (font-lock-mode -1))

(gdb--simple-get-buffer gdb--watchers gdb--watchers-update "Watchers" nil
  (gdb-watchers-mode))

(defun gdb--watcher-toggle-chilren ()
  (gdb--with-valid-session
   (let ((watcher (get-text-property (line-beginning-position) 'gdb--watcher)))
     (when (and watcher (> (gdb--watcher-children-count watcher) 0))
       (if (and (setf (gdb--watcher-open watcher) (not (gdb--watcher-open watcher)))
                (not  (gdb--watcher-children watcher)))
           (gdb--command (concat "-var-list-children --simple-values " (gdb--watcher-name watcher))
                         (cons 'gdb--context-watcher-list-children watcher))

         (cl-pushnew (current-buffer) (gdb--session-buffers-to-update session))
         (gdb--update))))))

(defun gdb--watcher-draw (table-or-parent watcher tick watcher-at-start watcher-under-cursor)
  (let* ((out-of-scope (eq (gdb--watcher-flag watcher) 'out-of-scope))
         (expr (concat (gdb--add-face (gdb--watcher-expr watcher) 'gdb-variable-face)
                       (when (and (gdb--watcher-thread-id watcher) (not (gdb--watcher-parent watcher)))
                         (eval-when-compile (propertize " HOLD" 'face 'gdb-watcher-hold-face)))))
         (children (gdb--watcher-children watcher))
         (row (gdb--table-add-row
               table-or-parent
               (list expr (gdb--add-face (gdb--watcher-type watcher) 'gdb-type-face)
                     (if out-of-scope
                         (eval-when-compile (propertize "Out of scope" 'face 'gdb-out-of-scope-face))
                       (gdb--add-face (gdb--watcher-value watcher)
                                      (and (eq (gdb--watcher-flag watcher) tick) 'gdb-modified-face))))
               (list 'gdb--watcher watcher)
               (and (not out-of-scope) (> (gdb--watcher-children-count watcher) 0)) #'gdb--watcher-toggle-chilren)))

    (when (eq watcher watcher-at-start)
      (let ((table (gdb--get-table-from-table-or-parent table-or-parent)))
        (setf (gdb--table-start-line table) (gdb--table-num-rows table))))

    (when (eq watcher watcher-under-cursor)
      (let ((table (gdb--get-table-from-table-or-parent table-or-parent)))
        (setf (gdb--table-target-line table) (gdb--table-num-rows table))))

    (when (and (not out-of-scope) (gdb--watcher-open watcher))
      (cl-loop for child in children
               do (gdb--watcher-draw row child tick watcher-at-start watcher-under-cursor)))))


(defun gdb--watchers-update ()
  (gdb--with-valid-session
   (let* ((window (get-buffer-window nil t))
          (window-start (and window (window-start window)))
          (table (make-gdb--table :start-line (and window-start (gdb--current-line window-start))
                                  :target-line (gdb--current-line))))

     (gdb--table-add-header table '("Expr" "Type" "Value"))

     (let ((watcher-at-start     (and window-start
                                      (get-text-property window-start              'gdb--watcher)))
           (watcher-under-cursor      (get-text-property (line-beginning-position) 'gdb--watcher))
           (tick (gdb--session-watchers-tick session)))
       (cl-loop for watcher in (gdb--session-root-watchers session)
                do (gdb--watcher-draw table watcher tick watcher-at-start watcher-under-cursor)))

     (gdb--table-insert table))))

(defun gdb--watchers-remove-records (session list)
  "Removes every reference to the watchers in LIST and their children."
  (cl-loop for watcher in list
           do (gdb--watchers-remove-records session (gdb--watcher-children watcher))
           do (remhash (gdb--watcher-name watcher) (gdb--session-watchers session))
           unless (gdb--watcher-parent watcher) do (setf (gdb--session-root-watchers session)
                                                         (delq watcher (gdb--session-root-watchers session)))))

(defsubst gdb--watcher-change-format (watcher format)
  (gdb--command (format "-var-set-format %s %s" (gdb--watcher-name watcher) format)
                (cons 'gdb--context-watcher-change-format watcher)))

(defun gdb--watcher-change-format-recursively (list format)
  (cl-loop for watcher in list
           do
           (gdb--watcher-change-format watcher format)
           (gdb--watcher-change-format-recursively (gdb--watcher-children watcher) format)))

(defun gdb--watcher-get-stack-depth-root (watcher)
  (while (gdb--watcher-parent watcher) (setq watcher (gdb--watcher-parent watcher)))
  (gdb--watcher-stack-depth watcher))


;; ------------------------------------------------------------------------------------------
;; Registers buffer
(defvar gdb-registers-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd     "p") #'previous-line)
    (define-key map (kbd     "n") #'next-line)
    (define-key map (kbd     "f") #'gdb-registers-change-format)
    (define-key map (kbd   "SPC") #'gdb-create-watcher-from)
    (define-key map (kbd "S-SPC") #'gdb-create-watcher-from-ask)
    (define-key map (kbd   "RET") #'gdb-create-watcher-from-switch)
    (define-key map (kbd "S-RET") #'gdb-create-watcher-from-switch-ask)
    map))

(define-derived-mode gdb-registers-mode nil "GDB Registers"
  (setq-local buffer-read-only t)
  (buffer-disable-undo)
  (font-lock-mode -1))

(gdb--simple-get-buffer gdb--registers gdb--registers-update "Registers" nil
  (gdb-registers-mode))

(defun gdb--registers-update ()
  (gdb--with-valid-session
   (let ((table (make-gdb--table :target-line (gdb--current-line)))
         (thread (gdb--session-selected-thread session)))
     (gdb--table-add-header table '("Name" "Value"))
     (when thread
       (cl-loop with tick = (gdb--thread-registers-tick thread)
                for  reg across (gdb--thread-registers thread)
                for  reg-tick = (and reg (gdb--register-tick reg))
                when reg
                do   (gdb--table-add-row
                      table (list (propertize (gdb--register-name  reg) 'face 'gdb-variable-face)
                                  (gdb--add-face (gdb--register-value reg)
                                                 (when (and reg-tick (= tick reg-tick))
                                                   'gdb-modified-face)))
                      (list 'gdb--register reg))))
     (gdb--table-insert table))))


;; ------------------------------------------------------------------------------------------
;; Disassembly buffer
(defvar gdb-disassembly-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "f") #'gdb-disassembly-change-format)
    (define-key map (kbd "F") #'gdb-disassembly-change-flavor)
    map))

(defconst gdb--disassembly-font-lock-keywords
  '(;; 0xNNNNNNNN [RawOpcode] Opcode
    ("^0x[[:xdigit:]]+[[:space:]]+\\(\\(?:[0-9a-fA-F][0-9a-fA-F] \\)*\\)\\(\\sw+\\)"
     (1 'gdb-raw-opcode-face nil t) (2 'gdb-opcode-face))
    ;; Hexadecimals
    ("0x[[:xdigit:]]+" . 'gdb-constant-face)
    ;; Source lines
    ("^\\(Line [0-9]+\\)\\(.*\\)$"
     (1 'gdb-disassembly-line-indicator-face) (2 'gdb-disassembly-src-face))
    ;; %register(at least i386)
    ("%\\sw+" . 'gdb-variable-face)
    ;; <FunctionName+Number>
    ("<\\([^+>]+\\)\\(?:\\+\\([0-9]+\\)\\)?>"
     (1 'gdb-function-face) (2 'gdb-constant-face nil t)))
  "Font lock keywords used in `gdb--disassembly'.")

(define-derived-mode gdb-disassembly-mode nil "GDB Disassembly"
  (setq-local font-lock-defaults '(gdb--disassembly-font-lock-keywords t))
  (setq-local buffer-read-only t)
  (buffer-disable-undo))

(gdb--simple-get-buffer gdb--disassembly gdb--disassembly-update "Disassembly" nil
  (gdb-disassembly-mode)
  (setf (gdb--buffer-info-data gdb--buffer-info) (make-gdb--disassembly-data))
  (gdb--disassembly-fetch (gdb--session-selected-frame session)))

(defun gdb--disassembly-fetch (frame)
  (gdb--with-valid-session
   (let ((data (gdb--buffer-get-data (gdb--get-buffer-with-type session 'gdb--disassembly))))
     (when data
       (cond
        (frame
         (let ((func (gdb--frame-func frame))
               (file (gdb--frame-file frame))
               (line (gdb--frame-line frame)))
           (cond
            ((and func file line)
             (if (string= (gdb--disassembly-data-func data) func)
                 (cl-pushnew 'gdb--disassembly (gdb--session-buffer-types-to-update session))
               (gdb--command (format "-data-disassemble -f %s -l %d -- %d"
                                     (gdb--escape-argument (gdb--local-path file)) line
                                     (gdb--disassembly-data-mode data))
                             (cons 'gdb--context-disassemble data))))
            (t
             (gdb--command (format "-data-disassemble -s $pc -e $pc+500 -- %d" (gdb--disassembly-data-mode data))
                           (cons 'gdb--context-disassemble data) frame)))

           (setf (gdb--disassembly-data-func data) func)))

        (t (gdb--remove-all-symbols session 'disassembly-indicator)))))))

(defsubst gdb--disassembly-func-and-offset (func offset)
  (cond ((and func offset) (concat "<" func "+" offset ">"))
        (func              (concat "<" func ">"))
        (t "")))

(defun gdb--disassembly-print-instrs (fmt list current-addr-num target-ref)
  (cl-loop for instr in list
           for addr       = (gdb--disassembly-instr-addr    instr)
           for addr-num   = (gdb--parse-address addr)
           for instr-text = (gdb--disassembly-instr-instr   instr)
           for func       = (gdb--disassembly-instr-func    instr)
           for offset     = (gdb--disassembly-instr-offset  instr)
           for opcodes    = (gdb--disassembly-instr-opcodes instr)
           when (= addr-num current-addr-num) do (setf (gv-deref target-ref) (gdb--current-line))
           do (insert (propertize
                       (if opcodes
                           (format fmt addr opcodes instr-text (gdb--disassembly-func-and-offset func offset))
                         (format fmt addr instr-text (gdb--disassembly-func-and-offset func offset)))
                       'gdb--instr instr
                       'gdb--addr-num addr-num))))

(defun gdb--disassembly-update ()
  (gdb--with-valid-session
   (let* ((data (gdb--buffer-get-data))
          (list (gdb--disassembly-data-list data))
          (with-source (eq (type-of (car list)) 'gdb--disassembly-src))
          (frame (gdb--session-selected-frame session))
          (current-addr-num (or (and frame (gdb--parse-address (gdb--frame-addr frame))) -1))
          target (target-ref (gv-ref target)))

     (gdb--remove-symbols-in-curr-buffer 'disassembly-indicator)

     (cond
      ((gdb--disassembly-data-new data)
       (let* ((widths (gdb--disassembly-data-widths data))
              (src-fmt   (format "Line %%-%ds %%s\n" (- (aref widths 0) 5)))
              (instr-fmt (if (> (aref widths 1) 0)
                             (format "%%-%ds %%%ds %%-%ds %%s\n" (aref widths 0) (aref widths 1) (aref widths 2))
                           (format "%%-%ds %%-%ds %%s\n" (aref widths 0) (aref widths 2)))))
         (setf (gdb--disassembly-data-new data) nil)
         (erase-buffer)
         (gdb--remove-symbols-in-curr-buffer 'breakpoint-indicator)

         (if with-source
             (cl-loop for src in list
                      for file = (gdb--complete-path (gdb--disassembly-src-file src))
                      for line-str = (gdb--disassembly-src-line-str src)
                      for line-contents = (gdb--get-line file (gdb--stn line-str) t)
                      for instrs = (gdb--disassembly-src-instrs src)
                      do  (when line-str (insert (format src-fmt line-str (or line-contents ""))))
                      do  (gdb--disassembly-print-instrs instr-fmt instrs current-addr-num target-ref))

           (gdb--disassembly-print-instrs instr-fmt list current-addr-num target-ref))

         (cond
          (target
           (gdb--place-symbol session  (current-buffer) target '((type . disassembly-indicator)))
           (gdb--scroll-buffer-to-line (current-buffer) target 'center))

          (t (gdb--scroll-buffer-to-line (current-buffer) 1)))

         (dolist (breakpoint (gdb--session-breakpoints session))
           (gdb--place-breakpoint-in-disassembly (current-buffer) breakpoint))))

      (t
       (let ((pos (text-property-any (point-min) (point-max) 'gdb--addr-num current-addr-num))
             line)
         (when pos
           (setq line (gdb--current-line pos))
           (gdb--place-symbol session  (current-buffer) line '((type . disassembly-indicator)))
           (gdb--scroll-buffer-to-line (current-buffer) line 'center))))))))

(defun gdb--disassembly-is-visible ()
  (gdb--with-valid-session
   (let ((buffer (gdb--get-buffer-with-type session 'gdb--disassembly)))
     (when buffer
       (cl-loop for window in (get-buffer-window-list buffer nil t)
                for frame = (and (window-live-p window) (window-frame window))
                when (and frame (frame-visible-p frame)) return t)))))


;; ------------------------------------------------------------------------------------------
;; Source buffers
(defun gdb--display-source-buffer (&optional force override-file override-line)
  "Display buffer of the selected source, and mark the current line.
When FORCE is non-nil, the source window will be created and shown.
FORCE may be the new window to display the source buffer.

The source file and line are fetched from the selected frame, unless OVERRIDE-FILE and OVERRIDE-LINE are set,
in which case those will be used.
OVERRIDE-LINE may also be `no-mark', which forces it to not mark any line."
  (gdb--with-valid-session
   (gdb--remove-all-symbols session 'source-indicator t)

   (when (window-live-p force) (setf (gdb--session-source-window session) force))

   (let* ((frame (gdb--session-selected-frame session))
          (file (cond (override-file)
                      (frame (gdb--frame-file frame))))
          (line (cond ((and (not (eq override-line 'no-mark)) override-line))
                      (frame (gdb--frame-line frame))))
          (buffer (and file (gdb--find-file file)))
          (window (gdb--session-source-window session))
          recenter-redisplay)

     (when (and (not (window-live-p window))
                (or force (and buffer (not (gdb--disassembly-is-visible)))))
       (setq window (gdb--create-frame-for-buffer session (gdb--comint-get-buffer session)))
       (setf (gdb--session-source-window session) window))

     (when (window-live-p window)
       (set-window-dedicated-p window nil)

       (when (and (not gdb--inhibit-display-source) buffer)
         (set-window-buffer window buffer)
         (when line
           (with-selected-window window
             (goto-char (point-min))
             (forward-line (1- line))
             (recenter))
           (gdb--place-symbol session buffer line '((type . source-indicator)))))

       (if (display-images-p)
           (set-window-fringes window 8)
         (set-window-margins window 2))))))


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

(defun gdb--log-error (err)
  (let ((session (gdb--infer-session)))
    (unless (and session (eq (window-buffer (selected-window)) (gdb--comint-get-buffer session)))
      (message "GDB Error: %s" err))))

(defun gdb--running (thread-id-str)
  (gdb--with-valid-session
   (let ((thread (gdb--get-thread-by-id (string-to-number thread-id-str)))
         (selected-thread (gdb--session-selected-thread session)))
     (when thread
       (setf (gdb--thread-state thread) "running"
             (gdb--thread-frames thread) nil)

       (when (eq thread selected-thread)
         (gdb--remove-all-symbols session 'source-indicator t)
         (if (gdb--session-persist-thread session)
             (gdb--switch-to-frame 'deselect-frame t)
           (cl-loop for thread in (gdb--session-threads session)
                    when (string= (gdb--thread-state thread) "stopped") return (gdb--switch-to-thread thread)
                    finally (gdb--switch-to-frame 'deselect-frame t)))
         (setf (gdb--session-persist-thread session) nil))

       (cl-pushnew 'gdb--threads (gdb--session-buffer-types-to-update session))
       (cl-pushnew 'gdb--frames  (gdb--session-buffer-types-to-update session))))))

(defun gdb--set-initial-file (file)
  (gdb--display-source-buffer nil (gdb--complete-path file) 'no-mark))

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

       (cond ((= (gdb--thread-registers-tick thread) most-negative-fixnum)
              (gdb--command "-data-list-changed-registers" nil thread) ;; NOTE(nox): Ignored
              (gdb--command (concat "-data-list-register-values --skip-unavailable "
                                    (gdb--thread-registers-format thread))
                            (cons 'gdb--context-registers-update thread) thread))
             (t
              (gdb--command "-data-list-changed-registers" (cons 'gdb--context-registers-get-changed
                                                                 (cons (gdb--thread-registers-format thread) thread))
                            thread))))

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
          (breakpoint (or existing-breakpoint (make-gdb--breakpoint :session session))))

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

     (gdb--place-breakpoint session breakpoint)
     (cl-pushnew 'gdb--breakpoints (gdb--session-buffer-types-to-update session)))))

(defun gdb--breakpoint-enable-disable (arg)
  (gdb--with-valid-session
   (let ((breakpoint (car arg))
         (state      (cdr arg)))
     (setf (gdb--breakpoint-enabled breakpoint) state)
     (gdb--breakpoint-remove-symbol breakpoint)
     (gdb--place-breakpoint session breakpoint))
   (cl-pushnew 'gdb--breakpoints (gdb--session-buffer-types-to-update session))))

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
   (let* ((expr (aref data 0))
          (watcher (make-gdb--watcher :name name :expr expr :type type :value value
                                      :children-count (or (gdb--stn num-child) 0)
                                      :thread-id (gdb--stn thread-id)
                                      :stack-depth (aref data 2)))
          (to-replace (aref data 1)))
     (if to-replace
         (progn (setf (gdb--session-root-watchers session)
                      (cl-nsubstitute watcher to-replace (gdb--session-root-watchers session)))
                (gdb--command (concat "-var-delete " (gdb--watcher-name to-replace)))
                (gdb--watchers-remove-records session (list to-replace)))
       (setf (gdb--session-root-watchers session)
             (append (gdb--session-root-watchers session) (list watcher))))

     (puthash name watcher (gdb--session-watchers session))
     (cl-pushnew 'gdb--watchers (gdb--session-buffer-types-to-update session)))))

(defun gdb--watchers-update-info (should-highlight &rest args)
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
                             (gdb--watcher-flag  watcher) (and should-highlight tick)))

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
   (setf
    (gdb--watcher-children-count parent) (length children)
    (gdb--watcher-children parent)
    (append
     (gdb--watcher-children parent)
     (cl-loop for (name expr num-children value type thread-id) in children
              if  (or (not (gdb--session-hide-access-spec session))
                      type (> (length value) 0))
              collect (let ((watcher (make-gdb--watcher :name name :expr expr :type type :value value :parent parent
                                                        :children-count (or (gdb--stn num-children) 0)
                                                        :thread-id (gdb--stn thread-id))))
                        (puthash name watcher (gdb--session-watchers session))
                        watcher)
              else ;; NOTE(nox): Both the value and the type are null, so this is an access qualifier
              do (gdb--command (concat "-var-list-children --simple-values " name)
                               (cons 'gdb--context-watcher-list-children parent)))))

   (cl-pushnew 'gdb--watchers (gdb--session-buffer-types-to-update session))))

(defun gdb--watcher-format-change (watcher new-value)
  (gdb--with-valid-session
   (setf (gdb--watcher-value watcher) new-value)
   (cl-pushnew 'gdb--watchers (gdb--session-buffer-types-to-update session))))

(defun gdb--set-register-names (thread count &rest names)
  (gdb--with-valid-session
   (setf (gdb--thread-registers thread) (make-vector count nil))

   (let ((registers (gdb--thread-registers thread)))
     (cl-loop for name in names
              for num from 0
              unless (string= name "")
              do (setf (aref registers num) (make-gdb--register :number num :name name))))))

(defun gdb--update-registers (thread &rest pairs)
  (gdb--with-valid-session
   (let ((registers (gdb--thread-registers      thread))
         (tick (1+  (gdb--thread-registers-tick thread))))
     (setf (gdb--thread-registers-tick thread) tick)
     (cl-loop for (num-str . value) in pairs
              for  num = (string-to-number num-str)
              for  reg = (aref registers num)
              when reg do (setf (gdb--register-value reg) value
                                (gdb--register-tick  reg) tick))
     (cl-pushnew 'gdb--registers (gdb--session-buffer-types-to-update session)))))

(defun gdb--set-disassembly (data widths &rest args)
  (gdb--with-valid-session
   (setf (gdb--disassembly-data-widths data) widths)

   (if (gdb--disassembly-src-p (car args))
       (setf (gdb--disassembly-data-list data) args)
     (setf (gdb--disassembly-data-list data) (car args)))

   (setf (gdb--disassembly-data-new data) t)
   (cl-pushnew 'gdb--disassembly (gdb--session-buffer-types-to-update session))))

(defun gdb--persist-thread ()
  (gdb--with-valid-session (setf (gdb--session-persist-thread session) t)))

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
            (define-key map (kbd    "<f8>") #'gdb-watcher-add)
            (define-key map (kbd  "<C-f8>") #'gdb-eval-expression)
            (define-key map (kbd    "<f9>") #'gdb-toggle-breakpoint)
            (define-key map (kbd   "<f10>") #'gdb-next)
            (define-key map (kbd "<M-f10>") #'gdb-next-instruction)
            (define-key map (kbd   "<f11>") #'gdb-step)
            (define-key map (kbd "<M-f11>") #'gdb-step-instruction)
            (define-key map (kbd "<C-f10>") #'gdb-until)
            (define-key map (kbd "<C-f11>") #'gdb-advance)
            (define-key map (kbd "<S-f11>") #'gdb-finish)
            (define-key map (kbd   "<f12>") #'gdb-switch-buffer/body)
            map)
  :group 'gdb)


;; ------------------------------------------------------------------------------------------
;; User commands
(defun gdb-watcher-add (&optional default watcher-to-replace hold-thread stack-depth)
  "Start watching an expression."
  (interactive)
  (gdb--with-valid-session
   (unless (gdb--session-selected-frame session) (user-error "No frame is selected"))

   (let* ((expression (cond ((consp default) (car default))
                            (t (gdb--read-line "Expression: " default))))
          hold-frame)
     (when (and expression
                (or (consp default) (not watcher-to-replace)
                    (not (string= expression (gdb--watcher-expr watcher-to-replace)))))

       (when hold-thread
         (setq hold-frame (and hold-thread (nth (- (length (gdb--thread-frames hold-thread)) stack-depth)
                                                (gdb--thread-frames hold-thread)))))

       (gdb--command (format "-var-create - %c \"%s\"" (if hold-thread ?* ?@) expression)
                     (cons 'gdb--context-watcher-create (vector expression watcher-to-replace stack-depth))
                     (or hold-frame (gdb--session-selected-frame session)))))))

(defun gdb-watcher-assign ()
  "Assign a value to the watched expression."
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
       (gdb--command "-var-update --all-values *" (cons 'gdb--context-watcher-update t) frame)))))

(defun gdb-watcher-edit-expression ()
  "Modify the watched expression.
If the watcher is holding to a frame, this expression will also be evaluated in that frame."
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--watchers)
     (let ((watcher (get-text-property (line-beginning-position) 'gdb--watcher))
           hold-thread stack-depth)
       (when (or (not watcher) (gdb--watcher-parent watcher)) (user-error "No root watcher under cursor"))
       (unless (eq (gdb--watcher-flag watcher) 'out-of-scope)
         (setq hold-thread (gdb--get-thread-by-id (gdb--watcher-thread-id watcher))
               stack-depth (gdb--watcher-get-stack-depth-root watcher)))
       (gdb-watcher-add (gdb--watcher-expr watcher) watcher hold-thread stack-depth)))))

(defun gdb-watcher-duplicate ()
  "Duplicate the watcher under cursor.
It may be a root or child watcher. There is a prompt for the new expression.
If the duplicated watcher is holding to a frame, this expression will also be evaluated in that frame."
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--watchers)
     (let ((watcher (get-text-property (line-beginning-position) 'gdb--watcher))
           hold-thread stack-depth)
       (unless watcher (user-error "No watcher under cursor"))
       (unless (eq (gdb--watcher-flag watcher) 'out-of-scope)
         (setq hold-thread (gdb--get-thread-by-id (gdb--watcher-thread-id watcher))
               stack-depth (gdb--watcher-get-stack-depth-root watcher)))
       (gdb-watcher-add (gdb--get-data (concat "-var-info-path-expression " (gdb--watcher-name watcher)) "path_expr")
                        nil hold-thread stack-depth)))))

(defun gdb-watcher-change-format (arg)
  "Change format of the watcher under cursor. When ARG is non-nil, also recursively change children's format."
  (interactive "P")
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--watchers)
     (let ((watcher (get-text-property (line-beginning-position) 'gdb--watcher))
           format)
       (unless watcher (user-error "No watcher under cursor"))
       (setq format (downcase (completing-read "Format: " '("Natural" "Binary" "Octal" "Decimal"
                                                            "Hexadecimal" "Zero-Hexadecimal")
                                               nil t)))
       (if arg
           (gdb--watcher-change-format-recursively (list watcher) format)
         (gdb--watcher-change-format watcher format))))))

(defun gdb-watcher-toggle-hold-frame ()
  "This command will toggle the hold state of the watcher under cursor.
If this is a floating watcher (not holding to any frame), this will hold the watcher to the selected frame.
Else, this will convert the watcher to floating again.

Notice that this is different from \"freezing\"! This will make the watcher become attached to
the selected frame; nevertheless, the watcher will update if something (eg. a pointer to it) modifies
it outside the frame it is attached to."
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--watchers)
     (unless (gdb--session-selected-frame session) (user-error "No frame is selected"))
     (let ((watcher (get-text-property (line-beginning-position) 'gdb--watcher))
           hold-thread stack-depth)
       (when (or (not watcher) (gdb--watcher-parent watcher)) (user-error "No root watcher under cursor"))
       (when (and (not (gdb--watcher-thread-id watcher)) (eq (gdb--watcher-flag watcher) 'out-of-scope))
         (user-error "The watcher under cursor is out of scope"))

       (unless (gdb--watcher-thread-id watcher)
         (setq hold-thread (gdb--session-selected-thread session)
               stack-depth (cl-loop with selected-frame =  (gdb--session-selected-frame session)
                                    for frame in           (gdb--thread-frames hold-thread)
                                    for depth from (length (gdb--thread-frames hold-thread)) downto 1
                                    when (eq frame selected-frame) return depth)))

       (gdb-watcher-add (cons (gdb--watcher-expr watcher) nil) watcher hold-thread stack-depth)))))

(defun gdb-watchers-toggle-access-specifiers ()
  "This will show or hide the access specifiers in watchers.
The default behaviour is defined by `gdb-watchers-hide-access-specifiers'."
  (interactive)
  (gdb--with-valid-session
   (setf (gdb--session-hide-access-spec session) (not (gdb--session-hide-access-spec session)))
   (cl-loop for watcher in (gdb--session-root-watchers session)
            do (gdb-watcher-add (cons (gdb--watcher-expr watcher) nil) watcher))))

(defun gdb-watcher-delete ()
  "Delete root watcher under cursor."
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--watchers)
     (let ((watcher (get-text-property (line-beginning-position) 'gdb--watcher)))
       (when (or (not watcher) (gdb--watcher-parent watcher)) (user-error "No root watcher under cursor"))
       (gdb--command (concat "-var-delete " (gdb--watcher-name watcher)))
       (gdb--watchers-remove-records session (list watcher))
       (cl-pushnew 'gdb--watchers (gdb--session-buffer-types-to-update session))
       (gdb--update)))))

(defun gdb-create-watcher-from (arg)
  "Create watcher from automatic variables or registers.
If ARG is non-nil, you may modify the watcher expression before creation."
  (interactive "P")
  (gdb--with-valid-session
   (let ((expression
          (cond
           ((gdb--is-buffer-type 'gdb--variables)
            (let ((var (get-text-property (line-beginning-position) 'gdb--var)))
              (unless var (user-error "No variable selected"))
              (gdb--variable-name var)))

           ((gdb--is-buffer-type 'gdb--registers)
            (let ((register (get-text-property (line-beginning-position) 'gdb--register)))
              (unless register (user-error "No register selected"))
              (concat "$" (gdb--register-name register)))))))

     (when expression
       (unless arg (message "Created watcher with expression %s" expression))
       (gdb-watcher-add (if arg expression (cons expression nil)))
       (accept-process-output (gdb--session-process session) 0.5)
       (gdb--scroll-buffer-to-last-line (gdb--watchers-get-buffer session))))))

(defun gdb-create-watcher-from-ask ()
  "Create watcher from automatic variables or registers, giving the possibility to change the expression."
  (interactive)
  (gdb-create-watcher-from t))

(defun gdb-create-watcher-from-switch (&optional arg)
  "Create watcher from automatic variables or registers, and switch to the watchers window."
  (interactive "P")
  (gdb--with-valid-session
   (gdb-create-watcher-from arg)
   (let* ((buffer (gdb--watchers-get-buffer session))
          (window (get-buffer-window buffer t)))
     (if window
         (progn
           (select-frame-set-input-focus (window-frame window))
           (select-window window))
       (gdb--create-frame-for-buffer session buffer)))))

(defun gdb-create-watcher-from-switch-ask ()
  "Create watcher from automatic variables or registers, ask for the expression and switch to the window."
  (interactive)
  (gdb-create-watcher-from-switch t))

(defun gdb-eval-expression ()
  "Evaluate expression once and print result."
  (interactive)
  (gdb--with-valid-session
   (let* ((frame (or (gdb--session-selected-frame session) (user-error "No frame is selected")))
          (expression (gdb--read-line "Expression to evaluate: "))
          result)
     (when expression
       (setq result (gdb--get-data (concat "-data-evaluate-expression " (gdb--escape-argument expression))
                                   "value" frame))
       (if result
           (message "Result: %s" result)
         (user-error "Expression %s not found!" (gdb--escape-argument expression)))))))

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
  "Step over."
  (interactive)
  (gdb--with-valid-session (gdb--command "-exec-next" 'gdb--context-persist-thread t)))

(defun gdb-next-instruction ()
  "Step over instruction."
  (interactive)
  (gdb--with-valid-session (gdb--command "-exec-next-instruction" 'gdb--context-persist-thread t)))

(defun gdb-step ()
  "Step into."
  (interactive)
  (gdb--with-valid-session (gdb--command "-exec-step" 'gdb--context-persist-thread t)))

(defun gdb-step-instruction ()
  "Step into instruction."
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
       (gdb--command (concat (if advance "advance " "until ") location) 'gdb--context-persist-thread nil nil t)))))

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

(defhydra gdb-switch-buffer
  (:hint nil :foreign-keys warn :exit t :body-pre (gdb--with-valid-session
                                                   "No session available!"
                                                   (setq gdb--open-buffer-new-frame nil)))
  "
Show GDB buffer in %s(if gdb--open-buffer-new-frame \"new frame\" \"selected window\") [_<f12>_]
_g_db shell     |  _t_hreads  |  _b_reakpoints  |  _v_ariables  |  _r_egisters | _s_ource buffer
_i_nferior i/o  |  _f_rames   |  _d_isassembly  |  _w_atcher    |  ^ ^         |
"
  ("g" (gdb--switch-buffer 'gdb--comint-get-buffer))
  ("i" (gdb--switch-buffer 'gdb--inferior-io-get-buffer))
  ("t" (gdb--switch-buffer 'gdb--threads-get-buffer))
  ("f" (gdb--switch-buffer 'gdb--frames-get-buffer))
  ("b" (gdb--switch-buffer 'gdb--breakpoints-get-buffer))
  ("d" (gdb--switch-buffer 'gdb--disassembly-get-buffer))
  ("v" (gdb--switch-buffer 'gdb--variables-get-buffer))
  ("w" (gdb--switch-buffer 'gdb--watchers-get-buffer))
  ("r" (gdb--switch-buffer 'gdb--registers-get-buffer))
  ("s" (progn (setf (gdb--session-source-window (gdb--infer-session)) (and (not gdb--open-buffer-new-frame)
                                                                           (selected-window)))
              (gdb--display-source-buffer t)))
  ("<f12>" (setq gdb--open-buffer-new-frame (not gdb--open-buffer-new-frame)) :exit nil))

(defun gdb-toggle-breakpoint (&optional arg)
  "Toggle breakpoint in the current location.
When ARG is non-nil, prompt for additional breakpoint settings.
If ARG is `dprintf' create a dprintf breakpoint instead."
  (interactive "P")
  (gdb--after-choosing-session
   (let ((breakpoint-on-point (gdb--infer-breakpoint session))
         (location (gdb--point-location))
         (dprintf (eq arg 'dprintf))
         type thread ignore-count condition format-args)
     (when (and (not location) breakpoint-on-point)
       (setq location (gdb--infer-breakpoint-location breakpoint-on-point)))

     (when (or arg (not location))
       (unless (string= (or location "")
                        (setq location (read-string (concat "Location: ") location)))
         (setq breakpoint-on-point nil))

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

(defun gdb-breakpoint-enable-disable ()
  "Enable or disable inferred breakpoint."
  (interactive)
  (gdb--with-valid-session
   (let ((breakpoint (gdb--infer-breakpoint)))
     (unless breakpoint (user-error "No breakpoint under cursor"))
     (gdb--command (format "%s %d" (if (gdb--breakpoint-enabled breakpoint) "-break-disable " "-break-enable ")
                           (gdb--breakpoint-number breakpoint))
                   (cons 'gdb--context-breakpoint-enable-disable
                         (cons breakpoint (not (gdb--breakpoint-enabled breakpoint))))))))

(defun gdb-delete-breakpoint ()
  "Delete inferred breakpoint."
  (interactive)
  (gdb--with-valid-session
   (let ((breakpoint-on-point (gdb--infer-breakpoint)))
     (when breakpoint-on-point
       (gdb--command (format "-break-delete %d" (gdb--breakpoint-number breakpoint-on-point))
                     (cons 'gdb--context-breakpoint-delete breakpoint-on-point))))))

(defun gdb-registers-change-format ()
  "Change the format the registers are displayed in."
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--registers)
     (let ((thread (gdb--session-selected-thread session))
           (collection '(("Binary" . "t") ("Decimal" . "d") ("Hexadecimal" . "x")
                         ("Natural" . "N") ("Octal" . "o") ("Raw" . "r"))))
       (unless thread (user-error "No thread is selected"))
       (setf (gdb--thread-registers-format thread) (cdr (assoc (completing-read "Format: " collection nil t)
                                                               collection)))
       (gdb--command (concat "-data-list-register-values --skip-unavailable "
                             (gdb--thread-registers-format thread))
                     (cons 'gdb--context-registers-update thread) thread)))))

(defun gdb-disassembly-change-format ()
  "Change the disassembly format."
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--disassembly)
     (let ((data (gdb--buffer-get-data))
           (collection '(("Disassembly only" . 0)
                           ("Disassembly with raw opcodes" . 2)
                           ("Mixed source and disassembly" . 4)
                           ("Mixed source and disassembly with raw opcodes" . 5))))
       (setf (gdb--disassembly-data-mode data) (cdr (assoc (completing-read "Format: " collection nil t)
                                                             collection)))

       (setf (gdb--disassembly-data-func data) nil)
       (gdb--disassembly-fetch (gdb--session-selected-frame session))))))

(defun gdb-disassembly-change-flavor ()
  "Change the disassembly flavor."
  (interactive)
  (gdb--with-valid-session
   (when (gdb--is-buffer-type 'gdb--disassembly)
     (let ((data (gdb--buffer-get-data))
           (collection '(("Intel" . "intel") ("AT&T"  . "att"))))
       (gdb--command (concat "-gdb-set disassembly-flavor "
                             (cdr (assoc (completing-read "Flavor: " collection nil t) collection))))

       (setf (gdb--disassembly-data-func data) nil)
       (gdb--disassembly-fetch (gdb--session-selected-frame session))))))

(defun gdb-table-toggle ()
  (interactive)
  (let ((func (get-text-property (line-beginning-position) 'gdb--table-fetch-func)))
    (when func (funcall func))))

(defun gdb-table-mouse-toggle (event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event)))
        func)
    (when (windowp window)
      (select-window window)
      (goto-char pos)
      (when (setq func (get-text-property pos 'gdb--table-fetch-func)) (funcall func)))))

(defun gdb-setup-windows (&optional session)
  "Setup windows in the main frame."
  (interactive)
  (setq session (or session (gdb--infer-session)))
  (unless session (user-error "Couldn't find a GDB session"))
  (funcall gdb-window-setup-function session))

(defun gdb-kill-session ()
  "Kill current GDB session."
  (interactive)
  (gdb--kill-session (gdb--infer-session)))

;;;###autoload
(defun gdb-create-session ()
  "Create GDB session. This will not associate any target with it."
  (interactive)
  (let ((gdb--session (make-gdb--session)))
    (push gdb--session gdb--sessions)
    (gdb--create-frame gdb--session)

    ;; NOTE(nox): Create essential buffers
    (gdb--comint-get-buffer gdb--session)
    (gdb--inferior-io-get-buffer gdb--session)

    ;; NOTE(nox): Essential settings
    (gdb--command "-gdb-set mi-async on")
    (gdb--command "-gdb-set non-stop on")

    (gdb-setup-windows gdb--session)
    (when gdb-enable-global-keybindings
      (gdb-keys-mode))

    (add-hook 'delete-frame-functions #'gdb--handle-delete-frame)
    (add-hook 'window-configuration-change-hook #'gdb--rename-frame)

    gdb--session))

;;;###autoload
(defun gdb-executable (debuggee-path)
  "Start debugging an executable at DEBUGGEE-PATH in the current session.
If no session is available, one is automatically created."
  (interactive
   (list (expand-file-name (read-file-name "Select executable to debug: " nil
                                           gdb--previous-executable t nil 'file-executable-p))))
  (let ((session (or (gdb--infer-session) (gdb-create-session))))
    (setq gdb--previous-executable debuggee-path)
    (setf (gdb--session-debuggee-path session) debuggee-path)

    (with-selected-frame (gdb--session-frame session)
      (gdb--command (concat "-file-exec-and-symbols " (gdb--escape-argument (gdb--local-path debuggee-path))))
      (gdb--command "-file-list-exec-source-file" 'gdb--context-initial-file)
      (gdb--rename-buffers-with-debuggee debuggee-path))

    (cl-loop for frame in (frame-list)
             when (eq (frame-parameter frame 'gdb--session) session)
             do (gdb--rename-frame frame))))

(provide 'gdb-mi)
;;; gdb-mi.el ends here
