;;; emacs-gdb.el --- GDB frontend                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Gonçalo Santos

;; Author: Gonçalo Santos <addr@host>
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
(require 'comint)
(add-to-list 'load-path (expand-file-name "."))
(require 'emacs-gdb-module)

(defvar gdb-debug-output nil
  "If not `nil', this will start outputting debug messages.")

(defvar gdb--last-debuggee nil
  "Last executable to be ran by GDB.")

(defvar gdb--last-args nil
  "Last args to be passed to GDB.")

(defvar gdb--comint-buffer nil
  "Buffer where the GDB process runs.")

(defvar-local gdb--frame nil
  "Frame where GDB runs.")

(defvar-local gdb--buffer-type nil
  "Type of current GDB buffer.")

(defvar-local gdb--thread-number nil
  "Number of the thread associated with the current GDB buffer.")

(defconst gdb--buffer-types
  '(gdb--comint
    gdb--inferior-io)
  "List of available buffer types.")

(defun gdb--comint-init ()
  (when (not (string= "*gdb-temp*" (buffer-name)))
    ;; NOTE(nox): Add mark to show where a new GDB session starts
    (goto-char (point-max))
    (insert "\n\n------------------------------\n"))
  (let* ((debuggee gdb--last-debuggee)
         (extra-args gdb--last-args)
         (debuggee-name (file-name-nondirectory debuggee))
         (debuggee-path (file-name-directory debuggee))
         (debuggee-exists (and (not (string= "" debuggee-name)) (file-executable-p debuggee)))
         (switches (split-string extra-args))
         (buffer-name (concat "*GDB" (when debuggee-exists (concat "-" debuggee-name)) "*"))
         (frame-name (concat "Emacs GDB" (when debuggee-exists
                                           (concat " - Debugging " debuggee-name)))))
    (rename-buffer buffer-name)
    (setq gdb--comint-buffer (current-buffer))
    ;; TODO(nox): We should rename this if file is changed inside GDB, no?
    (modify-frame-parameters nil `((name . ,frame-name)))
    (push "-i=mi" switches)
    (when debuggee-exists
      (cd debuggee-path)
      (push debuggee switches))
    (let ((process (get-buffer-process
                    (apply 'make-comint-in-buffer "GDB" gdb--comint-buffer "gdb" nil switches))))
      (set-process-sentinel process 'gdb--comint-sentinel))
    (gdb-mode)))

(defun gdb--comint-sentinel (process str)
  (when (or (not (buffer-name (process-buffer process)))
            (eq (process-status process) 'exit))
    (gdb-kill)))

(defun gdb--inferior-io-init ()
  (when (not (string= "*gdb-temp*" (buffer-name)))
    ;; NOTE(nox): Add mark to show where a new GDB session starts
    (goto-char (point-max))
    (insert "\n\n------------------------------\n"))
  (rename-buffer "*GDB I/O*")
  (let* ((gdb-process (gdb--get-comint-process))
         (inferior-process (get-buffer-process
                            (make-comint-in-buffer "GDB inferior" (current-buffer) nil)))
         (tty (or (process-get inferior-process 'remote-tty)
                  (process-tty-name inferior-process))))
    (comint-simple-send gdb-process (concat "-inferior-tty-set " tty))
    (set-process-sentinel inferior-process 'gdb--inferior-io-sentinel)
    (add-hook 'kill-buffer-hook 'gdb--inferior-io-killed nil t)))

(defun gdb--inferior-io-killed ()
  (with-current-buffer gdb--comint-buffer (comint-interrupt-subjob))
  (comint-simple-send (gdb--get-comint-process) "-interpreter-exec console kill"))

;; NOTE(nox): When the debuggee exits, Emacs gets an EIO error and stops listening to the
;; tty. This re-inits the buffer so everything works fine!
(defun gdb--inferior-io-sentinel (process str)
  (when (eq (process-status process) 'failed)
    (let ((buffer (process-buffer process)))
      (delete-process process)
      (if buffer
          (with-current-buffer buffer
            (gdb--inferior-io-init))))))

(defun gdb--get-buffer (buffer-type &optional thread)
  (catch 'found
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq gdb--buffer-type buffer-type)
                   (or (not thread)
                       (eq gdb--thread-number thread)))
          (throw 'found buffer))))))

(defun gdb--get-buffer-create (buffer-type &optional thread force-init)
  "Get GDB buffer of a specific type (and thread, if specified), creating it, if needed.

When creating, assign `gdb--buffer-type' to BUFFER-TYPE and
`gdb--thread-number' to THREAD (if provided).

BUFFER-TYPE must be a member of `gdb--buffer-types'."
  (when (member buffer-type gdb--buffer-types)
    (let* ((buffer (gdb--get-buffer buffer-type thread))
           (create (not buffer))
           (init-symbol (intern (concat (symbol-name buffer-type) "-init"))))
      (when create (setq buffer (generate-new-buffer "*gdb-temp*")))
      (when (or create force-init)
        (with-current-buffer buffer
          (funcall init-symbol)
          (setq-local gdb--buffer-type buffer-type)
          (setq-local gdb--thread-number thread)))
      buffer)))

(defmacro gdb--get-local (var)
  `(and (buffer-live-p gdb--comint-buffer) (buffer-local-value ',var gdb--comint-buffer)))

(defun gdb--get-comint-process ()
  (and gdb--comint-buffer (buffer-live-p gdb--comint-buffer) (get-buffer-process gdb--comint-buffer)))

(defun gdb--command (string)
  (let ((process (gdb--get-comint-process)))
    (when process (comint-simple-send process string))))

(defun gdb--output-filter (string)
  "Parse GDB/MI output."
  (gdb--parse-mi-output string))

(defun gdb--setup-windows ()
  ;; TODO(nox): Actually setup windows!!
  (let ((comint (gdb--get-buffer-create 'gdb--comint nil t))
        (inferior-io (gdb--get-buffer-create 'gdb--inferior-io nil t)))))

(define-derived-mode gdb-mode comint-mode "GDB"
  "Major mode for interacting with GDB debugger process."
  (setq-local comint-preoutput-filter-functions '(gdb--output-filter))
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-regexp "^(gdb) ")
  (setq-local comint-prompt-read-only nil)
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq-local mode-line-process '(":%s"))
  (setq-local gdb--frame (selected-frame)))

(defun gdb-kill (&optional frame)
  "Kill GDB, and delete frame if there are more visible frames.
When called interactively:
 - If there are more frames, this will first delete the frame,
     which will call this function again and proceed to kill GDB.
 - Else, this will kill GDB."
  (interactive)
  (let* ((no-arg (not frame))
         (gdb-frame (gdb--get-local gdb--frame))
         (frame-live (frame-live-p gdb-frame))
         (more-frames (> (length (visible-frame-list)) 1))
         (process (gdb--get-comint-process))
         (should-delete-frame (and no-arg frame-live more-frames))
         (should-cleanup (or no-arg (eq frame gdb-frame))))
    (if should-delete-frame
        (delete-frame gdb-frame t) ; Only delete frame when running command, this
                                        ; function will be called again
      (when should-cleanup
        (when process (kill-process process))
        (let ((inferior-process (get-process "GDB inferior")))
          (if inferior-process (delete-process inferior-process)))
        (setq delete-frame-functions (remove 'gdb-kill delete-frame-functions))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (and gdb--buffer-type
                       ; Keep comint and inferior I/O buffer, so that people may read all
                       ; their history, if wanted
                       (not (eq gdb--buffer-type 'gdb--comint))
                       (not (eq gdb--buffer-type 'gdb--inferior-io)))
              (kill-buffer nil))))))))

;;;###autoload
(defun gdb (arg)
  "Start GDB in a new frame, or switch to existing GDB session
TODO(nox): Complete this"
  (interactive "P")
  (let* ((this-frame (equal arg '(16)))
         (stop-or-specify (or this-frame (equal arg '(4)))))
    (if stop-or-specify (gdb-kill))
    (let* ((frame (gdb--get-local gdb--frame))
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
  (select-frame-set-input-focus (gdb--get-local gdb--frame)))

(provide 'emacs-gdb)
;;; emacs-gdb.el ends here
