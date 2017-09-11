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

(defvar gdb-frame nil
  "Frame where GDB runs.")

(defvar gdb-comint-buffer nil
  "Buffer where the GDB process runs.")

(defvar gdb-last-file nil
  "Last executable to be ran by GDB.")

(defvar gdb-last-args nil
  "Last args to be passed to GDB.")

(defvar gdb-debug-output nil
  "If not `nil', this will start outputting debug messages.")

(defvar gdb-prompt-regexp "^(gdb)"
  "Regexp for GDB's prompt.")

(define-derived-mode gdb-mode comint-mode "GDB"
  "Major mode for interacting with GDB debugger process."
  (setq-local comint-preoutput-filter-functions '(gdb--output-filter))
  (setq-local comint-prompt-regexp gdb-prompt-regexp)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-use-prompt-regexp t)
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start gdb-prompt-regexp)
  (setq mode-line-process '(":%s")))

(defun gdb--output-filter (string)
  "Parse GDB/MI output."
  (gdb--parse-mi-output string))

(defun gdb--run (debuggee extra-args)
  (let* ((debuggee-path (file-name-directory debuggee))
         (debuggee-name (file-name-base debuggee))
         (debuggee-exists (and (not (string= "" debuggee-name)) (file-executable-p debuggee)))
         (switches (split-string extra-args))
         (buffer-name (concat "*GDB" (when debuggee-exists (concat "-" debuggee-name)) "*"))
         (frame-name (concat "Emacs GDB" (when debuggee-exists
                                           (concat " - Debugging " debuggee-name)))))
    (setq gdb-comint-buffer (get-buffer-create buffer-name))
    (modify-frame-parameters nil `((name . ,frame-name)))
    (switch-to-buffer gdb-comint-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (add-to-list 'switches "-i=mi")
    (when debuggee-exists
      (cd debuggee-path)
      (add-to-list 'switches debuggee))
    (apply 'make-comint-in-buffer "GDB" gdb-comint-buffer "gdb" nil switches)
    (gdb-mode)))

(defun gdb-kill (&optional frame)
  "Kill GDB, and delete frame if there are more visible frames.
When called interactively:
 - If there are more frames, this will first delete the frame,
     which will call this function again and proceed to kill GDB.
 - Else, this will kill GDB."
  (interactive)
  (let* ((no-arg (not frame))
         (frame-live (frame-live-p gdb-frame))
         (more-frames (> (length (visible-frame-list)) 1))
         (process (get-buffer-process gdb-comint-buffer))
         (should-delete-frame (and no-arg frame-live more-frames))
         (should-kill-process (and process (or no-arg (eq frame gdb-frame)))))
    (if should-delete-frame
        (delete-frame gdb-frame t) ; Only delete frame when running command, this
                                        ; function will be called again
      (setq delete-frame-functions (remove 'gdb-kill delete-frame-functions))
      (when should-kill-process (kill-process process)))))

;;;###autoload
(defun gdb (arg)
  "Start GDB in a new frame, or switch to existing GDB session
TODO(nox): Complete this"
  (interactive "P")
  (let* ((this-frame (equal arg '(16)))
         (stop-or-specify (or this-frame (equal arg '(4)))))
    (if stop-or-specify (gdb-kill))
    (let ((frame-live (and (not stop-or-specify) (frame-live-p gdb-frame)))
          (gdb-running (and (not stop-or-specify) (get-buffer-process gdb-comint-buffer))))
      (cond ((and gdb-running frame-live)
             ;; TODO(nox): Do this when restore windows is available
             (with-selected-frame gdb-frame (ignore)))
            ((and gdb-running (not frame-live))
             (gdb-kill)
             (error "Frame was dead and process was still running - this should never happen."))
            (t
             (let* ((debuggee
                     (or (unless stop-or-specify gdb-last-file)
                         (and (y-or-n-p "Do you want to debug an executable?")
                              (expand-file-name (read-file-name
                                                 "Select file to debug: "
                                                 nil gdb-last-file t gdb-last-file 'file-executable-p)))
                         ""))
                    (extra-args (or (unless stop-or-specify gdb-last-args)
                                    (read-string "Extra arguments: " gdb-last-args))))
               (setq gdb-last-file debuggee)
               (setq gdb-last-args extra-args)
               (add-to-list 'delete-frame-functions 'gdb-kill)
               (if this-frame
                   (setq gdb-frame (selected-frame))
                 (unless frame-live (setq gdb-frame (make-frame '((fullscreen . maximized))))))
               (with-selected-frame gdb-frame (gdb--run debuggee extra-args)))))))
  (select-frame-set-input-focus gdb-frame))

(provide 'emacs-gdb)
;;; emacs-gdb.el ends here
