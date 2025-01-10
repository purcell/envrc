;;; envrc-list.el --- Manage envrc sessions -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

;; This is an interface to manage `envrc' async processes.

;;; Code:

;;;; Requirements

(require 'envrc)
(require 'vtable)
(require 'proced)

;;; ---

(defcustom envrc-list-display-buffer-action
  nil
  "The action used to display the envrc list buffer."
  :group 'envrc
  :type 'sexp)

(defun envrc-list--get-processes ()
  "Return processes started by `envrc'."
  (let (table)
    (maphash (lambda (key val)
               (let ((proc (alist-get 'process val))
                     (subscribed (seq-filter (lambda (buf)
                                               (and (buffer-live-p buf)
                                                    (not (minibufferp buf))))
                                             (alist-get 'subscribed val))))
                 (push `((process . ,proc) (path . ,key) (subscribed . ,subscribed))
                       table)))
             envrc--processes)
    (or table
        '(()))))

;; NOTE: the default face sets a foreground. Better to use one without so we can
;; propertize.
(defface envrc-list-even-face `((t :extend t))
  "Face used by `envrc-list-processes' for odd rows.")

(defface envrc-list-odd-face '((t :background "gray92" :extend t))
  "Face used by `envrc-list-processes' for odd rows.")

(defface envrc-list-pid-face '((t :inherit bold))
  "Face used by `envrc-list-processes' for odd rows.")

(defface envrc-list-path-face '((t :inherit link))
  "Face used by `envrc-list-processes' for odd rows.")

(defface envrc-list-subscribed-face '((t :foreground "OliveDrab"))
  "Face used by `envrc-list-processes' for odd rows.")

(defcustom envrc-list-auto-update-flag t
  "Non-nil means auto update envrc buffers."
  :group 'envrc
  :type 'boolean)

(defcustom envrc-list-auto-update-interval 0.5
  "Time interval in idle seconds for auto updating `envrc' buffers."
  :group 'envrc
  :type 'integer)

(defvar envrc-list-auto-update-timer nil
  "Stores if `Envrc' auto update timer is already installed.")

(defun envrc-list-auto-update ()
  "Auto-update `envrc' buffers using `run-at-time'.

If there are no `envrc' buffers, cancel the timer."
  (if (and envrc-list-auto-update-flag
           (get-buffer "*envrc-processes*"))
      (envrc-list-refresh)
    (cancel-timer envrc-list-auto-update-timer)
    (setq envrc-list-auto-update-timer nil)))

(defun envrc-list--object-find ()
  "Find item of vtable under point."
  (interactive)
  (let ((col (vtable-current-column)))
    (cond
     ((eql col 0)
      (let* ((key 'pid)
             (pid (save-excursion
                    ;; HACK: indentation messes up with `thing-at-point'.
                    (when (zerop (current-column))
                      (forward-char))
                    (thing-at-point 'number t)))
             (grammar (assq key proced-grammar-alist))
             (refiner (nth 7 grammar)))
        (when refiner
          (proced)
          (proced-toggle-tree t)
          (add-to-list 'proced-refinements (list refiner pid key grammar) t)
          (print proced-refinements)
          (proced-update))))
     ((eql col 1) (dired (thing-at-point 'filename t)))
     ((eql col 2) (when-let ((name (thing-at-point 'symbol t))
                             (buf (get-buffer name)))
                    (pop-to-buffer buf))))))

(defun envrc-list-kill-process ()
  "Kill process under POINT."
  (interactive)
  (when-let* ((table (vtable-current-table))
              (obj (vtable-current-object))
              (proc (alist-get 'process obj))) ; This ensures the empty entry is not deleted.
    (kill-process proc)
    ;; NOTE: prevent the table from being empty so it doesn't get automatically
    ;; deleted.
    (if (eql (length (vtable-objects table)) 1)
        (vtable-update-object table '(()) obj)
      (vtable-remove-object table obj))))

(defun envrc-list-refresh (&rest _)
  "Refresh `envrc' process list."
  (interactive)
  ;; HACK: running this while on minibuffer resets `window-point'.
  (unless (minibufferp)
    (when-let ((buf (get-buffer "*envrc-processes*")))
      (with-current-buffer buf
        (let* ((windows (get-buffer-window-list buf))
               (win-and-pts (mapcar (lambda (win)
                                      (cons win (window-point win)))
                                    windows)))
          (goto-char (point-min))
          (when (vtable-current-table)
            (vtable-revert-command))

          (dolist (wp win-and-pts)
            (set-window-point (car wp) (cdr wp))))))))

(defvar-keymap envrc-list-mode-map
  :doc "Keymap used in `envrc-list-mode'."
  "n" #'next-line
  "p" #'previous-line
  "k" #'envrc-list-kill-process
  "RET" #'envrc-list--object-find)

(define-derived-mode envrc-list-mode special-mode "envrc processes"
  "Mode for displaying `envrc' processes.

Type \\[envrc-list-toggle-auto-update] to automatically update the
process list.  The time interval for updates can be configured
via `envrc-list-auto-update-interval'."
  :interactive nil
  ;; HACK: since `envrc-list--object-action' deals with buffers as symbols
  ;; using `thing-at-point'. The '.' should be considered as part of a
  ;; symbol.
  (modify-syntax-entry ?. "_")

  (setq buffer-read-only t)
  (setq-local revert-buffer-function #'envrc-list-refresh)

  (if (and envrc-list-auto-update-flag
           envrc-list-auto-update-interval
           (not envrc-list-auto-update-timer))
      (setq envrc-list-auto-update-timer
            (run-with-idle-timer envrc-list-auto-update-interval t
                                 'envrc-list-auto-update))))

;;;###autoload
(defun envrc-list-processes ()
  "Open list of `envrc' started processes."
  (interactive)
  (with-current-buffer (get-buffer-create "*envrc-processes*")
    (let ((inhibit-read-only t))
      (envrc-list-mode)
      (erase-buffer)
      (make-vtable
       :columns
       '((:name "PID" :min-width 3 :getter (lambda (obj table)
                                             (when obj
                                               (when-let ((proc (alist-get 'process obj)))
                                                 (number-to-string (process-id proc))))))
         (:name "Path" :min-width 4 :getter (lambda (obj table)
                                              (when obj
                                                (when-let ((path (alist-get 'path obj)))
                                                  path))))
         (:name "Buffers" :min-width 7 :getter (lambda (obj table)
                                                 (when obj
                                                   (when-let ((buffers (alist-get 'subscribed obj)))
                                                     buffers)))))
       :objects-function #'envrc-list--get-processes
       :formatter (lambda (value column &rest _)
                    (if value
                        (cond
                         ((= column 0)
                          (propertize value 'face 'envrc-list-pid-face))
                         ((= column 1)
                          (propertize value
                                      'face 'envrc-list-path-face))
                         ((= column 2)
                          (propertize (string-join (mapcar (lambda (buf)
                                                             (buffer-name buf))
                                                           value)
                                                   ", ")
                                      'face 'envrc-list-subscribed-face)))
                      " "))
       :separator-width 2
       :row-colors '(envrc-list-even-face envrc-list-odd-face)
       :keymap
       (define-keymap
                "g" #'envrc-list-refresh))
      (pop-to-buffer (current-buffer) envrc-list-display-buffer-action))))

(provide 'envrc-list)

;;; envrc-list.el ends here
