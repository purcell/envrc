;;; envrc.el --- Support for `direnv' that operates buffer-locally  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: processes, tools
;; Homepage: https://github.com/purcell/envrc
;; Package-Requires: ((emacs "26.1") (inheritenv "0.1") (seq "2.24"))
;; Package-Version: 0.12

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use direnv (https://direnv.net/) to set environment variables on a
;; per-buffer basis.  This means that when you work across multiple
;; projects which have `.envrc` files, all processes launched from the
;; buffers "in" those projects will be executed with the environment
;; variables specified in those files.  This allows different versions
;; of linters and other tools to be installed in each project if
;; desired.

;; Enable `envrc-global-mode' late in your startup files.  For
;; interaction with this functionality, see `envrc-mode-map', and the
;; commands `envrc-reload', `envrc-allow' and `envrc-deny'.

;; In particular, you can enable keybindings for the above commands by
;; binding your preferred prefix to `envrc-command-map' in
;; `envrc-mode-map', e.g.

;;    (with-eval-after-load 'envrc
;;      (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))

;;; Code:

;; TODO: special handling for DIRENV_* vars? exclude them? use them to safely reload more aggressively?
;; TODO: handle nil default-directory (rarely happens, but is possible)
;; TODO: limit size of *direnv* buffer
;; TODO: special handling of compilation-environment?
;; TODO: handle use of "cd" and other changes of `default-directory' in a buffer over time?
;; TODO: handle "allow" asynchronously?
;; TODO: describe env
;; TODO: click on mode lighter to get details
;; TODO: handle when direnv is not installed?
;; TODO: provide a way to disable in certain projects?
;; TODO: cleanup the cache?

(require 'seq)
(require 'json)
(require 'subr-x)
(require 'ansi-color)
(require 'cl-lib)
(require 'diff-mode) ; for its faces
(require 'inheritenv)
(eval-when-compile (require 'tramp))

;;; Custom vars and minor modes

(defgroup envrc nil
  "Apply per-buffer environment variables using the direnv tool."
  :group 'processes)

(defcustom envrc-debug nil
  "Whether or not to output debug messages while in operation.
Messages are written into the *envrc-debug* buffer."
  :type 'boolean)

(defcustom envrc-add-to-mode-line-misc-info t
  "When non-nil, append the envrc indicator to the mode line.
Experienced users can set this to a nil value and then include the
`envrc-indicator' anywhere they want in `mode-line-format' or related."
  :group 'envrc
  :type 'boolean)

(defcustom envrc-async-processing t
  "Whether or not to update the environment asynchronously."
  :group 'envrc
  :type 'boolean)

(defcustom envrc-async-prompt-before-kill t
  "Whether or not to prompt the user before killing running envrc processes."
  :group 'envrc
  :type 'boolean)

(defcustom envrc-status-frames '("=  " "== " "===" " ==" "  =" "   ")
  "List of frames for the spinner."
  :group 'envrc
  :type '(repeat string))

(defcustom envrc-update-on-eshell-directory-change t
  "Whether envrc will update environment when changing directory in eshell."
  :type 'boolean)

(defcustom envrc-show-summary-in-minibuffer t
  "When non-nil, show a summary of the changes made by direnv in the minibuffer."
  :group 'envrc
  :type 'boolean)

(defcustom envrc-direnv-executable "direnv"
  "The direnv executable used by envrc."
  :type 'string)

(define-obsolete-variable-alias 'envrc--lighter 'envrc-indicator "2021-05-17")

(defcustom envrc-indicator '(" envrc[" (:eval (envrc--status)) "]")
  "The mode line lighter for `envrc-mode'.
You can set this to nil to disable the lighter."
  :type 'sexp)
(put 'envrc-indicator 'risky-local-variable t)

(defcustom envrc-none-indicator '((:propertize "none" face envrc-mode-line-none-face))
  "Construct spec used by the default `envrc-indicator' when envrc is inactive."
  :type 'sexp)

(defcustom envrc-on-indicator '((:propertize "on" face envrc-mode-line-on-face))
  "Construct spec used by the default `envrc-indicator' when envrc is on."
  :type 'sexp)

(defcustom envrc-denied-indicator '((:propertize "denied" face envrc-mode-line-denied-face))
  "Construct spec used by the default `envrc-indicator' when envrc is blocked."
  :type 'sexp)

(defcustom envrc-error-indicator '((:propertize "error" face envrc-mode-line-error-face))
  "Construct spec used by the default `envrc-indicator' when envrc has errored."
  :type 'sexp)

(defcustom envrc-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'envrc-allow)
    (define-key map (kbd "d") 'envrc-deny)
    (define-key map (kbd "r") 'envrc-reload)
    (define-key map (kbd "l") 'envrc-show-log)
    map)
  "Keymap for commands in `envrc-mode'.
See `envrc-mode-map' for how to assign a prefix binding to these."
  :type '(restricted-sexp :match-alternatives (keymapp)))
(fset 'envrc-command-map envrc-command-map)

(defcustom envrc-mode-map (make-sparse-keymap)
  "Keymap for `envrc-mode'.
To access `envrc-command-map' from this map, give it a prefix keybinding,
e.g. (define-key envrc-mode-map (kbd \"C-c e\") \\='envrc-command-map)"
  :type '(restricted-sexp :match-alternatives (keymapp)))

(defcustom envrc-remote nil
  "Whether or not to enable direnv over TRAMP."
  :type 'boolean)

(defcustom envrc-supported-tramp-methods '("ssh")
  "Tramp connection methods that are supported by envrc."
  :type '(repeat string))

(defvar envrc--used-mode-line-construct nil
  "Mode line construct last added by `notmuch-indicator-mode'.")

;;;###autoload
(define-minor-mode envrc-mode
  "A local minor mode in which env vars are set by direnv."
  :init-value nil
  :lighter 'envrc
  :keymap envrc-mode-map
  (if envrc-mode
      (progn
        (when envrc-add-to-mode-line-misc-info
          (setq envrc--used-mode-line-construct envrc-indicator)
           ;; NOTE since this is a minor mode, `mode-line-misc-info' needs to be
           ;; controlled locally.
          (make-local-variable 'mode-line-misc-info)
          (add-to-list 'mode-line-misc-info envrc-indicator))
        (envrc--update)
        (when (and (derived-mode-p 'eshell-mode) envrc-update-on-eshell-directory-change)
          (add-hook 'eshell-directory-change-hook #'envrc--update nil t)))
    (setq mode-line-misc-info (delete envrc--used-mode-line-construct mode-line-misc-info))
    (envrc--clear (current-buffer))
    (remove-hook 'eshell-directory-change-hook #'envrc--update t)))

;;;###autoload
(define-globalized-minor-mode envrc-global-mode envrc-mode
  (lambda ()
    (when
        (cond
         ((minibufferp) nil)
         ((file-remote-p default-directory)
          (and envrc-remote
               (seq-contains-p
                envrc-supported-tramp-methods
                (with-parsed-tramp-file-name default-directory vec vec-method))))
         (t (executable-find envrc-direnv-executable)))
      (envrc-mode 1))))

(defface envrc-mode-line-on-face '((t :inherit success))
  "Face used in mode line to indicate that direnv is in effect.")

(defface envrc-mode-line-denied-face '((t :inherit shadow))
  "Face used in mode line to indicate that direnv blocking env.")

(defface envrc-mode-line-error-face '((t :inherit error))
  "Face used in mode line to indicate that direnv failed.")

(defface envrc-mode-line-none-face '((t :inherit warning))
  "Face used in mode line to indicate that direnv is not active.")

(defface envrc-mode-line-loading-face '((t :inherit mode-line-emphasis))
  "Face used in mode line to indicate that direnv is loading the environment.")

;;; Global state

(defvar envrc--cache (make-hash-table :test 'equal :size 10)
  "Known envrc directories and their direnv results.
The values are as produced by `envrc--export'.")

(defvar envrc--processes (make-hash-table :test 'equal :size 10)
  "Asynchonous processes started by envrc.
Each entry uses an environment directory as key.

Each value of this table is an alist of
`((process . <process>) (subscribed . (buf_0 buf_1 ... buf_n)))'

The SUBSCRIBED field allows subscribing buffers to have the new
environment applied once the async process finishes.")

;;; Local state

(defvar-local envrc--status 'none
  "Symbol indicating state of the current buffer's direnv.
One of \\='(none loading on error).")

(defvar-local envrc--remote-path nil
  "Buffer local variable for remote path.
If set, this will override `tramp-remote-path' via connection
local variables.")

;;; Internals

(defvar envrc--status-timer nil
  "Timer for updating the spinner.")

(defvar envrc--status-index 0
  "Current index in the spinner frames list.")

(defvar envrc--loading-indicator nil
  "Current frame to display during the loading indicator state.")

(defun envrc--status ()
  "Return a colourised version of `envrc--status' for use in the mode line."
  (pcase envrc--status
    (`none envrc-none-indicator)
    (`loading envrc--loading-indicator)
    (`on envrc-on-indicator)
    (`denied envrc-denied-indicator)
    (`error envrc-error-indicator)))

(defvar envrc--loading-buf-list '()
  "List of buffers that are loading an environment.")

(defun envrc--status-update-buf (buf)
  "Update the spinner in BUF's mode line."
  (let ((spinner (nth envrc--status-index envrc-status-frames)))
    (with-current-buffer buf
      (setq envrc--loading-indicator `((:propertize ,spinner face envrc-mode-line-loading-face))
            envrc--status 'loading)
      (force-mode-line-update))))

(defun envrc-status-update ()
  "Update the spinner in the mode line.
ENV-DIR is the directory where to update the status"
  (let (keys)
    (maphash (lambda (key _value)
               (push key keys))
             envrc--processes)
    (walk-windows (lambda (win)
                    (let ((win-buf (window-buffer win)))
                      (unless (minibufferp win-buf)
                        (with-current-buffer win-buf
                          (when (member (envrc--find-env-dir) keys)
                            (envrc--status-update-buf win-buf)))))))))

(defun envrc-status-start ()
  "Start the spinner and update it periodically.
ENV-DIR is the directory where to update the status."
  (unless envrc--status-timer
    (setq envrc--status-index 0)
    (setq envrc--status-timer
          (run-with-timer 0 0.1 (lambda ()
                                  (setq envrc--status-index
                                        (mod (1+ envrc--status-index)
                                             (length envrc-status-frames)))
                                  (envrc-status-update))))))

(defun envrc-status-stop (env-dir)
  "Stop the spinner and remove it from the mode line.
ENV-DIR is the directory where to update the status."
  (when (and envrc--status-timer
             (zerop (hash-table-count envrc--processes)))
    (cancel-timer envrc--status-timer)
    (setq envrc--status-timer nil))

  (let ((status envrc--status))
    (dolist (buf (envrc--mode-buffers))
      (with-current-buffer buf
        (when (equal (envrc--find-env-dir) env-dir)
          (setq envrc--status status)
          (force-mode-line-update))))))


(defun envrc--env-dir-p (dir)
  "Return non-nil if DIR contains a config file for direnv."
  (or
   (file-exists-p (expand-file-name ".envrc" dir))
   (file-exists-p (expand-file-name ".env" dir))))

(defun envrc--find-env-dir ()
  "Return the envrc directory for the current buffer, if any.
This is based on a file scan.  In most cases we prefer to use the
cached list of known directories.

Regardless of buffer file name, we always use
`default-directory': the two should always match, unless the user
called `cd'"
  (let ((env-dir (locate-dominating-file default-directory #'envrc--env-dir-p)))
    (when env-dir
      ;; `locate-dominating-file' appears to sometimes return abbreviated paths, e.g. with ~
      (setq env-dir (expand-file-name env-dir)))
    env-dir))

(defun envrc--find-deny-hash (env-dir)
  "Return the envrc file hash from ENV-DIR."
  (when-let* ((file-path (or (expand-file-name ".envrc" env-dir)
                             (expand-file-name ".env" env-dir)))
              (contents (with-temp-buffer
                          (insert file-path "\n")
                          (buffer-string))))
    (secure-hash 'sha256 contents)))

(defun envrc--denied-p (env-dir)
  "Return TRUE if ENV-DIR is blocked."
  (let ((deny-hash (envrc--find-deny-hash env-dir))
        (deny-path (concat (or (getenv "XDG_DATA_HOME")
                               (concat (getenv "HOME") "/.local/share"))
                           "/direnv/deny")))
      (locate-file deny-hash
                   (list deny-path))))

(defun envrc--cache-key (env-dir process-env)
  "Get a hash key for the result of invoking direnv in ENV-DIR with PROCESS-ENV.
PROCESS-ENV should be the environment in which direnv was run,
since its output can vary according to its initial environment."
  (mapconcat 'identity (cons env-dir process-env) "\0"))

(defun envrc--update-async ()
  "Update the current buffer's env asynchronously if it is managed by direnv.
All envrc.el-managed buffers with this env will have their
environments updated."
  (let* ((cur-buf (current-buffer))
         (env-dir (envrc--find-env-dir))
         (cache-key (envrc--cache-key env-dir (default-value 'process-environment)))
         (cache (gethash cache-key envrc--cache))
         (update-callback
          (lambda (env)
            (unless cache
              (puthash cache-key env envrc--cache))
            (unwind-protect
                (when (buffer-live-p cur-buf)
                  (envrc--apply cur-buf env))
              (let ((subscribed (alist-get 'subscribed (gethash env-dir envrc--processes))))
                (dolist (buf subscribed)
                  (when (buffer-live-p buf)
                    (envrc--apply buf env))))))))
    (if env-dir
        (if cache
            (funcall update-callback cache)
          (let ((running-process (alist-get 'process (gethash env-dir envrc--processes)))
                (subscribed (alist-get 'subscribed (gethash env-dir envrc--processes))))
            (if running-process
                (progn
                  (unless (memq cur-buf subscribed)
                    (push cur-buf subscribed))
                  (puthash env-dir
                           `((process . ,running-process)
                             (subscribed . ,subscribed))
                           envrc--processes))
              (envrc--export-async update-callback env-dir))))
      (funcall update-callback 'none))))

(defun envrc--update-sync ()
  "Update the current buffer's env synchronously if it is managed by direnv.
All envrc.el-managed buffers with this env will have their
environments updated."
  (let ((env-dir (envrc--find-env-dir)))
    (let ((result
           (if env-dir
               (let ((cache-key (envrc--cache-key env-dir (default-value 'process-environment))))
                 (pcase (gethash cache-key envrc--cache 'missing)
                   (`missing (let ((calculated (envrc--export env-dir)))
                               (puthash cache-key calculated envrc--cache)
                               calculated))
                   (cached cached)))
             'none)))
      (envrc--apply (current-buffer) result))))

(defun envrc--update ()
  "Update the current buffer's environment if it is managed by direnv.
All envrc.el-managed buffers with this env will have their
environments updated.
Synchronously or asynchronously according to the value of
`envrc-async-processing'."
  (if envrc-async-processing
      (envrc--update-async)
    (envrc--update-sync)))

(defmacro envrc--at-end-of-special-buffer (name &rest body)
  "At the end of `special-mode' buffer NAME, execute BODY.
To avoid confusion, `envrc-mode' is explicitly disabled in the buffer."
  (declare (indent 1))
  `(with-current-buffer (get-buffer-create ,name)
     (unless (derived-mode-p 'special-mode)
       (special-mode))
     (when envrc-mode (envrc-mode -1))
     (goto-char (point-max))
     (let ((inhibit-read-only t))
       ,@body)))

(defun envrc--debug (msg &rest args)
  "A version of `message' which does nothing if `envrc-debug' is nil.
MSG and ARGS are as for that function."
  (when envrc-debug
    (envrc--at-end-of-special-buffer "*envrc-debug*"
      (insert (apply 'format msg args))
      (newline))))

(defun envrc--summarise-changes (items)
  "Create a summary string for ITEMS."
  (if items
      (cl-loop for (name . val) in items
               if (not (string-prefix-p "DIRENV_" name))
               collect (cons name
                             (if val
                                 (if (let ((process-environment (default-value 'process-environment)))
                                       (getenv name))
                                     '("~" diff-changed)
                                   '("+" diff-added))
                               '("-" diff-removed)))
               into entries
               finally return (cl-loop for (name prefix face) in (seq-sort-by 'car 'string< entries)
                                       collect (propertize (concat prefix name) 'face face) into strings
                                       finally return (string-join strings " ")))
    "no changes"))

(defun envrc--show-summary (result directory)
  "Summarise successful RESULT in the minibuffer.
DIRECTORY is the directory in which the environment changes."
  (message "direnv: %s %s"
           (envrc--summarise-changes result)
           (propertize (concat "(" (abbreviate-file-name (directory-file-name directory)) ")")
                       'face 'font-lock-comment-face)))

(defun envrc--export-async (callback env-dir)
  "Export the env vars for ENV-DIR using direnv.
Return value is either \\='error, \\='none, or an alist of environment
variable names and values.

CALLBACK the function which will get the return value."
  (let* ((default-directory env-dir)
         (stdout (generate-new-buffer "*envrc-export*"))
         (stderr (generate-new-buffer "*envrc-export-stderr*"))
         (export-callback
          (lambda (exit-code)
            (unless (envrc--env-dir-p env-dir)
              (error "%s is not a directory with a .envrc" env-dir))
            (message "Running direnv in %s ..." env-dir)
            (let (result)
              (with-current-buffer stdout
                (envrc--debug "Direnv exited with %s and stderr=%S, stdout=%S"
                              exit-code
                              (with-current-buffer stderr
                                (buffer-string))
                              (buffer-string))
                (cond ((eq 0 exit-code) ;; zerop is not an option, as exit-code may sometimes be a symbol
                       (progn
                         (if (zerop (buffer-size))
                             (setq result 'none)
                           (goto-char (point-min))
                           (prog1
                               (if (envrc--denied-p env-dir)
                                   (setq result 'denied)
                                 (setq result (let ((json-key-type 'string)) (json-read-object))))
                             (when envrc-show-summary-in-minibuffer
                               (envrc--show-summary result env-dir))))))
                      ((eq 9 exit-code)
                       (message "Direnv killed in %s" env-dir)
                       (if (envrc--denied-p env-dir)
                           (setq result 'denied)
                         (setq result 'error)))
                      (t
                       (message "Direnv failed in %s" env-dir)
                       (setq result 'error)))
                (envrc--at-end-of-special-buffer "*envrc*"
                  (insert "──── " (format-time-string "%Y-%m-%d %H:%M:%S") " ──── " env-dir " ────\n\n")
                  (let ((initial-pos (point))
                        ansi-color-context)
                    (insert (with-current-buffer stderr
                              (ansi-color-apply (buffer-string))))
                    (goto-char (point-max))
                    (add-face-text-property initial-pos (point) (if (eq 0 exit-code) 'success 'error)))
                  (insert "\n\n")
                  ;; Since the async processing interface allows the user to
                  ;; interactively kill the process, do not popup the buffer
                  ;; when `envrc-async-processing' is true.
                  (when (and (numberp exit-code) (and (/= 0 exit-code)
                                                      (/= 9 exit-code)))
                    (display-buffer (current-buffer)))))
              (kill-buffer stdout)
              (kill-buffer stderr)
              result)))
         (sentinel (lambda (process msg)
                     (funcall callback
                              (funcall export-callback
                                       (envrc--async-process-sentinel process msg))))))
    (envrc--start-process-with-global-env sentinel stdout stderr envrc-direnv-executable "export" "json")))

(defun envrc--export (env-dir)
  "Export the env vars for ENV-DIR using direnv.
Return value is either \\='error, \\='none, or an alist of environment
variable names and values."
  (unless (envrc--env-dir-p env-dir)
    (error "%s is not a directory with a .envrc" env-dir))
  (message "Running direnv in %s ... (C-g to abort)" env-dir)
  (let ((stderr-file (make-temp-file "envrc"))
        result)
    (unwind-protect
        (let ((default-directory env-dir))
          (with-temp-buffer
            (let ((exit-code (condition-case nil
                                 (envrc--call-process-with-global-env envrc-direnv-executable nil (list t stderr-file) nil "export" "json")
                               (quit
                                (message "interrupted!!")
                                'interrupted))))
              (envrc--debug "Direnv exited with %s and stderr=%S, stdout=%S"
                            exit-code
                            (with-temp-buffer
                              (insert-file-contents stderr-file)
                              (buffer-string))
                            (buffer-string))
              (cond ((eq 0 exit-code) ;; zerop is not an option, as exit-code may sometimes be a symbol
                     (progn
                       (if (zerop (buffer-size))
                           (setq result 'none)
                         (goto-char (point-min))
                         (prog1
                             (if (envrc--denied-p env-dir)
                                 (setq result 'denied)
                               (setq result (let ((json-key-type 'string)) (json-read-object))))
                           (when envrc-show-summary-in-minibuffer
                             (envrc--show-summary result env-dir))))))
                    ((eq 9 exit-code)
                     (message "Direnv killed in %s" env-dir)
                     (if (envrc--denied-p env-dir)
                         (setq result 'denied)
                       (setq result 'error)))
                    (t
                     (message "Direnv failed in %s" env-dir)
                     (setq result 'error)))
              (envrc--at-end-of-special-buffer "*envrc*"
                (insert "──── " (format-time-string "%Y-%m-%d %H:%M:%S") " ──── " env-dir " ────\n\n")
                (let ((initial-pos (point)))
                  (insert-file-contents stderr-file)
                  (goto-char (point-max))
                  (let (ansi-color-context)
                    (ansi-color-apply-on-region initial-pos (point)))
                  (add-face-text-property initial-pos (point) (if (eq 0 exit-code) 'success 'error)))
                (insert "\n\n")
                (when (and (numberp exit-code) (/= 0 exit-code))
                  (display-buffer (current-buffer)))))))
      (delete-file stderr-file))
    result))

;; Forward declaration for the byte compiler
(defvar eshell-path-env)

(defun envrc--merged-environment (process-env pairs)
  "Make a `process-environment' value that merges PROCESS-ENV with PAIRS.
PAIRS is an alist obtained from direnv's output.
Values from PROCESS-ENV will be included, but their values will
be masked by Emacs' handling of `process-environment' if they
also appear in PAIRS."
  (append (mapcar (lambda (pair)
                    (if (cdr pair)
                        (format "%s=%s" (car pair) (cdr pair))
                      ;; Plain env name is the syntax for unsetting vars
                      (car pair)))
                  pairs)
          process-env))

(defun envrc--clear (buf)
  "Remove any effects of `envrc-mode' from BUF."
  (with-current-buffer buf
    (kill-local-variable 'exec-path)
    (kill-local-variable 'process-environment)
    (kill-local-variable 'info-directory-list)
    (when (derived-mode-p 'eshell-mode)
      (if (fboundp 'eshell-set-path)
          (eshell-set-path (butlast exec-path))
        (kill-local-variable 'eshell-path-env)))))


(defun envrc--apply (buf result)
  "Update BUF with RESULT, which is a result of `envrc--export'."
  (with-current-buffer buf
    (setq-local envrc--status (if (listp result) 'on result))
    (if (memq result '(none error denied))
        (progn
          (envrc--clear buf)
          (envrc--debug "[%s] reset environment to default" buf))
      (envrc--debug "[%s] applied merged environment" buf)
      (let* ((remote (when-let ((fn (buffer-file-name buf)))
                       (file-remote-p fn)))
             (env (envrc--merged-environment
                   (default-value (if remote
                                      'tramp-remote-process-environment
                                    'process-environment))
                   result))
             (path (getenv-internal "PATH" env))
             (parsed-path (parse-colon-path path)))
        (if remote
            (setq-local tramp-remote-process-environment env)
          (setq-local process-environment env))
        ;; Get PATH from the merged environment: direnv may not have changed it
        (if remote
            (setq-local envrc--remote-path parsed-path)
          (setq-local exec-path parsed-path))
        (when (derived-mode-p 'eshell-mode)
          (if (fboundp 'eshell-set-path)
              (eshell-set-path path)
            (setq-local eshell-path-env path)))
        (when-let ((info-path (getenv-internal "INFOPATH" env)))
          (setq-local Info-directory-list
                      (seq-filter #'identity (parse-colon-path info-path))))))))

(defun envrc--update-env (env-dir)
  "Refresh the state of the direnv in ENV-DIR and apply in all relevant buffers."
  (envrc--debug "Invalidating cache for env %s" env-dir)
  (cl-loop for k being the hash-keys of envrc--cache
           if (string-prefix-p (concat env-dir "\0") k)
           do (remhash k envrc--cache))
  (envrc--debug "Refreshing all buffers in env  %s" env-dir)
  (dolist (buf (envrc--mode-buffers))
    (with-current-buffer buf
      (when (string= (envrc--find-env-dir) env-dir)
        (envrc--update)))))

(defun envrc--mode-buffers ()
  "Return a list of all live buffers in which `envrc-mode' is enabled."
  (seq-filter (lambda (b) (and (buffer-live-p b)
                               (with-current-buffer b
                                 envrc-mode)))
              (buffer-list)))

(defmacro envrc--with-required-current-env (varname &rest body)
  "With VARNAME set to the current env dir path, execute BODY.
If there is no current env dir, abort with a user error."
  (declare (indent 1))
  (cl-assert (symbolp varname))
  `(let ((,varname (envrc--find-env-dir)))
     (unless ,varname
       (user-error "No enclosing .envrc"))
     ,@body))

(defun envrc--call-process-with-global-env (&rest args)
  "Like `call-process', but always use the global process environment.
In particular, we ensure the default variable `exec-path' and
`process-environment' are used.  This ensures an .envrc doesn't take
`envrc-direnv-executable' out of our path.
ARGS is as for `call-process'."
  (let ((exec-path (default-value 'exec-path))
        (process-environment (default-value 'process-environment)))
    (apply 'process-file args)))

(defun envrc--start-process-with-global-env (sentinel out-buf err-buf &rest args)
  "Like `start-process', but always use the global process environment.
In particular, we ensure the default variable `exec-path' and
`process-environment' are used.  This ensures an .envrc doesn't take
`envrc-direnv-executable' out of our path.

SENTINEL, OUT-BUF, ERR-BUF and ARGS are the respective keywords of
`make-process'."
  (let* ((env-buf (current-buffer))
         (env-dir default-directory)
         (running-process (alist-get 'process (gethash env-dir envrc--processes)))
         (wrapped-sentinel (lambda (process msg)
                             (unless (buffer-live-p env-buf)
                               ;; Migrate to any buffer from the same env.
                               (setq env-buf
                                     (seq-find (lambda (buf)
                                                 (with-current-buffer buf
                                                   (equal (envrc--find-env-dir) env-dir)))
                                               (envrc--mode-buffers))))
                             (unwind-protect
                                 ;; NOTE: the call back and the status stop
                                 ;; should run in a buffer from the same
                                 ;; environment as the async process.
                                 (with-current-buffer env-buf
                                   (funcall sentinel process msg)
                                   (envrc-status-stop env-dir))
                               (remhash env-dir envrc--processes)))))
    (if running-process
        (envrc--debug "Ignoring, process already running for %s." env-dir)
      (let* ((exec-path (default-value 'exec-path))
             (process-environment (default-value 'process-environment))
             (process (make-process
                       :name "*envrc-process*"
                       :buffer out-buf
                       :stderr err-buf
                       :sentinel wrapped-sentinel
                       :connection-type 'pipe
                       :command args)))
        (puthash env-dir `((process . ,process)
                           (subscribed . (,(current-buffer))))
                 envrc--processes)
        (when envrc-add-to-mode-line-misc-info
          (envrc-status-start))))))

(defun envrc--kill-running-prompt (env-dir)
  "Prompt user to kill any process loading the environment of ENV-DIR."
  (when-let* ((proc (alist-get 'process (gethash env-dir envrc--processes)))
              (kill (if envrc-async-prompt-before-kill
                        (yes-or-no-p (format "Process %s is loading the environment, kill it? " proc))
                      t)))
    (kill-process proc)
    (while (alist-get 'process (gethash env-dir envrc--processes))
      (sleep-for 0.1))))

(defun envrc-reload ()
  "Reload the current env."
  (interactive)
  (envrc--with-required-current-env env-dir
    (envrc--kill-running-prompt env-dir)
    (let* ((default-directory env-dir)
           (exit-code (envrc--call-process-with-global-env envrc-direnv-executable nil (get-buffer-create "*envrc-reload*") nil "reload")))
      (if (zerop exit-code)
          (envrc--update-env env-dir)
        (display-buffer "*envrc-reload*")
        (user-error "Error running direnv reload")))))

(defun envrc--async-process-sentinel (process msg)
  "Return PROCESS's exit code.

Display MSG in debug buffer if `envrc-debug' is non-nil."
  (when (memq (process-status process) '(exit signal))
    (let ((exit-code (process-exit-status process)))
      (envrc--debug (concat (process-name process) " - " msg " - status: " (number-to-string exit-code)))
      exit-code)))

(defun envrc-allow ()
  "Run \"direnv allow\" in the current env."
  (interactive)
  (envrc--with-required-current-env env-dir
    (envrc--kill-running-prompt env-dir)
    (let* ((default-directory env-dir)
           (exit-code (envrc--call-process-with-global-env envrc-direnv-executable nil (get-buffer-create "*envrc-allow*") nil "allow")))
      (if (zerop exit-code)
          (envrc--update-env env-dir)
        (display-buffer "*envrc-allow*")
        (user-error "Error running direnv allow")))))

(defun envrc-deny ()
  "Run \"direnv deny\" in the current env."
  (interactive)
  (envrc--with-required-current-env env-dir
    (envrc--kill-running-prompt env-dir)
    (let* ((default-directory env-dir)
           (exit-code (envrc--call-process-with-global-env envrc-direnv-executable nil (get-buffer-create "*envrc-deny*") nil "deny")))
      (if (zerop exit-code)
          (envrc--update-env env-dir)
        (display-buffer "*envrc-deny*")
        (user-error "Error running direnv deny")))))

(defun envrc-reload-all ()
  "Reload direnvs for all buffers.
This can be useful if a .envrc has been deleted."
  (interactive)
  (envrc--debug "Invalidating cache for all envs")
  (clrhash envrc--cache)
  (dolist (buf (envrc--mode-buffers))
    (with-current-buffer buf
      (envrc--update))))

(defun envrc-show-log ()
  "Open envrc log buffer."
  (interactive)
  (if-let ((buffer (get-buffer "*envrc*")))
      (pop-to-buffer buffer)
    (message "Envrc log buffer does not exist")))


;;; Propagate local environment to commands that use temp buffers

(defun envrc-propagate-environment (orig &rest args)
  "Advice function to wrap a command ORIG and make it use our local env.
This can be used to force compliance where ORIG starts processes
in a temp buffer.  ARGS is as for ORIG."
  (if envrc-mode
      (inheritenv (apply orig args))
    (apply orig args)))

(defun envrc-propagate-tramp-environment (buf)
  "Advice function to propagate `tramp-remote-path' and
`tramp-remote-process-environment' from buffer local values."
  (when envrc-mode
    (let ((cur-path envrc--remote-path)
          (cur-env tramp-remote-process-environment))
      (with-current-buffer buf
        (setq-local tramp-remote-process-environment cur-env)
        (setq-local envrc--remote-path cur-path))))
  buf)

(defun envrc-get-remote-path (fn vec)
  "Advice function to wrap `tramp-get-remote-path'.
Shortcuts tramp caching direnv sets the exec-path."
  (with-current-buffer (tramp-get-connection-buffer vec)
    (or envrc--remote-path
        (apply fn vec nil))))

(advice-add 'shell-command :around #'envrc-propagate-environment)
(advice-add 'org-babel-eval :around #'envrc-propagate-environment)
(advice-add 'org-export-file :around #'envrc-propagate-environment)
(advice-add 'tramp-get-connection-buffer :filter-return #'envrc-propagate-tramp-environment)
(advice-add 'tramp-get-remote-path :around #'envrc-get-remote-path)


;;; Major mode for .envrc files

(defvar envrc-file-extra-keywords
  '("MANPATH_add" "PATH_add" "PATH_rm" "direnv_apply_dump" "direnv_layout_dir"
    "direnv_load" "direnv_version" "dotenv" "dotenv_if_exists"
    "env_vars_required" "expand_path" "fetchurl" "find_up" "has" "join_args"
    "layout" "load_prefix" "log_error" "log_status" "on_git_branch" "path_add"
    "path_rm" "rvm" "semver_search" "source_env" "source_env_if_exists"
    "source_up" "source_up_if_exists" "source_url" "strict_env" "unstrict_env"
    "use" "use_guix" "use_flake" "use_nix" "user_rel_path" "watch_dir" "watch_file")
  "Useful direnv keywords to be highlighted.")

(declare-function sh-set-shell "sh-script")

;;;###autoload
(define-derived-mode envrc-file-mode
  sh-mode "envrc"
  "Major mode for .envrc files as used by direnv.
\\{envrc-file-mode-map}"
  (sh-set-shell "bash")
  (font-lock-add-keywords
   nil `((,(regexp-opt envrc-file-extra-keywords 'symbols)
          (0 font-lock-keyword-face)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.envrc\\'" . envrc-file-mode))


(provide 'envrc)
;;; envrc.el ends here

;; LocalWords:  envrc direnv

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
