;;; envrc.el --- Support for `direnv' that operates buffer-locally  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: processes, tools
;; Homepage: https://github.com/purcell/envrc
;; Package-Requires: ((seq "2") (emacs "24.4") (inheritenv "0.1"))
;; Package-Version: 0

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
;; TODO: special handling for remote files
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
(require 'inheritenv)

;;; Custom vars and minor modes

(defgroup envrc nil
  "Apply per-buffer environment variables using the direnv tool."
  :group 'processes)

(defcustom envrc-debug nil
  "Whether or not to output debug messages while in operation.
Messages are written into the *envrc-debug* buffer."
  :type 'boolean)

(defcustom envrc--lighter '(:eval (envrc--lighter))
  "The mode line lighter for `envrc-mode'.
You can set this to nil to disable the lighter."
  :type 'sexp)

(put 'envrc--lighter 'risky-local-variable t)

(defcustom envrc-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'envrc-allow)
    (define-key map (kbd "d") 'envrc-deny)
    (define-key map (kbd "r") 'envrc-reload)
    map)
  "Keymap for commands in `envrc-mode'.
See `envrc-mode-map' for how to assign a prefix binding to these."
  :type 'keymap)
(fset 'envrc-command-map envrc-command-map)

(defcustom envrc-mode-map (make-sparse-keymap)
  "Keymap for `envrc-mode'.
To access `envrc-command-map' from this map, give it a prefix keybinding,
e.g. (define-key envrc-mode-map (kbd \"C-c e\") 'envrc-command-map)"
  :type 'keymap)

;;;###autoload
(define-minor-mode envrc-mode
  "A local minor mode in which env vars are set by direnv."
  :init-value nil
  :lighter envrc--lighter
  :keymap envrc-mode-map
  (if envrc-mode
      (envrc--update)
    (envrc--clear (current-buffer))))

;;;###autoload
(define-globalized-minor-mode envrc-global-mode envrc-mode
  (lambda () (unless (or (minibufferp) (file-remote-p default-directory))
          (envrc-mode 1))))

(defface envrc-mode-line-on-face '((t :inherit success))
  "Face used in mode line to indicate that direnv is in effect.")

(defface envrc-mode-line-error-face '((t :inherit error))
  "Face used in mode line to indicate that direnv failed.")

(defface envrc-mode-line-none-face '((t :inherit warning))
  "Face used in mode line to indicate that direnv is not active.")

;;; Global state

(defvar envrc--cache (make-hash-table :test 'equal :size 10)
  "Known envrc directories and their direnv results, as produced by `envrc--export'.")

;;; Local state

(defvar-local envrc--status 'none
  "Symbol indicating state of the current buffer's direnv.
One of '(none on error).")

;;; Internals

(defun envrc--lighter ()
  "Return a colourised version of `envrc--status' for use in the mode line."
  `(" env["
    (:propertize ,(symbol-name envrc--status)
                 face
                 ,(pcase envrc--status
                    (`on 'envrc-mode-line-on-face)
                    (`error 'envrc-mode-line-error-face)
                    (`none 'envrc-mode-line-none-face)))
    "]"))

(defun envrc--find-env-dir ()
  "Return the envrc directory for the current buffer, if any.
This is based on a file scan.  In most cases we prefer to use the
cached list of known directories.

Regardless of buffer file name, we always use
`default-directory': the two should always match, unless the user
called `cd'"
  (let ((env-dir (locate-dominating-file default-directory ".envrc")))
    (when env-dir
      ;; `locate-dominating-file' appears to sometimes return abbreviated paths, e.g. with ~
      (setq env-dir (expand-file-name env-dir)))
    env-dir))

(defun envrc--cache-key (env-dir process-env)
  "Get a hash key for the result of invoking direnv in ENV-DIR with PROCESS-ENV."
  (mapconcat 'identity (cons env-dir process-env) "\0"))

(defun envrc--update ()
  "Update the current buffer's environment if it is managed by direnv.
All envrc.el-managed buffers with this env will have their
environments updated."
  (let ((env-dir (envrc--find-env-dir)))
    ;; TODO: if no env-dir?
    (when env-dir
      (let* ((cache-key (envrc--cache-key env-dir process-environment))
             (result (pcase (gethash cache-key envrc--cache 'missing)
                       (`missing (let ((calculated (envrc--export env-dir)))
                                   (puthash cache-key calculated envrc--cache)
                                   calculated))
                       (cached cached))))
        (envrc--apply (current-buffer) result)
        ;; We assume direnv and envrc's use of it is idempotent, and
        ;; add a cache entry for the new process-environment on that
        ;; basis.
        (puthash (envrc--cache-key env-dir process-environment) result envrc--cache)))))

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

(defun envrc--directory-path-deeper-p (a b)
  "Return non-nil if directory path B is deeper than directory path A."
  (string-prefix-p (file-name-as-directory a) (file-name-as-directory b)))

(defun envrc--deepest-paths-first (paths)
  "Sort PATHS such that the deepest paths in a hierarchy appear first."
  (sort paths
        (lambda (a b) (or (envrc--directory-path-deeper-p b a)
                          (string< a b)))))

(defun envrc--export (env-dir)
  "Export the env vars for ENV-DIR using direnv.
Return value is either 'error, 'none, or an alist of environment
variable names and values."
  (unless (file-exists-p (expand-file-name ".envrc" env-dir))
    (error "%s is not a directory with a .envrc" env-dir))
  (message "Running direnv in %s..." env-dir)
  (let ((stderr-file (make-temp-file "envrc"))
        result)
    (unwind-protect
        (let ((default-directory env-dir))
          (with-temp-buffer
            (let ((exit-code (envrc--call-process-with-default-exec-path "direnv" nil (list t stderr-file) nil "export" "json")))
              (envrc--debug "Direnv exited with %s and output: %S" exit-code (buffer-string))
              (if (zerop exit-code)
                  (progn
                    (message "Direnv succeeded in %s" env-dir)
                    (if (zerop (buffer-size))
                        (setq result 'none)
                      (goto-char (point-min))
                      (setq result (let ((json-key-type 'string)) (json-read-object)))))
                (message "Direnv failed in %s" env-dir)
                (setq result 'error))
              (envrc--at-end-of-special-buffer "*envrc*"
                (insert "==== " (format-time-string "%Y-%m-%d %H:%M:%S") " ==== " env-dir " ====\n\n")
                (let ((initial-pos (point)))
                  (insert-file-contents (let (ansi-color-context)
                                          (ansi-color-apply stderr-file)))
                  (goto-char (point-max))
                  (add-face-text-property initial-pos (point) (if (zerop exit-code) 'success 'error)))
                (insert "\n\n")
                (unless (zerop exit-code)
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
    (kill-local-variable 'eshell-path-env)))


(defun envrc--apply (buf result)
  "Update BUF with RESULT, which is a result of `envrc--export'."
  (with-current-buffer buf
    (setq-local envrc--status (if (listp result) 'on result))
    (if (memq result '(none error))
        (progn
          (envrc--clear buf)
          (envrc--debug "[%s] reset environment to default" buf))
      (envrc--debug "[%s] applied merged environment" buf)
      (setq-local process-environment (envrc--merged-environment process-environment result))
      (let ((path (getenv "PATH"))) ;; Get PATH from the merged environment: direnv may not have changed it
        (setq-local exec-path (parse-colon-path path))
        (when (derived-mode-p 'eshell-mode)
          (setq-local eshell-path-env path))))))

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

(defun envrc--call-process-with-default-exec-path (&rest args)
  "Like `call-process', but ensures the default variable `exec-path' is used.
This ensures the globally-accessible \"direnv\" binary is
consistently available.  ARGS is as for `call-process'."
  (let ((exec-path (default-value 'exec-path)))
    (apply 'call-process args)))

(defun envrc-reload ()
  "Reload the current env."
  (interactive)
  (envrc--with-required-current-env env-dir
    (envrc--update-env env-dir)))

(defun envrc-allow ()
  "Run \"direnv allow\" in the current env."
  (interactive)
  (envrc--with-required-current-env env-dir
    (let* ((default-directory env-dir)
           (exit-code (envrc--call-process-with-default-exec-path "direnv" nil (get-buffer-create "*envrc-allow*") nil "allow")))
      (if (zerop exit-code)
          (envrc--update-env env-dir)
        (display-buffer "*envrc-allow*")))))

(defun envrc-deny ()
  "Run \"direnv deny\" in the current env."
  (interactive)
  (envrc--with-required-current-env env-dir
    (let* ((default-directory env-dir)
           (exit-code (envrc--call-process-with-default-exec-path "direnv" nil (get-buffer-create "*envrc-deny*") nil "deny")))
      (if (zerop exit-code)
          (envrc--update-env env-dir)
        (display-buffer "*envrc-deny*")))))

(defun envrc-reload-all ()
  "Reload direnvs for all buffers.
This can be useful if a .envrc has been deleted."
  (interactive)
  (envrc--debug "Invalidating cache for all envs")
  (clrhash envrc--cache)
  (dolist (buf (envrc--mode-buffers))
    (with-current-buffer buf
      (envrc--update))))



;;; Propagate local environment to commands that use temp buffers

(defun envrc-propagate-environment (orig &rest args)
  "Advice function to wrap a command ORIG and make it use our local env.
This can be used to force compliance where ORIG starts processes
in a temp buffer.  ARGS is as for ORIG."
  (if envrc-mode
      (inheritenv (apply orig args))
    (apply orig args)))

(advice-add 'shell-command-to-string :around #'envrc-propagate-environment)


;;; Major mode for .envrc files

(defvar envrc-file-extra-keywords
  '("MANPATH_add" "PATH_add" "direnv_layout_dir" "direnv_load" "dotenv"
    "expand_path" "find_up" "has" "join_args" "layout" "load_prefix"
    "log_error" "log_status" "path_add" "rvm" "source_env" "source_up"
    "use" "use_guix" "use_nix" "user_rel_path" "watch_file")
  "Useful direnv keywords to be highlighted.")

;;;###autoload
(define-derived-mode envrc-file-mode
  sh-mode "envrc"
  "Major mode for .envrc files as used by direnv.
\\{envrc-file-mode-map}"
  (font-lock-add-keywords
   nil `((,(regexp-opt envrc-file-extra-keywords 'symbols)
          (0 font-lock-keyword-face)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.envrc\\'" . envrc-file-mode))


(provide 'envrc)
;;; envrc.el ends here

;; LocalWords:  envrc direnv
