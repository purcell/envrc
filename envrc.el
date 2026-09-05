;;; envrc.el --- Support for `direnv' that operates buffer-locally  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: processes, tools
;; Homepage: https://github.com/purcell/envrc
;; Package-Requires: ((emacs "28.1") (inheritenv "0.1"))
;; Package-Version: 0.14

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

;; By default Emacs will be blocked while direnv environments are
;; recomputed, but this behaviour can be altered by setting
;; `envrc-async': see the documentation for that variable to
;; understand what will work best for you.

;;; Code:

;; TODO: special handling for DIRENV_* vars? exclude them? use them to safely reload more aggressively?
;; TODO: handle nil default-directory (rarely happens, but is possible)
;; TODO: special handling of compilation-environment?
;; TODO: handle use of "cd" and other changes of `default-directory' in a buffer over time?
;; TODO: click on mode lighter to get details
;; TODO: store merged environment in the direnv buffer rather than re-merging it elsewhere?
;; TODO: envrc-direnv-mode with a keymap, r to reload etc.
;; TODO: use direnv to find the env dir, to avoid possibility of mismatched logic?
;; TODO: run incidental direnv commands in the env's main direnv buffer
;; TODO: add hooks that will be invoked when the effective environment changes

(require 'seq)
(require 'json)
(require 'subr-x)
(require 'ansi-color)
(require 'cl-lib)
(require 'diff-mode) ; for its faces
(require 'inheritenv)
(require 'let-alist)
(eval-when-compile (require 'tramp))

;;; Custom vars and minor modes

(defgroup envrc nil
  "Apply per-buffer environment variables using the direnv tool."
  :group 'processes)

(defcustom envrc-debug nil
  "Whether or not to output debug messages while in operation.
Messages are written into the *envrc-debug* buffer."
  :type 'boolean)

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

(defcustom envrc-async nil
  "Whether (and when) to run direnv asynchronously.

When `envrc-mode' gets enabled in a given buffer, it immediately tries
to set that buffer's environment based on the output of direnv, re-using
past results if available.  If direnv hasn't yet been run for that
directory, it will get run at this point, and by default `envrc-mode'
will block until it finishes.  This gives predictable results in mode
hooks and programmatic usage, e.g. so that subsequent minor modes can
find any executables they need.

However, certain direnv environments can sometimes take a long time to
evaluate, particularly with Nix and Guix, and it becomes inconvenient
for Emacs to be blocked, so some users will prefer direnv to run
asynchronously.

This variable provides the following options for this:

If nil (default), then direnv invocation will always block Emacs until
direnv has finished running.  In this case, \\[keyboard-quit] can still
be used to stop waiting, but direnv will continue to run and the results
will take effect in the corresponding buffer(s) once complete.  To stop
the invocation, use `envrc-show-log' to switch to the direnv process
buffer and kill it.

If t, then direnv invocation will never block Emacs.

If set to a number, then Emacs will wait for up to that many
seconds before leaving direnv to run asynchronously."
  :type '(choice (const :tag "Always" t)
                 (const :tag "When interrupted" nil)
                 (number :tag "After timeout (seconds)" :value 5))
  :safe t)


(define-obsolete-variable-alias 'envrc--lighter 'envrc-lighter "2021-05-17")

(defcustom envrc-lighter '(:eval (envrc--lighter))
  "The mode line lighter for `envrc-mode'.
You can set this to nil to disable the lighter."
  :type 'sexp)
(put 'envrc-lighter 'risky-local-variable t)

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
To access bindings in variable `envrc-command-map' from this map, give
it a prefix keybinding, e.g. (define-key envrc-mode-map (kbd \"C-c e\")
\\='envrc-command-map)"
  :type '(restricted-sexp :match-alternatives (keymapp)))

(easy-menu-define envrc-mode-menu envrc-command-map
  "Envrc mode menu."
  '("Direnv (envrc)"
    ["Show direnv log" envrc-show-log t :visible envrc-mode]
    ["Allow" envrc-allow t :visible envrc-mode]
    ["Deny" envrc-deny t :visible envrc-mode]
    ["Reload" envrc-reload t :visible envrc-mode]))

(easy-menu-add-item global-map '(menu-bar tools) envrc-mode-menu)

(defcustom envrc-remote nil
  "Whether or not to enable direnv over TRAMP."
  :type 'boolean)

(defcustom envrc-supported-tramp-methods '("ssh" "sshx")
  "Tramp connection methods that are supported by envrc."
  :type '(repeat string))

;;;###autoload
(define-minor-mode envrc-mode
  "A local minor mode in which env vars are set by direnv."
  :init-value nil
  :lighter envrc-lighter
  :keymap envrc-mode-map
  (if envrc-mode
      (progn
        (envrc--get-current-env-or-run-direnv)
        (when (and (derived-mode-p 'eshell-mode) envrc-update-on-eshell-directory-change)
          (add-hook 'eshell-directory-change-hook #'envrc--get-current-env-or-run-direnv nil t)))
    (envrc--clear)
    (remove-hook 'eshell-directory-change-hook #'envrc--get-current-env-or-run-direnv t)))

;;;###autoload
(define-globalized-minor-mode envrc-global-mode envrc-mode
  (lambda ()
    (when
        (cond
         ((minibufferp) nil)
         ((string-prefix-p " *eldoc" (buffer-name)) nil)
         ((derived-mode-p 'envrc--special-mode) nil)
         ((file-remote-p default-directory)
          (and envrc-remote
               (seq-contains-p
                envrc-supported-tramp-methods
                (with-parsed-tramp-file-name default-directory vec vec-method))))
         (t (executable-find envrc-direnv-executable)))
      (envrc-mode 1)))
  :predicate t)

(defface envrc-mode-line-on-face '((t :inherit success))
  "Face used in mode line to indicate that direnv is in effect.")

(defface envrc-mode-line-error-face '((t :inherit error))
  "Face used in mode line to indicate that direnv failed.")

(defface envrc-mode-line-none-face '((t :inherit warning))
  "Face used in mode line to indicate that direnv is not active.")

(defface envrc-mode-line-running-face '((t))
  "Face used in mode line to indicate that direnv is currently running.")

;;; Local state

(defvar-local envrc--status 'none
  "Symbol indicating state of the current buffer's direnv.
One of \\='(none on error).")

(defvar-local envrc--running nil
  "Whether direnv is known to be running for the current buffer's environment.")

(defvar-local envrc--remote-path nil
  "Buffer local variable for remote path.
If set, this will override `tramp-remote-path' via connection
local variables.")

(defvar-local envrc--env-dir nil
  "The env dir for this buffer.
To avoid repeated filesystem traversals, this is cached in each buffer.")

;;; Internals

(defun envrc--lighter ()
  "Return a colourised version of `envrc--status' for use in the mode line."
  (list " envrc["
        (list :propertize (symbol-name envrc--status)
              'face
              (pcase envrc--status
                (`on 'envrc-mode-line-on-face)
                (`error 'envrc-mode-line-error-face)
                (`denied 'envrc-mode-line-error-face)
                (`none 'envrc-mode-line-none-face)))
        ;; Cache this detail to avoid overhead in redisplay, e.g. when scrolling,
        ;; and don't display it at all for remote files
        (when envrc--running
          (list :propertize "*" 'face 'envrc-mode-line-running-face))
        "]"))

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
  (when-let* ((env-dir (locate-dominating-file default-directory #'envrc--env-dir-p)))
    ;; `locate-dominating-file' appears to sometimes return abbreviated paths, e.g. with ~
    (expand-file-name env-dir)))

(defmacro envrc--with-required-current-env (varname &rest body)
  "With VARNAME set to the current env dir path, execute BODY.
If there is no current env dir, abort with a user error."
  (declare (indent 1))
  `(progn
     (unless envrc-mode
       (user-error "This is not an envrc-mode buffer"))
     (unless envrc--env-dir
       (user-error "No enclosing .envrc"))
     (let ((,varname envrc--env-dir))
       ,@body)))

(define-derived-mode envrc--special-mode special-mode "Envrc Special"
  "Special mode for internal envrc buffers.")

(defmacro envrc--with-special-buffer (name &rest body)
  "In special buffer NAME, execute BODY.
Ensures the buffer is temporarily writeable, and that `envrc-mode' is
not enabled in it."
  (declare (indent 1))
  `(with-current-buffer (get-buffer-create ,name)
     (unless (derived-mode-p 'envrc--special-mode)
       (envrc--special-mode))
     (let ((inhibit-read-only t))
       ,@body)))

(defmacro envrc--at-end-of-special-buffer (name &rest body)
  "At the end of `special-mode' buffer NAME, execute BODY.
To avoid confusion, `envrc-mode' is explicitly disabled in the buffer."
  (declare (indent 1))
  (cl-assert (stringp name))
  `(envrc--with-special-buffer ,name
     (goto-char (point-max))
     ,@body))

(defun envrc--debug (msg &rest args)
  "A version of `message' which does nothing if `envrc-debug' is nil.
MSG and ARGS are as for that function."
  (when envrc-debug
    (let ((text (format "[%s] %s" (current-buffer) (format msg args))))
      (envrc--at-end-of-special-buffer "*envrc-debug*"
        (insert text)
        (newline)))))

(defun envrc--summarise-changes (items)
  "Create a summary string for ITEMS."
  (if items
      (cl-loop for (name . val) in items
               with process-environment = (default-value 'process-environment)
               unless (string-prefix-p "DIRENV_" name)
               collect (cons name
                             (if val
                                 (if (getenv name)
                                     '("~" diff-changed)
                                   '("+" diff-added))
                               '("-" diff-removed)))
               into entries
               finally return (cl-loop for (name prefix face) in (seq-sort-by 'car 'string< entries)
                                       collect (propertize (concat prefix name) 'face face) into strings
                                       finally return (string-join strings " ")))
    "no changes"))

(defun envrc--current-env-dir-message-string (directory)
  "Colourised mention of current DIRECTORY to include in a `message' call."
  (propertize (format "(%s)" (abbreviate-file-name (directory-file-name directory)))
              'face 'font-lock-comment-face))

(defun envrc--show-summary (result directory)
  "Summarise successful RESULT in the minibuffer.
DIRECTORY is the directory in which the environment changes."
  (message "direnv: %s %s"
           (envrc--summarise-changes result)
           (envrc--current-env-dir-message-string directory)))

;; Forward declarations for the byte compiler
(defvar eshell-path-env)
(defvar Info-directory-list)

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

(defun envrc--clear ()
  "Remove any effects of `envrc-mode' from this buffer."
  (kill-local-variable 'exec-path)
  (kill-local-variable 'process-environment)
  (kill-local-variable 'tramp-remote-process-environment)
  (kill-local-variable 'Info-directory-list)
  (when (derived-mode-p 'eshell-mode)
    (if (fboundp 'eshell-set-path)
        (eshell-set-path (butlast exec-path))
      (kill-local-variable 'eshell-path-env))))

(defun envrc--remote-p ()
  "Returns non-nil if this is a tramp buffer."
  (when-let* ((fn (or (buffer-file-name) default-directory)))
    (file-remote-p fn)))

(defun envrc--apply (buf result)
  "Update BUF with RESULT, which is a result of `envrc--direnv-export'."
  (with-current-buffer buf
    (setq-local envrc--running (eq result 'running))
    (unless envrc--running
      (setq-local envrc--status (if (listp result) 'on result))
      (envrc--clear)
      (envrc--debug "applying %s" result)
      (if (listp result)
          (let* ((remote (envrc--remote-p))
                 (process-env-var (if remote
                                      'tramp-remote-process-environment
                                    'process-environment))
                 (env (envrc--merged-environment (default-value process-env-var) result))
                 (path (getenv-internal "PATH" env))
                 (parsed-path (parse-colon-path path)))
            (set (make-local-variable process-env-var) env)
            (envrc--debug "applied merged %s" process-env-var)
            ;; Get PATH from the merged environment: direnv may not have changed it
            (if remote
                (setq-local envrc--remote-path parsed-path)
              (setq-local exec-path parsed-path))
            (cond ((derived-mode-p 'eshell-mode)
                   (if (fboundp 'eshell-set-path)
                       (eshell-set-path path)
                     (setq-local eshell-path-env path)))
                  ((derived-mode-p 'Info-mode)
                   (when-let* ((info-path (getenv-internal "INFOPATH" env)))
                     (setq-local Info-directory-list
                                 (append (seq-filter #'identity (parse-colon-path info-path))
                                         (default-value 'Info-directory-list)))))))
        (envrc--debug "reset environment to default")))))



;;; Plumbing for running direnv

;; There is a direnv buffer for each loaded environment.  A direnv
;; process is started here as necessary, and its result is stored in
;; local variables in this buffer.  Subsequently-opened buffers in the
;; same environment will re-use the results indefinitely, unless
;; `envrc-reload' is used, or that buffer is killed.

(defun envrc--direnv-buffer-name (env-dir)
  "Return the name of the direnv buffer for ENV-DIR."
  (format "*envrc-direnv - %s*" (abbreviate-file-name env-dir)))

(defmacro envrc--with-direnv-buffer (&rest body)
  "Execute BODY in a buffer specific to the current env directory."
  `(save-excursion
     (envrc--with-required-current-env default-directory
       (envrc--with-special-buffer (envrc--direnv-buffer-name default-directory)
         ,@body))))

(defvar-local envrc--direnv-result nil
  "Parsed output of last direnv invocation in the envrc buffer.")
(defvar-local envrc--direnv-status nil
  "Status of the process in the envrc buffer.
Either \='success or \='error.  If nil, then direnv has not yet been
executed.")
(defvar-local envrc--direnv-exit-status nil
  "Exit code of the last direnv invocation.")
(defvar-local envrc--direnv-global-process-environment nil
  "The global process environment used for the last envrc invocation.")

(defun envrc--direnv-apply-status-to (buf)
  "From the current direnv buffer, propagate the status to `envrc-mode' buffer BUF."
  (envrc--apply buf (pcase envrc--direnv-status
                      (`success envrc--direnv-result)
                      (_ envrc--direnv-status))))

(defun envrc--direnv-allowed-status-code ()
  "Get direnv's numeric code for the status of the found environment, if any."
  (when-let* ((output (with-temp-buffer
                        (when (zerop (process-file envrc-direnv-executable nil t nil "status" "--json"))
                          (buffer-substring (point-min) (point-max))))))
    (condition-case _
        (let-alist (json-read-from-string output) .state.foundRC.allowed)
      (error
       ;; Old direnv that can't actually output status as JSON
       (cond ((string-search "Found RC allowed true\n" output) 0)
             ((string-search "Found RC allowed false\n" output) 2))))))

(defun envrc--direnv-set-status (status)
  "Save direnv STATUS locally and propagate it to relevant `envrc-mode' buffers."
  (message "%s %s"
           (propertize (format "direnv: %s" (symbol-name status))
                       'face
                       (pcase status
                         (`success 'success)
                         (`error 'error)
                         (`denied 'warning)))
           (envrc--current-env-dir-message-string default-directory))
  (setq envrc--direnv-status status)
  (dolist (buf (envrc--mode-buffers))
    (when (string= default-directory (with-current-buffer buf envrc--env-dir))
      (envrc--direnv-apply-status-to buf))))

(defun envrc--direnv-colourise-output (exit-status)
  "Colourise the current buffer contents.
The text will first have any ANSI colour applied, and then be colourised
according to the indicated process EXIT-STATUS."
  (let ((inhibit-read-only t)
        ansi-color-context
        (face (if (zerop exit-status) 'success 'error)))
    (ansi-color-apply-on-region (point-min) (point-max))
    (add-face-text-property (point-min) (point-max) face)))

(defun envrc--direnv-export ()
  "Run direnv asynchronously in the process buffer for the current env.
When the process has exited, apply the results to the environment in all
coresponding buffers."
  (cl-assert (string-prefix-p "*envrc-direnv" (buffer-name (current-buffer))))
  ;; Deal with any existing invocation first
  (when-let* ((proc (get-buffer-process (current-buffer))))
    ;; First ensure it will not overwrite the status vars
    (set-process-sentinel proc nil)
    (envrc--debug "cancelled previous direnv invocation")
    (kill-process proc))

  ;; Record the environment in which we're running direnv
  (setq envrc--direnv-global-process-environment (default-value 'process-environment))
  (envrc--direnv-set-status 'running)
  ;; First check whether direnv is enabled here
  (pcase (envrc--direnv-allowed-status-code)
    ((pred null)
     (envrc--direnv-set-status 'none))
    ((or 1 2)
     (envrc--direnv-set-status 'denied))
    (0
     (let ((raw-json ""))
       (kill-region (point-min) (point-max))
       ;; todo tramp? e.g. start-file-process
       (make-process
        :name "direnv"
        :buffer (current-buffer)
        :command (list envrc-direnv-executable "export" "json")
        :stderr (current-buffer)
        :filter (lambda (_ output) (setq raw-json (concat raw-json output)))
        :sentinel (lambda (proc event)
                    (with-current-buffer (process-buffer proc)
                      (condition-case err
                          (progn
                            (if (string-equal event "finished\n")
                                (progn
                                  (envrc--debug "direnv finished with output: %s" raw-json)
                                  (setq envrc--direnv-result (unless (string-empty-p raw-json)
                                                               (let ((json-key-type 'string))
                                                                 (json-read-from-string raw-json))))
                                  (envrc--direnv-set-status 'success)
                                  ;; TODO: set env locally here too, to allow efficient direnv reload?
                                  (when envrc-show-summary-in-minibuffer
                                    (envrc--show-summary envrc--direnv-result default-directory)))
                              ;; Process signalled or exited with failure
                              (envrc--debug "direnv exited: %s" event)
                              (envrc--direnv-set-status 'error))
                            (unless (process-live-p proc)
                              (envrc--direnv-colourise-output (process-exit-status proc))))
                        (error
                         (envrc--debug "Sentinel died with error: %s" err)
                         (envrc--direnv-set-status 'error))))))))

    (_
     ;; Assertion for future unhandled values
     (error "Unknown direnv foundRC state"))))

(defun envrc--maybe-wait ()
  "If direnv is currently running, block if `envrc-async' says to."
  (when (and envrc--running (not (eq t envrc-async)))
    (let ((waited 0)
          (step 0.2))
      (ignore-error quit                ; Stop waiting upon C-g
        (while (and envrc--running (or (null envrc-async) (< waited envrc-async)))
          (sleep-for step)
          (setq waited (+ waited step))))
      (envrc--debug "waited for %s" waited)
      (when envrc--running
        (message "direnv continuing async in %s"
                 (envrc--direnv-buffer-name envrc--env-dir))))))

(defun envrc--get-current-env-or-run-direnv (&optional force)
  "Find the last exported env and apply it, or run direnv if necessary.
According to `envrc-async', any resulting direnv invocation may block
for a limited time, or indefinitely.

If the global `process-environment' has changed since the last
invocation of `direnv', also re-run direnv, because the changes can
affect the results of direnv.

If FORCE is non-nil, then direnv will be run unconditionally."
  (cl-assert envrc-mode nil "must only be called from an `envrc-mode' buffer")
  (let ((orig-buffer (current-buffer)))
    (setq-local envrc--env-dir (envrc--find-env-dir))
    (if envrc--env-dir
        (progn
          (envrc--with-direnv-buffer
           (cl-assert (not (eq orig-buffer (current-buffer))))
           (if (and (not force)
                    envrc--direnv-status
                    (eq (default-value 'process-environment)
                        envrc--direnv-global-process-environment))
               ;; Re-use the cached status directly
               (progn
                 (envrc--debug "re-using cached direnv result")
                 (envrc--direnv-apply-status-to orig-buffer))
             ;; Run direnv for the first time unless it's already running
             (envrc--debug "need to (re-)run direnv")
             (if (eq 'running envrc--direnv-status)
                 (envrc--debug "will wait for existing process")
               (envrc--direnv-export))))
          (envrc--maybe-wait))
      (envrc--debug "no current env dir")
      (envrc--apply orig-buffer 'none))))

(defun envrc--mode-buffers ()
  "Return a list of all live buffers in which `envrc-mode' is enabled."
  (seq-filter (lambda (b)
                (and (buffer-live-p b)
                     (with-current-buffer b envrc-mode)))
              (buffer-list)))

(defun envrc--run-direnv (verb)
  "Run direnv command named by VERB, then refresh current env."
  (envrc--with-required-current-env env-dir
    (let* ((outbuf (get-buffer-create (format "*envrc-%s: %s*" verb env-dir)))
           (default-directory env-dir)
           (exit-code (process-file envrc-direnv-executable nil outbuf nil verb)))
      (if (zerop exit-code)
          (progn
            (kill-buffer outbuf)
            (envrc--get-current-env-or-run-direnv t))
        (display-buffer outbuf)
        (user-error "Error running direnv %s" verb)))))


;;; Commands for end users

(defun envrc-reload ()
  "Reload the current env."
  (interactive)
  (envrc--run-direnv "reload"))

(defun envrc-allow ()
  "Run \"direnv allow\" in the current env."
  (interactive)
  (envrc--run-direnv "allow"))

(defun envrc-deny ()
  "Run \"direnv deny\" in the current env."
  (interactive)
  (envrc--run-direnv "deny"))

(defun envrc-show-log ()
  "Open direnv log buffer for this `envrc-mode' buffer."
  (interactive)
  (envrc--with-required-current-env env-dir
    (if-let* ((buffer (get-buffer (envrc--direnv-buffer-name env-dir))))
        (pop-to-buffer buffer)
      (user-error "Envrc log buffer does not exist"))))


;;; Propagate local environment to commands that use temp buffers

(defun envrc-propagate-environment (orig &rest args)
  "Advice function to wrap a command ORIG and make it use our local env.
This can be used to force compliance where ORIG starts processes
in a temp buffer.  ARGS is as for ORIG."
  (if envrc-mode
      (inheritenv (apply orig args))
    (apply orig args)))

(defun envrc-propagate-tramp-environment (buf)
  "Advice function to propagate tramp vars into local values in BUF.
`tramp-remote-path' and `tramp-remote-process-environment' are propagated."
  (when envrc-mode
    (let ((cur-path envrc--remote-path)
          (cur-env tramp-remote-process-environment))
      (with-current-buffer buf
        (setq-local tramp-remote-process-environment cur-env)
        (setq-local envrc--remote-path cur-path))))
  buf)

(defun envrc-get-remote-path (fn vec)
  "Advice function to wrap FN (`tramp-get-remote-path') with its argument VEC.
Shortcuts tramp caching direnv sets the variable `exec-path'."
  (with-current-buffer (tramp-get-connection-buffer vec)
    (or envrc--remote-path
        (apply fn vec nil))))

(advice-add 'async-shell-command :around #'envrc-propagate-environment)
(advice-add 'shell-command :around #'envrc-propagate-environment)
(advice-add 'shell-command-to-string :around #'envrc-propagate-environment)
(advice-add 'dired-shell-command :around #'envrc-propagate-environment)
(advice-add 'org-babel-eval :around #'envrc-propagate-environment)
(advice-add 'org-export-file :around #'envrc-propagate-environment)
(advice-add 'vc-do-command :around #'envrc-propagate-environment)
(advice-add 'vc-call-backend :around #'envrc-propagate-environment)
(advice-add 'vc-dir :around #'envrc-propagate-environment)
(advice-add 'tramp-get-connection-buffer :filter-return #'envrc-propagate-tramp-environment)
(advice-add 'tramp-get-remote-path :around #'envrc-get-remote-path)


;;; Major mode for .envrc files

;; Generate direnv keywords with:
;;     $ rg "Usage:\s+([^_]\w+)" DIRENV_SRC/stdlib.sh -Nor '"$1"' | sort | uniq
(defvar envrc-file-extra-keywords
  '("MANPATH_add" "PATH_add" "PATH_rm" "direnv_apply_dump" "direnv_layout_dir"
    "direnv_load" "direnv_version" "dotenv" "dotenv_if_exists"
    "env_vars_required" "expand_path" "fetchurl" "find_up" "has" "join_args"
    "layout" "load_prefix" "log_error" "log_status" "on_git_branch" "path_add"
    "path_rm" "require_allowed" "rvm" "semver_search" "source_env" "source_env_if_exists"
    "source_up" "source_up_if_exists" "source_url" "strict_env" "unstrict_env"
    "use" "use_flake" "use_flox" "use_guix" "use_nix" "use_vim" "user_rel_path"
    "watch_dir" "watch_file")
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
