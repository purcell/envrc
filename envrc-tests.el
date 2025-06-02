;;; envrc-tests.el --- Test suite for envrc          -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords:

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

;; Just a few basic regression tests

;;; Code:

(require 'envrc)
(require 'ert)
(require 'cl-lib)

(defgroup envrc-tests nil "Envrc.el tests." :group 'test)

(setq envrc-debug t)



(defun envrc-tests--exec (&rest args)
  (when envrc-async-processing
    (sleep-for 0.1))
  (should (apply 'call-process envrc-direnv-executable nil nil nil args)))

(defmacro envrc-tests--with-extra-global-env-var (key val &rest body)
  "Temporarily set var KEY to VAL in the global `process-environment', while BODY is evaluated."
  (declare (indent 2))
  (let ((old-env (cl-gensym)))
    `(let ((,old-env (default-value 'process-environment)))
       (push (format "%s=%s" ,key ,val) (default-value 'process-environment))
       (unwind-protect
           (progn
             ,@body)
         (setq-default process-environment ,old-env)))))

(defmacro envrc-tests--with-temp-directory (var &rest body)
  "Create a temporary directory, bind it to VAR, make it current, and execute BODY."
  (declare (indent 1))
  (let ((passed (cl-gensym)))
    `(let* ((default-directory (make-temp-file "envrc" t))
            (envrc-debug t)
            ,passed
            (,var default-directory))
       (unwind-protect
           (progn
             (when (get-buffer "*envrc-debug*")
               (kill-buffer "*envrc-debug*"))
             ,@body
             (setq ,passed t))
         (unless ,passed
           (message "Debug output: %s"
                    (when (get-buffer "*envrc-debug*")
                      (with-current-buffer "*envrc-debug*" (buffer-string)))))))))

(ert-deftest envrc-no-op ()
  "When there's no .envrc, do nothing."
  (envrc-tests--with-temp-directory _
    (with-temp-buffer
      (envrc-mode 1)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (eq envrc--status 'none))
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (not (local-variable-p 'process-environment))))))



(ert-deftest envrc-direnv-is-available ()
  "Check the executable is executable!"
  (when envrc-async-processing
    (sleep-for 0.1))
  (should (executable-find envrc-direnv-executable)))

(ert-deftest envrc-no-op-unless-allowed ()
  "When the .envrc isn't allowed, do nothing."
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))
    (with-temp-buffer
      (envrc-mode 1)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (not (local-variable-p 'process-environment)))
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (eq envrc--status 'error)))))

(ert-deftest envrc-setting-propagates-when-mode-enabled ()
  "Pick up existing .envrc at mode startup."
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))

    (envrc-tests--exec "allow")

    (with-temp-buffer
      (envrc-mode 1)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (local-variable-p 'process-environment))
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal "BAR" (getenv "FOO")))
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (eq envrc--status 'on)))))

(ert-deftest envrc-setting-propagates-when-allowed ()
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))

    (with-temp-buffer
      (envrc-mode 1)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (not (local-variable-p 'process-environment)))
      (envrc-allow)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (local-variable-p 'process-environment))
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal "BAR" (getenv "FOO")))
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (eq envrc--status 'on)))))

(ert-deftest envrc-setting-removed-when-denied ()
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))
    (envrc-tests--exec "allow")

    (with-temp-buffer
      (envrc-mode 1)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (local-variable-p 'process-environment))
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal "BAR" (getenv "FOO")))
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (eq envrc--status 'on))
      (envrc-deny)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (not (local-variable-p 'process-environment)))
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (eq envrc--status 'error)))))

(ert-deftest envrc-reload-existing-buffer ()
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))

    (envrc-tests--exec "allow")

    (with-temp-buffer
      (envrc-mode 1)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal "BAR" (getenv "FOO")))
      (with-temp-file ".envrc"
        (insert "export FOO=BAZ"))
      (envrc-tests--exec "allow")
      (envrc-reload)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal "BAZ" (getenv "FOO"))))))

(ert-deftest envrc-masks-global-var-when-overridden ()
  (envrc-tests--with-extra-global-env-var "FOO" "BANANA"
    (envrc-tests--with-temp-directory _
      (with-temp-file ".envrc"
        (insert "export FOO=BAR"))

      (envrc-tests--exec "allow")

      (with-temp-buffer
        (when envrc-async-processing
          (sleep-for 0.1))
        (should (equal "BANANA" (getenv "FOO")))
        (envrc-mode 1)
        (when envrc-async-processing
          (sleep-for 0.1))
        (should (equal "BAR" (getenv "FOO")))))))

(ert-deftest envrc-state-shared-between-buffers-in-dir ()
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))

    (envrc-tests--exec "allow")

    (with-temp-buffer
      (envrc-mode 1)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (local-variable-p 'process-environment))
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal "BAR" (getenv "FOO")))

      (envrc-tests--exec "deny")

      (with-temp-buffer
        (envrc-mode 1)
        (when envrc-async-processing
          (sleep-for 0.1))
        (should (local-variable-p 'process-environment))
        (when envrc-async-processing
          (sleep-for 0.1))
        (should (equal "BAR" (getenv "FOO")))
        (envrc-reload)
        (when envrc-async-processing
          (sleep-for 0.1))
        (should (eq envrc--status 'error)))

      (when envrc-async-processing
        (sleep-for 0.1))
      (should (eq envrc--status 'error))
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (not (local-variable-p 'process-environment))))))

(ert-deftest envrc-remove-variable ()
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))

    (envrc-tests--exec "allow")

    (with-temp-buffer
      (envrc-mode 1)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal "BAR" (getenv "FOO")))
      (with-temp-file ".envrc"
        (insert ""))
      (envrc-allow)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal nil (getenv "FOO"))))))

(ert-deftest envrc-cache-is-refreshed-if-global-env-changes ()
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))

    (envrc-tests--exec "allow")

    (with-temp-buffer
      (envrc-mode 1)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal "BAR" (getenv "FOO")))
      (envrc-tests--with-extra-global-env-var (symbol-name (cl-gensym)) "blah"
        (with-temp-file ".envrc"
          (insert "export FOO=BAZ"))
        (envrc-tests--exec "allow")
        (with-temp-buffer
          ;; We expect a cache miss, and therefore a refresh
          (envrc--debug "buffer is %S" (current-buffer))
          (envrc-mode 1)
          (when envrc-async-processing
            (sleep-for 0.1))
          (should (local-variable-p 'process-environment))
          (should (equal "BAZ" (getenv "FOO"))))

        ;; TODO?
        ;; (should (local-variable-p 'process-environment))
        ;; (should (equal "BAZ" (getenv "FOO")))
        ))))

;; ;; Now requires a per-user config setting for direnv,
;; ;; so tests will fail by default.
;; (ert-deftest envrc-fall-back-to-env-files ()
;;   (envrc-tests--with-temp-directory _
;;     (with-temp-file ".env"
;;       (insert "FOO=BAR"))

;;     (envrc-tests--exec "allow")

;;     (with-temp-buffer
;;       (envrc-mode 1)
;;       (should (equal "BAR" (getenv "FOO"))))))
(require 'eshell)

(ert-deftest envrc-eshell-updates-environment-when-changing-directory ()
  (let ((current-dir default-directory))
    (eshell)
    (envrc-tests--with-temp-directory envrc-dir
      (with-temp-file ".envrc"
        (insert "export FOO=BAR"))

      (envrc-tests--exec "allow")

      ;; envrc mode is not activated
      (eshell/cd envrc-dir)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal nil (getenv "FOO")))

      ;; envrc mode is activated with option set to not update env on directory change
      (eshell/cd current-dir)
      (let ((envrc-update-on-eshell-directory-change nil))
        (envrc-mode 1))
      (eshell/cd envrc-dir)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal nil (getenv "FOO")))

      ;; envrc mode is activated and updates environment with default options
      (eshell/cd current-dir)
      (envrc-mode -1)
      (envrc-mode 1)
      (eshell/cd envrc-dir)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal "BAR" (getenv "FOO")))

      ;; environment is cleared when exiting directory
      (eshell/cd current-dir)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal nil (getenv "FOO")))

      ;; environment is cleared when envrc-mode is disabled
      (eshell/cd envrc-dir)
      (envrc-mode -1)
      (when envrc-async-processing
        (sleep-for 0.1))
      (should (equal nil (getenv "FOO"))))))

;; TODO:
;; - Setting exec-path and eshell-path-env


(provide 'envrc-tests)
;;; envrc-tests.el ends here
