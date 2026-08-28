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
  (should (zerop (apply 'call-process envrc-direnv-executable nil nil nil args))))

(defmacro envrc-tests--with-extra-global-env-var (key val &rest body)
  "Evaluate BODY with var KEY set to VAL in the global `process-environment'."
  (declare (indent 2))
  (let ((old-env (gensym)))
    `(let ((,old-env (default-value 'process-environment)))
       (push (format "%s=%s" ,key ,val) (default-value 'process-environment))
       (unwind-protect
           (progn
             ,@body)
         (setq-default process-environment ,old-env)))))

(defmacro envrc-tests--with-temp-directory (var &rest body)
  "Create a temporary directory, bind it to VAR, make it current, and execute BODY."
  (declare (indent 1))
  (let ((passed (gensym)))
    `(let* ((default-directory (make-temp-file "envrc" t))
            (envrc-global-mode nil)
            (envrc-async nil)
            (envrc-debug t)
            ,passed
            (,var default-directory))
       (unwind-protect
           (progn
             (when-let* ((buf (get-buffer "*envrc-debug*")))
               (kill-buffer buf))
             ,@body
             (setq ,passed t))
         (when-let* ((buf (get-buffer (envrc--direnv-buffer-name (file-name-as-directory default-directory)))))
           (kill-buffer buf))
         (unless ,passed
           (message "Debug output: %s"
                    (when (get-buffer "*envrc-debug*")
                      (with-current-buffer "*envrc-debug*" (buffer-string)))))))))

(ert-deftest envrc-no-op ()
  "When there's no .envrc, do nothing."
  (envrc-tests--with-temp-directory _
    (with-temp-buffer
      (envrc-mode 1)
      (should (eq envrc--status 'none))
      (should (not (local-variable-p 'process-environment))))))



(ert-deftest envrc-direnv-is-available ()
  "Check the executable is executable!"
  (should (executable-find envrc-direnv-executable)))

(ert-deftest envrc-no-op-unless-allowed ()
  "When the .envrc isn't allowed, do nothing."
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))
    (with-temp-buffer
      (envrc-mode 1)
      (should (not (local-variable-p 'process-environment)))
      (should (eq envrc--status 'denied)))))

(ert-deftest envrc-setting-propagates-when-mode-enabled ()
  "Pick up existing .envrc at mode startup."
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))

    (envrc-tests--exec "allow")

    (with-temp-buffer
      (envrc-mode 1)
      (should (local-variable-p 'process-environment))
      (should (equal "BAR" (getenv "FOO")))
      (should (eq envrc--status 'on)))))

(ert-deftest envrc-setting-propagates-when-allowed ()
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))

    (with-temp-buffer
      (envrc-mode 1)
      (should (not (local-variable-p 'process-environment)))
      (envrc-allow)
      (should (local-variable-p 'process-environment))
      (should (equal "BAR" (getenv "FOO")))
      (should (eq envrc--status 'on)))))

(ert-deftest envrc-setting-removed-when-denied ()
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))
    (envrc-tests--exec "allow")

    (with-temp-buffer
      (envrc-mode 1)
      (should (local-variable-p 'process-environment))
      (should (equal "BAR" (getenv "FOO")))
      (should (eq envrc--status 'on))
      (envrc-deny)
      (should (not (local-variable-p 'process-environment)))
      (should (eq envrc--status 'denied)))))

(ert-deftest envrc-reload-existing-buffer ()
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))

    (envrc-tests--exec "allow")

    (with-temp-buffer
      (envrc-mode 1)
      (should (equal "BAR" (getenv "FOO")))
      (with-temp-file ".envrc"
        (insert "export FOO=BAZ"))
      (envrc-tests--exec "allow")
      (envrc-reload)
      (should (equal "BAZ" (getenv "FOO"))))))

(ert-deftest envrc-masks-global-var-when-overridden ()
  (envrc-tests--with-extra-global-env-var "FOO" "BANANA"
    (envrc-tests--with-temp-directory _
      (with-temp-file ".envrc"
        (insert "export FOO=BAR"))

      (envrc-tests--exec "allow")

      (with-temp-buffer
        (should (equal "BANANA" (getenv "FOO")))
        (envrc-mode 1)
        (should (equal "BAR" (getenv "FOO")))))))

(ert-deftest envrc-state-shared-between-buffers-in-dir ()
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))

    (envrc-tests--exec "allow")

    (with-temp-buffer
      (envrc-mode 1)
      (should (local-variable-p 'process-environment))
      (should (equal "BAR" (getenv "FOO")))

      (envrc-tests--exec "deny")

      (with-temp-buffer
        (envrc-mode 1)
        (should (local-variable-p 'process-environment))
        (should (equal "BAR" (getenv "FOO")))
        (envrc-reload)
        (should (eq envrc--status 'denied)))

      (should (eq envrc--status 'denied))
      (should (not (local-variable-p 'process-environment))))))

(ert-deftest envrc-remove-variable ()
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))

    (envrc-tests--exec "allow")

    (with-temp-buffer
      (envrc-mode 1)
      (should (equal "BAR" (getenv "FOO")))
      (with-temp-file ".envrc"
        (insert ""))
      (envrc-allow)
      (should (equal nil (getenv "FOO"))))))

(ert-deftest envrc-cache-is-refreshed-if-global-env-changes ()
  (envrc-tests--with-temp-directory _
    (with-temp-file ".envrc"
      (insert "export FOO=BAR"))

    (envrc-tests--exec "allow")

    (with-temp-buffer
      (envrc-mode 1)
      (should (equal "BAR" (getenv "FOO")))
      (envrc-tests--with-extra-global-env-var (symbol-name (gensym)) "blah"
        (with-temp-file ".envrc"
          (insert "export FOO=BAZ"))
        (envrc-tests--exec "allow")
        (with-temp-buffer
          ;; We expect a cache miss, and therefore a refresh
          (envrc-mode 1)
          (should (equal 'on envrc--status))
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


(ert-deftest envrc-eshell-when-changing-directory ()
  (let* ((non-env-dir (make-temp-file "envrc" t))
         (default-directory non-env-dir))
    (with-temp-buffer
      (let ((eshell-buffer-name (buffer-name))
            (envrc-update-on-eshell-directory-change t))
        (eshell)
        (envrc-mode 1)
        (should (equal nil (getenv "FOO")))
        (envrc-tests--with-temp-directory envrc-dir
          (with-temp-file ".envrc"
            (insert "export FOO=BAR"))

          (envrc-tests--exec "allow")

          (eshell/cd envrc-dir)
          (should (equal "BAR" (getenv "FOO")))

          (eshell/cd non-env-dir)
          (should (equal nil (getenv "FOO")))

          ;; environment is cleared when envrc-mode is disabled
          (eshell/cd envrc-dir)
          (envrc-mode -1)
          (should (equal nil (getenv "FOO"))))))))


(ert-deftest envrc-eshell-when-ignoring-changing-directory ()
  (let* ((non-env-dir (make-temp-file "envrc" t))
         (default-directory non-env-dir))
    (with-temp-buffer
      (let ((eshell-buffer-name (buffer-name))
            (envrc-update-on-eshell-directory-change nil))
        (eshell)
        (envrc-mode 1)
        (should (equal nil (getenv "FOO")))
        (envrc-tests--with-temp-directory envrc-dir
          (with-temp-file ".envrc"
            (insert "export FOO=BAR"))

          (envrc-tests--exec "allow")

          (eshell/cd envrc-dir)
          (should (equal nil (getenv "FOO")))

          (eshell/cd non-env-dir)
          (should (equal nil (getenv "FOO")))
          )))))


(ert-deftest envrc-async-has-delayed-effect ()
  (envrc-tests--with-temp-directory _
    (let ((envrc-async t))
      (with-temp-file ".envrc"
        (insert "sleep 1\n")
        (insert "export FOO=BAR\n"))
      (envrc-tests--exec "allow")

      (with-temp-buffer
        (envrc-mode 1)
        ;; No immediate effect
        (should (not (local-variable-p 'process-environment)))
        (should (equal nil (getenv "FOO")))
        (should (equal envrc--status 'none))
        (should envrc--running)
        (sleep-for 1.1)
        (should (local-variable-p 'process-environment))
        (should (equal "BAR" (getenv "FOO")))
        (should (eq envrc--status 'on))
        (should (not envrc--running))))))

(ert-deftest envrc-async-resolution-updates-all-buffers ()
  (envrc-tests--with-temp-directory _
    (let ((envrc-async t))
      (with-temp-file ".envrc"
        (insert "sleep 1\n")
        (insert "export FOO=BAR\n"))
      (envrc-tests--exec "allow")

      (let (buf1)
        (with-temp-buffer
          (setq buf1 (current-buffer))
          (envrc-mode 1)
          (should (not (local-variable-p 'process-environment)))
          (should (equal nil (getenv "FOO")))
          (should (equal envrc--status 'none))
          (should envrc--running)

          (with-temp-buffer
            (envrc-mode 1)
            (should (not (local-variable-p 'process-environment)))
            (should (equal nil (getenv "FOO")))
            (should (equal envrc--status 'none))
            (should envrc--running)

            (sleep-for 1.1)
            (should (local-variable-p 'process-environment))
            (should (equal "BAR" (getenv "FOO")))
            (should (eq envrc--status 'on))
            (should (not envrc--running))

            (with-current-buffer buf1
              (should (local-variable-p 'process-environment))
              (should (equal "BAR" (getenv "FOO")))
              (should (eq envrc--status 'on))
              (should (not envrc--running)))))))))


;; TODO:
;; - Setting exec-path and eshell-path-env


(provide 'envrc-tests)
;;; envrc-tests.el ends here
