EMACS ?= emacs

# A space-separated list of required package names
DEPS = seq inheritenv

INIT_PACKAGES := "(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(PACKAGES)) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
        (package-refresh-contents)) \
      (package-install pkg))) \
  )"

all: compile test package-lint clean-elc

test-sync: SYNC_MODE = --eval "(setq envrc-async-processing nil)"

test: test-sync test-async

test-%:
	${EMACS} -Q --eval $(subst PACKAGES,${DEPS},${INIT_PACKAGES}) ${SYNC_MODE} -batch -l envrc.el -l envrc-tests.el -f ert-run-tests-batch-and-exit

package-lint:
	${EMACS} -Q --eval $(subst PACKAGES,package-lint,${INIT_PACKAGES}) -batch -f package-lint-batch-and-exit envrc.el

compile: clean-elc
	${EMACS} -Q --eval $(subst PACKAGES,${DEPS},${INIT_PACKAGES}) -L . -batch -f batch-byte-compile *.el

clean-elc:
	rm -f f.elc

.PHONY:	all compile clean-elc package-lint
