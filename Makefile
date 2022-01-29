EMACS ?= emacs

# A space-separated list of required package names
NEEDED_PACKAGES = package-lint seq inheritenv

INIT_PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
        (package-refresh-contents)) \
      (package-install pkg))) \
  )"

all: compile test package-lint clean-elc

test:
	${EMACS} -Q --eval ${INIT_PACKAGES} -batch -l envrc.el -l envrc-tests.el --eval "(ert t)"

package-lint:
	${EMACS} -Q --eval ${INIT_PACKAGES} -batch -f package-lint-batch-and-exit envrc.el

compile: clean-elc
	${EMACS} -Q --eval ${INIT_PACKAGES} -L . -batch -f batch-byte-compile *.el

clean-elc:
	rm -f f.elc

.PHONY:	all compile clean-elc package-lint
