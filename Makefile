# commands
EMACS ?= emacs
MAKEM = ./makem.sh/makem.sh
# https://github.com/ekalinin/github-markdown-toc
GH_MD_TOC ?= gh-md-toc

# directories
SRC = .
TEST = ./test
SANDBOX = ./sandbox

# files
SRC_EL = $(wildcard $(SRC)/*.el)
TEST_EL = $(wildcard $(TEST)/*.el)
SRC_ELC = $(SRC_EL:.el=.elc)
TEST_ELC = $(TEST_EL:.el=.elc)

# makem options
MAKEM_LINT = lint-compile lint-declare lint-indent lint-package lint-regexps
MAKEM_TEST = tests
MAKEM_SANDBOX =

# tasks

.PHONY: default
default: test

.PHONY: init
init:
	git submodule update --init

.PHONY: clean
clean:
	-rm $(SRC_ELC) $(TEST_ELC)
	-rm -r $(SANDBOX)

.PHONY: compile
compile:
	$(MAKEM) compile --emacs $(EMACS) --verbose

.PHONY: test
test:
# lint-checkdoc may report false positive errors and warnings.
# run lint-checkdoc as advice.
	-$(MAKEM) lint-checkdoc --emacs $(EMACS) --verbose $(MAKEM_SANDBOX)
	$(MAKEM) $(MAKEM_LINT) $(MAKEM_TEST) --emacs $(EMACS) --verbose $(MAKEM_SANDBOX) --install-deps --install-linters

.PHONY: test-sandboxed
test-sandboxed:
	$(MAKE) test MAKEM_SANDBOX="--sandbox=$(SANDBOX)"

.PHONY: readme-toc
readme-toc:
	$(GH_MD_TOC) --insert README.md
# remove timestamp comment
	sed -i '/^<!-- Added by:.* -->/d' README.md
# remove backups
	rm README.md.orig.*
	rm README.md.toc.*
