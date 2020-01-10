EMACS ?= emacs
CASK  ?= cask

# directories
SRC = .
TEST = ./test

# files
SRC_EL = $(wildcard $(SRC)/*.el)
TEST_EL = $(wildcard $(TEST)/*.el)
SRC_ELC = $(SRC_EL:.el=.elc)
TEST_ELC = $(TEST_EL:.el=.elc)

# tasks

.PHONY: init-dev
init-dev:
	$(CASK)

.PHONY: clean
clean:
	$(CASK) clean-elc
	-rm $(SRC_ELC) $(TEST_ELC)

$(SRC)/%.elc: $(SRC)/%.el
	$(CASK) exec $(EMACS) -Q -batch -L $(SRC) -f batch-byte-compile $<

# the only difference from SRC is including test directory to load-path.
$(TEST)/%.elc: $(TEST)/%.el
	$(CASK) exec $(EMACS) -Q -batch -L $(SRC) -L $(TEST) -f batch-byte-compile $<

.PHONY: compile
compile: ${SRC_ELC} ${TEST_ELC}

.PHONY: raw-test
raw-test: clean
	$(CASK) exec ert-runner $(TEST_EL)

.PHONY: compiled-test
compiled-test: compile
	$(CASK) exec ert-runner $(TEST_ELC)

# Pushing melpa is needed because package-lint refers package information.
# package-initialize is needed to call package-lint-batch-and-exit.
.PHONY: lint
lint:
	$(CASK) exec emacs \
	  -batch \
	  -eval "(progn \
	           (require 'package) \
	           (push '(\"melpa\" . \"https://melpa.org/packages/\") \
	                 package-archives) \
	           (package-initialize))" \
	  -f package-lint-batch-and-exit $(SRC_EL)

# "compile" is a checking for byte compilation warning.
.PHONY: test
test: raw-test compile compiled-test lint

# do not compile when using undercover.el
.PHONY: coverage
coverage: raw-test

.PHONY: precommit
precommit:
	$(CASK) pkg-file
	$(MAKE) test
