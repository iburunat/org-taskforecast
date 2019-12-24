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
.PHONY: init-dev clean compile raw-test compiled-test test coverage precommit

init-dev:
	$(CASK)

clean:
	$(CASK) clean-elc
	-rm $(SRC_ELC) $(TEST_ELC)

$(SRC)/%.elc: $(SRC)/%.el
	$(CASK) exec $(EMACS) -Q -batch -L $(SRC) -f batch-byte-compile $<

# the only difference from SRC is including test directory to load-path.
$(TEST)/%.elc: $(TEST)/%.el
	$(CASK) exec $(EMACS) -Q -batch -L $(SRC) -L $(TEST) -f batch-byte-compile $<

compile: ${SRC_ELC} ${TEST_ELC}

raw-test: clean
	$(CASK) exec ert-runner $(TEST_EL)

compiled-test: compile
	$(CASK) exec ert-runner $(TEST_ELC)

# "compile" is a checking for byte compilation warning.
test: raw-test compile compiled-test

# do not compile when using undercover.el
coverage: raw-test

precommit:
	$(CASK) pkg-file
	$(MAKE) test
