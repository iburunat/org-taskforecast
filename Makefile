EMACS ?= emacs
MAKEM = ./makem.sh/makem.sh

# directories
SRC = .
TEST = ./test

# files
SRC_EL = $(wildcard $(SRC)/*.el)
TEST_EL = $(wildcard $(TEST)/*.el)
SRC_ELC = $(SRC_EL:.el=.elc)
TEST_ELC = $(TEST_EL:.el=.elc)

# tasks

.PHONY: default
default: test

.PHONY: init
init:
	git submodule update --init

.PHONY: clean
clean:
	-rm $(SRC_ELC) $(TEST_ELC)

.PHONY: compile
compile:
	$(MAKEM) compile --emacs $(EMACS) --verbose

.PHONY: test
test:
	$(MAKEM) all --emacs $(EMACS) --verbose

.PHONY: test-sandbox
test-sandbox:
	$(MAKEM) all --sandbox --install-deps --install-linters --emacs $(EMACS) --verbose
