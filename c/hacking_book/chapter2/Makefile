SHELL=/usr/bin/env bash
BASENAMES=char_array\
stack_example\
test_stdin\
time_example2

BIN=$(addsuffix .out,$(BASENAMES))
SRC=$(addsuffix .c,$(BASENAMES))

# case: macos
# macos native gcc does not work
# Use gcc-8 installed by
#   $ brew install gcc@8
GCC8=$(shell which gcc-8)

# Implicit rule variables for *.c files
# Ref: https://www.gnu.org/software/make/manual/html_node/Catalogue-of-Rules.html#Catalogue-of-Rules
CC=gcc
ifneq (, $(GCC8))
	CC=$(GCC8)
endif

default: help

%.out : %.c
	$(CC) $< -o $@

compile: $(BIN) ## Compile

print: ## debug
	@echo BIN=$(BIN)
	@echo SRC=$(SRC)

clean:
	rm -f $(BIN) $(BASENAMES)

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: help clean compile hello debug
