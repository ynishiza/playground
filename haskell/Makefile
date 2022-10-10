SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

DIRS=.
SRC=$(call map,get_source_in_directory,$(DIRS))
OBJ=$(SRC:.hs=.o)

COMPILER=ghc
EXEC=runhaskell
BIN=script
TESTSCRIPT=script.hs

default: help

install: ## Install packages
	cabal update
	cabal install random

compile: $(BIN)

$(BIN): $(SRC)
	$(COMPILER) $(SRC)

run: 
	$(EXEC) $(TESTSCRIPT)

test: run

# watch: ## Watch for changes
# 	watchman watch .
# 	watchman -- trigger . build "*.tex" "*.cls" -- make compile

# unwatch: ## Clear watch
# 	watchman watch-del .
#
debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo SRC=$(SRC)
	@echo OBJ=$(OBJ)

clean: ## Clean
	rm -f $(BIN) $(OBJ) 

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: debug run clean watch unwatch compile help
