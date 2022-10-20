SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

DIRS=.

COMPILER=ghc
OPTIONS=-XRecursiveDo -XTypeSynonymInstances -XFlexibleInstances
EXEC=runhaskell
SRC=script.hs
OBJ=$(SRC:.hs=.o)
BIN=$(SRC:.hs=.out)

HELLOSRC=hellomain.hs
HELLOBIN=$(HELLOSRC:.hs=.out)

default: help

install: ## Install packages
	cabal update
	cabal install random

compile: $(BIN)

$(BIN): $(SRC)
	$(COMPILER) $(OPTIONS) $(SRC) -o $(BIN)

hello: ## Run hello
	$(EXEC) $(OPTIONS) $(HELLOSRC)

run: ## Run test
	$(EXEC) $(OPTIONS) $(SRC)

runAll: ## Run all tests
	yes | make run

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
	@echo HELLOBIN=$(HELLOBIN)
	@echo HELLOSRC=$(HELLOSRC)

clean: ## Clean
	rm -f $(BIN) $(OBJ) $(HELLOBIN) *.hi *.o

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: hello debug run runAll clean watch unwatch compile help
