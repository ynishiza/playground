SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

DIRS=.

COMPILER=ghc
VPATH=.:lib
EXEC=runhaskell
MAINSRC=script.hs
SRC=$(wildcard ./lib/*.hs) $(MAINSRC)
BIN=main
COMPONENTMAIN=playground:exe:main

HELLOSRC=hellomain.hs
HELLOBIN=$(HELLOSRC:.hs=.out)

default: help

install: ## Install 
	stack build

uninstall: ## Uninstall
	stack purge

compile: ## Compile
	stack build $(COMPONENTMAIN)

compile-profile: ## Compile with profiler
	stack build --profile $(COMPONENTMAIN)

hello: ## Run hello
	stack runhaskell $(HELLOSRC)

run:  ## Run test
	stack runhaskell $(MAINSRC)

runAll: ## Run all tests
	yes | make run

watch: ## Watch for changes
	watchman watch .
	watchman -- trigger . build "*.hs" "lib/*.hs" -- make compile

unwatch: ## Clear watch
	watchman watch-del .

debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo MAINSRC=$(MAINSRC)
	@echo SRC=$(SRC)
	@echo OBJ=$(OBJ)
	@echo HELLOBIN=$(HELLOBIN)
	@echo HELLOSRC=$(HELLOSRC)

clean: ## Clean
	stack clean
	rm -f $(BIN) $(OBJ) $(HELLOBIN) *.hi *.o

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: hello debug run runAll clean watch unwatch compile help
