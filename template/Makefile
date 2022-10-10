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
	echo "Not implemented" && exit 1

uninstall: ## Uninstall packages
	echo "Not implemented" && exit 1

compile: $(BIN) ## Compile test script

$(BIN): $(SRC)
	# $(COMPILER) $(SRC)
	echo "Not implemented" && exit 1

run: ## Run test script
	# $(EXEC) $(TESTSCRIPT)
	echo "Not implemented" && exit 1

test: run

watch: ## Watch for changes
	# watchman watch .
	# watchman -- trigger . build "*.tex" "*.cls" -- make compile
	echo "Not implemented" && exit 1

unwatch: ## Clear watch
	# watchman watch-del .
	echo "Not implemented" && exit 1

debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo SRC=$(SRC)
	@echo OBJ=$(OBJ)

clean: ## Clean cache
	# rm -f $(BIN) $(OBJ) 
	echo "Not implemented" && exit 1

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: install uninstall compile run test watch unwatch clean debug help
