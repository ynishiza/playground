SHELL=/usr/local/bin/bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

DIRS=.
SRC=$(call map,get_source_in_directory,$(DIRS))

COMPILER=ghc
EXEC=runhaskell
BIN=script
TESTSCRIPT=script.hs
OBJ=$(SRC:.hs=.o)

default: help

install: ## Installs
	# echo "Not implemented" && exit 1

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

clean: ## Clean
	rm -f $(BIN) $(OBJ) 

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: run clean watch unwatch compile help
