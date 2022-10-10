SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

DIRS=.
SRC=$(call map,get_source_in_directory,$(DIRS))
OBJ=$(SRC:.hs=.o)

COMPILER=ghc
EXEC=runhaskell
TESTSCRIPT=script.hs
BIN=$(basename $(TESTSCRIPT))

HELLOSCRIPT=hello.hs
HELLOBIN=$(basename $(HELLOSCRIPT))

default: help

install: ## Install packages
	echo "Not implemented" && exit 1

uninstall: ## Uninstall packages
	echo "Not implemented" && exit 1

hello: ## Run hello
	$(COMPILER) $(HELLOSCRIPT)
	./$(HELLOBIN)

compile: $(BIN) ## Compile test script

$(BIN): $(SRC)
	# $(COMPILER) $(SRC)
	echo "Not implemented" && exit 1

run: ## Run test script
	# $(EXEC) $(TESTSCRIPT)
	echo "Not implemented" && exit 1

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
	@echo HELLOSCRIPT=$(HELLOSCRIPT)

clean: ## Clean cache
	# rm -f $(BIN) $(OBJ) 
	echo "Not implemented" && exit 1

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: hello install uninstall compile run hello watch unwatch clean debug help
