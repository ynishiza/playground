SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

DIRS=.
SRC=$(call map,get_source_in_directory,$(DIRS))
SRC=script.hs
OBJ=$(SRC:.hs=.o)
BIN=$(basename $(SRC))

HELLOSRC=hello.hs
HELLOBIN=$(basename $(HELLOSRC))

COMPILER=ghc
EXEC=runhaskell

.PHONY: 
default: help

.PHONY: install
install: ## Install packages
	echo "Not implemented" && exit 1

.PHONY: uninstall
uninstall: ## Uninstall packages
	echo "Not implemented" && exit 1

.PHONY: hello
hello: ## Run hello
	$(COMPILER) $(HELLOSRC)
	./$(HELLOBIN)

.PHONY: compile
compile: $(BIN) ## Compile test script

$(BIN): $(SRC)
	# $(COMPILER) $(SRC)
	echo "Not implemented" && exit 1

.PHONY: run
run: ## Run test script
	# $(EXEC) $(SRC)
	echo "Not implemented" && exit 1

.PHONY: watch
watch: ## Watch for changes
	# watchman watch .
	# watchman -- trigger . build "*.tex" "*.cls" -- make compile
	echo "Not implemented" && exit 1

.PHONY: unwatch
unwatch: ## Clear watch
	# watchman watch-del .
	echo "Not implemented" && exit 1

.PHONY: debug
debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo BIN=$(BIN)
	@echo EXEC=$(EXEC)
	@echo SRC=$(SRC)
	@echo OBJ=$(OBJ)
	@echo HELLOBIN=$(HELLOBIN)
	@echo HELLOSRC=$(HELLOSRC)

.PHONY: clean
clean: ## Clean cache
	# rm -f $(BIN) $(OBJ) 
	echo "Not implemented" && exit 1

.PHONY: default
help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

