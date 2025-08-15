SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

DIRS=.
SRC=$(call map,get_source_in_directory,$(DIRS))


.PHONY: 
default: help

.PHONY: hello
hello: ## Run hello
	dot -Tpng ./hello.dot -o hello.png

.PHONY: build
build: ##
	dot -Tpng ./graph.dot -o graph.png 

.PHONY: build_svg
build_svg: ##
	dot -Tsvg ./graph.dot -o graph.svg 

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

