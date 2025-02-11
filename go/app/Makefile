SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.go)

DIRS=.
SRC=$(call map,get_source_in_directory,$(DIRS))
SRC=./app.go
BIN=$(basename $(SRC))

COMPILER=ghc
EXEC=runhaskell

default: help

.PHONY: install
install: ## Install packages
	go install $(SRC)

uninstall: ## Uninstall packages
	echo "Not implemented" && exit 1

.PHONY: compile
compile: $(BIN) ## Compile test script

$(BIN): $(SRC)
	go mod tidy
	go fmt
	go build $(SRC)

.PHONY: run
run: $(BIN) ## Run test script
	go run $(SRC)

godoc: ## Godoc
	godoc -notes=BUG,NOTE

hello: ## Run hello
	go run ./cmd/hello

.PHONY: test
test: ## Run test script
	go mod tidy
	go test -v -fullpath ./...

.PHONY:
debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo BIN=$(BIN)
	@echo EXEC=$(EXEC)
	@echo SRC=$(SRC)

.PHONY: clean
clean: ## Clean cache
	go clean 
	rm -f __debug* debug.test*

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: hello install uninstall compile run hello watch unwatch clean debug help
