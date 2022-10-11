SHELL=/usr/bin/env bash

BUILDDIR=build
ENTRY=src/main.ts
BIN=$(BUILDDIR)/main.js
HELLOSCRIPT=src/hello.ts
HELLOBIN=$(BUILDDIR)/hello.js

COMPILER=npx tsc
EXEC=node
COMPILEFLAGS=--outDir $(BUILDDIR) --sourceMap --esModuleInterop

default: help

install: ## Install packages
	npm ci

uninstall: ## Uninstall packages
	rm -rf ./node_modules

hello: ## Run hello
	$(COMPILER) $(COMPILEFLAGS) $(HELLOSCRIPT)
	$(EXEC) $(HELLOBIN)

compile:  ## Compile test script
	$(COMPILER) $(COMPILEFLAGS) $(ENTRY)

run: ## Run test script
	[[ ! -e $(BIN) ]] && make compile; \
	$(EXEC) $(BIN)

watch: ## Watch for changes
	$(COMPILER) $(COMPILEFLAGS) --watch $(ENTRY)

debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo COMPILER=$(COMPILER)
	@echo BIN=$(BIN)

clean: ## Clean cache
	rm -r ./$(BUILDDIR)

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: hello install uninstall compile run hello watch unwatch clean debug help
