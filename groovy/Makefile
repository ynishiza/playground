SHELL=/usr/bin/env bash

EXEC=groovy
TESTSCRIPT=test.groovy

default: help

run: ## Run test script
	$(EXEC) $(TESTSCRIPT)

test: run

debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo SRC=$(EXEC)

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: install uninstall compile run test watch unwatch clean debug help
