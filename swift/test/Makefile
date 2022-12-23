SHELL=/usr/bin/env bash
# map=$(foreach value,$(2),$(call $(1),$(value)))
# get_source_in_directory=$(wildcard $(value)/*.hs)

default: help

install: ## Install packages
	make compile

uninstall: ## Uninstall packages
	echo "Not implemented" && exit 1

compile:  ## compile
	swift build

run: ## Run test script
	swift run
	swift test

debug: ## Print variables
	@echo SHELL=$(SHELL)

clean: ## Clean cache
	rm -rf .build

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: install uninstall compile run clean debug help
