SHELL=/usr/bin/env bash

PROJECT=myproject
APP1=app1
APP2=app2

default: help

install: ## Install 
	stack build

uninstall: ## Uninstall
	make clean
	stack purge

compile: ## Compile
	stack build 

compile-profile: ## Compile with profiler
	stack build --profile

run:  ## Run test
	stack $(EXECOPTS) exec -- $(APP1) $(APPOPTS)
	stack $(EXECOPTS) exec -- $(APP2) $(APPOPTS)

run-profile: ## Run with profiler
	make EXECOPTS=--profile APPOPTS="+RTS -xc" run
	# stack --profile exec -- main +RTS -xc

test:
	stack test $(PROJECT):test:$(APP1)-test
	stack test $(PROJECT):test:$(APP2)-test

debug: ## Print variables
	@echo SHELL=$(SHELL)

ide: ## Test IDE settings
	stack ide targets
	haskell-language-server-wrapper ./lib/*.hs
	haskell-language-server-wrapper ./src/app1/*.hs
	haskell-language-server-wrapper ./src/app2/*.hs
	haskell-language-server-wrapper ./test/app1/*.hs
	haskell-language-server-wrapper ./test/app2/*.hs

clean: ## Clean
	stack clean
	rm -f *.hi *.o *.cabal *.png *.svg *.html

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: compile compile-profile ide run run-profile test ide debug clean help
