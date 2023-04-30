SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

COMPILER=ghc
EXEC=runhaskell

PROJECTNAME=haskellInDepth

HELLOSRC=hellomain.hs
HELLOBIN=$(HELLOSRC:.hs=.out)

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
	@echo "Chapter 2"
	@stack exec -- stockquotes ./src/data/quotes.csv -n "My Company" -H "./stockreport.html"

ide: ## Check IDE setup
	haskell-language-server-wrapper ./lib/*.hs
	haskell-language-server-wrapper ./src/stockquotes/*.hs

debug: ## Print variables
	@echo SHELL=$(SHELL)

clean: ## Clean
	stack clean
	rm -f *.hi *.o *.cabal *.png *.svg *.html
	find src test \
		-iname '*.o' \
		-o -iname '*.hi' \
		-o -iname '*dump-*' \
		-delete

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: default install uninstall compile compile-profile run  run-profile benchmark test ide debug clean help
