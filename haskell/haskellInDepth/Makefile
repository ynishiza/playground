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

	echo "Chapter 6"
	@stack exec -- du test -d 3 -e ".hs" -L
	@stack exec -- du test --depth 3 --extension ".hs" -L

	@echo "Chapter 7"
	@stack exec -- suntimes -f ./src/data/suntimes/input.txt -c ./src/data/suntimes/config.json

	@echo "Chapter 8"
	@stack exec -- iplookup -d benchmark/data/iplookup/3.iprs -i 1.1.1.1,2.2.2.2 
	@stack exec -- iplookup -d benchmark/data/iplookup/3.iprs -n 500000

	@echo "Chapter 9"
	@stack exec -- isprime 7
	
	@make test

	@make benchmark

benchmark: ## Run benchmark
	@stack bench

test: ## Run tests
	@echo "Tests"
	@stack test 

ide: ## Check IDE setup
	haskell-language-server-wrapper ./lib/*.hs
	# haskell-language-server-wrapper ./src/stockquotes/*.hs
	haskell-language-server-wrapper ./src/du/*.hs
	haskell-language-server-wrapper ./test/du/*.hs
	haskell-language-server-wrapper ./src/ip/lookup/*.hs
	haskell-language-server-wrapper ./test/iplookup/*.hs
	haskell-language-server-wrapper ./src/isprime/*.hs

debug: ## Print variables
	@echo SHELL=$(SHELL)

clean: ## Clean
	stack clean
	rm -f *.hi *.o *.cabal *.png *.svg *.html
	find src test benchmark \
		-iname '*.o' \
		-o -iname '*.hi' \
		-o -iname '*dump-*' \
		-delete

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: default install uninstall compile compile-profile run  run-profile benchmark test ide debug clean help
