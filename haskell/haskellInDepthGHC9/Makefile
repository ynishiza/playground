SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

COMPILER=ghc
EXEC=runhaskell

PROJECTNAME=haskellInDepthGHC9
MAINEXE_NAME=main
MAINEXE=$(PROJECTNAME):exe:$(MAINEXE_NAME)
MAINTEST=$(PROJECTNAME):test:main-test

default: help

install: ## Install 
	stack build

uninstall: ## Uninstall
	make clean
	stack purge

compile: ## Compile
	stack build 

compile-profile: ## Compile with profiler
	stack build --profile $(MAINEXE)

run:  ## Run test
	@echo "Chapter 1"
	echo 1 $$'\n' | stack exec -- $(MAINEXE_NAME) ./src/data/hamlet.txt 20
	echo 1 $$'\n'| stack exec -- $(MAINEXE_NAME) ./src/data/forsyte.txt 20

	@echo "Chapter 2"
	@echo 2 $$'\n' | stack exec -- $(MAINEXE_NAME) -r ./src/data/rotations1.txt North
	@echo 2 $$'\n' | stack exec -- $(MAINEXE_NAME) -o ./src/data/directions1.txt 

	echo "Chapter 5"
	@echo 3 $$'\n' | stack exec -- $(MAINEXE_NAME) 

	echo "Chapter 6"
	@echo 4 $$'\n' | stack exec -- $(MAINEXE_NAME) 

	@echo "Chapter 6 - du"
	@make run-du

	@echo "Chapter 7"
	@echo 5 $$'\n' | stack exec -- $(MAINEXE_NAME) 
	@echo 6 $$'\n' | stack exec -- $(MAINEXE_NAME) 

	@echo "Chapter 7 - suntimes"
	@make run-suntimes

	@echo "Chapter 7 - iplookup"
	@make run-iplookup

	@echo "Chapter 9"
	@echo 7 $$'\n' | stack exec -- $(MAINEXE_NAME) 

	@echo "Chapter 9 - isprime"
	@make run-isprime

	@echo "Chapter 11"
	@echo 8 $$'\n' | stack exec -- $(MAINEXE_NAME) 

	@echo "Chapter 12"
	@echo 9 $$'\n' | stack exec -- $(MAINEXE_NAME) 

	@echo "Chapter 13"
	@echo 10 $$'\n' | stack exec -- $(MAINEXE_NAME) 
	
	@make run-singletons
	@make run-scratch
	@make test
	@make benchmark

run-singletons: ## Singletons tutorial https://blog.jle.im/entries/series/+introduction-to-singletons.html
	@echo "Singletons tutorial https://blog.jle.im/entries/series/+introduction-to-singletons.html"
	@stack exec -- singletonsPart3
	@stack exec -- singletonsPart4

run-du: ## du
	@stack exec -- du test -d 3 -e ".hs" -L
	@stack exec -- du test --depth 3 --extension ".hs" -L

run-suntimes: ## suntimes
	@echo "Chapter 7"
	@stack exec -- suntimes -f ./src/data/suntimes/input.txt -c ./src/data/suntimes/config.json

run-iplookup: ## iplookup
	@echo "Chapter 8"
	@stack exec -- iplookup -d benchmark/data/iplookup/3.iprs -i 1.1.1.1,2.2.2.2 
	@stack exec -- iplookup -d benchmark/data/iplookup/3.iprs -n 500000

run-isprime: ## isprime
	@echo "Chapter 9"
	@stack exec -- isprime 7

run-scratch: ## Run scratch space
	@stack build $(PROJECTNAME):exe:scratch
	@stack exec -- scratch

run-profile:
	stack exec --profile -- $(MAINEXE_NAME) +RTS -p

benchmark: ## Run benchmark
	@stack bench

test: ## Run tests
	@echo "Tests"
	@stack test 

ide: ## Check IDE setup
	haskell-language-server-wrapper ./src/main/main.hs
	haskell-language-server-wrapper ./test/main/*.hs
	haskell-language-server-wrapper ./src/rpc/*.hs
	haskell-language-server-wrapper ./test/rpc/*.hs
	haskell-language-server-wrapper ./src/restapi/*.hs
	haskell-language-server-wrapper ./test/restapi/*.hs
	haskell-language-server-wrapper ./src/dependentTypes/*.hs
	haskell-language-server-wrapper ./test/dependentTypes/*.hs
	haskell-language-server-wrapper ./src/radarSafe/*.hs
	haskell-language-server-wrapper ./src/singletons/part3/*.hs
	haskell-language-server-wrapper ./src/singletons/part4/*.hs

debug: ## Print variables
	@echo SHELL=$(SHELL)

clean: ## Clean
	stack clean
	rm -f *.hi *.o *.cabal *.png *.svg *.html
	find src test benchmark \
		-iname '*.o' \
		-o -iname '*.hi' \
		-o -iname '*.hy' \
		-o -iname '*dump-*' \
		-delete

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: default install uninstall compile compile-profile run  run-profile run-singletons benchmark test ide debug clean help
