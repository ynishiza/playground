SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

COMPILER=ghc
EXEC=runhaskell

PROJECTNAME=haskellInDepth
MAINEXE_NAME=main
MAINEXE=$(PROJECTNAME):exe:$(MAINEXE_NAME)
MAINTEST=$(PROJECTNAME):test:main-test

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
	stack build --profile $(MAINEXE)

run:  ## Run test
	@echo "Chapter 1"
	echo 1 $$'\n' | stack exec -- $(MAINEXE_NAME) ./src/data/hamlet.txt 20
	echo 1 $$'\n'| stack exec -- $(MAINEXE_NAME) ./src/data/forsyte.txt 20

	@echo "Chapter 2"
	@echo 2 $$'\n' | stack exec -- $(MAINEXE_NAME) -r ./src/data/rotations1.txt North
	@echo 2 $$'\n' | stack exec -- $(MAINEXE_NAME) -o ./src/data/directions1.txt 
	@echo 2 $$'\n' | stack exec -- stockquotes ./src/data/quotes.csv -n "My Company" -H "./stockreport.html"

	echo "Chapter 5"
	@echo 3 $$'\n' | stack exec -- $(MAINEXE_NAME) 

	@echo "Tests"
	@stack test -- $(MAINTEST)

debug: ## Print variables
	@echo SHELL=$(SHELL)

clean: ## Clean
	stack clean
	rm -f *.hi *.o *.cabal *.png *.svg *.html

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: hello debug run clean watch unwatch compile help
