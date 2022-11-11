SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

SRC=$(call map,get_source_in_directory,./app ./app/** ./lib ./lib/**) 
OBJ=$(SRC:.hs=.o)

PROJECT=playground
COMPONENTMAIN=$(PROJECT):exe:main
HIDMAIN=$(PROJECT):exe:hid
HIDTEST=$(PROJECT):test:hid-test
BIN=main

HELLOSRC=hellomain.hs
HELLOBIN=$(HELLOSRC:.hs=.out)

default: help

install: ## Install 
	stack build

uninstall: ## Uninstall
	make clean
	stack purge

compile: ## Compile
	stack build $(COMPONENTMAIN)

compile-hid: ## Compile hid
	stack build $(HIDMAIN)

compile-profile: ## Compile with profiler
	stack build --profile $(COMPONENTMAIN)

hello: ## Run hello
	stack runhaskell $(HELLOSRC)

run:  ## Run test
	stack exec main

run-profile:
	stack --profile exec -- main +RTS -xc

runAll: ## Run all tests
	yes | make run

debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo SRC=$(SRC)
	@echo OBJ=$(OBJ)
	@echo HELLOBIN=$(HELLOBIN)
	@echo HELLOSRC=$(HELLOSRC)

clean: ## Clean
	stack clean
	rm -f $(BIN) $(OBJ) $(HELLOBIN) *.hi *.o

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: hello debug run runAll clean watch unwatch compile help
