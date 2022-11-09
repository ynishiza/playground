SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

COMPILER=ghc
EXEC=runhaskell
MAINSRC=script.hs
SRC=$(call map,get_source_in_directory,./lib) $(MAINSRC)
OBJ=$(SRC:.hs=.o)

COMPONENTMAIN=playground:exe:main
HIDMAIN=playground:exe:hid
HIDTEST=playground:test:hid-test
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
	make run-hid

run-hid: ## Haskell in depth
	@echo "Chapter 1"
	@echo 1 | stack exec -- hid ./haskellInDepth/data/hamlet.txt 20
	@echo 1 | stack exec -- hid ./haskellInDepth/data/forsyte.txt 20

	@echo "Chapter 2"
	@echo 2 | stack exec -- hid -r ./haskellInDepth/data/rotations1.txt North
	@echo 2 | stack exec -- hid -o ./haskellInDepth/data/directions1.txt 

	@echo "Tests"
	@stack test -- $(HIDTEST)

watch: ## Watch for changes
	watchman watch .
	watchman -- trigger . build "*.hs" "lib/*.hs" -- make compile

unwatch: ## Clear watch
	watchman watch-del .

debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo MAINSRC=$(MAINSRC)
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
