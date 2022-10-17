SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.c)
get_header_in_directory=$(wildcard $(value)/*.h)

# case: macos
# macos native gcc does not work
# Use gcc-8 installed by
#   $ brew install gcc@8
GCC8=$(shell which gcc-8)

# Implicit rule variables for *.c files
# Ref: https://www.gnu.org/software/make/manual/html_node/Catalogue-of-Rules.html#Catalogue-of-Rules
CC=gcc
ifneq (, $(GCC8))
	CC=$(GCC8)
endif
CFLAGS=-g

DEPS=stack.h
MAINBIN=main
DIRS=./lib ./src

SRC=$(call map,get_source_in_directory,$(DIRS)) main.c stack.c
HEADERS=$(call map,get_header_in_directory,$(DIRS))
OBJ=$(SRC:.c=.o)

HELLOSRC=hellomain.c
HELLOBIN=$(basename $(HELLOSRC))
HELLOOBJ=$(HELLOSRC:.c=.o)

default: help

compile: $(MAINBIN)

$(MAINBIN): $(OBJ)
	$(CC) $(CFLAGS) -o $(MAINBIN) $^

$(OBJ): $(HEADERS)

hello: $(HELLOBIN) ## hello
	./$(HELLOBIN)

$(HELLOBIN) : $(HELLOOBJ)
	$(CC) $(CFLAGS) -o $(HELLOBIN) $<

run: $(MAINBIN)
	./$(MAINBIN)

debug: ## Print variables
	@echo GCC8=$(GCC8)
	@echo CC=$(CC)
	@echo HEADERS=$(HEADERS) 
	@echo OBJ=$(OBJ) 
	@echo SRC=$(SRC)
	@echo HELLOBIN=$(HELLOBIN)

clean: hacking_book_clean ## Clean cache
	rm -rf $(MAINBIN) $(OBJ) $(HELLOBIN) $(HELLOOBJ)

hacking_book_compile: ## Compile hacking_book code
	pushd ./hacking_book/chapter2 >/dev/null && make compile && popd >/dev/null
	pushd ./hacking_book/auth_overflow >/dev/null && make compile && popd >/dev/null

hacking_book_clean: ## Cleanup
	pushd ./hacking_book/chapter2 >/dev/null && make clean && popd >/dev/null
	pushd ./hacking_book/auth_overflow >/dev/null && make clean && popd >/dev/null

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: help abc clean compile hello debug hacking_book_compile hacking_book_clean
