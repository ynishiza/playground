SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.c)
get_header_in_directory=$(wildcard $(value)/*.h)

# macos native gcc
# CC=gcc
# gcc-8 installed by
#   $ brew install gcc@8
CC=gcc-8
FLAGS=-g
DEPS=stack.h
MAIN=main
DIRS=. ./lib ./src

SRC=$(call map,get_source_in_directory,$(DIRS))
HEADERS=$(call map,get_header_in_directory,$(DIRS))
OBJ=$(SRC:.c=.o)

compile: $(MAIN)

$(MAIN): $(OBJ)
	$(CC) $(FLAGS) -o $(MAIN) $^

$(OBJ): $(HEADERS)

%.o: %.c
	$(CC) -c $(FLAGS) -o $@ $<


print:
	@echo HEADERS=$(HEADERS) 
	@echo OBJ=$(OBJ) 
	@echo SRC=$(SRC)

clean:
	rm -rf $(MAIN) $(OBJ)

.PHONY: clean compile print
