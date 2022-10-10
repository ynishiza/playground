SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

EXEC=ansible-playbook
ENVNAME=myenv

OPTIONS=-v
# OPTIONS=-vv

default: help

install: $(ENVNAME) ## Installs
	./install.sh

uninstall: ## Uninstall
	rm -r ./myenv

run_hello: ## Run hello
	source "$(ENVNAME)/bin/activate" && \
	$(EXEC) $(OPTIONS) hello.yml

run_playground:  ## run playground.yml
	source "$(ENVNAME)/bin/activate" && \
	$(EXEC) $(OPTIONS) playground.yml

debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo ENVNAME=$(ENVNAME)

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: install uninstall compile run test watch unwatch clean help
