SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.sql)

DIRS=.
SRC=$(call map,get_source_in_directory,$(DIRS))

DOCKER_COMPOSE=./docker-compose.yml

.PHONY: 
default: help

.PHONY: docker_start
docker_start: ## Start docker
	docker compose -f $(DOCKER_COMPOSE) up

.PHONY: docker_stop
docker_stop: ## Stop docker
	docker-compose -f $(DOCKER_COMPOSE) down -v

.PHONY: psql
psql: ## Run hello
	psql service=playground

.PHONY: debug
debug: ## Print variables
	@echo SHELL=$(SHELL)

.PHONY: clean
clean: ## Clean cache
	docker compose -f $(DOCKER_COMPOSE) rm -v
	docker image rm playground-postgres -f

.PHONY: default
help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

