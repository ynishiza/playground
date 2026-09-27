SHELL=/usr/bin/env bash

DOCKER_COMPOSE=docker-compose.yml
AIRFLOW_SERVER_CONTAINER=airflow-airflow_playground_apiserver-1

CODE_SOURCE=.

.PHONY: default
default: help

.PHONY: airflow
airflow: ## Execute airflow command
	docker exec -ti $(AIRFLOW_SERVER_CONTAINER) bash

.PHONY: docker_start
docker_start: ## Start local development containers.
	docker compose -f $(DOCKER_COMPOSE) up

.PHONY: docker_stop
docker_stop: ## Stop all docker composes.
	docker compose -f $(DOCKER_COMPOSE) down

.PHONY: docker_rm
docker_rm: ## Cleanup resources used by the docker compose settings.
	@# down: remove containers, named volumes, networks
	docker compose -f $(DOCKER_COMPOSE) down --volumes
	@echo "Use docker compose -f $(DOCKER_COMPOSE) rm -rmi to remove images"

.PHONY: fetch_airflow_admin_password
fetch_airflow_admin_password: ## Show the local Airflow admin password (generated on first boot, in hmc_airflow_apiserver's logs).
	@line=$$(docker compose -f $(DOCKER_COMPOSE) logs airflow_playground_apiserver 2>/dev/null | grep "Password for user" | tail -1); \
	if [ -z "$$line" ]; then \
		echo "No password found. Is hmc_airflow_apiserver running? (make docker_start)"; \
	else \
		echo "$$line" | sed 's/.*Password for user/Password for user/'; \
	fi

.PHONY: clean
clean: ## Clean caches and test files.
	@echo TODO
	uv clean

.PHONY: debug
debug: ## Debug variables.
	@echo SHELL=$(SHELL)
	@echo SHELL=$(DOCKER_COMPOSE)

.PHONY: install
install: ## Setup uv env
	uv python install && uv sync --all-extras --dev

.PHONY: help
help: ## Display this help.
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
