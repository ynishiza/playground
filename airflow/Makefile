SHELL=/usr/bin/env bash

VENV_DIR=./.venv
DOCKER_COMPOSE=docker-compose.yml
AIRFLOW_SERVER_CONTAINER=airflow-airflow_playground_apiserver-1

CODE_SOURCE=.


.PHONY: default
default: help

.PHONY: install
install: ## Setup uv env
	uv python install && uv sync --all-extras --dev

.PHONY: airflow_start
airflow_start: ## Start airflow
	source $(VENV_DIR)/bin/activate ; \
	airflow standalone

.PHONY: airflow_info
airflow_info: ## Execute airflow command
	source $(VENV_DIR)/bin/activate && airflow info

.PHONY: airflow_password
airflow_password: ## Show the local Airflow admin password (generated on first boot, in hmc_airflow_apiserver's logs).
	cat ./airflow/simple_auth_manager_passwords.json.generated
	@echo "Server at localhost:8080"

.PHONY: clean
clean: ## Clean caches and test files.
	@echo TODO
	uv clean

.PHONY: debug
debug: ## Debug variables.
	@echo SHELL=$(SHELL)
	@echo SHELL=$(DOCKER_COMPOSE)

.PHONY: help
help: ## Display this help.
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
