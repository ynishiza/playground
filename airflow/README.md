# hmc-dwh

- [Resources](#resources)
- [Overview](#overview)
  - [Architecture](#architecture)
- [Development](#development)
  - [Airflow usage](#airflow-usage)
  - [Tips](#tips)
    - [Python](#python)


# Resources

- [Airflow](https://airflow.apache.org/)
- [ORCA](https://www.orca.med.or.jp/index.html)
  - [日レセ home](https://www.orca.med.or.jp/receipt/considering/index.html)
  - [日レセ 技術情報](https://www.orca.med.or.jp/receipt/users/tec/index.html)
  - [日レセ ユーザーマニュアル](https://orcamanual.orca.med.or.jp/gairai/)

[Top](#hmc-dwh)

# Overview

Requirements:

- Python and `uv`
- Docker
- `make`
- `terraform`

File structure:

```
airflow/                    Airflow code and artifacts
airflow/dags/               DAG definitions
airflow/plugins/

db/                         Database related files
db/initdb.d                 Database initialization scripts (Dev only)

documents/                  Documents for reference
documents/ORCA              ORCA spec documents

secrets/

src/                        Main application code
src/hmc_dwh/
src/hmc_dwh/extractors/     Extract process of ELT
src/hmc_dwh/lib/            Shared framework code, incl. db_table.py (generic
                             DB tree writer used by Load)
src/hmc_dwh/weborca/load/   Load process of ELT. See docs/load.md
tests/
```

Development files:

```
.env.example                Base .env file
pg.conf                     Database configuration. See below for usage.
pyproject.toml
docker-compose.yml
Makefile
```

[Top](#hmc-dwh)

## Architecture

Core technology:

- Airflow: dataflow management, ETL processes in particular.

- Python: source code

- PostgreSQL: application data, including the data warehouse.

- AWS: infrastructure


Databases:

1. `hmc_db`: the main application database, including the data warehouse.

1. `airflow_db`: database used by Airflow


[Top](#hmc-dwh)

# Development

Initial setup:

```bash
$ make install
$ cp .env.example .env
```

Commands for core development workflow are provided as `make` commands.

```bash
# Basic
$ make          # Show help
$ make help     # Same


# Application
$ make install
$ make code_check_all
$ make code_check_all_fix
$ make clean

# Development environment
$ make docker_start
$ make docker_stop
$ make docker_rm        # stop and cleanup docker resources

# Connect to database
$ PGSERVICEFILE=pg.conf psql service=hmc_db
$ PGSERVICEFILE=pg.conf psql service=airflow_db
```

[Top](#hmc-dwh)

## Airflow usage

Step 1) Setup

```bash
$ make docker_start

# Get initial password
$ make fetch_airflow_admin_password
```

Step 2) Connect

Open [http://localhost:8080](http://localhost:8080) in the browser.\
Log in as `admin` with the password from step 1.

[Top](#hmc-dwh)

## Tips

### Python

Basic:

```bash
$ uv run ruff check .
$ uv run ruff format .
$ uv run ty check
```

Testing and debugging.

```bash
# Using the debugger
$ uv run pytest --pdb           # Break on first error

$ uv run pytest --trace         # Break on test start
```

[Top](#hmc-dwh)
