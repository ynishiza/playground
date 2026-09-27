"""Airflow DAG: WebORCA extract, one task per API-call step, per day in a date range."""

from __future__ import annotations

from logging import getLogger

from airflow.sdk import dag, task

logger = getLogger(__name__)


@dag(
    dag_id="test_dag",
    description="Test DAG",
    schedule=None,
    catchup=False,
    tags=["Test"],
)
def test_dag() -> None:
    @task
    def test_task() -> int:
        # logger.info("Task")
        return 0

    test_task()
    logger.info("Hello")


test_dag()
