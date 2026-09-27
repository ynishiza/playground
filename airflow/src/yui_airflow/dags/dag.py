from __future__ import annotations

from logging import getLogger
from typing import cast

from airflow.sdk import dag, task, task_group, TaskInstance
from airflow.providers.standard.operators.trigger_dagrun import TriggerDagRunOperator
from yui_airflow.logger import messge

logger = getLogger(__name__)


@task
def task_hello(name: str):
    logger.info(f"Hello {name}")

@task
def task_bye(name: str):
    logger.info(f"Bye {name}")

@task
def task_meta(name: str, run_id: str, **kwargs):
    t = cast(TaskInstance, kwargs["task_instance"])
    logger.info(f"name {name}")
    logger.info(f"run_id {run_id}")
    logger.info(f"task {t}")

@task_group(group_id="group_b", prefix_group_id=True)
def group_b():
    task_hello("deep child") >> task_bye("deep child")

@task_group(group_id="group_a", prefix_group_id=True)
def group_a():
    task_hello("child") >> task_bye("child")
    group_b()

@task_group(group_id="root_group", prefix_group_id=True)
def root_group():
    group_a()

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
        logger.info(f"Task {messge}")
        return 0

    sub = TriggerDagRunOperator(
        task_id="Hello_Task",
        trigger_dag_id="dag_hello",
        wait_for_completion=True,
    )
    root_group()
    sub >> test_task()
    task_meta("ABC")
    logger.info("Hello")


@dag(
    dag_id="dag_hello",
    description="Test DAG 2",
    schedule=None,
    catchup=False,
    tags=["Test"],
)
def dag_hello() -> None:
    task_hello("AA") >> task_bye("BB")

@dag(
    dag_id="dag_bye",
    description="Test DAG 2",
    schedule=None,
    catchup=False,
    tags=["Test"],
)
def dag_bye() -> None:
    task_bye("AAA")

test_dag()
dag_hello()
dag_bye()

