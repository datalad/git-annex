#!/usr/bin/env python3
from __future__ import annotations

__python_requires__ = ">= 3.9"
__requires__ = [
    "ghreq ~= 0.2",
    "ghtoken ~= 0.1",
    "pydantic ~= 2.0",
    "requests ~= 2.20",
    "ruamel.yaml ~= 0.15",
]

from collections import Counter
from collections.abc import Iterator
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from enum import Enum
from pathlib import Path
import re
import sys
from tempfile import TemporaryFile
from typing import List
from xml.sax.saxutils import escape
from zipfile import Path as ZipPath
from ghreq import Client, make_user_agent
from ghtoken import get_ghtoken
from pydantic import BaseModel, Field
import requests
from ruamel.yaml import YAML

WINDOW = timedelta(days=1)

WORKFLOW_REPO = "datalad/git-annex"
WORKFLOWS = ["build-ubuntu.yaml", "build-macos.yaml", "build-windows.yaml"]

CLIENTS_REPO = "datalad/git-annex-ci-client-jobs"
CLIENTS_WORKFLOW = "handle-result.yaml"

CLIENT_INFO_FILE = Path(__file__).parents[3] / "clients" / "clients.yaml"

APPVEYOR_PROJECT = "mih/git-annex"


class Outcome(Enum):
    PASS = "PASSED"
    FAIL = "FAILED"
    ERROR = "ERRORED"
    INCOMPLETE = "INCOMPLETE"

    @classmethod
    def from_conclusion(cls, concl: str | None) -> Outcome:
        if concl == "success":
            return cls.PASS
        elif concl == "failure":
            return cls.FAIL
        elif concl == "timed_out":
            return cls.ERROR
        elif concl in {"neutral", "action_required", "cancelled", "skipped", "stale"}:
            return cls.INCOMPLETE
        else:
            raise ValueError(f"Unknown GitHub workflow conclusion: {concl!r}")

    @classmethod
    def from_appveyor_status(cls, status: str) -> Outcome:
        if status == "success":
            return cls.PASS
        elif status == "failed":
            return cls.FAIL
        elif status == "cancelled":
            return cls.INCOMPLETE
        else:
            raise ValueError(f"Unknown Appveyor status: {status!r}")

    def as_html(self) -> str:
        if self is Outcome.PASS:
            return '<span style="color: green">PASS</span>'
        elif self is Outcome.FAIL:
            return '<span style="color: red">FAIL</span>'
        elif self is Outcome.ERROR:
            return '<span style="color: red; text-weight: bold">ERROR</span>'
        else:
            return '<span style="color: grey">&#x2014;</span>'


@dataclass
class DailyStatus:
    github_runs: list[WorkflowStatus]
    client_runs: list[ClientStatus | ResultProcessError]
    all_clients: set[str]
    appveyor_builds: list[AppveyorBuild]

    def get_subject_body(self) -> tuple[str, str]:
        qtys: Counter[Outcome] = Counter()
        body = "<ul>\n<li><p>GitHub:</p>\n<ul>\n"
        if self.github_runs:
            for wfstatus in self.github_runs:
                body += "<li>" + wfstatus.as_html() + "</li>\n"
                qtys.update(wfstatus.get_summary())
        else:
            body += "<li>[no runs]</li>\n"
        body += "</ul>\n</li>\n<li><p>Local Clients:</p>\n<ul>\n"
        if self.client_runs:
            seen = set()
            for cstatus in self.client_runs:
                if cstatus.client_id not in seen:
                    idattr = f' id="{cstatus.client_id}"'
                    seen.add(cstatus.client_id)
                else:
                    idattr = ""
                body += f"<li{idattr}>" + cstatus.as_html() + "</li>\n"
                qtys.update(cstatus.get_summary())
        else:
            body += "<li>[no runs]</li>\n"
        body += "</ul>\n</li>\n<li><p>Appveyor Builds:</p>\n<ul>\n"
        if self.appveyor_builds:
            for build in self.appveyor_builds:
                body += "<li>" + build.as_html() + "</li>\n"
                qtys.update(build.get_summary())
        else:
            body += "<li>[no builds]</li>\n"
        body += "</ul>\n</li>\n</ul>"
        if qtys:
            subject = f"{WORKFLOW_REPO} daily summary: " + ", ".join(
                f"{n} {oc.value}" for oc, n in qtys.items()
            )
            missing_workflows = set(WORKFLOWS).difference(
                r.file for r in self.github_runs
            )
            missing_clients = self.all_clients.difference(
                r.client_id for r in self.client_runs
            )
            if absent := len(missing_workflows) + len(missing_clients):
                subject += f", {absent} ABSENT"
        else:
            subject = f"{WORKFLOW_REPO} daily summary: NOTHING"
        body = (
            "<!DOCTYPE html>\n"
            '<html lang="en">\n'
            "<head>\n"
            '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">\n'
            f"<title>{subject}</title>\n"
            "</head>\n"
            "<body>\n"
            f"{body}\n"
            "</body>\n"
            "</html>"
        )
        return (subject, body)


class WorkflowRun(BaseModel):
    name: str | None = None
    head_branch: str | None = None
    run_number: int
    event: str
    status: str | None = None
    conclusion: str | None = None
    created_at: datetime
    artifacts_url: str
    jobs_url: str
    html_url: str
    check_suite_id: int


class WorkflowJob(BaseModel):
    name: str
    html_url: str
    started_at: datetime
    conclusion: str

    @property
    def outcome(self) -> Outcome:
        return Outcome.from_conclusion(self.conclusion)

    def as_html(self) -> str:
        return f'{self.outcome.as_html()} <a href="{self.html_url}">{escape(self.name)}</a> {self.started_at}'


class Artifact(BaseModel):
    id: int
    created_at: datetime
    archive_download_url: str


@dataclass
class WorkflowStatus:
    file: str
    name: str
    build_id: int
    url: str
    timestamp: datetime
    outcome: Outcome
    jobs: list[WorkflowJob]

    def get_summary(self) -> Counter[Outcome]:
        return Counter(j.outcome for j in self.jobs)

    def as_html(self) -> str:
        s = f'<p>{self.outcome.as_html()} <a href="{self.url}">{escape(self.name)} #{self.build_id}</a> {self.timestamp}</p>\n<ul>\n'
        for j in self.jobs:
            s += "<li>" + j.as_html() + "</li>\n"
        return s + "</ul>\n"


@dataclass
class ClientStatus:
    client_id: str
    build_id: int
    timestamp: datetime
    artifact_url: str
    tests: dict[str, Outcome]

    def get_summary(self) -> Counter[Outcome]:
        return Counter(self.tests.values())

    def as_html(self) -> str:
        s = f'<p>{escape(self.client_id)} #{self.build_id} [<a href="{self.artifact_url}">download logs</a>] {self.timestamp}</p><ul>\n'
        for testname, oc in self.tests.items():
            s += f"<li>{oc.as_html()} {escape(testname)}</li>\n"
        return s + "</ul>"


@dataclass
class ResultProcessError:
    client_id: str
    build_id: int
    timestamp: datetime
    url: str

    def get_summary(self) -> Counter[Outcome]:
        return Counter([Outcome.ERROR])

    def as_html(self) -> str:
        return f'{Outcome.ERROR.as_html()} processing results for {escape(self.client_id)} #{self.build_id} [<a href="{self.url}">logs</a>] {self.timestamp}'


class AppveyorJob(BaseModel):
    jobId: str
    name: str
    status: str

    @property
    def outcome(self) -> Outcome:
        return Outcome.from_appveyor_status(self.status)

    def url(self, build_id: int) -> str:
        return f"https://ci.appveyor.com/project/{APPVEYOR_PROJECT}/builds/{build_id}/job/{self.jobId}"

    def as_html(self, build_id: int) -> str:
        return f'{self.outcome.as_html()} <a href="{self.url(build_id)}">{escape(self.name)}</a>'


class AppveyorBuild(BaseModel):
    buildId: int
    finished: datetime | None
    started: datetime | None = None  # could have been canceled thus never started
    version: str
    status: str
    jobs: List[AppveyorJob]

    @property
    def outcome(self) -> Outcome:
        return Outcome.from_appveyor_status(self.status)

    @property
    def url(self) -> str:
        return (
            f"https://ci.appveyor.com/project/{APPVEYOR_PROJECT}/builds/{self.buildId}"
        )

    def get_summary(self) -> Counter[Outcome]:
        return Counter(j.outcome for j in self.jobs)

    def as_html(self) -> str:
        s = f'<p>{self.outcome.as_html()} <a href="{self.url}">{self.version}</a> {self.started or self.status}</p>\n<ul>\n'
        for j in self.jobs:
            s += "<li>" + j.as_html(self.buildId) + "</li>\n"
        return s + "</ul>\n"


class AppveyorProject(BaseModel):
    build: AppveyorBuild


class AppveyorHistory(BaseModel):
    builds: List[AppveyorBuild] = Field(default_factory=list)


def main() -> None:
    outfile = sys.argv[1]
    token = get_ghtoken()
    user_agent = make_user_agent(
        "daily-status.py", url="https://github.com/datalad/git-annex"
    )
    cutoff = datetime.now(timezone.utc) - WINDOW

    with CLIENT_INFO_FILE.open() as fp:
        client_info = YAML(typ="safe").load(fp)
    all_clients = set(client_info.keys())

    with Client(token=token, user_agent=user_agent) as client:
        github_statuses = []
        wfrepo = client / "repos" / WORKFLOW_REPO
        for wffilename in WORKFLOWS:
            wfep = wfrepo / "actions" / "workflows" / wffilename
            wf = wfep.get()
            for data in (wfep / "runs").paginate():
                run = WorkflowRun.model_validate(data)
                if run.status != "completed" or run.event not in (
                    "schedule",
                    "workflow_dispatch",
                ):
                    continue
                if run.created_at <= cutoff:
                    break
                jobs = [
                    WorkflowJob.model_validate(jdata)
                    for jdata in client.paginate(run.jobs_url)
                ]
                github_statuses.append(
                    WorkflowStatus(
                        file=wffilename,
                        name=wf["name"],
                        build_id=run.run_number,
                        url=run.html_url,
                        timestamp=run.created_at,
                        outcome=Outcome.from_conclusion(run.conclusion),
                        jobs=jobs,
                    )
                )

        client_statuses: list[ClientStatus | ResultProcessError] = []
        for data in client.paginate(
            f"/repos/{CLIENTS_REPO}/actions/workflows/{CLIENTS_WORKFLOW}/runs"
        ):
            run = WorkflowRun.model_validate(data)
            if run.status != "completed":
                continue
            if run.created_at <= cutoff:
                break
            assert run.head_branch is not None
            m = re.fullmatch(r"result-(.+)-(\d+)", run.head_branch)
            assert m
            if Outcome.from_conclusion(run.conclusion) is Outcome.PASS:
                (artdata,) = client.paginate(run.artifacts_url)
                artifact = Artifact.model_validate(artdata)
                client_statuses.append(
                    ClientStatus(
                        client_id=m[1],
                        build_id=int(m[2]),
                        timestamp=run.created_at,
                        artifact_url=(
                            f"https://github.com/{CLIENTS_REPO}/suites"
                            f"/{run.check_suite_id}/artifacts"
                            f"/{artifact.id}"
                        ),
                        tests=get_client_test_outcomes(
                            client, artifact.archive_download_url
                        ),
                    )
                )
            else:
                client_statuses.append(
                    ResultProcessError(
                        client_id=m[1],
                        build_id=int(m[2]),
                        timestamp=run.created_at,
                        url=run.html_url,
                    )
                )

    appveyor_builds = []
    with requests.Session() as s:
        s.headers["User-Agent"] = user_agent
        for build in get_appveyor_builds(s):
            if build.finished is None:
                continue
            if build.finished <= cutoff:
                break
            # Appveyor's build history endpoint omits job information, so we
            # need to refetch each build via its individual endpoint to get the
            # jobs.
            r = s.get(
                f"https://ci.appveyor.com/api/projects/{APPVEYOR_PROJECT}"
                f"/build/{build.version}"
            )
            r.raise_for_status()
            project = AppveyorProject.model_validate(r.json())
            appveyor_builds.append(project.build)

    status = DailyStatus(
        github_runs=github_statuses,
        client_runs=client_statuses,
        all_clients=all_clients,
        appveyor_builds=appveyor_builds,
    )
    (subject, body) = status.get_subject_body()
    print(subject)
    with open(outfile, "w") as fp:
        print(body, file=fp)


def get_client_test_outcomes(client: Client, artifact_url: str) -> dict[str, Outcome]:
    tests = {}
    with TemporaryFile() as fp:
        with client.get(artifact_url, stream=True) as r:
            for chunk in r.iter_content(chunk_size=8192):
                fp.write(chunk)
        fp.flush()
        fp.seek(0)
        for p in ZipPath(fp).iterdir():
            if p.name.endswith(".rc"):
                tests[p.name[:-3]] = (
                    Outcome.PASS if int(p.read_text()) == 0 else Outcome.FAIL
                )
    return tests


def get_appveyor_builds(s: requests.Session) -> Iterator[AppveyorBuild]:
    params = {"recordsNumber": 20}
    while True:
        r = s.get(
            f"https://ci.appveyor.com/api/projects/{APPVEYOR_PROJECT}/history",
            params=params,
        )
        r.raise_for_status()
        history = AppveyorHistory.model_validate(r.json())
        if history.builds:
            yield from history.builds
            params["startBuildId"] = history.builds[-1].buildId
        else:
            break


if __name__ == "__main__":
    main()
