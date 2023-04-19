#!/usr/bin/env python3
from __future__ import annotations

__python_requires__ = ">= 3.8"
__requires__ = [
    "python-dateutil ~= 2.7",
    "PyGithub ~= 1.53",
    "requests ~= 2.20",
    "ruamel.yaml ~= 0.15",
]

from collections import Counter
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from enum import Enum
import os
from pathlib import Path
import re
from tempfile import TemporaryFile
from zipfile import Path as ZipPath
from xml.sax.saxutils import escape

from dateutil.parser import isoparse
from github import Github
import requests
from ruamel.yaml import YAML

WINDOW = timedelta(days=1)

WORKFLOW_REPO = "datalad/git-annex"
WORKFLOWS = ["build-ubuntu.yaml", "build-macos.yaml", "build-windows.yaml"]

CLIENTS_REPO = "datalad/git-annex-ci-client-jobs"
CLIENTS_WORKFLOW = "handle-result.yaml"

CLIENT_INFO_FILE = Path(__file__).parents[3] / "clients" / "clients.yaml"


class Outcome(Enum):
    PASS = "PASSED"
    FAIL = "FAILED"
    ERROR = "ERRORED"
    INCOMPLETE = "INCOMPLETE"

    @classmethod
    def from_conclusion(cls, concl: str) -> Outcome:
        if concl == "success":
            return cls.PASS
        elif concl == "failure":
            return cls.FAIL
        elif concl == "timed_out":
            return cls.ERRROR
        elif concl in {"neutral", "action_required", "cancelled", "skipped", "stale"}:
            return cls.INCOMPLETE
        else:
            raise ValueError(f"Unknown GitHub workflow conclusion: {concl!r}")

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

    def get_subject_body(self) -> tuple[str, str]:
        qtys = Counter()
        body = "<ul>\n<li><p>GitHub:</p>\n<ul>\n"
        if self.github_runs:
            for wfstatus in self.github_runs:
                body += "<li>" + wfstatus.as_html() + "</li>\n"
                qtys.update(wfstatus.get_summary())
        else:
            body += "<li>[no runs]</li>\n"
        body += "</ul>\n</li>\n<li><p>Local Clients:</p>\n<ul>\n"
        if self.client_runs:
            for cstatus in self.client_runs:
                body += "<li>" + cstatus.as_html() + "</li>\n"
                qtys.update(cstatus.get_summary())
        else:
            body += "<li>[no runs]</li>\n"
        body += "</ul>"
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
        return (subject, body)


@dataclass
class WorkflowStatus:
    file: str
    name: str
    build_id: int
    url: str
    timestamp: datetime
    outcome: Outcome
    jobs: list[JobStatus]

    def get_summary(self) -> Counter[Outcome]:
        return Counter(j.outcome for j in self.jobs)

    def as_html(self) -> str:
        s = f'<p>{self.outcome.as_html()} <a href="{self.url}">{escape(self.name)} #{self.build_id}</a> {self.timestamp}</p>\n<ul>\n'
        for j in self.jobs:
            s += "<li>" + j.as_html() + "</li>\n"
        return s + "</ul>\n"


@dataclass
class JobStatus:
    name: str
    url: str
    timestamp: datetime
    outcome: Outcome

    def as_html(self) -> str:
        return f'{self.outcome.as_html()} <a href="{self.url}">{escape(self.name)}</a> {self.timestamp}'


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


def main() -> None:
    token = os.environ["GITHUB_TOKEN"]
    gh = Github(token)
    cutoff = datetime.now(timezone.utc) - WINDOW

    with CLIENT_INFO_FILE.open() as fp:
        client_info = YAML(typ="safe").load(fp)
    all_clients = set(client_info.keys())

    with requests.Session() as s:
        s.headers["Authorization"] = f"bearer {token}"

        github_statuses = []
        wfrepo = gh.get_repo(WORKFLOW_REPO)
        for wffilename in WORKFLOWS:
            wf = wfrepo.get_workflow(wffilename)
            for run in wf.get_runs():
                if run.status != "completed" or run.event not in (
                    "schedule",
                    "workflow_dispatch",
                ):
                    continue
                dt = ensure_aware(run.created_at)
                if dt <= cutoff:
                    break
                r = s.get(run.jobs_url)
                r.raise_for_status()
                job_statuses = [
                    JobStatus(
                        name=j["name"],
                        url=j["html_url"],
                        timestamp=isoparse(j["started_at"]),
                        outcome=Outcome.from_conclusion(j["conclusion"]),
                    )
                    for j in r.json()["jobs"]
                ]
                github_statuses.append(
                    WorkflowStatus(
                        file=wffilename,
                        name=wf.name,
                        build_id=run.run_number,
                        url=run.html_url,
                        timestamp=dt,
                        outcome=Outcome.from_conclusion(run.conclusion),
                        jobs=job_statuses,
                    )
                )

        client_statuses = []
        for run in gh.get_repo(CLIENTS_REPO).get_workflow(CLIENTS_WORKFLOW).get_runs():
            if run.status != "completed":
                continue
            dt = ensure_aware(run.created_at)
            if dt <= cutoff:
                break
            m = re.fullmatch(r"result-(.+)-(\d+)", run.head_branch)
            assert m
            if Outcome.from_conclusion(run.conclusion) is Outcome.PASS:
                r = s.get(run.artifacts_url)
                r.raise_for_status()
                (artifact,) = r.json()["artifacts"]
                client_statuses.append(
                    ClientStatus(
                        client_id=m[1],
                        build_id=int(m[2]),
                        timestamp=dt,
                        artifact_url=(
                            f"https://github.com/{CLIENTS_REPO}/suites"
                            f"/{run.raw_data['check_suite_id']}/artifacts"
                            f"/{artifact['id']}"
                        ),
                        tests=get_client_test_outcomes(
                            s, artifact["archive_download_url"]
                        ),
                    )
                )
            else:
                client_statuses.append(
                    ResultProcessError(
                        client_id=m[1],
                        build_id=int(m[2]),
                        timestamp=dt,
                        url=run.html_url,
                    )
                )

    status = DailyStatus(
        github_runs=github_statuses,
        client_runs=client_statuses,
        all_clients=all_clients,
    )
    (subject, body) = status.get_subject_body()
    print(subject)
    with open("body.html", "w") as fp:
        print(body, file=fp)


def ensure_aware(dt: datetime) -> datetime:
    # Pygithub returns naïve datetimes for timestamps with a "Z" suffix.  Until
    # that's fixed <https://github.com/PyGithub/PyGithub/pull/1831>, we need to
    # make such datetimes timezone-aware manually.
    return dt.replace(tzinfo=timezone.utc) if dt.tzinfo is None else dt


def get_client_test_outcomes(
    s: requests.Session, artifact_url: str
) -> dict[str, Outcome]:
    tests = {}
    with TemporaryFile() as fp:
        with s.get(artifact_url, stream=True) as r:
            r.raise_for_status()
            for chunk in r.iter_content(chunk_size=8192):
                fp.write(chunk)
        fp.seek(0)
        for p in ZipPath(fp).iterdir():
            if p.name.endswith(".rc"):
                tests[p.name[:-3]] = (
                    Outcome.PASS if int(p.read_text()) == 0 else Outcome.FAIL
                )
    return tests


if __name__ == "__main__":
    main()
