#!/usr/bin/env python3
from __future__ import annotations
from collections.abc import Iterator
from dataclasses import dataclass
import logging
import os
import os.path
from pathlib import Path
import shlex
import subprocess
import sys
from tempfile import NamedTemporaryFile, TemporaryDirectory
from typing import Any, Dict
import click
from click_loglevel import LogLevel
from pydantic import BaseModel, TypeAdapter
from ruamel.yaml import YAML

JOB_REPOSITORY = "git@github.com:datalad/git-annex-ci-client-jobs.git"
RESULT_WORKFLOW = ".github/workflows/handle-result.yaml"

log = logging.getLogger("testannex")


@dataclass
class Test:
    name: str
    shell: str
    body: str

    def run(self, result_dir: Path, **kwargs) -> bool:
        log.info("Running test %r", self.name)
        with NamedTemporaryFile("w+") as script:
            print(self.body, file=script, flush=True)
            with (result_dir / f"{self.name}.log").open("wb") as fp:
                r = runcmd(
                    self.shell,
                    script.name,
                    stdout=fp,
                    stderr=subprocess.STDOUT,
                    check=False,
                    **kwargs,
                )
        rc = r.returncode
        (result_dir / f"{self.name}.rc").write_text(f"{rc}\n")
        if rc == 0:
            log.info("Test passed")
        else:
            log.error("Test %r failed with return code %d", self.name, rc)
        return rc == 0


class Client(BaseModel):
    shell: str = "/bin/bash"
    tests: Dict[str, str]

    def get_tests(self) -> Iterator[Test]:
        for name, body in self.tests.items():
            yield Test(name=name, shell=self.shell, body=body)


def parse_clients(path: Path | None = None) -> dict[str, Client]:
    if path is None:
        path = Path(__file__).with_name("clients.yaml")
    with path.open() as fp:
        data = YAML(typ="safe").load(fp)
    adapter = TypeAdapter(Dict[str, Client])
    return adapter.validate_python(data)


@dataclass
class GitRepo:
    path: Path

    def run(self, *args: str | Path, **kwargs: Any) -> subprocess.CompletedProcess:
        kwargs.setdefault("cwd", self.path)
        return runcmd("git", *args, **kwargs)

    def read(self, *args: str | Path, **kwargs: Any) -> str:
        kwargs.setdefault("cwd", self.path)
        return readcmd("git", *args, **kwargs)

    def readlines(self, *args: str | Path, **kwargs: Any) -> list[str]:
        return self.read(*args, **kwargs).splitlines()

    def ls_branches(self, pattern: str | None) -> list[str]:
        cmd = ["branch", "--format=%(refname:short)"]
        if pattern is not None:
            cmd.append("--list")
            cmd.append(pattern)
        return self.readlines(*cmd)

    def ls_remote_branches(
        self, remote: str = "origin", pattern: str | None = None
    ) -> list[str]:
        heads: list[str] = []
        cmd = ["ls-remote", "--heads", remote]
        if pattern is not None:
            cmd.append(pattern)
        prefix = "refs/heads/"
        for line in self.readlines(*cmd):
            ref = line.strip().partition("\t")[2]
            if ref.startswith(prefix):
                ref = ref[len(prefix) :]
            heads.append(ref)
        return heads


@click.command()
@click.option(
    "-l",
    "--log-level",
    type=LogLevel(),
    default=logging.INFO,
    help="Set logging level  [default: INFO]",
)
@click.argument("clientid")
@click.argument("jobdir", type=click.Path(file_okay=False, path_type=Path))
def main(clientid: str, jobdir: Path, log_level: int) -> None:
    logging.basicConfig(
        format="%(asctime)s [%(levelname)-8s] %(name)s: %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=log_level,
    )

    cfg = parse_clients()
    try:
        client = cfg[clientid]
    except KeyError:
        raise click.UsageError(f"Client {clientid} not listed in clients.yaml")
    if not client.tests:
        raise click.UsageError(f"No tests configured for client {clientid}")

    jobrepo = GitRepo(jobdir)
    if not jobdir.exists():
        log.info("Job directory does not exist; cloning repo")
        jobdir.mkdir(parents=True, exist_ok=True)
        runcmd("git", "clone", "--single-branch", JOB_REPOSITORY, jobdir)
    else:
        log.info("Updating master branch of job repo")
        jobrepo.run("checkout", "master")
        jobrepo.run("-c", "pull.rebase=false", "pull", "origin", "master")

    log.info("Fetching jobs ...")
    jobrepo.run(
        "fetch",
        "--prune",
        "origin",
        f"+refs/heads/build-{clientid}-*:refs/remotes/origin/build-{clientid}-*",
    )
    builds = jobrepo.readlines(
        "for-each-ref",
        "--format",
        "%(refname:lstrip=3)",
        f"refs/remotes/origin/build-{clientid}-*",
    )
    if not builds:
        log.info("No jobs for %s", clientid)
        return

    failed_jobs = 0
    total_jobs = 0
    remote_results = set(jobrepo.ls_remote_branches("origin", f"result-{clientid}-*"))
    for buildno, build_branch in sorted((int(b.split("-")[-1]), b) for b in builds):
        result_branch = f"result-{clientid}-{buildno}"
        if result_branch in remote_results:
            log.warning(
                "Both build branch %r and result branch %r found on remote;"
                " deleting build branch and skipping job",
                build_branch,
                result_branch,
            )
            jobrepo.run("push", "origin", f":refs/heads/{build_branch}")
            continue
        total_jobs += 1

        log.info("Verifying signature of %s ...", build_branch)
        try:
            jobrepo.run("verify-commit", f"origin/{build_branch}")
        except subprocess.CalledProcessError:
            log.error("Commit signature failed verification!")
            failed_jobs += 1
            invalid_branch = f"invalid-{clientid}-{buildno}"
            log.info("Renaming remote branch to %s", invalid_branch)
            jobrepo.run(
                "push",
                "origin",
                f"origin/{build_branch}:refs/heads/{invalid_branch}",
                f":refs/heads/{build_branch}",
            )
            jobrepo.run("branch", "-D", "-r", f"origin/{invalid_branch}")
            continue

        log.info("Running tests for build %d", buildno)
        jobrepo.run(
            "checkout",
            "-f",
            "-B",
            build_branch,
            f"refs/remotes/origin/{build_branch}",
        )
        (debpkg,) = jobdir.glob("*.deb")
        debpkg = debpkg.resolve()
        passes = 0
        failures = 0
        total = 0
        with TemporaryDirectory() as install_dir:
            log.info("Installing git-annex to a temporary directory ...")
            with TemporaryDirectory() as unpack_dir:
                runcmd("ar", "-x", debpkg, cwd=unpack_dir)
                runcmd("tar", "-C", install_dir, "-xzf", "data.tar.gz", cwd=unpack_dir)
            jobrepo.run("checkout", "-f", "--orphan", result_branch)
            # The result workflow needs to be on the branch in order for the
            # workflow to be triggered when pushed to GitHub:
            jobrepo.run("checkout", "master", RESULT_WORKFLOW)
            jobrepo.run("add", RESULT_WORKFLOW)
            jobrepo.run("rm", "-f", debpkg.name)
            env = dict(os.environ)
            env["PATH"] = f"{os.path.join(install_dir, 'usr', 'bin')}:{env['PATH']}"
            env["BUILDNO"] = str(buildno)
            script_cwd = os.path.dirname(__file__)
            for t in client.get_tests():
                if t.run(result_dir=jobdir, env=env, cwd=script_cwd):
                    passes += 1
                else:
                    failures += 1
                total += 1
                jobrepo.run("add", f"{t.name}.log", f"{t.name}.rc")
        if failures:
            log.error("%d/%d tests failed!", failures, total)
            failed_jobs += 1
            status = "FAIL"
        else:
            log.info("All tests passed!")
            status = "PASS"
        log.info("Saving & pushing results ...")
        msg = f"[{status}] Tested git-annex build {buildno} ({passes}/{total} tests passed)"
        jobrepo.run("commit", "-m", msg)
        jobrepo.run("push", "origin", result_branch)
        jobrepo.run("branch", "-D", build_branch)
        jobrepo.run("push", "origin", f":refs/heads/{build_branch}")
        remote_results.add(result_branch)

    log.info("Cleaning up repo ...")
    local_results = set(jobrepo.ls_branches(f"result-{clientid}-*"))
    for branch in local_results - remote_results:
        jobrepo.run("branch", "-D", branch)
        # It appears that `git push origin result-*` doesn't create a branch
        # under origin/ when in a "single branch" clone
        # jobrepo.run("branch", "-D", "-r", f"origin/{branch}")
    jobrepo.run("gc")

    if failed_jobs:
        log.error("%d/%d jobs failed!", failed_jobs, total_jobs)
        sys.exit(1)
    else:
        log.info("All jobs passed!")


def runcmd(*args: str | Path, **kwargs: Any) -> subprocess.CompletedProcess:
    argstrs = [str(a) for a in args]
    log.debug("Running: %s", " ".join(map(shlex.quote, argstrs)))
    kwargs.setdefault("check", True)
    return subprocess.run(argstrs, **kwargs)


def readcmd(*args: str | Path, **kwargs: Any) -> str:
    kwargs["stdout"] = subprocess.PIPE
    kwargs["universal_newlines"] = True
    r = runcmd(*args, **kwargs)
    assert isinstance(r.stdout, str)
    return r.stdout.strip()


if __name__ == "__main__":
    main()
