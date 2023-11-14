import os.path
import nox


@nox.session
def typing_testannex(session: nox.Session) -> None:
    session.install("-r", "clients/requirements.txt")
    session.install("mypy")
    session.run("mypy", "clients/testannex.py")


@nox.session
def typing_daily_status(session: nox.Session) -> None:
    path = ".github/workflows/tools/daily-status.py"
    install_requires(session, path)
    session.install("mypy", "types-python-dateutil", "types-requests")
    session.run("mypy", path)


@nox.session
def typing_dispatch_build(session: nox.Session) -> None:
    path = ".github/workflows/tools/dispatch-build"
    install_requires(session, path)
    # PyGithub uses python-dateutil and requests, so apparently their typing
    # stubs have to be installed in order for mypy to analyze PyGithub
    session.install("mypy", "types-python-dateutil", "types-requests")
    session.run("mypy", path)


def install_requires(session: nox.Session, path: str) -> None:
    tmpdir = session.create_tmp()
    reqfile = os.path.join(tmpdir, "requirements.txt")
    session.install("pip-run")
    with open(reqfile, "w", encoding="utf-8") as fp:
        session.run(
            "python",
            "-m",
            "pip_run.read-deps",
            "--separator",
            "newline",
            path,
            stdout=fp,
        )
    session.install("-r", reqfile)
