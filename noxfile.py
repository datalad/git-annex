import nox


@nox.session
def typing_testannex(session):
    session.install("-r", "clients/requirements.txt")
    session.install("mypy")
    session.run("mypy", "clients/testannex.py")
