# Extra tests

Tests that CI runs on top of `git annex test` and the DataLad test battery.
Each test targets a specific real-world scenario, often a regression that
was seen in the wild and would slip past both upstream test suites.

Written for **pytest**.  Each test declares its own skip conditions
(missing `strace`, unsupported platform, git-annex not on PATH) so the
default is: run everywhere, skip only where the required tool isn't
available.  The URL-backend `get` test is `xfail(strict=False)` below
the known-fix version so old git-annex builds do not red the run but a
regression on a fixed build fails loudly.

## Why pytest and not bats?

A parallel [Bats](https://bats-core.readthedocs.io/) prototype was
evaluated and dropped.  The reasons, briefly:

- **Cross-platform install cost.**  Bats needs three distinct install
  recipes (Ubuntu `apt`, macOS `brew`, Windows git-clone bootstrap)
  and runs under Git Bash on Windows where `timeout`, `chmod -R u+w`
  and `sort -V` behave subtly differently.  Pytest is one
  `pip install pytest` on all four runners, and Python is already
  needed by the `test-datalad` job.
- **No real `xfail` primitive.**  Bats only has `skip`, which cannot
  distinguish "known-broken on this version, expected to fail" from
  "unexpectedly passed, tell me".  pytest's
  `@pytest.mark.xfail(strict=False)` gives the correct
  regression-guard semantics on the URL-backend `get` test.
- **Fixtures and shared helpers.**  `conftest.py` gives us cached
  `git annex version` parsing, a shared version-reporting hook,
  `tmp_path_factory` module-scoped clones, and clean parametrization
  — all of which the bats port was re-implementing by hand in
  progressively-hairier shell.

## Tests

| Test          | Purpose                                                                                                                                    | Platforms |
| ------------- | ------------------------------------------------------------------------------------------------------------------------------------------ | --------- |
| `dynlibs`     | Regression guard on the number of failed dynamic-library lookups (`strace -e ENOENT` on `libpcre.*so`) during `git-annex version` / `init`. | Linux     |
| `url_backend` | Regression guard for parsing "odd" URL-backend keys (URL-encoded characters like `&c`, `%%`, `,63v` etc.) on a real DataLad dataset.        | All       |

## Running locally

```bash
python -m pytest -v tests/extra/pytest/
```

Assumes `git-annex` is on `PATH`.  The `url_backend` test clones a
small (~18 MB) real DataLad dataset from `datasets.datalad.org`, so
needs network access.
