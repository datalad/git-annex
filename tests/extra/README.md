# Extra tests

Tests that CI runs on top of `git annex test` and the DataLad test battery.
Each test targets a specific real-world scenario, often a regression that
was seen in the wild and would slip past both upstream test suites.

Two parallel implementations of the same suite are kept side-by-side so we
can compare style and eventually pick one to promote (upstream-friendly or
otherwise):

- `pytest/` — Python + pytest.  Fixtures for temp directories and repo
  cloning; skip markers are `@pytest.mark.skipif(...)`.
- `bats/` — [Bats](https://bats-core.readthedocs.io/) shell tests.  Skip
  logic uses the `skip` built-in inside `setup()`.

CI runs **both**.  Each test declares its own skip conditions (missing
`strace`, missing `yt-dlp`, unsupported platform) so the default is: run
everywhere, skip only where the required tool isn't available.

## Tests

| Test          | Purpose                                                                                                                              | Platforms       |
| ------------- | ------------------------------------------------------------------------------------------------------------------------------------ | --------------- |
| `dynlibs`     | Regression guard on the number of failed dynamic-library lookups (`strace -e ENOENT` on `libpcre.*so`) during `git-annex version`/`init`. | Linux           |
| `url_backend` | Regression guard for parsing "odd" URL-backend keys (URL-encoded characters like `&c`, `%%`, `,63v` etc.) on a real DataLad dataset.  | All             |

## Running locally

```bash
# pytest suite
python -m pytest -v tests/extra/pytest/

# bats suite (needs bats-core installed)
bats tests/extra/bats/
```

Both suites assume `git-annex` is on `PATH`.  The `url_backend` test
clones a small (~18 MB) real DataLad dataset from `datasets.datalad.org`
so needs network access.
