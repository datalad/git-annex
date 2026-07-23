# Setup

Operational documentation for maintainers of `con/git-annex`: what
secrets the CI expects, what permissions those secrets must carry, and
how to rotate them. End-user documentation (branch layout, patch
submission, licensing) lives in [`README.md`](./README.md).

## CI secrets

The workflows in this repository require the following secrets to be
configured on `con/git-annex` (repository secrets, or inherited from
the `con` organization):

| Secret                                                                                | Used by                                  | Purpose                                                                                                                    |
| ------------------------------------------------------------------------------------- | ---------------------------------------- | -------------------------------------------------------------------------------------------------------------------------- |
| `DATALAD_GITHUB_TOKEN`                                                                | `update-mirror.yml`, `daily-status.yaml` | PAT used to `workflow_dispatch` new-tag builds on `con/git-annex` and to push daily reports to `con/git-annex-ci-reports`. |
| `CLIENT_JOBS_SSH_KEY`                                                                 | `build-ubuntu.yaml`                      | SSH deploy key with write access to `con/git-annex-ci-client-jobs`.                                                        |
| `DOCKER_TOKEN`                                                                        | `build-linux-buildenv.yaml`              | Docker Hub push token.                                                                                                     |
| `NOTIFY_SMTP_HOST` / `_PORT` / `_USERNAME` / `_PASSWORD` / `_SECURE` / `_IGNORE_CERT` | all build workflows, `daily-status.yaml` | SMTP relay for failure notifications and daily-status e-mail.                                                              |
| `NOTIFY_RECIPIENT`                                                                    | build workflows                          | Address for per-build failure notifications.                                                                               |
| `DAILY_STATUS_RECIPIENTS`                                                             | `daily-status.yaml`                      | Address(es) for daily-status e-mail.                                                                                       |

## `DATALAD_GITHUB_TOKEN` — required PAT scope

This is a **fine-grained personal access token** owned by a bot user
(currently `yarikoptic-gitmate`). It must have access to both
`con/git-annex` and `con/git-annex-ci-reports` with the following
repository permissions:

- `Actions`: Read and write
- `Contents`: Read and write
- `Metadata`: Read-only
- `Workflows`: Read and write

Fine-grained PATs on org-owned repos require **explicit per-repo
grants** on the token itself — being a collaborator alone is not
sufficient. A missing `Contents: write` grant on
`con/git-annex-ci-reports` manifests as:

```
remote: Permission to con/git-annex-ci-reports.git denied to <bot-user>.
fatal: unable to access '.../git-annex-ci-reports/': The requested URL returned error: 403
```

### Creating or rotating the token

GitHub CLI does **not** support creating personal access tokens —
this must be done in the web UI, logged in as the bot user:

<https://github.com/settings/personal-access-tokens/new>

Recommended settings:

- Token name: `con/git-annex CI (expires YYYY-MM-DD)`
- Expiration: 1 year — set a calendar reminder to rotate before it expires
- Resource owner: `con` (organization)
- Repository access → **Only select repositories**: `con/git-annex`, `con/git-annex-ci-reports`
- Repository permissions: as listed above

The token creation may require org-owner approval; see
<https://docs.github.com/en/organizations/managing-programmatic-access-to-your-organization/managing-requests-for-personal-access-tokens-in-your-organization>.

After creating a new token, update the secret from an account with
admin on `con/git-annex`:

```bash
gh secret set DATALAD_GITHUB_TOKEN --repo con/git-annex
# paste the token when prompted
```

Verify with:

```bash
gh workflow run daily-status.yaml --repo con/git-annex
sleep 30
gh run list --repo con/git-annex --workflow daily-status.yaml --limit 1
```

## `CLIENT_JOBS_SSH_KEY` — SSH deploy key

Generate an ed25519 keypair, register the **public** key as a deploy
key with **write access** on `con/git-annex-ci-client-jobs`, and store
the **private** key (including the `-----BEGIN…-----` and
`-----END…-----` lines, with real newlines) as the
`CLIENT_JOBS_SSH_KEY` secret:

```bash
ssh-keygen -t ed25519 -f /tmp/client-jobs-key -N '' -C 'con/git-annex CI'
# Add /tmp/client-jobs-key.pub as a deploy key with write access:
gh repo deploy-key add /tmp/client-jobs-key.pub \
  --repo con/git-annex-ci-client-jobs \
  --title 'con/git-annex CI (build-ubuntu)' \
  --allow-write
# Store the private key as a secret on this repo:
gh secret set CLIENT_JOBS_SSH_KEY --repo con/git-annex < /tmp/client-jobs-key
shred -u /tmp/client-jobs-key /tmp/client-jobs-key.pub
```

**Caveat with `gh repo deploy-key add`:** the deploy key is bound to
the `gh` CLI auth token of whoever ran the command; if that person
later re-authenticates `gh` or revokes the CLI app, GitHub removes the
deploy key as well, and Ubuntu builds start failing with the symptoms
below. If you want the key to outlive your `gh` session, add it via
the web UI (repo Settings → Deploy keys) instead.

**Common pitfall — mangled newlines.** Pasting the private key via a
web form or through shell heredocs can strip newlines. Symptoms:

- Every `-` character in workflow logs is redacted as `***` (e.g.
  `git-annex` shows as `git***annex`, `--no-tags` as `***no***tags`),
  because GitHub Actions has masked short substrings of a
  multiline-secret line that ended up being just `-`.
- `Load key "...": error in libcrypto` followed by
  `git@github.com: Permission denied (publickey)` in the "Clone
  con/git-annex-ci-client-jobs" step.
- Cascading downstream failure: `build-package` outputs get scrubbed
  (`##[warning]Skip output 'build-version' since it may contain
  secret.`), so downstream `test-annex` / `test-datalad` jobs try to
  download an artifact with an empty version suffix and fail with
  `Artifact not found for name: git-annex-…-installer_`.

Always set the secret via `gh secret set … < keyfile` (stdin from the
raw key file), which preserves newlines exactly.

## SMTP notification secrets

Failure e-mails are sent via [`dawidd6/action-send-mail`][send-mail].
If the `NOTIFY_SMTP_*` secrets are missing, the failure-notify step
itself fails with `getaddrinfo ENOTFOUND ***` — not blocking to the
build, but no one gets notified. Populate all of:

- `NOTIFY_SMTP_HOST`, `NOTIFY_SMTP_PORT`
- `NOTIFY_SMTP_USERNAME`, `NOTIFY_SMTP_PASSWORD`
- `NOTIFY_SMTP_SECURE`, `NOTIFY_SMTP_IGNORE_CERT` (booleans)
- `NOTIFY_RECIPIENT` (per-build failure e-mails)
- `DAILY_STATUS_RECIPIENTS` (daily-status e-mail; comma-separated
  addresses accepted)

[send-mail]: https://github.com/dawidd6/action-send-mail

## `DOCKER_TOKEN`

Used by `build-linux-buildenv.yaml` to push the build-environment
container image to Docker Hub. Create at
<https://hub.docker.com/settings/security> with `Read, Write, Delete`
scope on the target repository, and store:

```bash
gh secret set DOCKER_TOKEN --repo con/git-annex
```

## Related repositories

The CI depends on write access to these sibling repositories:

- [`con/git-annex-ci-reports`](https://github.com/con/git-annex-ci-reports)
  — daily-status HTML reports, published at
  <https://con.github.io/git-annex-ci-reports/>.
- [`con/git-annex-ci-client-jobs`](https://github.com/con/git-annex-ci-client-jobs)
  — orphan `build-*` branches carrying the built `.deb` artifact for
  each client machine to pick up and test against.

Both must live under the `con` org and grant the CI credentials above
the write access they need. If either is renamed or moved, the
credentials (fine-grained PAT scope and deploy key registration) must
be re-scoped accordingly.
