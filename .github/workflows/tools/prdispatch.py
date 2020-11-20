#!/usr/bin/env python3
# GitHub's REST API doesn't seem to expose when a PR or branch was last pushed
# to, so we have to go through the GraphQL API for that instead.  Interaction
# with workflows still needs to be done through the REST API, though.

__python_requires__ = "~= 3.8"
__requires__ = ["PyGithub == 1.*", "python-dateutil ~= 2.7", "requests ~= 2.20"]

from datetime import datetime, timedelta, timezone
from functools import cached_property
import json
import logging
import os
import sys
from time import sleep
from dateutil.parser import isoparse
from github import Github
import requests

GITHUB_GRAPHQL_URL = "https://api.github.com/graphql"

REPO_OWNER = "datalad"
REPO_NAME = "git-annex"
BASE_BRANCH = "upstream/master"
THIS_WORKFLOW = "build-prs.yml"
THIS_STEP = "Run PR workflow dispatch script"
WORKFLOWS = ["build-macos.yaml", "build-ubuntu.yaml", "build-windows.yaml"]
PAGE_SIZE = 50
DEFAULT_TIME_WINDOW = timedelta(seconds=300)

log = logging.getLogger(__name__)


class PRDispatcher:
    def __init__(self, token):
        self.token = token
        self.s = requests.Session()
        self.s.headers["Authorization"] = f"bearer {token}"

    def run(self):
        since = self.get_last_successful_start_time()
        if since is None:
            since = datetime.now(timezone.utc) - DEFAULT_TIME_WINDOW
        for pr in self.get_active_prs(since):
            log.info("Triggering workflows for PR #%s ...", pr["number"])
            for w in self.workflows:
                w.create_dispatch(ref="master", inputs={"pr": str(pr["number"])})

    def gql_query(self, query, variables=None):
        r = self.s.post(
            GITHUB_GRAPHQL_URL, json={"query": query, "variables": variables or {}}
        )
        r.raise_for_status()
        data = r.json()
        if data.get("errors"):
            raise RuntimeError(
                "GraphQL API replied with error:\n"
                + json.dumps(data["errors"], sort_keys=True, indent=4)
            )
        return data

    def gql_paginate(self, query, variables, conn_path):
        while True:
            data = self.gql_query(query, variables)
            conn = data
            for p in conn_path:
                conn = conn[p]
            yield from conn["nodes"]
            new_cursor = conn["pageInfo"]["endCursor"]
            if conn["pageInfo"]["hasNextPage"]:
                variables = {**variables, "cursor": new_cursor}
            else:
                return

    def get_active_prs(self, since: datetime):
        """
        Returns an iterator of pull requests to ``BASE_BRANCH`` in the
        repository that are open, are mergeable, and have had development
        activity (commits pushed, PR head force-pushed, or PR base changed)
        since ``since``.
        """
        log.info("Fetching open PRs with activity since %s", str(since))
        q = """
            query(
                $repo_owner: String!,
                $repo_name: String!,
                $base_branch: String!,
                $since: DateTime!,
                $page_size: Int!,
                $cursor: String,
            ) {
                repository(owner: $repo_owner, name: $repo_name) {
                    pullRequests(
                        baseRefName: $base_branch,
                        states: [OPEN],
                        orderBy: {field: CREATED_AT, direction: ASC},
                        first: $page_size,
                        after: $cursor,
                    ) {
                        nodes {
                            id
                            number
                            title
                            createdAt
                            mergeable
                            timelineItems(
                                since: $since,
                                itemTypes: [
                                    PULL_REQUEST_COMMIT,
                                    HEAD_REF_FORCE_PUSHED_EVENT,
                                    BASE_REF_CHANGED_EVENT,
                                ],
                            ) {
                                filteredCount
                            }
                        }
                        pageInfo {
                            endCursor
                            hasNextPage
                        }
                    }
                }
            }
        """
        variables = {
            "repo_owner": REPO_OWNER,
            "repo_name": REPO_NAME,
            "base_branch": BASE_BRANCH,
            "since": since.isoformat(),
            "page_size": PAGE_SIZE,
        }
        for pr in self.gql_paginate(
            q, variables, ("data", "repository", "pullRequests"),
        ):
            log.info("Found open PR #%s: %s", pr["number"], pr["title"])
            created = isoparse(pr["createdAt"])
            if created <= since and pr["timelineItems"]["filteredCount"] == 0:
                log.info("No recent commit activity on PR; skipping")
            else:
                while pr["mergeable"] == "UNKNOWN":
                    log.info("Waiting for GitHub to determine PR mergeability ...")
                    sleep(0.1)
                    pr["mergeable"] = self.get_pr_mergeability(pr["id"])
                if pr["mergeable"] != "MERGEABLE":
                    log.info("PR is not mergeable; skipping")
                else:
                    log.info("PR is active & mergeable; proceeding")
                    yield pr

    def get_pr_mergeability(self, prid):
        q = """
            query($prid: ID!) {
                node(id: $prid) {
                    ... on PullRequest {
                        mergeable
                    }
                }
            }
        """
        return self.gql_query(q, {"prid": prid})["data"]["node"]["mergeable"]

    @cached_property
    def repo(self):
        gh = Github(self.token)
        return gh.get_repo(f"{REPO_OWNER}/{REPO_NAME}")

    @cached_property
    def workflows(self):
        return [self.repo.get_workflow(w) for w in WORKFLOWS]

    def get_last_successful_start_time(self):
        for wfrun in self.repo.get_workflow(THIS_WORKFLOW).get_runs():
            if wfrun.status != "completed":
                sys.exit("Previous run has not completed; aborting")
            elif wfrun.conclusion == "success":
                # As of version 1.53, PyGithub does not support getting
                # workflow step data, so we have to do it ourselves.
                r = self.s.get(wfrun.jobs_url)
                r.raise_for_status()
                for step in r.json()["jobs"][0]["steps"]:
                    if step["name"] == THIS_STEP:
                        return isoparse(step["started_at"])
                raise RuntimeError(
                    "Could not find this step in previous successful run"
                )
        return None


def main():
    logging.basicConfig(
        format="%(asctime)s [%(levelname)-8s] %(name)s %(message)s",
        datefmt="%Y-%m-%dT%H:%M:%S%z",
        level=logging.INFO,
    )
    token = os.environ.get("GITHUB_TOKEN")
    if not token:
        sys.exit("GITHUB_TOKEN not set")
    PRDispatcher(token).run()


if __name__ == "__main__":
    main()
