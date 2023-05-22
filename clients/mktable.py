#!/usr/bin/env python3
"""Generate a table of test status badges for a Markdown README"""
from testannex import parse_clients

BADGE_BASE = "https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges"
BASE_REPORT = "https://datalad.github.io/git-annex-ci-reports/"


def main():
    cfg = parse_clients()
    print("| Client | Test Status |")
    print("| --- | --- |")
    for clientid, cl in cfg.items():
        print(f"| [{clientid}]({BASE_REPORT}#{clientid}) |", end="")
        print(f" ![Overall test status]({BADGE_BASE}/{clientid}.svg)", end="")
        for test in cl.tests.keys():
            print(f" ![{test} test status]({BADGE_BASE}/{clientid}/{test}.svg)", end="")
        print(" |")


if __name__ == "__main__":
    main()
