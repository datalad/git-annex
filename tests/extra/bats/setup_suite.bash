# Suite-wide setup for tests/extra/bats/.  Bats-core loads this file
# automatically when running a directory and calls setup_suite once
# before any test.  See <https://bats-core.readthedocs.io/en/stable/writing-tests.html#setup-and-teardown-pre--and-post-test-hooks>.

setup_suite() {
    load helpers
    print_versions
}
