### Please describe the problem.
This is more of a feature request then a bug. 

It would be nice and more intuitive if the webapp --listen parameter accepted a port specifier too allowing configuration of the port. 

For my workflow, I thought I would contain all of git annexes dependencies inside a docker image since I'm quite comfortable with docker(and emerge on gentoo took a long time and finally failed). With an unconfigurable dynamic port though, it makes running the webapp subcommand in docker not really viable since I don't want to use docker run's port range mapping feature which will lock all those ports.

### What steps will reproduce the problem?
Dockerfile:
ARG DEBIAN_TAG=buster-slim
FROM debian:${DEBIAN_TAG}

RUN set -ex \
  && apt-get update \
  && apt-get install -y \
     git \
     git-annex

docker build -t git-annex:test .
docker run --rm -it git-annex:test git annex webapp --listen 0.0.0.0:8888

### What version of git-annex are you using? On what operating system?

git-annex version: 7.20190129
build flags: Assistant Webapp Pairing S3(multipartupload)(storageclasses) WebDAV Inotify DBus DesktopNotify TorrentParser MagicMime Feeds Testsuite
dependency versions: aws-0.20 bloomfilter-2.0.1.0 cryptonite-0.25 DAV-1.3.3 feed-1.0.0.0 ghc-8.4.4 http-client-0.5.13.1 persistent-sqlite-2.8.2 torrent-10000.1.1 uuid-1.3.13 yesod-1.6.0


### Please provide any additional information below.

[[!format sh """
# If you can, paste a complete transcript of the problem occurring here.
# If the problem is with the git-annex assistant, paste in .git/annex/daemon.log


# End of transcript or log.
"""]]

### Have you had any luck using git-annex before? (Sometimes we get tired of reading bug reports all day and a lil' positive end note does wonders)
git annex seems awesome with the little bit of testing I've done. It seems like the perfect tool for what I want to accomplish. Thanks!

