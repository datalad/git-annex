FROM debian:trixie

# Apparently snapshots.debian.org has heavy rate limiting policy so we better
# provide some apt conf tune ups with hope to make apt more robust in talking to it
# see https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=959518  for more info
# (later apt as now in sid should be more robust)
RUN echo 'Acquire::http::Dl-Limit "200";' >| /etc/apt/apt.conf.d/20snapshots \
    && echo 'Acquire::https::Dl-Limit "200";' >> /etc/apt/apt.conf.d/20snapshots \
    && echo 'Acquire::Retries "10";' >> /etc/apt/apt.conf.d/20snapshots

# Notes:
# - in APT for NeuroDebian we have #deb-src  for debian-devel, so we need to change
#   that too.
# - APT specification switched away from .list to .sources format in bookworm.
#   nd_freeze works on policy output so works fine but does not disable old source
#   file, so we are moving it to .disabled "manually" here if we do not detect
#   use of snapshot url there (we might implement support that way).
#   Remove after https://github.com/neurodebian/neurodebian/issues/90 is fixed/released
#   for the used date of nd_freeze.
# - Originally used neurodebian, now switched to debian based for trixie
RUN set -ex; \
    apt update; \
    apt install neurodebian-freeze; \
    nd_freeze 20250819; \
    f=/etc/apt/sources.list.d/debian.sources; if [ -e "$f" ] && ! grep -q "^URIs:.*snapshot\." "$f"; then mv "$f" "$f.disabled"; fi; \
    sed -i -e 's,\(^deb\) \(.*\),\1 \2\ndeb-src \2,g' /etc/apt/sources.list.d/*.list; \
    apt-get update -qq; \
    export DEBIAN_FRONTEND=noninteractive; \
    apt-get build-dep -y -q git-annex; \
    # Needed additional tools
    apt-get install -y devscripts quilt; \
    # Some helper utilities just in case
    apt-get install -y vim wget strace time ncdu gnupg curl procps datalad pigz less tree; \
    apt-get clean; \
    rm -rf /var/lib/apt/lists/*
