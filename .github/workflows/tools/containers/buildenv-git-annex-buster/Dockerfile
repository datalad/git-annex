FROM neurodebian:bullseye

# Apparently snapshots.debian.org has heavy rate limiting policy so we better
# provide some apt conf tune ups with hope to make apt more robust in talking to it
# see https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=959518  for more info
# (later apt as now in sid should be more robust)
RUN echo 'Acquire::http::Dl-Limit "1000";' >| /etc/apt/apt.conf.d/20snapshots \
    && echo 'Acquire::https::Dl-Limit "1000";' >> /etc/apt/apt.conf.d/20snapshots \
    && echo 'Acquire::Retries "5";' >> /etc/apt/apt.conf.d/20snapshots

# We need to enable backports to get more recent than 2.30 git.
# ref: https://git-annex.branchable.com/forum/error__58___bogus_format_in_GIT__95__CONFIG__95__PARAMETERS/
RUN set -ex; \
    apt update; \
    sed -i -e 's,^\(.* \)bullseye\( main.*\),\1bullseye\2\n\1bullseye-backports\2,g' /etc/apt/sources.list; \
    apt install neurodebian-freeze; \
    nd_freeze 20220327; \
    sed -i -e 's,^#deb-src,deb-src,g' /etc/apt/sources.list.d/neurodebian.sources.list; \
    apt-get update -qq; \
    export DEBIAN_FRONTEND=noninteractive; \
    apt-get build-dep -y -q git-annex-standalone; \
    apt-get install -y -q -t bullseye-backports git; \
    # Needed additional build-depends which might have not yet in "released" version
    apt-get install -y libghc-criterion-dev libghc-http-client-restricted-dev; \
    # Needed additional tools
    apt-get install -y devscripts quilt; \
    # Some helper utilities just in case
    apt-get install -y vim wget strace time ncdu gnupg curl procps datalad pigz less tree; \
    apt-get clean; \
    rm -rf /var/lib/apt/lists/*
