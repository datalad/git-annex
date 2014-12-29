#! /bin/sh

# This simple script will make sure files downloaded by the
# [Transmission BitTorrent client](https://www.transmissionbt.com/) will
# be added into git-annex.
#
# To enable it, install it to /usr/local/bin and add the following to
# your settings.json:
#    "script-torrent-done-enabled": true,
#    "script-torrent-done-filename": "/usr/local/bin/transmission-git-annex-add",
# -- [[anarcat]]

set -e

# environment from transmission:
# TR_APP_VERSION
# TR_TIME_LOCALTIME
# TR_TORRENT_DIR
# TR_TORRENT_HASH
# TR_TORRENT_ID
# TR_TORRENT_NAME
# source: https://trac.transmissionbt.com/wiki/Scripts

if [ -z "$TR_APP_VERSION" ]; then
  echo "missing expected $TR_APP_VERSION from Transmission"
  exit 1
fi

message="transmission adding torrent '$TR_TORRENT_NAME'

TR_APP_VERSION: $TR_APP_VERSION
TR_TIME_LOCALTIME: $TR_TIME_LOCALTIME
TR_TORRENT_DIR: $TR_TORRENT_DIR
TR_TORRENT_HASH: $TR_TORRENT_HASH
TR_TORRENT_ID: $TR_TORRENT_ID
TR_TORRENT_NAME: $TR_TORRENT_NAME
"

echo $message
cd "$TR_TORRENT_DIR"
git annex add "$TR_TORRENT_NAME" && \
git commit -F- <<EOF
$message
EOF
