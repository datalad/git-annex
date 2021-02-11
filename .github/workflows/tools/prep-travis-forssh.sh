#!/bin/bash
set -eu

mkdir -p "$HOME/.ssh"

cat >>"$HOME/.ssh/config" <<'EOF'

Host datalad-test
HostName localhost
Port 42241
User dl
StrictHostKeyChecking no
IdentityFile /tmp/dl-test-ssh-id
EOF

cat >>"$HOME/.ssh/config" <<'EOF'

Host datalad-test2
HostName localhost
Port 42242
User dl
StrictHostKeyChecking no
IdentityFile /tmp/dl-test-ssh-id
EOF

ls -l "$HOME/.ssh"
chmod go-rwx -R "$HOME/.ssh"
ls -ld "$HOME/.ssh"
ls -l "$HOME/.ssh"

ssh-keygen -f /tmp/dl-test-ssh-id -N ""

sh "$(dirname "$0")"/setup-docker-ssh --key=/tmp/dl-test-ssh-id.pub -2

tries=60
n=0
while true
do
    nc -vz localhost 42241 && nc -vz localhost 42242 && break
    ((n++))
    if [ "$n" -lt "$tries" ]
    then sleep 1
    else exit 1
    fi
done

ssh -v datalad-test exit
ssh -v datalad-test2 exit
