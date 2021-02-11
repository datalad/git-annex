#!/bin/bash
set -eu

mkdir -p "$HOME/.ssh"

if command -V docker-machine &> /dev/null
then docker_host="$(docker-machine inspect --format='{{.Driver.IPAddress}}' default)"
else docker_host=localhost
fi

cat >>"$HOME/.ssh/config" <<EOF

Host datalad-test
HostName $docker_host
Port 42241
User dl
StrictHostKeyChecking no
IdentityFile /tmp/dl-test-ssh-id
EOF

cat >>"$HOME/.ssh/config" <<EOF

Host datalad-test2
HostName $docker_host
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
    nc -vz "$docker_host" 42241 && nc -vz "$docker_host" 42242 && break
    ((n++))
    if [ "$n" -lt "$tries" ]
    then sleep 1
         docker ps -a
         echo 'BEGIN datalad-tests-ssh LOGS --------'
         docker logs datalad-tests-ssh
         echo 'END datalad-tests-ssh LOGS --------'
         echo 'BEGIN datalad-tests-ssh2 LOGS --------'
         docker logs datalad-tests-ssh2
         echo 'END datalad-tests-ssh2 LOGS --------'
    else exit 1
    fi
done

ssh -v datalad-test exit
ssh -v datalad-test2 exit
