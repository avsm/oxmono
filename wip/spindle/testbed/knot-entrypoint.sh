#!/bin/sh
set -eu
ssh-keygen -A
chown git:git /home/git /home/git/repositories
/usr/sbin/sshd -D -e &
exec runuser -u git -- /usr/local/bin/knot server
