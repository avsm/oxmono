#!/bin/sh
set -eu
mkdir -p /var/dav/data /var/dav/locks
exec httpd -DFOREGROUND
