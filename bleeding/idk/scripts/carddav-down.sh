#!/bin/sh
docker rm -f "${1:-carddav-oracle}" >/dev/null 2>&1 && echo "stopped"
