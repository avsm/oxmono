#!/bin/sh
# Print the environment for the oracle started by oracle-up.sh.
echo "export JMAP_ORACLE_URL=http://localhost:${JMAP_ORACLE_HTTP_PORT:-18080}/.well-known/jmap"
echo "export JMAP_ORACLE_LMTP=localhost:${JMAP_ORACLE_LMTP_PORT:-18024}"
