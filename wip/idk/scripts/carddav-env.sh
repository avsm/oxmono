#!/bin/sh
# Print the environment for the server started by carddav-up.sh.
echo "export CARDDAV_ORACLE_URL=http://localhost:${CARDDAV_ORACLE_HTTP_PORT:-15232}/"
