#!/usr/bin/env python3
"""Run the Tangled CLI against the existing local PLC/PDS/knot/spindle stack."""
from pathlib import Path
import os
import subprocess

ROOT = Path(__file__).resolve().parents[5]
BINARY = ROOT / "_build/default/bleeding/atp/bin/tangled/cli/main.exe"
CA = ROOT / "bleeding/atp/testbed/.state/certs/ca.crt"
FIXTURE = ROOT / "bleeding/spindle/testbed/.state"
subprocess.run([
    "docker", "run", "--rm", "--network", "oxmono-atp_default",
    "--read-only", "--tmpfs", "/tmp:exec", "--user", f"{os.getuid()}:{os.getgid()}",
    "--cap-drop", "ALL", "--security-opt", "no-new-privileges",
    "--entrypoint", "python3", "-e", "SSL_CERT_FILE=/certs/ca.crt",
    "-v", f"{BINARY}:/test/tangled:ro",
    "-v", f"{CA}:/certs/ca.crt:ro",
    "-v", f"{FIXTURE / 'id_ed25519'}:/test/id_ed25519:ro",
    "-v", f"{FIXTURE / 'fixture'}:/fixture:ro",
    "-v", f"{Path(__file__).with_name('docker_test.py')}:/test/docker_test.py:ro",
    "oxmono-tangled:dev", "/test/docker_test.py",
], check=True)
