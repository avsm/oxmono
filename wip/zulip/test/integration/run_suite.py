#!/usr/bin/env python3
"""Run both live suites, even if the first reports a failure."""
import subprocess
import sys

server, echo, preflight = sys.argv[1:]
api_result = subprocess.run([server, '--color=never']).returncode
cli_result = subprocess.run([sys.executable, 'cli_probe.py', echo, preflight]).returncode
sys.exit(0 if api_result == cli_result == 0 else 1)
