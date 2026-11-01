#!/usr/bin/env python3
"""Manage the persistent local ATProto development stack."""

import argparse
from pathlib import Path
import subprocess
import tempfile

from prepare import main as prepare


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("pull", "up", "test", "down", "reset"))
    args = parser.parse_args()
    root = Path(__file__).resolve().parent
    prepare()
    compose = ["docker", "compose", "-f", str(root / "compose.yml")]
    if args.command == "pull":
        # These images are public. Use anonymous access rather than a user's
        # unrelated GHCR credentials, which can deny otherwise public pulls.
        with tempfile.TemporaryDirectory(prefix="oxmono-docker-") as config:
            subprocess.run(["docker", "--config", config, "compose", "-f",
                            str(root / "compose.yml"), "--profile", "test", "pull"],
                           check=True)
    elif args.command in ("up", "test"):
        subprocess.run(compose + ["up", "-d", "--pull", "never", "--wait",
                                  "--wait-timeout", "90"], check=True)
        if args.command == "test":
            subprocess.run(compose + ["run", "--rm", "--pull", "never", "test"],
                           check=True)
    else:
        flags = ["--volumes"] if args.command == "reset" else []
        subprocess.run(compose + ["down", "--remove-orphans"] + flags, check=True)


if __name__ == "__main__":
    main()
