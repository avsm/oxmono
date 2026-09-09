#!/usr/bin/env python3
"""Build and test the OCaml spindle on the local ATP Docker network."""

import argparse
from pathlib import Path
import subprocess

from prepare import main as prepare


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("test", "up", "down"))
    args = parser.parse_args()
    root = Path(__file__).resolve().parent
    repo = root.parents[2]
    compose = ["docker", "compose", "--env-file", str(root / ".state/compose.env"),
               "-f", str(root / "compose.yml")]
    if args.command == "down":
        subprocess.run(compose + ["down"], check=True)
        return
    bootstrap = "up" if (repo / "bleeding/atp/testbed/.state/owner-did").exists() \
        else "test"
    subprocess.run(["python3", "bleeding/atp/testbed/run.py", bootstrap],
                   cwd=repo, check=True)
    subprocess.run(["opam", "exec", "--switch=5.2.0+ox", "--", "dune", "build",
                    "--profile", "release-check", "@bleeding/spindle/all"],
                   cwd=repo, check=True)
    prepare()
    subprocess.run(compose + ["build", "spindle"], check=True)
    subprocess.run(compose + ["up", "-d", "spindle"], check=True)
    if args.command == "test":
        subprocess.run(compose + ["run", "--rm", "test"], check=True)
        subprocess.run(compose + ["restart", "spindle"], check=True)
        subprocess.run(compose + ["run", "--rm", "test", "--recovery"], check=True)
        subprocess.run(compose + ["up", "-d", "slow"], check=True)
        try:
            subprocess.run(compose + ["run", "--rm", "test", "--lifecycle"],
                           check=True)
            subprocess.run(compose + ["restart", "slow"], check=True)
            subprocess.run(compose + ["run", "--rm", "test", "--interrupted"],
                           check=True)
        finally:
            subprocess.run(compose + ["rm", "-sf", "slow"], check=True)


if __name__ == "__main__":
    main()
