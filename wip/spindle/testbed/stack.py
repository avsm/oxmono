#!/usr/bin/env python3
"""Run knot2 from a sibling Tangled checkout and the OCaml spindle on the
local ATP stack, and test them end to end through the Tangled CLI."""

import argparse
import base64
import os
from pathlib import Path
import secrets
import shutil
import subprocess

ROOT = Path(__file__).resolve().parent
REPO = ROOT.parents[2]
STATE = ROOT / ".state"
ATP = REPO / "wip/atp/testbed"


def run(*args, **kwargs):
    return subprocess.run(args, check=True, **kwargs)


def build(core):
    run("opam", "exec", "--switch=5.2.0+ox", "--", "dune", "build",
        "--profile", "release-check", "wip/spindle/bin/main.exe",
        "wip/atp/bin/tangled/cli/main.exe", cwd=REPO)
    for source, target in (("wip/spindle/bin/main.exe", "spindle"),
                           ("wip/atp/bin/tangled/cli/main.exe", "tangled")):
        temporary = STATE / (target + ".new")
        shutil.copyfile(REPO / "_build/default" / source, temporary)
        temporary.chmod(0o755)
        temporary.replace(STATE / target)
    revision = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], cwd=core, text=True).strip()
    run("docker", "build", "-q", "-f", str(ROOT / "Dockerfile.knot2"),
        "-t", "oxmono-knot2:dev", str(core))
    (STATE / "core-revision").write_text(revision + "\n")


def prepare():
    STATE.mkdir(exist_ok=True)
    (STATE / "stack-spindle").mkdir(exist_ok=True)
    key = STATE / "id_ed25519"
    if not key.exists():
        run("ssh-keygen", "-t", "ed25519", "-N", "", "-C", "tangled-e2e",
            "-f", str(key), "-q")
    master = STATE / "knot-master-key"
    if not master.exists():
        master.write_text(base64.b64encode(secrets.token_bytes(32)).decode())
        master.chmod(0o600)
    (STATE / "passwd").write_text(
        "root:x:0:0::/root:/bin/sh\n"
        f"client:x:{os.getuid()}:{os.getgid()}::/home/client:/bin/sh\n")
    owner = (ATP / ".state/owner-did").read_text().strip()
    (STATE / "stack.env").write_text(
        f"OWNER={owner}\nKNOT_MASTER_KEY={master.read_text().strip()}\n"
        f"UID_GID={os.getuid()}:{os.getgid()}\n")


def compose(*args):
    return run("docker", "compose", "--env-file", str(STATE / "stack.env"),
               "-f", str(ROOT / "stack.yml"), *args)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("up", "test", "down", "reset"))
    parser.add_argument("--core", type=Path, default=REPO.parent / "core",
                        help="Tangled core checkout to build knot2 from")
    args = parser.parse_args()
    if args.command in ("up", "test"):
        run("python3", str(ATP / "run.py"), "up")
        prepare()
        build(args.core.resolve())
        compose("up", "-d", "--build", "--wait", "--wait-timeout", "120",
                "knot", "knot-jetstream", "stack-spindle", "stack-gateway")
        if args.command == "test":
            compose("--profile", "test", "build", "client")
            compose("--profile", "test", "run", "--rm", "--use-aliases",
                    "client")
    else:
        prepare()
        flags = ["--volumes"] if args.command == "reset" else []
        compose("down", "--remove-orphans", *flags)
        if args.command == "reset":
            shutil.rmtree(STATE / "stack-spindle", ignore_errors=True)


if __name__ == "__main__":
    main()
