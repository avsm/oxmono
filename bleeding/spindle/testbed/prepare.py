#!/usr/bin/env python3
"""Prepare a local Git fixture and the host-built spindle image context."""

import os
from pathlib import Path
import shutil
import subprocess


def main():
    root = Path(__file__).resolve().parent
    repo = root.parents[2]
    state = root / ".state"
    state.mkdir(exist_ok=True)
    fixture = state / "fixture"
    fixture.mkdir(exist_ok=True)
    (state / "data").mkdir(exist_ok=True)
    (state / "slow-data").mkdir(exist_ok=True)
    if not (fixture / ".git").exists():
        subprocess.run(["git", "init", "-b", "main", str(fixture)], check=True)
        (fixture / "README.md").write_text("Local spindle checkout fixture.\n")
        (fixture / "spindle-marker.txt").write_text("OCaml inspection works.\n")
        subprocess.run(["git", "-C", str(fixture), "add", "."], check=True)
        subprocess.run(["git", "-C", str(fixture), "-c", "user.name=Spindle Test",
                        "-c", "user.email=spindle@tangled.test", "commit",
                        "-m", "Create local spindle fixture"], check=True)
    sha = subprocess.check_output(
        ["git", "-C", str(fixture), "rev-parse", "HEAD"], text=True).strip()
    (state / "commit").write_text(sha + "\n")
    owner = (repo / "bleeding/atp/testbed/.state/owner-did").read_text().strip()
    (state / "compose.env").write_text(
        f"SPINDLE_OWNER={owner}\nSPINDLE_UID={os.getuid()}\n"
        f"SPINDLE_GID={os.getgid()}\n")
    binary = repo / "_build/default/bleeding/spindle/bin/main.exe"
    temporary = state / "spindle.new"
    shutil.copyfile(binary, temporary)
    temporary.chmod(0o755)
    temporary.replace(state / "spindle")
    slow = repo / "_build/default/bleeding/spindle/test/slow_server.exe"
    shutil.copyfile(slow, state / "slow-server.new")
    (state / "slow-server.new").chmod(0o755)
    (state / "slow-server.new").replace(state / "slow-server")
    print(f"Prepared {sha} for {owner}", flush=True)


if __name__ == "__main__":
    main()
