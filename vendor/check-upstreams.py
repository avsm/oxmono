#!/usr/bin/env python3
"""Compare recorded vendor bases with upstream branch tips without fetching trees."""

import argparse
from concurrent.futures import ThreadPoolExecutor
import json
import os
from pathlib import Path
import re
import subprocess
import sys


MANIFEST = Path(__file__).with_name("upstreams.json")
OID = re.compile(r"[0-9a-f]{40}(?:[0-9a-f]{24})?\Z")


def parse_tip(output, branch):
    """Return the advertised branch and commit, resolving HEAD's symbolic ref."""
    target = "HEAD" if branch == "HEAD" else "refs/heads/" + branch
    resolved = branch
    revision = None
    for line in output.splitlines():
        value, sep, name = line.partition("\t")
        if not sep or name != target:
            continue
        if value.startswith("ref: refs/heads/"):
            resolved = value.removeprefix("ref: refs/heads/")
        elif OID.fullmatch(value):
            revision = value
    if revision is None:
        raise ValueError(f"upstream did not advertise {target}")
    return resolved, revision


def check(entry, timeout):
    result = {"name": entry["name"], "base": entry.get("revision"),
              "branch": entry.get("branch", "HEAD"), "tip": None}
    target = "HEAD" if result["branch"] == "HEAD" else "refs/heads/" + result["branch"]
    try:
        output = subprocess.run(
            ["git", "ls-remote", "--symref", "--", entry["url"], target],
            capture_output=True, text=True, check=True, timeout=timeout,
            env={**os.environ, "GIT_TERMINAL_PROMPT": "0"},
        ).stdout
        result["branch"], result["tip"] = parse_tip(output, result["branch"])
        result["status"] = ("unknown-base" if result["base"] is None else
                            "current" if result["base"] == result["tip"] else "changed")
    except (OSError, ValueError, subprocess.SubprocessError) as ex:
        result["status"] = "error"
        result["error"] = (ex.stderr.strip() if isinstance(ex, subprocess.CalledProcessError)
                           else str(ex))
    return result


def load_manifest(path):
    entries = json.loads(path.read_text())["vendors"]
    seen = set()
    for entry in entries:
        name = entry["name"]
        if name in seen or not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]*", name):
            raise ValueError(f"duplicate or invalid vendor name: {name!r}")
        seen.add(name)
        if not isinstance(entry["url"], str) or not entry["url"]:
            raise ValueError(f"missing URL for {name}")
        if entry.get("revision") is not None and not OID.fullmatch(entry["revision"]):
            raise ValueError(f"invalid base revision for {name}")
        branch = entry.get("branch", "HEAD")
        if not isinstance(branch, str) or not branch or any(c.isspace() for c in branch):
            raise ValueError(f"invalid branch for {name}")
    directories = {p.name for p in path.parent.iterdir() if p.is_dir() and not p.name.startswith((".", "__"))}
    if directories != seen:
        raise ValueError(f"manifest coverage mismatch: unrecorded={sorted(directories - seen)}, "
                         f"missing directories={sorted(seen - directories)}")
    return sorted(entries, key=lambda entry: entry["name"])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("names", nargs="*", help="vendors to check (default: all)")
    parser.add_argument("--jobs", type=int, default=8, help="concurrent Git queries (default: 8)")
    parser.add_argument("--timeout", type=float, default=30, help="seconds per query (default: 30)")
    parser.add_argument("--json", action="store_true", help="emit full revisions as JSON")
    args = parser.parse_args()
    if args.jobs < 1 or args.timeout <= 0:
        parser.error("--jobs and --timeout must be positive")
    try:
        entries = load_manifest(MANIFEST)
    except (OSError, ValueError, KeyError, TypeError) as ex:
        parser.error(str(ex))
    unknown = set(args.names) - {entry["name"] for entry in entries}
    if unknown:
        parser.error("unknown vendors: " + ", ".join(sorted(unknown)))
    if args.names:
        entries = [entry for entry in entries if entry["name"] in args.names]
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        results = list(pool.map(lambda entry: check(entry, args.timeout), entries))
    if args.json:
        print(json.dumps(results, indent=2))
    else:
        print(f'{"Vendor":20} {"Base":12} {"Tip":12} {"Branch":16} Status')
        for row in results:
            print(f'{row["name"]:20} {(row["base"] or "unknown")[:12]:12} '
                  f'{(row["tip"] or "-")[:12]:12} {row["branch"]:16} {row["status"]}')
            if "error" in row:
                print(f'  {row["error"]}', file=sys.stderr)
    # A changed tip is not necessarily a descendant; report it without guessing ancestry.
    return 2 if any(row["status"] == "error" for row in results) else int(
        any(row["status"] != "current" for row in results))


if __name__ == "__main__":
    sys.exit(main())
