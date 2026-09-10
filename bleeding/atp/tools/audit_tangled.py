#!/usr/bin/env python3
"""Compare local Tangled lexicons with a checkout, without network access."""

import argparse
import json
from pathlib import Path
import subprocess
import shutil


def lexicons(root):
    result = {}
    for path in sorted(root.rglob("*.json")):
        document = json.loads(path.read_text())
        nsid = document["id"]
        if nsid in result:
            raise ValueError(f"duplicate lexicon: {nsid}")
        result[nsid] = document
    return result


def without_descriptions(value):
    if isinstance(value, dict):
        return {k: without_descriptions(v) for k, v in value.items()
                if k != "description" or not isinstance(v, str)}
    if isinstance(value, list):
        return [without_descriptions(v) for v in value]
    return value


def changes(old, new, path=""):
    if isinstance(old, dict) and isinstance(new, dict):
        result = []
        for key in sorted(old.keys() | new.keys()):
            pointer = path + "/" + key.replace("~", "~0").replace("/", "~1")
            if key not in old:
                result.append({"path": pointer, "added": new[key]})
            elif key not in new:
                result.append({"path": pointer, "removed": old[key]})
            else:
                result.extend(changes(old[key], new[key], pointer))
        return result
    if old == new:
        return []
    return [{"path": path, "old": old, "new": new}]


def revision(path):
    return subprocess.check_output(
        ["git", "-C", str(path), "rev-parse", "HEAD"], text=True).strip()


def main():
    atp = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("upstream", type=Path)
    parser.add_argument("--sync", action="store_true",
                        help="replace the vendored Tangled documents")
    parser.add_argument("--check", action="store_true",
                        help="fail if any vendored document differs")
    args = parser.parse_args()
    destination = atp / "lexicons/tangled/json"
    upstream = lexicons(args.upstream / "lexicons")
    # Include the shared strongRef dependency supplied by Tangled itself.
    new = upstream
    if args.sync:
        # Parse every document before changing the vendored tree.
        for path in destination.rglob("*.json"):
            path.unlink()
        for source in sorted((args.upstream / "lexicons").rglob("*.json")):
            if json.loads(source.read_text())["id"] in new:
                target = destination / source.relative_to(args.upstream / "lexicons")
                target.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(source, target)
    old = lexicons(destination)
    common = old.keys() & new.keys()
    changed = sorted(k for k in common if old[k] != new[k])
    structural = [k for k in changed if
                  without_descriptions(old[k]) != without_descriptions(new[k])]
    added = sorted(new.keys() - old.keys())
    removed = sorted(old.keys() - new.keys())
    report = {
        "oxmono_revision": revision(atp),
        "upstream_revision": revision(args.upstream),
        "counts": {"local": len(old), "upstream": len(new),
                   "added": len(added), "removed": len(removed),
                   "changed": len(changed), "structural": len(structural),
                   "unchanged": len(common) - len(changed)},
        "added": added,
        "removed": removed,
        "description_only": sorted(set(changed) - set(structural)),
        "changes": {k: changes(old[k], new[k]) for k in changed},
    }
    print(json.dumps(report, indent=2))
    if args.check and (added or removed or changed):
        raise SystemExit(1)


if __name__ == "__main__":
    main()
