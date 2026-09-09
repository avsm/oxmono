#!/usr/bin/env python3
"""Compare local Tangled lexicons with a checkout, without network access."""

import argparse
import json
from pathlib import Path
import subprocess


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
    args = parser.parse_args()
    old = lexicons(atp / "lexicons/tangled/json")
    upstream = lexicons(args.upstream / "lexicons")
    new = {k: v for k, v in upstream.items() if k.startswith("sh.tangled.")}
    common = old.keys() & new.keys()
    changed = sorted(k for k in common if old[k] != new[k])
    structural = [k for k in changed if
                  without_descriptions(old[k]) != without_descriptions(new[k])]
    added = sorted(new.keys() - old.keys())
    removed = sorted(old.keys() - new.keys())
    atproto = lexicons(atp / "lexicons/atproto/json")
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
        "other_upstream": {k: {"identical_atproto_copy": atproto.get(k) == v}
                           for k, v in sorted(upstream.items())
                           if k not in new},
    }
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
