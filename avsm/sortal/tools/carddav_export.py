#!/usr/bin/env python3
"""Offline Sortal V2 migration bundle. Requires PyYAML; never contacts a server.

Each Sortal field maps to a vCard property or a small named extension. The
originals/ archive retains exact YAML, photos, feed caches, annotations and
Git files. See ../spec/carddav-migration.md for sync semantics.
"""

import argparse
import base64
import datetime
import hashlib
import json
from pathlib import Path
import shutil
import stat
import sys
import uuid
from urllib.parse import urlsplit

import yaml

import sortal_vcard as mapping


class UniqueLoader(yaml.SafeLoader):
    """Reject duplicate YAML keys instead of silently discarding a value."""


def unique_mapping(loader, node, deep=False):
    result = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node, deep=deep)
        if key in result:
            raise ValueError(f"duplicate YAML key: {key}")
        result[key] = loader.construct_object(value_node, deep=deep)
    return result


UniqueLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
                             unique_mapping)


def digest(data):
    return hashlib.sha256(data).hexdigest()


def read_properties(data):
    result = {}
    for p in mapping.parse(data):
        result.setdefault(p["name"], []).append(p["value"])
    return result


def one(props, name):
    values = props.get(name, [])
    if len(values) != 1:
        raise ValueError(f"expected exactly one {name}, found {len(values)}")
    return mapping.untext(values[0])


def safe_path(root, relative):
    p = Path(relative)
    if p.is_absolute() or not p.parts or ".." in p.parts:
        raise ValueError(f"not a relative path within the bundle: {relative}")
    target = root / p
    if not target.resolve().is_relative_to(root.resolve()):
        raise ValueError(f"path escapes root: {relative}")
    return target


def inventory(root):
    paths = sorted(root.rglob("*"))
    for path in paths:
        mode = path.lstat().st_mode
        if not (stat.S_ISREG(mode) or stat.S_ISDIR(mode)):
            raise ValueError(f"cannot archive special file or symlink: {path}")
    return [p.relative_to(root).as_posix() for p in paths if p.is_file()]


def verify(bundle, source=None):
    manifest = json.loads((bundle / "manifest.json").read_text())
    if manifest["version"] not in {1, 2}:
        raise ValueError("unsupported manifest version")
    originals = bundle / "originals"
    expected = set(manifest["files"])
    if set(inventory(originals)) != expected:
        raise ValueError("snapshot file inventory differs from manifest")
    if source and set(inventory(source)) != expected:
        raise ValueError("source file inventory changed")
    for name, entry in manifest["files"].items():
        raw = safe_path(originals, name).read_bytes()
        if len(raw) != entry["bytes"] or digest(raw) != entry["sha256"]:
            raise ValueError(f"snapshot checksum mismatch: {name}")
        if source and safe_path(source, name).read_bytes() != raw:
            raise ValueError(f"source differs from snapshot: {name}")
    cards = []
    seen = set()
    for entry in manifest["contacts"]:
        data = safe_path(bundle, entry["card"]).read_bytes()
        if digest(data) != entry["sha256"]:
            raise ValueError(f"vCard checksum mismatch: {entry['card']}")
        props = read_properties(data)
        if one(props, "UID") != entry["uid"] or entry["uid"] in seen:
            raise ValueError("UID mismatch or duplicate")
        seen.add(entry["uid"])
        if (one(props, "X-SORTAL-ID") != entry["handle"]
                or one(props, "X-SORTAL-STORE") != manifest["store_id"]):
            raise ValueError("Sortal identity mismatch")
        raw = safe_path(originals, entry["source"]).read_bytes()
        contact = yaml.load(raw, Loader=UniqueLoader)
        if manifest["version"] == 1:
            # Read the first archival export solely to retain its identity
            # bindings when upgrading; new exports never emit this property.
            meta = json.loads(one(props, "X-SORTAL-META"))
            if (meta["version"] != 1 or meta["source"] != entry["source"]
                    or meta["yaml"].encode("utf-8") != raw or meta["sha256"] != digest(raw)):
                raise ValueError("embedded source does not round-trip exactly")
            photo = contact.get("photo")
            if photo and not urlsplit(photo).scheme:
                if base64.b64decode(one(props, "PHOTO"), validate=True) != safe_path(originals, photo).read_bytes():
                    raise ValueError("embedded photo does not round-trip exactly")
        else:
            if "X-SORTAL-META" in props:
                raise ValueError("field mapping must not contain a serialized contact payload")
            decoded, photos = mapping.decode(data)
            if decoded != mapping.canonical(contact):
                raise ValueError(f"{entry['source']}: field mapping does not reconstruct the complete contact")
            for filename, photo in photos.items():
                if photo != safe_path(originals, filename).read_bytes():
                    raise ValueError("embedded photo does not round-trip exactly")
        cards.append(data)
    if (bundle / "contacts.vcf").read_bytes() != b"".join(cards):
        raise ValueError("combined vCard file differs from individual cards")
    return manifest


def export(source, output, previous=None, renames=(), as_of=None, vcard_version="3.0"):
    source, output = source.resolve(), output.absolute()
    if not source.is_dir() or output.exists() or output.is_symlink():
        raise ValueError("source must be a directory and output must not exist")
    if output.resolve().is_relative_to(source) or source.is_relative_to(output.resolve()):
        raise ValueError("source and output must be separate trees")
    old = verify(previous) if previous else None
    store_id = old["store_id"] if old else str(uuid.uuid4())
    identities = {c["handle"]: c["uid"] for c in old["contacts"]} if old else {}
    renamed, rename_targets = set(), set()
    for spec in renames:
        before, after = spec.split("=", 1)
        if before not in identities or after in identities or not after:
            raise ValueError(f"invalid identity rename: {spec}")
        identities[after] = identities.pop(before)
        renamed.add(before)
        rename_targets.add(after)
    as_of = as_of or datetime.date.today()
    files = inventory(source)
    contact_files = [p for p in files if "/" not in p and Path(p).suffix in {".yaml", ".yml"}]
    if not contact_files:
        raise ValueError("no contact YAML files found")
    manifest = {"version": 2, "vcard_version": vcard_version, "store_id": store_id, "as_of": as_of.isoformat(),
                "source": str(source), "files": {}, "contacts": []}
    output.mkdir(mode=0o700)
    try:
        originals = output / "originals"
        originals.mkdir(mode=0o700)
        (output / "cards").mkdir(mode=0o700)
        for name in files:
            src, dst = safe_path(source, name), safe_path(originals, name)
            dst.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
            shutil.copy2(src, dst)
            raw = dst.read_bytes()
            manifest["files"][name] = {"bytes": len(raw), "sha256": digest(raw)}
        cards, handles = [], set()
        for name in contact_files:
            raw = safe_path(originals, name).read_bytes()
            contact = yaml.load(raw, Loader=UniqueLoader)
            if not isinstance(contact, dict) or contact.get("version") != 2:
                raise ValueError(f"{name}: this exporter requires Sortal V2")
            handle = contact["handle"]
            if not isinstance(handle, str) or not handle or handle in handles or handle in renamed:
                raise ValueError(f"{name}: invalid, duplicate or old renamed handle")
            handles.add(handle)
            uid = identities.get(handle) or str(uuid.uuid5(uuid.UUID(store_id), handle))
            warnings = []
            data = mapping.encode(contact, uid, store_id, originals, safe_path, vcard_version, warnings)
            card_path = f"cards/{uid}.vcf"
            (output / card_path).write_bytes(data)
            manifest["contacts"].append({"handle": handle, "uid": uid,
                                         "source": name, "card": card_path,
                                         "sha256": digest(data), "warnings": warnings})
            cards.append(data)
        if not rename_targets.issubset(handles):
            raise ValueError("renamed handle is absent from the source")
        (output / "contacts.vcf").write_bytes(b"".join(cards))
        (output / "manifest.json").write_text(json.dumps(manifest, ensure_ascii=False, indent=2) + "\n")
        verify(output, source)
    except BaseException:
        # This directory was created exclusively by this invocation.
        shutil.rmtree(output)
        raise
    return manifest


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    create = commands.add_parser("export", help="create and verify a new local bundle")
    create.add_argument("source", type=Path)
    create.add_argument("output", type=Path)
    create.add_argument("--previous", type=Path, help="previous bundle, required to retain identities on re-export")
    create.add_argument("--rename", action="append", default=[], metavar="OLD=NEW", help="retain a previous UID across a handle rename")
    create.add_argument("--as-of", type=datetime.date.fromisoformat, help="reference date recorded in the manifest (full affiliation history is exported)")
    create.add_argument("--vcard-version", choices=["3.0", "4.0"], default="3.0", help="3.0 for Fastmail compatibility, 4.0 for standard social profiles and alternate names")
    check = commands.add_parser("verify", help="verify source files and reconstruct contacts from vCard fields")
    check.add_argument("bundle", type=Path)
    check.add_argument("--source", type=Path, help="also compare every original file with the source root")
    args = parser.parse_args()
    try:
        if args.command == "export":
            manifest = export(args.source, args.output, args.previous, args.rename, args.as_of, args.vcard_version)
        else:
            manifest = verify(args.bundle, args.source)
    except (ValueError, KeyError, TypeError, OSError, yaml.YAMLError) as error:
        print(f"Error: {error}", file=sys.stderr)
        return 1
    print(f"Verified {len(manifest['contacts'])} vCards and {len(manifest['files'])} original files; no network operations.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
