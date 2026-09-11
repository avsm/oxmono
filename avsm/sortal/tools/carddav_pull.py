#!/usr/bin/env python3
"""Prepare and apply conservative remote edits against the initial sync baseline.

Supports names, kind, email lists and additional vCard properties. Changes to
other mapped properties are conflicts, not deletions or silently ignored edits.
Requires PyYAML and ruamel.yaml. Never writes to the CardDAV server.
"""

import argparse
from collections import Counter
from copy import deepcopy
import io
import json
import os
from pathlib import Path
import stat
import sys
import tempfile

import yaml
from ruamel.yaml import YAML

import carddav_export as export
import carddav_trial as trial
import sortal_vcard as mapping


MISSING = object()
EDITABLE = {"FN", "EMAIL", "KIND", "X-ADDRESSBOOKSERVER-KIND"}
ADMIN = {"REV", "PRODID"}
EXTRA = {"NICKNAME", "TEL", "NOTE", "BDAY", "ANNIVERSARY", "CATEGORIES",
         "IMPP", "LANG", "GENDER", "RELATED", "KEY", "TZ", "GEO"}


def signatures(properties, normalize_client=False):
    def signature(p):
        params = dict(p["params"])
        if normalize_client:
            params.pop("X-SORTAL-PATH", None)
            params.pop("PROP-ID", None)
        for key in ("TYPE", "VALUE", "ENCODING"):
            if key in params:
                params[key] = ",".join(sorted(params[key].lower().split(",")))
        return ("" if normalize_client else p["group"], p["name"],
                tuple(sorted(params.items())), p["value"])
    return Counter(signature(p) for p in properties)


def wire_header(p):
    params = {k: v for k, v in p["params"].items() if not k.startswith("X-SORTAL-")}
    writer = mapping.Writer("3.0", [])
    writer.add(p["name"], "", group=p["group"] or None, params=params, raw=True)
    return writer.lines[-1][:-1]


def extra_fields(props):
    result = {}
    for p in props:
        if p["name"] not in EXTRA | {"EMAIL"}:
            continue
        key = wire_header(p)
        if key in result:
            raise ValueError("repeated passthrough header needs an explicit unique group")
        result[key] = p["value"]
    return result


def merge_value(base, local, remote, field):
    if remote == base:
        return local
    if local == base or local == remote:
        return remote
    raise ValueError(f"concurrent local/remote edits conflict at {field}")


def remote_record(base, old_data, new_data, uid, store_id):
    old, new = mapping.parse(old_data), mapping.parse(new_data)
    if mapping.untext(mapping.only(new, "UID")["value"]) != uid:
        raise ValueError("remote UID changed")
    for name, expected in (("X-SORTAL-ID", base["handle"]), ("X-SORTAL-STORE", store_id)):
        values = [mapping.untext(p["value"]) for p in new if p["name"] == name]
        if values and values != [expected]:
            raise ValueError("conflicting remote Sortal identity")
    ignored = EDITABLE | ADMIN | EXTRA
    # Regrouping, PROP-ID additions, lost field annotations and token casing
    # do not change Sortal values. Keep the complete remote card in the journal.
    old_fixed = [p for p in old if p["name"] not in ignored]
    new_fixed = [p for p in new if p["name"] not in ignored]
    if signatures(old_fixed, True) != signatures(new_fixed, True):
        raise ValueError("changes outside the supported pull fields require reconciliation")
    candidate = deepcopy(base)
    name = mapping.untext(mapping.only(new, "FN")["value"])
    if not name:
        raise ValueError("empty primary name")
    candidate["names"] = [name, *base["names"][1:]]
    kinds = [p["value"].lower() for p in new if p["name"] in {"KIND", "X-ADDRESSBOOKSERVER-KIND"}]
    if kinds:
        if len(set(kinds)) != 1 or kinds[0] not in {"individual", "org"}:
            raise ValueError("ambiguous remote kind")
        candidate["kind"] = {"individual": "person", "org": "organization"}[kinds[0]]
    emails = [mapping.untext(p["value"]) for p in new if p["name"] == "EMAIL"]
    if len(set(emails)) != len(emails):
        raise ValueError("duplicate remote email values require reconciliation")
    if emails != base.get("emails", []):
        candidate["emails"] = emails
    before_extra, after_extra = extra_fields(old), extra_fields(new)
    extras = deepcopy(base.get("vcard", {}))
    if not isinstance(extras, dict):
        raise ValueError("vcard passthrough must be a header/value mapping")
    for header in sorted(before_extra.keys() | after_extra.keys()):
        before, after = before_extra.get(header, MISSING), after_extra.get(header, MISSING)
        if before == after:
            continue
        if header in extras and extras[header] != before:
            raise ValueError("existing passthrough value differs from the remote baseline")
        if after is MISSING:
            extras.pop(header, None)
        else:
            extras[header] = after
    if extras or "vcard" in base:
        candidate["vcard"] = extras
    return candidate


def merge_records(base, local, candidate):
    result = deepcopy(local)
    for field in sorted(candidate.keys() | base.keys()):
        value = merge_value(base.get(field, MISSING), local.get(field, MISSING),
                            candidate.get(field, MISSING), field)
        if value is MISSING:
            result.pop(field, None)
        else:
            result[field] = deepcopy(value)
    return result


def reconcile(base, local, old_data, new_data, uid, store_id):
    return merge_records(base, local, remote_record(base, old_data, new_data, uid, store_id))


def update_yaml(raw, merged):
    rt = YAML(typ="rt")
    rt.preserve_quotes = True
    rt.allow_duplicate_keys = False
    rt.indent(mapping=2, sequence=4, offset=2)
    if b"\r\n" in raw:
        rt.line_break = "\r\n"
    document = rt.load(raw.decode())
    def update(node, value):
        if mapping.canonical(node) == value:
            return node
        if isinstance(node, dict) and isinstance(value, dict):
            for key in list(node):
                if key not in value:
                    del node[key]
            for key, child in value.items():
                node[key] = update(node[key], child) if key in node else child
            return node
        if isinstance(node, list) and isinstance(value, list):
            for i, child in enumerate(value):
                if i < len(node):
                    node[i] = update(node[i], child)
                else:
                    node.append(child)
            del node[len(value):]
            return node
        return value
    document = update(document, merged)
    output = io.StringIO()
    rt.dump(document, output)
    result = output.getvalue().encode()
    if mapping.canonical(yaml.load(result, Loader=export.UniqueLoader)) != merged:
        raise ValueError("YAML update does not reproduce the planned contact")
    return result


def prepare(bundle, snapshot, source, output, previous=None):
    if output.exists() or output.is_symlink():
        raise ValueError("pull journal directory must be new")
    protected = [source, snapshot, bundle / "originals", bundle / "cards"]
    if previous:
        protected.append(previous)
    if any(output.resolve().is_relative_to(path.resolve()) for path in protected):
        raise ValueError("pull journal must be outside source, snapshot and saved contact/journal trees")
    manifest = export.verify(bundle)
    seed = json.loads((bundle / "fastmail-seed/report.json").read_text())
    current = json.loads((snapshot / "report.json").read_text())
    if seed["account"] != current["account"] or seed["book"]["href"] != current["book"]["href"]:
        raise ValueError("snapshot belongs to a different destination")
    uploaded = {r["uid"]: r for r in seed["results"] if r["status"] == "verified"}
    entries = {r["uid"]: r for r in manifest["contacts"]}
    baselines, visited = {}, set()
    cursor = previous
    while cursor is not None:
        cursor = cursor.resolve()
        if cursor in visited:
            raise ValueError("cycle in previous pull journals")
        if output.resolve().is_relative_to(cursor):
            raise ValueError("pull journal must be outside previous journals")
        visited.add(cursor)
        prior = json.loads((cursor / "report.json").read_text())
        if (prior["status"], prior["account"], prior["book"]["href"], prior["store_id"]) != (
                "applied", seed["account"], seed["book"]["href"], manifest["store_id"]):
            raise ValueError("previous pull is incomplete or belongs to a different destination")
        for change in prior["changes"]:
            uid = change["uid"]
            if uid not in uploaded or change["href"] != uploaded[uid]["href"] or change["status"] != "applied":
                raise ValueError("previous pull has an invalid identity binding")
            raw = (cursor / uid / "remote-get.vcf").read_bytes()
            baseline_name = "common.yaml" if "common_sha256" in change else "after.yaml"
            local = (cursor / uid / baseline_name).read_bytes()
            expected_local = change.get("common_sha256", change["after_sha256"])
            if (export.digest(raw), export.digest(local)) != (change["remote_get_sha256"], expected_local):
                raise ValueError("previous pull baseline checksum mismatch")
            baselines.setdefault(uid, (raw, local))
        cursor = Path(prior["previous_pull"]) if prior.get("previous_pull") else None
    remote = {}
    for path in (snapshot / "before").glob("*.vcf"):
        data = path.read_bytes()
        uid = mapping.untext(mapping.only(mapping.parse(data), "UID")["value"])
        if uid in remote:
            raise ValueError("duplicate snapshot UID")
        remote[uid] = data
    if set(remote) != set(uploaded):
        raise ValueError("new/deleted remote contacts require reconciliation before this pull")
    changes = []
    for uid, binding in uploaded.items():
        entry = entries[uid]
        old_data = (bundle / "fastmail-seed/after" / (uid + ".vcf")).read_bytes()
        if export.digest(old_data) != binding["sha256"]:
            raise ValueError("upload baseline checksum mismatch")
        base_raw = (bundle / "originals" / entry["source"]).read_bytes()
        if uid in baselines:
            old_data, base_raw = baselines[uid]
        if signatures(mapping.parse(old_data)) == signatures(mapping.parse(remote[uid])):
            continue
        local_path = export.safe_path(source, entry["source"])
        if local_path.is_symlink() or not local_path.is_file():
            raise ValueError("source contact must be a regular file")
        before = local_path.read_bytes()
        base = mapping.canonical(yaml.load(base_raw, Loader=export.UniqueLoader))
        local = mapping.canonical(yaml.load(before, Loader=export.UniqueLoader))
        candidate = remote_record(base, old_data, remote[uid], uid, manifest["store_id"])
        merged = merge_records(base, local, candidate)
        # Only changes received from the remote card advance the common
        # baseline. Unrelated local edits remain pending for a later push.
        common = update_yaml(base_raw, candidate) if candidate != base else base_raw
        after = update_yaml(before, merged) if merged != local else before
        projected = mapping.encode(merged, uid, manifest["store_id"], source, export.safe_path)
        reconstructed, photos = mapping.decode(projected)
        if reconstructed != merged or any(raw != export.safe_path(source, p).read_bytes() for p, raw in photos.items()):
            raise ValueError("merged contact does not survive vCard export and reverse mapping")
        changes.append((entry, binding, before, after, common, old_data, remote[uid], projected))
    output.mkdir(mode=0o700)
    report = {"version": 1, "account": seed["account"], "book": seed["book"],
              "source": str(source.resolve()), "bundle": str(bundle.resolve()),
              "previous_pull": str(previous.resolve()) if previous else None,
              "snapshot": str(snapshot.resolve()), "store_id": manifest["store_id"],
              "status": "prepared", "changes": []}
    for entry, binding, before, after, common, old, remote_data, projected in changes:
        directory = output / entry["uid"]
        directory.mkdir(mode=0o700)
        for name, data in (("before.yaml", before), ("after.yaml", after), ("common.yaml", common),
                           ("baseline.vcf", old), ("remote.vcf", remote_data), ("projected.vcf", projected)):
            (directory / name).write_bytes(data)
        report["changes"].append({"uid": entry["uid"], "handle": entry["handle"],
                                  "source": entry["source"], "href": binding["href"],
                                  "before_sha256": export.digest(before), "after_sha256": export.digest(after),
                                  "common_sha256": export.digest(common),
                                  "remote_sha256": export.digest(remote_data),
                                  "status": "unchanged" if before == after else "prepared"})
    trial.save_json(output / "report.json", report)
    print(json.dumps({"changed_remote_cards": len(changes),
                      "local_updates": sum(c["status"] == "prepared" for c in report["changes"]),
                      "handles": [c["handle"] for c in report["changes"]]}, ensure_ascii=False))


def apply(output, username, password_file, dry_run=False, server=None):
    report_path = output / "report.json"
    report = json.loads(report_path.read_text())
    if username != report["account"]:
        raise ValueError("account does not match the prepared pull")
    password = password_file.read_text().strip()
    if not password or "\n" in password or "\r" in password:
        raise ValueError("password file must contain one nonempty password")
    dav = trial.Dav(username, password, root=server or report.get("book", {}).get("href", trial.ROOT), readonly=True)
    preview = {"dry_run": True, "local_updates": [], "unchanged": []}
    for change in report["changes"]:
        directory = output / change["uid"]
        target = export.safe_path(Path(report["source"]), change["source"])
        before, after = (directory / "before.yaml").read_bytes(), (directory / "after.yaml").read_bytes()
        remote = (directory / "remote.vcf").read_bytes()
        if (export.digest(before), export.digest(after), export.digest(remote)) != (
                change["before_sha256"], change["after_sha256"], change["remote_sha256"]):
            raise ValueError("prepared pull checksum mismatch")
        if "common_sha256" in change and export.digest((directory / "common.yaml").read_bytes()) != change["common_sha256"]:
            raise ValueError("prepared common baseline checksum mismatch")
        if target.is_symlink() or not target.is_file():
            raise ValueError("source contact must be a regular file")
        # A previous run may have written the source just before interruption.
        # Verify the prepared result, then finish the same journal entry.
        if target.read_bytes() not in (before, after):
            raise ValueError("local contact changed after preparation")
        status, headers, fetched = dav.request("GET", change["href"])
        if status != 200 or not headers.get("etag") or headers["etag"].startswith("W/"):
            raise ValueError("remote readback failed or lacks a strong ETag")
        if signatures(mapping.parse(fetched)) != signatures(mapping.parse(remote)):
            raise ValueError("remote contact changed after preparation; refetch and replan")
        if dry_run:
            key = "unchanged" if target.read_bytes() == after else "local_updates"
            preview[key].append(change["source"])
            continue
        (directory / "remote-get.vcf").write_bytes(fetched)
        change.update(etag=headers["etag"], remote_get_sha256=export.digest(fetched), status="applying")
        trial.save_json(report_path, report)
        if target.read_bytes() != after:
            mode = stat.S_IMODE(target.stat().st_mode)
            fd, name = tempfile.mkstemp(prefix=".sortal-pull-", dir=target.parent)
            try:
                with os.fdopen(fd, "wb") as stream:
                    os.fchmod(stream.fileno(), mode)
                    stream.write(after)
                    stream.flush()
                    os.fsync(stream.fileno())
                if target.read_bytes() != before:
                    raise ValueError("local contact changed during preparation")
                os.replace(name, target)
            finally:
                if os.path.exists(name):
                    os.unlink(name)
        if target.read_bytes() != after:
            raise ValueError("local readback failed")
        change["status"] = "applied"
        trial.save_json(report_path, report)
    if dry_run:
        print(json.dumps(preview))
        return preview
    report["status"] = "applied"
    trial.save_json(report_path, report)
    print(f"Applied and verified {len(report['changes'])} contact pull(s); no server writes.")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    plan = sub.add_parser("prepare")
    plan.add_argument("bundle", type=Path)
    plan.add_argument("--snapshot", type=Path, required=True)
    plan.add_argument("--source", type=Path, required=True)
    plan.add_argument("--output", type=Path, required=True)
    plan.add_argument("--previous-pull", type=Path, help="last applied pull journal, retaining the common baseline")
    write = sub.add_parser("apply")
    write.add_argument("output", type=Path)
    write.add_argument("--username", required=True)
    write.add_argument("--password-file", type=Path, required=True)
    write.add_argument("--dry-run", action="store_true", help="validate and preview without writing source files or changing the journal")
    write.add_argument("--server", help="HTTPS CardDAV origin; defaults to the prepared collection")
    args = parser.parse_args()
    try:
        if args.command == "prepare":
            prepare(args.bundle, args.snapshot, args.source, args.output, args.previous_pull)
        else:
            apply(args.output, args.username, args.password_file, args.dry_run, args.server)
    except Exception as error:
        print(f"Pull failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
