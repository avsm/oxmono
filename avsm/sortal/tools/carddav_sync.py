#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = ["PyYAML>=6,<7", "ruamel.yaml>=0.18,<0.19"]
# ///
"""Preview current Sortal contacts against a CardDAV server without applying changes.

Dry-run is the default and the only operation of this combined preview command.
It creates a private report and fresh export, uses a read-only DAV transport,
and never applies source changes or advances synchronization journals.
"""

import argparse
import contextlib
import difflib
import html
import io
import json
from pathlib import Path
import sys
from types import SimpleNamespace

import carddav_export as export
import carddav_pull as pull
import carddav_trial as trial


def cell(value):
    return html.escape(str(value)).replace("|", "&#124;").replace("\n", " ").replace("\r", " ")


def markdown_report(report):
    counts, pulled = report["upload_counts"], report["pull"]
    lines = ["# CardDAV dry run", "",
             f"Account: {cell(report['account'])}. Collection: {cell(report['book']['href'])}.", "",
             "No contact writes were performed. The preview is saved in this new report directory; "
             "the source, server contacts, and existing sync journals were unchanged.", "",
             "| Upload decision | Contacts |", "| --- | ---: |",
             *[f"| {label} | {counts[key]} |" for key, label in
               (("create", "Would create"), ("unchanged", "Unchanged"), ("review", "Needs review"))], "",
             "Existing contacts are not automatically merged or overwritten. Name, email or "
             "account matches are held for review. Unlinked server contacts are retained.", "",
             "## Pull preview", "", cell(pulled["message"]), ""]
    if pulled["status"] == "planned":
        lines += [f"Would update {pulled['local_updates']} local contact(s).", ""]
        for change in pulled["changes"]:
            lines += [f"- {cell(change['handle'])}: [{cell(change['source'])} diff]"
                      f"(pull/{change['uid']}/changes.diff)"]
        lines += [""]
    lines += ["## Upload decisions", "", "| Sortal ID | Action | Reason / candidates |", "| --- | --- | --- |"]
    for row in report["upload_plan"]:
        details = [row.get("reason", "")]
        details += [f"local: {handle}" for handle in row.get("source_candidates", [])]
        details += row.get("candidates", [])
        lines.append(f"| {cell(row['handle'])} | {cell(row['action'])} | {cell('; '.join(filter(None, details)))} |")
    lines += ["", "## Files", "",
              "- [Full plan](report.json)", "- [Server snapshot and candidate details](server/report.json)",
              "- [Fresh export manifest](export/manifest.json)", "",
              "The fresh export includes current source edits and retains UIDs from the supplied bundle. "
              "This is a plan against the fetched server snapshot; it does not guarantee how a server "
              "or editor would transform an actual write.", ""]
    return "\n".join(lines)


def run(args):
    source, bundle, output = args.source.resolve(), args.bundle.resolve(), args.report.absolute()
    trial.Dav.url_origin(args.server)
    if output.exists() or output.is_symlink():
        raise ValueError("report directory must be new")
    protected = [source, bundle / "originals", bundle / "cards"]
    if args.previous_pull:
        protected.append(args.previous_pull.resolve())
        prior = json.loads((args.previous_pull / "report.json").read_text())
        if prior["account"] != args.username or prior["status"] != "applied":
            raise ValueError("previous pull must be applied and belong to the selected account")
    if any(output.resolve().is_relative_to(path) or path.is_relative_to(output.resolve()) for path in protected):
        raise ValueError("report must be outside the source and saved contact/journal trees")
    export.verify(bundle)
    output.mkdir(mode=0o700)
    # A fresh export is essential: a previous bundle may predate a pulled edit.
    export.export(source, output / "export", previous=bundle)
    with contextlib.redirect_stdout(io.StringIO()):
        remote = trial.run(SimpleNamespace(
            bundle=output / "export", username=args.username, password_file=args.password_file,
            report=output / "server", server=args.server, collection=args.collection,
            dry_run=True, apply=False))
    counts = {action: sum(row["action"] == action for row in remote["plan"])
              for action in ("create", "unchanged", "review")}
    pulled = {"status": "unavailable", "local_updates": 0, "changes": [],
              "message": "No verified sync baseline for this account and collection. "
                         "Existing contacts are reviewed for matches; a common baseline is required to preview pull merges."}
    seed_path = bundle / "fastmail-seed/report.json"
    seed = json.loads(seed_path.read_text()) if seed_path.exists() else None
    matching = seed and (seed["account"], seed["book"]["href"]) == (args.username, remote["book"]["href"])
    if args.previous_pull and (not matching or prior["book"]["href"] != remote["book"]["href"]):
        raise ValueError("previous pull cannot be used for this account and collection")
    if matching:
        try:
            with contextlib.redirect_stdout(io.StringIO()):
                pull.prepare(bundle, output / "server", source, output / "pull", args.previous_pull)
            journal = json.loads((output / "pull/report.json").read_text())
            for change in journal["changes"]:
                directory = output / "pull" / change["uid"]
                diff = difflib.unified_diff(
                    (directory / "before.yaml").read_text().splitlines(keepends=True),
                    (directory / "after.yaml").read_text().splitlines(keepends=True),
                    fromfile=change["source"] + " (current)", tofile=change["source"] + " (proposed)")
                (directory / "changes.diff").write_text("".join(diff))
            pulled = {"status": "planned", "message": "Compared with the saved baseline; proposed YAML diffs are saved under pull/.",
                      "local_updates": sum(c["status"] == "prepared" for c in journal["changes"]),
                      "changes": journal["changes"]}
        except (ValueError, KeyError, TypeError) as error:
            pulled = {"status": "conflict", "message": f"Pull requires review: {error}",
                      "local_updates": 0, "changes": []}
    report = {"version": 1, "dry_run": True, "account": args.username,
              "book": remote["book"], "source": str(source), "identity_bundle": str(bundle),
              "previous_pull": str(args.previous_pull.resolve()) if args.previous_pull else None,
              "existing_contacts": remote["existing_contacts"], "upload_counts": counts,
              "upload_plan": remote["plan"], "pull": pulled}
    trial.save_json(output / "report.json", report)
    (output / "report.md").write_text(markdown_report(report))
    print(f"Dry run: {counts['create']} would create, {counts['unchanged']} unchanged, {counts['review']} need review.")
    print(f"Pull: {pulled['local_updates']} local updates proposed; {pulled['status']}.")
    print(f"Report: {output / 'report.md'}")
    return report


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--dry-run", action="store_true", help="preview only (the default); no apply mode is exposed")
    p.add_argument("--source", type=Path, required=True, help="current Sortal root")
    p.add_argument("--bundle", type=Path, required=True, help="previous export retaining stable Sortal/CardDAV identities")
    p.add_argument("--server", default=trial.ROOT, help="HTTPS CardDAV origin or discovery endpoint; defaults to Fastmail")
    p.add_argument("--collection", help="full address-book URL when discovery finds multiple collections")
    p.add_argument("--username", required=True)
    p.add_argument("--password-file", type=Path, required=True)
    p.add_argument("--report", type=Path, required=True, help="new private report directory outside the source")
    p.add_argument("--previous-pull", type=Path, help="last applied pull journal for this account, if available")
    args = p.parse_args()
    try:
        run(args)
    except Exception as error:
        print(f"Dry run failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
