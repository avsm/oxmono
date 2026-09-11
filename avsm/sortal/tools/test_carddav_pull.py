"""Synthetic regression cases based on the Fastmail edit transformations."""

import base64
from copy import deepcopy
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import json

import yaml

import carddav_export as export
import carddav_pull as pull
import sortal_vcard as mapping


def render(properties):
    writer = mapping.Writer("3.0", [])
    writer.lines = []
    for p in properties:
        writer.add(p["name"], p["value"], group=p["group"] or None, params=p["params"], raw=True)
    return ("\r\n".join(mapping.fold(line) for line in writer.lines) + "\r\n").encode()


class PullTest(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.photo = base64.b64decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aXioAAAAASUVORK5CYII=")
        (self.root / "casey.png").write_bytes(self.photo)
        self.base = {"version": 2, "kind": "person", "handle": "casey",
                     "names": ["Casey Example", "C. Example"],
                     "links": ["https://example.invalid/"], "photo": "casey.png"}
        self.old = mapping.encode(self.base, "uid", "store", self.root, export.safe_path)
        props = mapping.parse(self.old)
        for p in props:
            if p["name"] in {"FN", "X-ADDRESSBOOKSERVER-KIND"}:
                p["params"].pop("X-SORTAL-PATH", None)
            if p["name"] in {"URL", "PHOTO"}:
                p["group"] = ""
                p["params"]["PROP-ID"] = "generated-" + p["name"].lower()
            if p["name"] == "PHOTO":
                p["params"]["ENCODING"] = "B"
            if p["name"] == "X-ADDRESSBOOKSERVER-KIND":
                p["value"] = "INDIVIDUAL"
        props[-1:-1] = [
            {"group": "", "name": "EMAIL", "params": {"PREF": "1", "TYPE": "WORK,PREF", "PROP-ID": "email-id"}, "value": "casey@example.invalid"},
            {"group": "", "name": "NICKNAME", "params": {"PROP-ID": "nick"}, "value": "Caz"},
            {"group": "", "name": "REV", "params": {}, "value": "20260911T070000Z"}]
        self.new = render(props)

    def merge(self, local=None, new=None):
        return pull.reconcile(self.base, local if local is not None else self.base,
                              self.old, new or self.new, "uid", "store")

    def test_web_edit_retains_names_kind_photo_and_extra_metadata(self):
        merged = self.merge()
        for field in self.base:
            self.assertEqual(merged[field], self.base[field])
        self.assertEqual(merged["emails"], ["casey@example.invalid"])
        self.assertEqual(merged["vcard"]["NICKNAME;PROP-ID=nick"], "Caz")
        self.assertTrue(any("TYPE=WORK,PREF" in key for key in merged["vcard"]))

    def test_merged_export_has_one_email_and_roundtrips_all_metadata(self):
        merged = self.merge()
        for version in ("3.0", "4.0"):
            data = mapping.encode(merged, "uid", "store", self.root, export.safe_path, version)
            props = mapping.parse(data)
            self.assertEqual(len([p for p in props if p["name"] == "EMAIL"]), 1)
            self.assertEqual(mapping.only(props, "EMAIL")["params"]["TYPE"], "WORK,PREF")
            decoded, photos = mapping.decode(data)
            self.assertEqual(decoded, merged)
            self.assertEqual(photos, {"casey.png": self.photo})

    def test_repeat_pull_does_not_duplicate_email_or_nickname(self):
        first = self.merge()
        self.assertEqual(self.merge(local=first), first)

    def test_conflicting_email_change_is_rejected(self):
        local = {**self.base, "emails": ["local@example.invalid"]}
        with self.assertRaisesRegex(ValueError, "conflict at emails"):
            self.merge(local=local)

    def test_unrelated_local_changes_are_retained(self):
        local = {**self.base, "feeds": [{"url": "https://example.invalid/rss", "type": "rss"}]}
        self.assertEqual(self.merge(local=local)["feeds"], local["feeds"])

    def test_changed_photo_is_a_conflict_until_photo_import_is_supported(self):
        props = mapping.parse(self.new)
        mapping.only(props, "PHOTO")["value"] = base64.b64encode(b"changed photo").decode()
        with self.assertRaisesRegex(ValueError, "outside"):
            self.merge(new=render(props))

    def test_identity_conflict_is_rejected(self):
        props = mapping.parse(self.new)
        mapping.only(props, "X-SORTAL-ID")["value"] = "different"
        with self.assertRaisesRegex(ValueError, "identity"):
            self.merge(new=render(props))

    def test_yaml_comments_quotes_and_unknown_fields_survive(self):
        raw = b'# context\nversion: 2\nkind: person\nhandle: "casey" # stable\nnames:\n  - Casey Example # full name\ncustom: keep\n'
        contact = yaml.safe_load(raw)
        contact["emails"] = ["casey@example.invalid"]
        result = pull.update_yaml(raw, contact)
        self.assertIn(b'# context', result)
        self.assertIn(b'handle: "casey" # stable', result)
        self.assertIn(b'Casey Example # full name', result)
        self.assertEqual(yaml.safe_load(result), contact)

    def test_passthrough_quoted_header_and_repeated_properties_roundtrip(self):
        c = deepcopy(self.base)
        c["vcard"] = {'itemA.TEL;TYPE="work,voice"': '+123', 'itemB.TEL;TYPE=cell': '+456',
                      'NOTE;X-SERVICE="https://example.invalid/~path"': r'Line one\nLine two'}
        data = mapping.encode(c, "uid", "store", self.root, export.safe_path)
        self.assertEqual(mapping.decode(data)[0], c)

    def test_passthrough_keeps_labels_and_repeated_phones_in_the_same_group(self):
        c = {**self.base, "vcard": {"itemA.TEL;TYPE=work": "+123",
                                  "itemA.TEL;TYPE=cell": "+456", "itemA.X-ABLabel": "Office"}}
        data = mapping.encode(c, "uid", "store", self.root, export.safe_path)
        props = [p for p in mapping.parse(data) if p["name"] in {"TEL", "X-ABLABEL"}]
        self.assertEqual(len(props), 3)
        self.assertEqual(len({p["group"] for p in props}), 1)
        self.assertEqual(mapping.decode(data)[0], c)

    def test_passthrough_header_cannot_contain_an_unquoted_value_delimiter(self):
        c = {**self.base, "vcard": {"NOTE:unintended-value": "Actual value"}}
        with self.assertRaisesRegex(ValueError, "invalid vcard passthrough"):
            mapping.encode(c, "uid", "store", self.root, export.safe_path)

    def test_edited_collections_keep_comments_on_unchanged_values(self):
        raw = (b'emails:\n  - "old@example.invalid" # preferred\n'
               b'  - keep@example.invalid # retain this\n'
               b'vcard:\n  NOTE: "Keep" # context\n  NICKNAME: Old # short name\n')
        merged = yaml.safe_load(raw)
        merged["emails"][0] = "new@example.invalid"
        merged["vcard"]["NICKNAME"] = "New"
        result = pull.update_yaml(raw, merged)
        for comment in (b"# preferred", b"# retain this", b"# context", b"# short name"):
            self.assertIn(comment, result)
        self.assertEqual(yaml.safe_load(result), merged)

    def test_prepare_cannot_write_a_journal_into_the_source(self):
        output = self.root / "journal"
        with self.assertRaisesRegex(ValueError, "outside source"):
            pull.prepare(self.root / "bundle", self.root / "snapshot", self.root, output)
        self.assertFalse(output.exists())

    def test_passthrough_cannot_override_identity(self):
        c = {**self.base, "vcard": {"UID": "foreign"}}
        with self.assertRaisesRegex(ValueError, "cannot override"):
            mapping.encode(c, "uid", "store", self.root, export.safe_path)

    def test_stale_email_overlay_is_rejected(self):
        c = self.merge()
        c["emails"] = ["different@example.invalid"]
        with self.assertRaisesRegex(ValueError, "matching native email"):
            mapping.encode(c, "uid", "store", self.root, export.safe_path)

    def prepared_apply(self):
        directory = self.root / "pull"
        (directory / "uid").mkdir(parents=True)
        before = yaml.safe_dump(self.base).encode()
        after = pull.update_yaml(before, self.merge())
        for name, raw in (("before.yaml", before), ("after.yaml", after), ("remote.vcf", self.new)):
            (directory / "uid" / name).write_bytes(raw)
        (self.root / "casey.yaml").write_bytes(before)
        (self.root / "password").write_text("synthetic-password")
        report = {"account": "test@example.invalid", "source": str(self.root), "changes": [
            {"uid": "uid", "source": "casey.yaml", "href": "https://carddav.fastmail.com/test/uid.vcf",
             "before_sha256": export.digest(before), "after_sha256": export.digest(after),
             "remote_sha256": export.digest(self.new)}]}
        (directory / "report.json").write_text(json.dumps(report))
        return directory, before, after

    def test_remote_change_after_preparation_prevents_local_write(self):
        directory, before, _ = self.prepared_apply()
        with patch.object(pull.trial.Dav, "request", return_value=(200, {"etag": '"etag"'}, self.old)):
            with self.assertRaisesRegex(ValueError, "remote contact changed"):
                pull.apply(directory, "test@example.invalid", self.root / "password")
        self.assertEqual((self.root / "casey.yaml").read_bytes(), before)

    def test_local_change_after_preparation_prevents_write(self):
        directory, before, _ = self.prepared_apply()
        changed = before + b"# a new local edit\n"
        (self.root / "casey.yaml").write_bytes(changed)
        with patch.object(pull.trial.Dav, "request") as request:
            with self.assertRaisesRegex(ValueError, "local contact changed"):
                pull.apply(directory, "test@example.invalid", self.root / "password")
            request.assert_not_called()
        self.assertEqual((self.root / "casey.yaml").read_bytes(), changed)

    def test_apply_and_interrupted_replay_preserve_one_result(self):
        directory, _, after = self.prepared_apply()
        with patch.object(pull.trial.Dav, "request", return_value=(200, {"etag": '"etag"'}, self.new)):
            pull.apply(directory, "test@example.invalid", self.root / "password")
            pull.apply(directory, "test@example.invalid", self.root / "password")
        self.assertEqual((self.root / "casey.yaml").read_bytes(), after)
        report = json.loads((directory / "report.json").read_text())
        self.assertEqual(report["status"], "applied")
        self.assertEqual(report["changes"][0]["etag"], '"etag"')

    def test_dry_apply_leaves_source_and_every_journal_file_unchanged(self):
        directory, before, _ = self.prepared_apply()
        def snapshot():
            return {p.relative_to(self.root).as_posix(): p.read_bytes()
                    for p in self.root.rglob("*") if p.is_file()}
        original = snapshot()
        with patch.object(pull.trial.Dav, "request", return_value=(200, {"etag": '"etag"'}, self.new)), \
                patch.object(pull.os, "replace", side_effect=AssertionError("dry run wrote a file")):
            result = pull.apply(directory, "test@example.invalid", self.root / "password", dry_run=True)
        self.assertEqual(result["local_updates"], ["casey.yaml"])
        self.assertEqual(snapshot(), original)
        self.assertEqual((self.root / "casey.yaml").read_bytes(), before)


if __name__ == "__main__":
    unittest.main()
