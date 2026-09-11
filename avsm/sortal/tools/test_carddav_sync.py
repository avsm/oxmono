"""Dry-run behavior, with fake remote data and transport guards."""

import io
import json
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch
import contextlib
import yaml

import carddav_export as export
import carddav_sync as sync
import carddav_trial as trial
import sortal_vcard as mapping


def tree(root):
    return {p.relative_to(root).as_posix(): p.read_bytes() for p in root.rglob("*") if p.is_file()}


class DryRunTest(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.source = self.root / "source"
        self.source.mkdir()
        (self.source / "casey.yaml").write_text(
            "version: 2\nkind: person\nhandle: casey\nnames: [Casey Example]\n"
            "emails: [old@example.invalid]\n")
        self.bundle = self.root / "bundle"
        self.manifest = export.export(self.source, self.bundle)
        self.entry = self.manifest["contacts"][0]
        self.data = (self.bundle / self.entry["card"]).read_bytes()
        self.password = self.root / "password"
        self.password.write_text("synthetic-private-password")
        self.book = {"href": "https://contacts.example.invalid:8443/dav/personal/", "name": "Personal",
                     "max_resource_size": None, "supported_address_data": [{"content-type": "text/vcard", "version": "3.0"}]}
        self.args = SimpleNamespace(source=self.source, bundle=self.bundle, report=self.root / "preview",
                                    username="test@example.invalid", password_file=self.password,
                                    server="https://contacts.example.invalid:8443/dav/", collection=None,
                                    previous_pull=None, dry_run=True)

    def run_preview(self, remote):
        def discover(dav, collection=None):
            self.assertTrue(dav.readonly)
            self.assertEqual(dav.origin, ("https", "contacts.example.invalid", 8443))
            return self.book
        def fetch(dav, book):
            self.assertTrue(dav.readonly)
            return remote
        with patch.object(trial, "discover", side_effect=discover), patch.object(trial, "fetch_all", side_effect=fetch), \
                patch.object(trial, "seed_one", side_effect=AssertionError("dry run attempted upload")), \
                contextlib.redirect_stdout(io.StringIO()):
            return sync.run(self.args)

    def test_fresh_source_is_previewed_without_modifying_source_or_baseline(self):
        path = self.source / "casey.yaml"
        path.write_bytes(path.read_bytes().replace(b"old@example.invalid", b"new@example.invalid"))
        before_source, before_bundle = tree(self.source), tree(self.bundle)
        result = self.run_preview([])
        self.assertEqual(result["upload_counts"], {"create": 1, "unchanged": 0, "review": 0})
        self.assertEqual(tree(self.source), before_source)
        self.assertEqual(tree(self.bundle), before_bundle)
        data = (self.args.report / "export" / self.entry["card"]).read_bytes()
        self.assertIn(b"new@example.invalid", data)
        self.assertEqual(mapping.only(mapping.parse(data), "UID")["value"], self.entry["uid"])
        self.assertEqual(self.args.report.stat().st_mode & 0o777, 0o700)
        self.assertTrue(result["dry_run"])
        self.assertNotIn(b"synthetic-private-password", b"".join(tree(self.args.report).values()))

    def test_existing_name_is_reviewed_not_created_or_merged(self):
        data = b"BEGIN:VCARD\r\nVERSION:3.0\r\nUID:foreign\r\nFN:Casey Example\r\nTEL:123\r\nEND:VCARD\r\n"
        remote = [{"uid": "foreign", "href": self.book["href"] + "foreign.vcf", "etag": '"e"',
                   "data": data, "props": mapping.parse(data)}]
        result = self.run_preview(remote)
        self.assertEqual(result["upload_counts"], {"create": 0, "unchanged": 0, "review": 1})
        self.assertEqual(result["pull"]["status"], "unavailable")
        self.assertIn("possible duplicate", (self.args.report / "report.md").read_text())

    def test_pull_diff_is_saved_without_applying_source_or_advancing_baseline(self):
        seed = self.bundle / "fastmail-seed"
        (seed / "after").mkdir(parents=True)
        (seed / "after" / (self.entry["uid"] + ".vcf")).write_bytes(self.data)
        href = self.book["href"] + self.entry["uid"] + ".vcf"
        (seed / "report.json").write_text(json.dumps({"account": self.args.username, "book": self.book,
            "results": [{"uid": self.entry["uid"], "href": href, "status": "verified", "sha256": export.digest(self.data)}]}))
        data = self.data.replace(b"old@example.invalid", b"remote@example.invalid")
        remote = [{"uid": self.entry["uid"], "href": href, "etag": '"new"',
                   "data": data, "props": mapping.parse(data)}]
        original_source, original_bundle = tree(self.source), tree(self.bundle)
        with patch.object(sync.pull, "apply", side_effect=AssertionError("dry run attempted local apply")):
            result = self.run_preview(remote)
        self.assertEqual(result["pull"]["status"], "planned")
        self.assertEqual(result["pull"]["local_updates"], 1)
        self.assertIn("remote@example.invalid", (self.args.report / "pull" / self.entry["uid"] / "changes.diff").read_text())
        self.assertEqual(tree(self.source), original_source)
        self.assertEqual(tree(self.bundle), original_bundle)

    def test_pull_does_not_mark_unpushed_local_edits_as_common_baseline(self):
        seed = self.bundle / "fastmail-seed"
        (seed / "after").mkdir(parents=True)
        uid = self.entry["uid"]
        href = self.book["href"] + uid + ".vcf"
        (seed / "after" / (uid + ".vcf")).write_bytes(self.data)
        (seed / "report.json").write_text(json.dumps({"account": self.args.username, "book": self.book,
            "results": [{"uid": uid, "href": href, "status": "verified", "sha256": export.digest(self.data)}]}))
        local = self.source / "casey.yaml"
        local.write_bytes(local.read_bytes().replace(b"Casey Example", b"Local Casey"))
        remote = self.data.replace(b"old@example.invalid", b"remote@example.invalid")
        snapshot = self.root / "snapshot"
        (snapshot / "before").mkdir(parents=True)
        (snapshot / "before" / (uid + ".vcf")).write_bytes(remote)
        (snapshot / "report.json").write_text(json.dumps({"account": self.args.username, "book": self.book}))
        output = self.root / "first-pull"
        with contextlib.redirect_stdout(io.StringIO()):
            sync.pull.prepare(self.bundle, snapshot, self.source, output)
        common = yaml.safe_load((output / uid / "common.yaml").read_bytes())
        merged = yaml.safe_load((output / uid / "after.yaml").read_bytes())
        self.assertEqual(common["names"], ["Casey Example"])
        self.assertEqual(merged["names"], ["Local Casey"])
        self.assertEqual(common["emails"], ["remote@example.invalid"])
        with patch.object(trial.Dav, "request", return_value=(200, {"etag": '"new"'}, remote)), \
                contextlib.redirect_stdout(io.StringIO()):
            sync.pull.apply(output, self.args.username, self.password)
        # A later competing remote name edit must conflict with the still
        # unpushed local name, rather than overwriting it as if synchronized.
        changed = remote.replace(b"/names/0:Casey Example", b"/names/0:Remote Casey")
        (snapshot / "before" / (uid + ".vcf")).write_bytes(changed)
        with self.assertRaisesRegex(ValueError, "conflict at names"):
            sync.pull.prepare(self.bundle, snapshot, self.source, self.root / "second-pull", previous=output)
        self.assertEqual(yaml.safe_load(local.read_bytes())["names"], ["Local Casey"])

    def test_report_inside_source_is_rejected_before_any_write(self):
        self.args.report = self.source / "dry-run"
        before = tree(self.source)
        with self.assertRaisesRegex(ValueError, "outside"):
            sync.run(self.args)
        self.assertEqual(tree(self.source), before)
        self.assertFalse(self.args.report.exists())

    def test_report_inside_saved_cards_is_rejected(self):
        self.args.report = self.bundle / "cards" / "dry-run"
        with self.assertRaisesRegex(ValueError, "outside"):
            sync.run(self.args)
        self.assertFalse(self.args.report.exists())

    def test_dry_run_blocks_all_http_mutations_before_opening_connection(self):
        dav = trial.Dav("user", "secret", root=self.args.server, readonly=True)
        with patch.object(trial.http.client, "HTTPSConnection") as connection:
            for method in ("PUT", "DELETE", "POST", "PATCH", "MKCOL", "MOVE", "COPY", "PROPPATCH"):
                with self.assertRaisesRegex(ValueError, "forbids"):
                    dav.request(method, self.book["href"])
            connection.assert_not_called()

    def test_custom_server_origin_and_port_are_enforced(self):
        dav = trial.Dav("user", "secret", root=self.args.server, readonly=True)
        dav.validate_url(self.book["href"])
        for url in ("https://contacts.example.invalid/dav/", "https://elsewhere.example.invalid:8443/dav/"):
            with self.assertRaisesRegex(ValueError, "outside"):
                dav.validate_url(url)
        for url in ("http://contacts.example.invalid/", "https://user:secret@example.invalid/", "https://example.invalid:0/"):
            with self.assertRaises(ValueError):
                trial.Dav("user", "secret", root=url)

    def test_cross_origin_redirect_cannot_receive_credentials(self):
        dav = trial.Dav("user", "secret", root=self.args.server, readonly=True)
        with patch.object(trial.http.client, "HTTPSConnection") as connection:
            response = connection.return_value.getresponse.return_value
            response.status = 302
            response.getheaders.return_value = [("Location", "https://other.example.invalid/")]
            response.read.return_value = b""
            with self.assertRaisesRegex(ValueError, "outside"):
                dav.request("PROPFIND", self.args.server)
            self.assertEqual(connection.call_count, 1)
            self.assertEqual(connection.return_value.request.call_count, 1)

    def test_dry_run_and_apply_flags_are_mutually_exclusive(self):
        with patch("sys.argv", ["carddav_trial", str(self.bundle), "--username", "test",
                                "--password-file", str(self.password), "--report", str(self.args.report),
                                "--dry-run", "--apply"]), contextlib.redirect_stderr(io.StringIO()):
            with self.assertRaises(SystemExit) as error:
                trial.main()
            self.assertEqual(error.exception.code, 2)

    def test_other_account_pull_journal_is_not_reused(self):
        previous = self.root / "previous"
        previous.mkdir()
        (previous / "report.json").write_text(json.dumps({"account": "other@example.invalid", "status": "applied"}))
        self.args.previous_pull = previous
        with self.assertRaisesRegex(ValueError, "selected account"):
            sync.run(self.args)
        self.assertFalse(self.args.report.exists())
