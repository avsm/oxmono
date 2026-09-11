"""Safety checks for initial seeding; all data and DAV responses are synthetic."""

from pathlib import Path
import tempfile
import unittest

import carddav_export as export
import carddav_trial as trial
import sortal_vcard as mapping


class TrialTest(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        source = self.root / "source"
        source.mkdir()
        (source / "casey.yaml").write_text(
            "version: 2\nkind: person\nhandle: casey\nnames: [Casey Example]\n"
            "emails: [casey@example.invalid]\n")
        self.bundle = self.root / "bundle"
        self.manifest = export.export(source, self.bundle)
        self.entry = self.manifest["contacts"][0]
        self.data = (self.bundle / self.entry["card"]).read_bytes()
        self.book = {"href": trial.ROOT + "test/", "max_resource_size": None}

    def remote(self, data):
        props = mapping.parse(data)
        return [{"uid": mapping.only(props, "UID")["value"], "props": props,
                 "data": data, "href": trial.ROOT + "test/existing.vcf", "etag": '"before"'}]

    def action(self, remote):
        return trial.plan(self.bundle, self.manifest, self.book, remote)[0]["action"]

    def test_name_collision_holds_existing_card_with_unmapped_data(self):
        data = (b"BEGIN:VCARD\r\nVERSION:3.0\r\nUID:foreign\r\n"
                b"FN:  CASEY   example\r\nN:Example;Casey;;;\r\n"
                b"TEL:123\r\nNOTE:Keep this\r\nEND:VCARD\r\n")
        self.assertEqual(self.action(self.remote(data)), "review")

    def test_account_collision_holds_different_name(self):
        data = (b"BEGIN:VCARD\r\nVERSION:3.0\r\nUID:foreign\r\nFN:Different\r\n"
                b"EMAIL:casey@EXAMPLE.INVALID\r\nEND:VCARD\r\n")
        self.assertEqual(self.action(self.remote(data)), "review")

    def test_repeat_seed_is_unchanged(self):
        self.assertEqual(self.action(self.remote(self.data)), "unchanged")

    def test_unmapped_remote_additions_are_kept(self):
        data = self.data.replace(b"END:VCARD", b"TEL:123\r\nNOTE:Keep this\r\nEND:VCARD")
        self.assertEqual(self.action(self.remote(data)), "unchanged")

    def test_missing_display_fallback_is_held(self):
        data = b"\r\n".join(line for line in self.data.split(b"\r\n") if not line.startswith(b"N:"))
        self.assertEqual(self.action(self.remote(data)), "review")

    def test_existing_remote_edit_is_held(self):
        data = self.data.replace(b":casey@example.invalid", b":changed@example.invalid")
        self.assertEqual(self.action(self.remote(data)), "review")

    def test_store_identity_conflict_is_held(self):
        data = self.data.replace(self.manifest["store_id"].encode(), b"another-store")
        self.assertEqual(self.action(self.remote(data)), "review")

    def test_size_limit_blocks_creation(self):
        self.book["max_resource_size"] = 10
        self.assertEqual(self.action([]), "review")

    def test_truncated_multistatus_is_rejected(self):
        data = b'''<d:multistatus xmlns:d="DAV:"><d:response>
          <d:href>/test/</d:href><d:status>HTTP/1.1 507 Insufficient Storage</d:status>
          <d:error><d:number-of-matches-within-limits/></d:error>
          </d:response></d:multistatus>'''
        with self.assertRaisesRegex(ValueError, "failed resource"):
            trial.responses(data, trial.ROOT)

    def test_conflicting_create_never_falls_back_to_overwrite(self):
        calls = []

        class ConflictDav:
            def request(self, method, url, body=None, headers=None):
                calls.append((method, headers))
                return 412, {}, b""

        row = trial.plan(self.bundle, self.manifest, self.book, [])[0]
        with self.assertRaisesRegex(ValueError, "412"):
            trial.seed_one(ConflictDav(), self.bundle, row, self.root)
        self.assertEqual(calls, [("PUT", {"Content-Type": "text/vcard; charset=utf-8", "If-None-Match": "*"})])

    def test_credentials_cannot_follow_other_origin(self):
        dav = trial.Dav("test", "synthetic-password")
        with self.assertRaisesRegex(ValueError, "outside"):
            dav.request("GET", "https://other.example.invalid/")


if __name__ == "__main__":
    unittest.main()
