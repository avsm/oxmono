"""Synthetic fixtures only. Run: python3 -m unittest discover -s avsm/sortal/tools."""

import base64
import datetime
import json
from pathlib import Path
import tempfile
import unittest

import yaml

import carddav_export as c
import sortal_vcard as v


class ExportTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.source = self.root / "source"
        self.source.mkdir()
        self.output = self.root / "export"
        self.raw = (
            '# Retain comments and CRLF in the local archive.\r\n'
            'version: 2\r\nkind: person\r\nhandle: élise\r\n'
            'names: ["Élise Example; Jr.", "E. Example"]\r\n'
            'emails: ["test@example.invalid", "other@example.invalid"]\r\n'
            'photo: avatar.png\r\n'
            'accounts:\r\n  atproto:\r\n    handle: example.invalid\r\n'
            '    did: did:plc:example\r\n    apps: [bluesky, tangled]\r\n'
            '  github: [example, alternate]\r\n'
            'affiliations:\r\n'
            '  - {org: Past, until: "2020"}\r\n'
            '  - {org: Current, from: "2020", address: "Room 2, Example St"}\r\n'
            '  - {org: Future, from: "2099"}\r\n'
            'feeds: [{type: atom, url: "https://example.invalid/feed", paused: true}]\r\n'
            'links: [{url: "https://example.invalid/", label: "雪, semi; backslash \\\\ ' + 'é雪' * 80 + '"}]\r\n'
        ).encode("utf-8")
        (self.source / "example.yaml").write_bytes(self.raw)
        self.photo = base64.b64decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aXioAAAAASUVORK5CYII=")
        (self.source / "avatar.png").write_bytes(self.photo)
        (self.source / "unused.png").write_bytes(b"another original asset")
        (self.source / "feeds").mkdir()
        (self.source / "feeds" / "annotations.json").write_bytes(b'{"read":true}')
        (self.source / ".git").mkdir()
        (self.source / ".git" / "HEAD").write_bytes(b"ref: refs/heads/main\n")

    def run_export(self, **kwargs):
        return c.export(self.source, self.output, as_of=datetime.date(2026, 9, 10), **kwargs)

    def test_exact_recovery_and_projection(self):
        m = self.run_export()
        data = (self.output / m["contacts"][0]["card"]).read_bytes()
        # Every physical line is valid UTF-8 and <= 75 bytes.
        for line in data.split(b"\r\n"):
            line.decode("utf-8")
            self.assertLessEqual(len(line), 75)
        self.assertNotIn(b"\n", data.replace(b"\r\n", b""))
        p = c.read_properties(data)
        self.assertEqual(c.one(p, "FN"), "Élise Example; Jr.")
        self.assertEqual(p["ORG"], ["Past", "Current", "Future"])
        self.assertEqual(len(p["URL"]), 6)
        self.assertEqual(base64.b64decode(c.one(p, "PHOTO")), self.photo)
        self.assertNotIn("X-SORTAL-META", p)
        self.assertEqual(v.decode(data)[0], v.canonical(yaml.safe_load(self.raw)))
        self.assertEqual((self.output / "originals" / "example.yaml").read_bytes(), self.raw)
        self.assertEqual(len(c.verify(self.output, self.source)["files"]), 5)
        self.assertEqual(self.output.stat().st_mode & 0o777, 0o700)

    def test_stable_ids_and_rename(self):
        old = self.run_export()
        second = self.root / "second"
        new = c.export(self.source, second, previous=self.output)
        self.assertEqual(old["contacts"][0]["uid"], new["contacts"][0]["uid"])
        (self.source / "example.yaml").write_bytes(self.raw.replace('handle: élise'.encode(), b'handle: renamed'))
        renamed = c.export(self.source, self.root / "third", previous=second, renames=["élise=renamed"])
        self.assertEqual(old["contacts"][0]["uid"], renamed["contacts"][0]["uid"])

    def test_duplicate_handle_fails_without_output(self):
        (self.source / "duplicate.yaml").write_bytes(self.raw)
        with self.assertRaisesRegex(ValueError, "duplicate"):
            self.run_export()
        self.assertFalse(self.output.exists())

    def test_duplicate_yaml_key_fails(self):
        (self.source / "example.yaml").write_bytes(self.raw + b'version: 2\n')
        with self.assertRaisesRegex(ValueError, "duplicate YAML key"):
            self.run_export()

    def test_missing_photo_fails(self):
        (self.source / "avatar.png").unlink()
        with self.assertRaises(FileNotFoundError):
            self.run_export()
        self.assertFalse(self.output.exists())

    def test_trailing_url_newline_retained_per_field(self):
        raw = self.raw.replace(b'https://example.invalid/", label:', b'https://example.invalid/\\n", label:')
        (self.source / "example.yaml").write_bytes(raw)
        m = self.run_export()
        p = c.read_properties((self.output / m["contacts"][0]["card"]).read_bytes())
        self.assertIn("https://example.invalid/", p["URL"])
        self.assertTrue(m["contacts"][0]["warnings"])
        self.assertEqual(c.one(p, "X-SORTAL-ORIGINAL-URL"), "https://example.invalid/\n")
        self.assertEqual(v.decode((self.output / m["contacts"][0]["card"]).read_bytes())[0],
                         v.canonical(yaml.safe_load(raw)))

    def test_existing_output_is_never_overwritten(self):
        self.output.mkdir()
        marker = self.output / "keep"
        marker.write_bytes(b"keep")
        with self.assertRaises(ValueError):
            self.run_export()
        self.assertEqual(marker.read_bytes(), b"keep")

    def test_snapshot_corruption_detected(self):
        self.run_export()
        (self.output / "originals" / "unused.png").write_bytes(b"corrupted")
        with self.assertRaisesRegex(ValueError, "checksum mismatch"):
            c.verify(self.output)

    def test_stripped_extension_detected_even_with_updated_checksums(self):
        m = self.run_export()
        path = self.output / m["contacts"][0]["card"]
        data = path.read_bytes()
        data = data.replace(b"X-FEED-PAUSED;X-SORTAL-PATH=/feeds/0/paused:TRUE", b"X-CLIENT-REMOVED:TRUE")
        path.write_bytes(data)
        (self.output / "contacts.vcf").write_bytes(data)
        m["contacts"][0]["sha256"] = c.digest(data)
        (self.output / "manifest.json").write_text(json.dumps(m))
        with self.assertRaisesRegex(ValueError, "does not reconstruct"):
            c.verify(self.output)

    def test_unmapped_fields_refused(self):
        (self.source / "example.yaml").write_bytes(self.raw + b'future-field: preserve-me\n')
        with self.assertRaisesRegex(ValueError, "unmapped Sortal fields"):
            self.run_export()
        self.assertFalse(self.output.exists())

    def test_unknown_account_refused(self):
        (self.source / "example.yaml").write_bytes(self.raw.replace(b'github: [example, alternate]', b'future-service: example'))
        with self.assertRaisesRegex(ValueError, "unmapped account platform"):
            self.run_export()

    def test_vcard4_standard_names_and_social_profiles(self):
        m = self.run_export(vcard_version="4.0")
        data = (self.output / m["contacts"][0]["card"]).read_bytes()
        props = c.read_properties(data)
        self.assertEqual(props["VERSION"], ["4.0"])
        self.assertEqual(len(props["FN"]), 2)
        self.assertEqual(len(props["SOCIALPROFILE"]), 2)
        self.assertNotIn("X-SOCIALPROFILE", props)
        self.assertNotIn("X-SORTAL-ALT-NAME", props)
        self.assertEqual(v.decode(data)[1]["avatar.png"], self.photo)

    def test_wire_order_does_not_change_collection_order(self):
        m = self.run_export()
        data = (self.output / m["contacts"][0]["card"]).read_bytes()
        import re
        unfolded = re.sub(rb"\r\n[ \t]", b"", data).split(b"\r\n")
        reordered = b"\r\n".join(unfolded[:2] + list(reversed(unfolded[2:-2])) + unfolded[-2:])
        self.assertEqual(v.decode(reordered)[0], v.canonical(yaml.safe_load(self.raw)))

    def test_changed_visible_field_is_read_instead_of_an_old_payload(self):
        m = self.run_export()
        data = (self.output / m["contacts"][0]["card"]).read_bytes()
        changed = data.replace(b'EMAIL;TYPE=INTERNET,PREF;X-SORTAL-PATH=/emails/0:test@example.invalid',
                               b'EMAIL;TYPE=INTERNET,PREF;X-SORTAL-PATH=/emails/0:new@example.invalid')
        decoded, _ = v.decode(changed)
        self.assertEqual(decoded["emails"], ["new@example.invalid", "other@example.invalid"])
        self.assertEqual(decoded["accounts"]["atproto"]["did"], "did:plc:example")

    def test_empty_collections_and_explicit_defaults(self):
        contact = {"version": 2, "handle": "empty", "names": ["Empty"], "emails": [],
                   "links": [], "affiliations": [], "vcard": [],
                   "accounts": {"atproto": {"handle": "example.invalid", "apps": []}, "github": []},
                   "feeds": [{"url": "https://example.invalid/feed", "type": "rss", "paused": False, "name": "", "hint": ""}]}
        data = v.encode(contact, "uid", "store", self.source, c.safe_path)
        self.assertEqual(v.decode(data)[0], contact)

    def test_quoted_username_and_complex_hint_use_text_fields(self):
        contact = {"version": 2, "handle": "quoted", "names": ["Quoted"],
                   "accounts": {"zulip": 'A "Quoted" Name@chat.example.invalid'},
                   "feeds": [{"type": "manual", "url": "https://example.invalid/", "hint": 'line one\nline two: "quoted"; comma, caret ^'}]}
        for version in ("3.0", "4.0"):
            data = v.encode(contact, "uid", "store", self.source, c.safe_path, version)
            self.assertEqual(v.decode(data)[0], contact)

    def test_partial_dates_and_department_structure(self):
        contact = {"version": 2, "handle": "dates", "names": ["Dates"],
                   "affiliations": [{"org": "Org; With, Punctuation", "department": "Lab; A", "from": "2001-02", "until": "2005-03-04"}]}
        data = v.encode(contact, "uid", "store", self.source, c.safe_path)
        self.assertEqual(v.decode(data)[0], contact)

    def test_conflicting_social_username_and_url_is_not_silently_imported(self):
        contact = {"version": 2, "handle": "test", "names": ["Test"], "accounts": {"github": "old"}}
        data = v.encode(contact, "uid", "store", self.source, c.safe_path)
        with self.assertRaisesRegex(ValueError, "conflict"):
            v.decode(data.replace(b'https://github.com/old', b'https://github.com/new'))

    def test_feeds_are_visible_as_urls_with_version_appropriate_media_types(self):
        contact = {"version": 2, "handle": "test", "names": ["Test"],
                   "feeds": [{"url": "https://example.invalid/" + kind, "type": kind}
                             for kind in ("atom", "rss", "json", "manual")]}
        for version in ("3.0", "4.0"):
            data = v.encode(contact, "uid", "store", self.source, c.safe_path, version)
            urls = [p for p in v.parse(data) if p["name"] == "URL"]
            self.assertEqual(len(urls), 4)
            self.assertNotIn(b".X-FEED;", data)
            for p in urls:
                kind = p["params"]["X-FEED-TYPE"]
                self.assertEqual(p["params"].get("MEDIATYPE"),
                                 v.FEED_MEDIA_TYPES.get(kind) if version == "4.0" else None)
            self.assertEqual(v.decode(data)[0], contact)

    def test_changed_feed_url_updates_the_subscription(self):
        contact = {"version": 2, "handle": "test", "names": ["Test"],
                   "feeds": [{"url": "https://example.invalid/old", "type": "atom", "paused": True}]}
        data = v.encode(contact, "uid", "store", self.source, c.safe_path)
        import re
        data = re.sub(rb"\r\n[ \t]", b"", data)
        decoded, _ = v.decode(data.replace(b'https://example.invalid/old', b'https://example.invalid/new'))
        self.assertEqual(decoded["feeds"], [{"url": "https://example.invalid/new", "type": "atom", "paused": True}])

    def test_social_profiles_have_visible_links(self):
        contact = {"version": 2, "handle": "test", "names": ["Test"], "accounts": {"github": "example"}}
        for version in ("3.0", "4.0"):
            data = v.encode(contact, "uid", "store", self.source, c.safe_path, version)
            props = c.read_properties(data)
            self.assertEqual(props["URL"], ["https://github.com/example"])
            self.assertEqual(v.decode(data)[0], contact)

    def test_edit_to_visible_social_url_cannot_be_ignored(self):
        contact = {"version": 2, "handle": "test", "names": ["Test"], "accounts": {"github": "old"}}
        data = v.encode(contact, "uid", "store", self.source, c.safe_path)
        changed = data.replace(b'URL;X-SORTAL-DERIVED=profile:https://github.com/old',
                               b'URL;X-SORTAL-DERIVED=profile:https://github.com/new')
        with self.assertRaisesRegex(ValueError, "visible URL conflict"):
            v.decode(changed)

    def test_edit_to_atproto_app_url_cannot_be_ignored(self):
        contact = {"version": 2, "handle": "test", "names": ["Test"],
                   "accounts": {"atproto": {"handle": "example.invalid", "apps": ["tangled"]}}}
        data = v.encode(contact, "uid", "store", self.source, c.safe_path)
        with self.assertRaisesRegex(ValueError, "visible URL conflict"):
            v.decode(data.replace(b'https://tangled.org/@example.invalid', b'https://tangled.org/@different.invalid'))

    def test_bare_atproto_identity_has_a_visible_link(self):
        contact = {"version": 2, "handle": "test", "names": ["Test"], "accounts": {"atproto": "example.invalid"}}
        data = v.encode(contact, "uid", "store", self.source, c.safe_path)
        self.assertEqual(c.read_properties(data)["URL"], ["https://bsky.app/profile/example.invalid"])
        self.assertEqual(v.decode(data)[0], contact)

    def test_photo_path_escape_rejected(self):
        (self.source / "example.yaml").write_bytes(self.raw.replace(b'photo: avatar.png', b'photo: ../outside.png'))
        with self.assertRaisesRegex(ValueError, "relative path"):
            self.run_export()

    def test_symlink_rejected(self):
        (self.source / "link").symlink_to(self.root)
        with self.assertRaisesRegex(ValueError, "symlink"):
            self.run_export()

    def test_nested_output_rejected(self):
        with self.assertRaisesRegex(ValueError, "separate trees"):
            c.export(self.source, self.source / "export")

    def test_unimplemented_passthrough_rejected(self):
        (self.source / "example.yaml").write_bytes(self.raw + b'vcard: [[TEL, "123"]]\n')
        with self.assertRaisesRegex(ValueError, "passthrough"):
            self.run_export()


if __name__ == "__main__":
    unittest.main()
