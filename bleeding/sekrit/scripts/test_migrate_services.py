# /// script
# requires-python = ">=3.9"
# dependencies = ["tomlkit==0.13.3"]
# ///
"""Integration tests using isolated fixtures and real local binaries."""
import hashlib
import json
import os
from pathlib import Path
import runpy
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

import tomlkit

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[2]
SEKRIT = REPO / "_build/default/bleeding/sekrit/bin/main.exe"
CROW = REPO / "_build/default/avsm/crowthebot/bin/main.exe"
SCRIPT = HERE / "migrate_services.py"


class Migration(unittest.TestCase):
    def setUp(self):
        previous_umask = os.umask(0o077)
        self.addCleanup(os.umask, previous_umask)
        self.temp = tempfile.TemporaryDirectory(prefix="sekrit-service-test-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.config = self.root / "config"
        self.env = {k: v for k, v in os.environ.items() if not (
            k.startswith(("XDG_", "SEKRIT_")) or k.endswith(("_CONFIG_DIR", "_DATA_DIR")))}
        self.env.update(PYTHONDONTWRITEBYTECODE="1", HOME=str(self.root), XDG_CONFIG_HOME=str(self.config),
                        XDG_DATA_HOME=str(self.root / "data"),
                        XDG_STATE_HOME=str(self.root / "state"),
                        XDG_CACHE_HOME=str(self.root / "cache"))

    def write(self, path, value, mode=0o600):
        path.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
        path.write_bytes(value.encode() if isinstance(value, str) else value)
        path.chmod(mode)
        return path

    def invoke(self, *args, ok=True):
        result = subprocess.run([sys.executable, str(SCRIPT),
                                 "--sekrit-bin", str(SEKRIT), "--crowthebot-bin", str(CROW), *args],
                                env=self.env, cwd=self.root, capture_output=True, text=True)
        if ok:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        else:
            self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("fixture-password", result.stdout + result.stderr)
        return result

    def secret(self, reference):
        target = self.root / "verification"
        target.unlink(missing_ok=True)
        subprocess.run([str(SEKRIT), "export", reference[7:], "--file", str(target)],
                       env=self.env, check=True, capture_output=True)
        value = target.read_bytes()
        target.unlink()
        return value

    def snapshot(self):
        return {str(p.relative_to(self.root)): p.read_bytes()
                for p in self.root.rglob("*") if p.is_file()}

    def setup_services(self):
        self.write(self.root / "carddav", "fixture-password\n")
        self.write(self.config / "dooit/config.toml", '''# keep this comment
backend = "webdav"
root = "~/tasks"
[webdav]
url = "https://example.test/"
username = "operator"
password = "fixture-password"
''')
        self.write(self.config / "sortal/config.toml", '''[carddav]
username = "operator"
password_file = "carddav"
''')
        self.write(self.config / "termanil/config.toml", '''[contacts.carddav]
url = "https://example.test/"
username = "operator"
password_file = "../../carddav"
[dooit]
config = "../../extra-dooit.toml"
''')
        self.write(self.root / "extra-dooit.toml", '''[webdav]
url = "https://example.test/"
username = "operator"
password_file = "carddav"
''')
        self.write(self.config / "bushel/config.toml", '''# embedding model must survive
[data]
local_dir = "~/site"
[embeddings]
backend = "http"
url = "https://example.test/"
model = "my-model"
dimensions = 32
api_key_file = "../../carddav"
''', 0o644)
        self.write(self.config / "jmap/profiles/main", '''url=https://example.test/jmap/session
auth=basic
user=operator
secret=fixture-password
''')
        self.write(self.config / "jmap/profiles/token", '''url=https://example.test/jmap/session
auth=bearer
secret=fixture-password
''')
        for name in ("home", "work"):
            self.write(self.config / f"crowthebot/secrets/{name}/openrouter.json",
                       json.dumps({"version": 1, "selected": "main", "entries": [
                           {"name": "main", "value": {"api_key": "fixture-password"}}]}))

    def test_all_services_dry_run_apply_rerun(self):
        self.setup_services()
        original = self.snapshot()
        self.invoke()
        self.assertEqual(self.snapshot(), original)
        first = self.invoke("--apply")
        self.assertIn("Private backups:", first.stdout)
        self.assertEqual((self.config / "sekrit/master.key").stat().st_mode & 0o777, 0o600)
        dooit = (self.config / "dooit/config.toml").read_text()
        self.assertIn("# keep this comment", dooit)
        table = tomlkit.parse(dooit)["webdav"]
        self.assertNotIn("password", table)
        self.assertEqual(self.secret(table["secret"]), b"fixture-password")
        jmap = (self.config / "jmap/profiles/main").read_text()
        self.assertNotIn("user=", jmap)
        ref = [s[11:] for s in jmap.splitlines() if s.startswith("secret_ref=")][0]
        self.assertEqual(self.secret(ref), b"operator:fixture-password")
        for name in ("home", "work"):
            sealed = (self.config / f"crowthebot/secrets/{name}/openrouter.json").read_bytes()
            self.assertTrue(sealed.startswith(b"SEKRIT\x00\x01"))
            self.assertNotIn(b"fixture-password", sealed)
        self.assertEqual((self.root / "carddav").read_text(), "fixture-password\n")
        backups = list((self.root / "state/sekrit/migrations").glob("*/manifest.json"))
        self.assertEqual(len(backups), 1)
        entries = json.loads(backups[0].read_text())
        self.assertEqual(len(entries), 9)
        for entry in entries:
            saved = backups[0].parent / entry["backup"]
            self.assertEqual(saved.stat().st_mode & 0o777, 0o600)
            self.assertEqual(hashlib.sha256(saved.read_bytes()).hexdigest(), entry["sha256"])
        before = self.snapshot()
        self.invoke("--apply")
        self.assertEqual(self.snapshot(), before)

    def test_changed_secret_after_partial_import_is_refused(self):
        path = self.write(self.config / "dooit/config.toml", '[webdav]\npassword="fixture-password"\n')
        original = path.read_bytes()
        self.invoke("--apply")
        # Simulate an interruption after importing but before config publication.
        self.write(path, original)
        self.invoke("--apply")
        self.write(path, original.replace(b"fixture-password", b"different-password"))
        result = self.invoke("--apply", ok=False)
        self.assertIn("differs", result.stderr)
        self.assertIn(b"different-password", path.read_bytes())

    def test_missing_key_is_never_recreated_for_crow(self):
        self.write(self.config / "crowthebot/secrets/home/openrouter.json", b"SEKRIT\x00\x01fixture")
        self.invoke("--apply", ok=False)
        self.assertFalse((self.config / "sekrit/master.key").exists())

    def test_missing_named_reference_keeps_config_unchanged(self):
        path = self.write(self.config / "dooit/config.toml", '[webdav]\nsecret="sekrit:missing/name"\n')
        before = self.snapshot()
        self.invoke("--apply", ok=False)
        self.assertEqual(self.snapshot(), before)

    def test_empty_host_and_unconfigured_sortal_do_nothing(self):
        self.invoke("--apply", "--sekrit-bin", "/does-not-exist")
        self.assertEqual(self.snapshot(), {})
        self.write(self.config / "sortal/config.toml", '[carddav]\nusername=""\npassword_file="missing"\n')
        before = self.snapshot()
        self.invoke("--apply", "--sekrit-bin", "/does-not-exist")
        self.assertEqual(self.snapshot(), before)

    def test_standalone_install_uses_path(self):
        module = runpy.run_path(str(SCRIPT))
        resolve = module["binary"]
        # A shallow install location has no checkout ancestor.
        resolve.__globals__["__file__"] = "/migration.py"
        with mock.patch("shutil.which", return_value=str(SEKRIT)):
            self.assertEqual(resolve(None, "sekrit", "unused"), str(SEKRIT))

    def test_malformed_input_and_permissions_fail_before_mutation(self):
        path = self.write(self.config / "dooit/config.toml", '[webdav]\npassword="fixture-password"\n', 0o644)
        self.invoke("--apply", ok=False)
        self.assertFalse((self.config / "sekrit/master.key").exists())
        self.write(path, '[webdav\npassword="fixture-password"')
        self.invoke("--apply", ok=False)
        self.assertFalse((self.config / "sekrit/master.key").exists())

    def test_config_symlink_is_refused(self):
        source = self.write(self.root / "source", '[webdav]\npassword="fixture-password"\n')
        path = self.config / "dooit/config.toml"
        path.parent.mkdir(parents=True)
        path.symlink_to(source)
        self.invoke("--apply", ok=False)
        self.assertIn("fixture-password", source.read_text())

    def test_app_override_is_parent_and_extra_jmap_file_reference(self):
        base = self.root / "overridden"
        self.env["SORTAL_CONFIG_DIR"] = str(base)
        self.write(base / "sortal/carddav-password", "fixture-password\n")
        self.write(base / "sortal/config.toml", '[carddav]\nusername="operator"\n')
        source = self.write(self.root / "token", "fixture-password\n")
        self.write(self.root / "jmap/extra", f"url=https://example.test/jmap\nauth=bearer\nsecret_ref={source}\n")
        self.invoke("--apply", "--jmap-profiles", str(self.root / "jmap"))
        self.assertIn("sekrit:", (base / "sortal/config.toml").read_text())
        self.assertIn("sekrit:", (self.root / "jmap/extra").read_text())

    def test_restart_only_after_success(self):
        self.write(self.config / "dooit/config.toml", '[webdav]\npassword="fixture-password"\n')
        log = self.root / "restarted"
        self.write(self.root / "bin/systemctl", f"#!/bin/sh\nprintf '%s\\n' \"$@\" > '{log}'\n", 0o700)
        self.env["PATH"] = str(self.root / "bin") + os.pathsep + self.env["PATH"]
        self.invoke("--restart-systemd-user", "crow.service")
        self.assertFalse(log.exists())
        self.invoke("--apply", "--restart-systemd-user", "crow.service")
        self.assertIn("try-restart", log.read_text())
        log.unlink()
        self.write(self.config / "dooit/config.toml", '[webdav]\npassword="fixture-password"\n', 0o644)
        self.invoke("--apply", "--restart-systemd-user", "crow.service", ok=False)
        self.assertFalse(log.exists())


if __name__ == "__main__":
    unittest.main()
