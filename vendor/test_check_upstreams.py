"""Offline integration tests for the upstream checker, using a real local Git repo."""

import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


sys.dont_write_bytecode = True
spec = importlib.util.spec_from_file_location("checker", Path(__file__).with_name("check-upstreams.py"))
checker = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checker)


class CheckerTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.repo = Path(self.tmp.name) / "upstream with spaces"
        self.repo.mkdir()
        self.git("init", "-b", "main")
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.invalid",
                 "-c", "commit.gpgsign=false", "commit", "--allow-empty", "-m", "base")
        self.base = self.git("rev-parse", "HEAD").strip()
        self.entry = {"name": "example", "url": str(self.repo), "revision": self.base}

    def git(self, *args):
        return subprocess.run(["git", "-C", str(self.repo), *args], check=True,
                              capture_output=True, text=True).stdout

    def test_default_branch_and_changed_tip(self):
        current = checker.check(self.entry, 5)
        self.assertEqual((current["status"], current["branch"], current["tip"]),
                         ("current", "main", self.base))
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.invalid",
                 "-c", "commit.gpgsign=false", "commit", "--allow-empty", "-m", "update")
        changed = checker.check(self.entry, 5)
        self.assertEqual(changed["status"], "changed")
        self.assertNotEqual(changed["tip"], self.base)
        # Checking the remote must not advance the recorded base.
        self.assertEqual(self.entry["revision"], self.base)

    def test_explicit_branch_and_missing_branch(self):
        self.git("branch", "maintenance")
        self.assertEqual(checker.check({**self.entry, "branch": "maintenance"}, 5)["status"], "current")
        result = checker.check({**self.entry, "branch": "missing"}, 5)
        self.assertEqual(result["status"], "error")
        self.assertIn("did not advertise", result["error"])

    def test_unknown_base_and_unreachable_remote(self):
        self.assertEqual(checker.check({**self.entry, "revision": None}, 5)["status"], "unknown-base")
        self.assertEqual(checker.check({**self.entry, "url": str(self.repo / "missing")}, 5)["status"], "error")

    def test_manifest_requires_complete_coverage(self):
        root = Path(self.tmp.name) / "vendor"
        root.mkdir()
        (root / "example").mkdir()
        manifest = root / "upstreams.json"
        manifest.write_text(json.dumps({"vendors": [self.entry]}))
        self.assertEqual(checker.load_manifest(manifest), [self.entry])
        (root / "forgotten").mkdir()
        with self.assertRaisesRegex(ValueError, "unrecorded=.*forgotten"):
            checker.load_manifest(manifest)


if __name__ == "__main__":
    unittest.main()
