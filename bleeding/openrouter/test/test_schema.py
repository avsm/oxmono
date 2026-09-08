"""Regression checks for safely importing public schema examples."""

import json
from pathlib import Path
import sys
import unittest

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import sanitize


class SchemaTests(unittest.TestCase):
    def test_keys_and_abbreviated_labels(self):
        prefix = "sk-or-" + "v1-"
        source = json.dumps({"key": prefix + "a" * 64,
                             "label": prefix + "abc...123"}, indent=2)
        self.assertEqual(sanitize.key_lines(source), [2, 3])
        cleaned = sanitize.sanitize(source)
        self.assertEqual(json.loads(cleaned),
                         {"key": sanitize.PLACEHOLDER,
                          "label": sanitize.PLACEHOLDER})
        self.assertEqual(sanitize.key_lines(cleaned), [])
        self.assertEqual(sanitize.sanitize(cleaned), cleaned)

    def test_non_credentials_and_layout_are_preserved(self):
        source = '{\n  "type": "string", "example": "ordinary value"\n}\n'
        self.assertEqual(sanitize.sanitize(source), source)

    def test_invalid_json_is_rejected(self):
        with self.assertRaises(json.JSONDecodeError):
            sanitize.sanitize("<html>not a schema</html>")


if __name__ == "__main__":
    unittest.main()
