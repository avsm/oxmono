import json
from pathlib import Path
import subprocess
import sys
import tempfile

with tempfile.TemporaryDirectory() as directory:
    root = Path(directory)
    (root / "good.json").write_text(json.dumps({
        "lexicon": 1, "id": "com.example.good",
        "defs": {"main": {"type": "object", "properties": {}}}}))
    (root / "bad.json").write_text('{"lexicon":')
    result = subprocess.run([sys.argv[1], "generate", str(root), "-o", str(root / "out")],
                            capture_output=True, text=True)
    assert result.returncode != 0
    assert "Error parsing" in result.stderr
    assert not list((root / "out").glob("*.ml")), "Generated a partial lexicon set"
    print("PASS: invalid lexicons cannot silently produce partial bindings")
