"""Check that token serialization cannot be replaced through the public API."""
from pathlib import Path
import subprocess
import sys
import tempfile

compiler, interface = sys.argv[1:]
include = str(Path(interface).resolve().parent)
with tempfile.TemporaryDirectory() as directory:
    root = Path(directory)
    good = root / "good.ml"
    good.write_text("let raw (t : Jsonwt.t) = Jsonwt.raw t\n")
    command = [compiler, "-I", include, "-c"]
    subprocess.run(command + [str(good)], check=True, capture_output=True)
    bad = root / "bad.ml"
    bad.write_text('let forge (t : Jsonwt.t) = { t with raw = "forged" }\n')
    result = subprocess.run(
        command + [str(bad)], capture_output=True, text=True)
    assert result.returncode != 0, "JWT record update unexpectedly compiled"
    assert "Unbound record field" in result.stderr, result.stderr
    assert "raw" in result.stderr, result.stderr
print("Abstract token API: compiler rejects record forgery")
