"""Binary lookup caches preserve repository precedence and detect edits."""
import os
import shutil
import statistics
import time
from pathlib import Path
import subprocess
import sys
import tempfile

OX = str(Path(sys.argv[1]).resolve())


def write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


with tempfile.TemporaryDirectory(prefix="ox-run-cache-") as tmp:
    root = Path(tmp)
    repo = root / "repo"
    write(repo / "repo", 'opam-version: "2.0"\n')
    write(repo / "packages/oxcaml/oxcaml.1/opam", 'opam-version: "2.0"\n')

    def provider(repository, name, binary):
        write(repository / f"packages/{name}/{name}.1/opam",
              'opam-version: "2.0"\nx-ox-binaries: ["' + binary + '"]\n'
              'install: [["cp" "tool" "%{bin}%/' + binary + '"]\n'
              '["chmod" "+x" "%{bin}%/' + binary + '"]]\n')
        write(repository / f"packages/{name}/{name}.1/files/tool",
              '#!/bin/sh\nprintf "%s\\n" "$PWD" "$@"\n')

    provider(repo, "provider", "tool")
    if "--benchmark" in sys.argv[2:]:
        for i in range(5000):
            write(repo / f"packages/dummy-{i}/dummy-{i}.1/opam",
                  'opam-version: "2.0"\nsynopsis: "Benchmark metadata"\ndepends: ["oxcaml"]\n')
    args = [OX, "run", "--repository", str(repo), "--cache-dir",
            str(root / "cache"), "--data-dir", str(root / "data")]

    def run(*extra, ok=True):
        p = subprocess.run([*args, *extra], cwd=root, capture_output=True,
                           text=True, timeout=60)
        assert (p.returncode == 0) == ok, (extra, p.stdout, p.stderr)
        return p

    expected = str(root.resolve()) + "\nargument with spaces\n"
    assert run("tool", "--", "argument with spaces").stdout == expected
    indexes = list((root / "data/binary-owners").glob("*.sexp"))
    assert len(indexes) == 1
    os.utime(indexes[0], (1, 1))
    p = run("tool", "--", "argument with spaces")
    assert p.stdout == expected and "Using cached day10 layers" in p.stderr
    assert indexes[0].stat().st_mtime == 1
    # A second binary from the same metadata reuses the ownership index.
    assert "No package or binary mapping" in run("unknown", ok=False).stderr
    assert indexes[0].stat().st_mtime == 1
    assert len(list((root / "data/binary-owners").glob("*.sexp"))) == 1
    if "--benchmark" in sys.argv[2:]:
        cached, rebuild = [], []
        for _ in range(5):
            start = time.perf_counter()
            run("tool")
            cached.append(time.perf_counter() - start)
            for index in (root / "data/binary-owners").glob("*.sexp"):
                index.unlink()
            start = time.perf_counter()
            run("tool")
            rebuild.append(time.perf_counter() - start)
        print(f"5000-package local repo: indexed median {statistics.median(cached):.3f}s, "
              f"rebuilt-index median {statistics.median(rebuild):.3f}s")
    # A damaged derived index produces a bounded diagnostic, not a traceback.
    index = next((root / "data/binary-owners").glob("*.sexp"))
    index.write_text("(")
    assert "Invalid binary index" in run("tool", ok=False).stderr
    index.unlink()
    assert run("tool").stdout == str(root.resolve()) + "\n"
    # Local metadata edits invalidate the index and reveal new ambiguity.
    provider(repo, "other", "tool")
    assert "ambiguous" in run("tool", ok=False).stderr
    assert run("--with", "provider", "tool").stdout == str(root.resolve()) + "\n"
    # Earlier definitions shadow the same package/version in a later repo.
    overlay = root / "overlay"
    write(overlay / "repo", 'opam-version: "2.0"\n')
    provider(overlay, "other", "different")
    assert run("--overlay", str(overlay), "tool").stdout == str(root.resolve()) + "\n"
    assert run("--overlay", str(overlay), "different").stdout == str(root.resolve()) + "\n"
    # Restoring only an absent dependency prefix still works with a ready union.
    shutil.rmtree(root / "cache/prefixes")
    assert run("--overlay", str(overlay), "tool").stdout == str(root.resolve()) + "\n"
print("ox run cache: reuse, invalidation, ambiguity, precedence, cwd and restoration: OK")
