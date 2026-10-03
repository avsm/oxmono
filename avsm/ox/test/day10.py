"""Check cold toolchain recipes and source/cache semantics without a host compiler."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

OX = str(Path(sys.argv[1]).resolve())


def write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


def call(args, code=0):
    p = subprocess.run(args, text=True, capture_output=True)
    assert p.returncode == code, (args, p.returncode, p.stdout, p.stderr)
    return p


def commit(path):
    call(["git", "-C", str(path), "add", "."])
    call(["git", "-C", str(path), "-c", "user.name=Ox test",
          "-c", "user.email=ox@example.invalid", "commit", "-qm", "snapshot"])


with tempfile.TemporaryDirectory(prefix="ox-day10-") as temp:
    root = Path(temp)
    # Neither the CLI nor an inherited compiler may supply the toolchain.
    for name in ["opam", "ocamlc", "ocamlopt"]:
        write(root / "forbidden" / name, "#!/bin/sh\nexit 97\n")
        (root / "forbidden" / name).chmod(0o755)
    os.environ["PATH"] = str(root / "forbidden") + os.pathsep + os.environ["PATH"]
    os.environ["HOME"] = str(root / "home")
    (root / "home").mkdir()
    repo = root / "repo"
    write(repo / "repo", 'opam-version: "2.0"\n')

    def recipe(name, body, version="1"):
        directory = repo / "packages" / name / (name + "." + version)
        write(directory / "opam", 'opam-version: "2.0"\n' + body)
        return directory

    # A tiny default toolchain fixture. The separate real compiler check builds
    # OxCaml from its repository recipe; this keeps routine tests inexpensive.
    tc = recipe("oxcaml", '''
install: [["cp" "fixture-cc" "%{bin}%/fixture-cc"]
          ["chmod" "+x" "%{bin}%/fixture-cc"]]
setenv: [[OX_FIXTURE = "%{share}%/fixture"]]
''')
    write(tc / "files/fixture-cc", '#!/bin/sh\ncp "$1" "$2"\nchmod +x "$2"\n')
    write(tc / "files/oxcaml.config", 'opam-version: "2.0"\nvariables { greeting: "configured" }\n')
    source = root / "source"
    source.mkdir()
    call(["git", "init", "-q", str(source)])

    def program(value):
        write(source / "hello.in", '#!/bin/sh\nprintf "%s\\n" "' + value +
              ':%{version}%:%{oxcaml:greeting}%:$OX_FIXTURE"\n')
        commit(source)

    program("first")
    hello = recipe("hello", f'''
depends: ["oxcaml"]
url {{ src: "git+file://{source}#HEAD" }}
substs: ["hello"]
build: [["test" "!" "-f" "hello.opam"] ["fixture-cc" "hello" "hello-built"]]
''')
    write(hello / "files/hello.install", 'bin: ["hello-built" {"hello"}]\n')
    args = [OX, "run", "--repository", str(repo), "--cache-dir", str(root / "cache"),
            "--data-dir", str(root / "data")]
    p = call(args + ["hello"])
    assert p.stdout.startswith("first:1:configured:"), p
    assert p.stdout.strip().endswith("/share/fixture")
    assert "Building oxcaml.1" in p.stderr
    assert not list((root / "cache").rglob(".opam-switch"))
    layers = list((root / "cache/layers").glob("*/*/layer.json"))
    compiler_layer = next(p.parent for p in layers if json.loads(p.read_text())["package"] == "oxcaml.1")
    original_compiler = (compiler_layer / "fs/bin/fixture-cc").read_bytes()
    # An unrelated root must reuse the compiler and hello, even if its installer
    # overwrites a dependency file. Cached layer hardlinks must stay intact.
    recipe("extra", '''depends: ["oxcaml"]
install: [["sh" "-c" "printf changed > %{bin}%/fixture-cc"]]
''')
    p = call(args + ["--with", "hello", "--with", "extra", "hello"])
    assert p.stdout.startswith("first:1:configured:")
    assert "Building hello.1" not in p.stderr and "Building oxcaml.1" not in p.stderr
    assert (compiler_layer / "fs/bin/fixture-cc").read_bytes() == original_compiler
    assert sum(json.loads(p.read_text())["package"] == "hello.1"
               for p in (root / "cache/layers").glob("*/*/layer.json")) == 1
    # Refresh mutable refs even when their opam metadata did not change.
    program("second")
    p = call(args + ["hello"])
    assert p.stdout.startswith("first:"), p
    p = call(args + ["--refresh", "hello"])
    assert p.stdout.startswith("second:1:configured:"), p
    assert "Cached oxcaml.1" in p.stderr
    # Downloads must verify declared checksums before any recipe runs.
    archive = root / "bad.tar"
    archive.write_bytes(b"invalid source archive")
    recipe("bad", f'''url {{ src: "file://{archive}" checksum: "sha256={'0' * 64}" }}
''')
    p = call(args + ["--with", "hello", "--with", "bad", "hello"], code=124)
    assert "Could not fetch verified source" in p.stderr
    bad_file = recipe("bad-file", 'extra-files: [["patch" "sha256=' + '0' * 64 + '"]]\n')
    write(bad_file / "files/patch", "incorrect patch contents\n")
    p = call(args + ["--with", "hello", "--with", "bad-file", "hello"], code=124)
    assert "Repository file failed checksum verification" in p.stderr
    # A version named dev still runs release recipes. The solver and builder
    # must agree about disabled dev/test/doc dependencies and actions.
    recipe("release", '''depends: ["oxcaml" "missing" {dev | with-test | with-doc}]
build: [["false"] {dev | with-test | with-doc}]
''', version="dev")
    call(args + ["--with", "hello", "--with", "release.dev", "hello"])
    # An unresolved pin cannot silently select a release from another source.
    recipe("pinned", f'''depends: ["hello"]
pin-depends: [["hello.1" "git+file://{source}#HEAD"]]
''')
    p = call(args + ["--with", "pinned", "hello"], code=124)
    assert "pin-depends" in p.stderr
    # A valid checksummed tar uses the same prepare path and works offline.
    archive = root / "good.tar"
    call(["tar", "-cf", str(archive), "-C", str(source), "hello.in"])
    digest = hashlib.sha256(archive.read_bytes()).hexdigest()
    recipe("archive", f'''url {{ src: "file://{archive}" checksum: "sha256={digest}" }}
build: [["test" "-f" "hello.in"]]
''')
    call(args + ["--with", "hello", "--with", "archive", "hello"])
    source.rename(root / "offline")
    archive.unlink()
    p = call(args + ["--with", "hello", "--with", "archive", "hello"])
    assert "Using cached day10 layers" in p.stderr
print("ox day10: cold toolchain, configs, substs, install files, environment, sharing, refresh, checksums: OK")
