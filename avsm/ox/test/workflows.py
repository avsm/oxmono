"""Package and working-tree builds share the day10 path and repository policy."""
import os
from pathlib import Path
import subprocess
import sys
import tempfile

OX = str(Path(sys.argv[1]).resolve())


def write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


def call(args, cwd=None, code=0):
    p = subprocess.run([OX] + args, cwd=cwd, text=True, capture_output=True, timeout=60)
    assert p.returncode == code, (args, p.returncode, p.stdout, p.stderr)
    return p


def git(path, *args):
    subprocess.run(["git", "-C", str(path), *args], check=True, capture_output=True)


with tempfile.TemporaryDirectory(prefix="ox-workflows-") as tmp:
    root = Path(tmp)
    base = root / "base"
    write(base / "repo", 'opam-version: "2.0"\n')

    def pkg(name, body, version="1"):
        directory = base / "packages" / name / (name + "." + version)
        write(directory / "opam", 'opam-version: "2.0"\n' + body)
        return directory

    pkg("oxcaml", 'depends: ["oxcaml-patch-guards"]\n')
    pkg("oxcaml-patch-guards", 'depends: ["oxcaml-local-lib" "oxcaml-external-lib"]\n')
    pkg("oxcaml-local-lib", 'conflicts: ["local-lib"]\n', "guard")
    pkg("oxcaml-external-lib", 'conflicts: ["external-lib" {< "2"}]\n', "guard")
    pkg("external-lib", 'build: [["false"]]\n')
    pkg("external-lib", 'install: [["mkdir" "-p" "%{lib}%/external-lib"] ["sh" "-c" "echo patched > %{lib}%/external-lib/value"]]\n', "2")
    pkg("test-helper", 'install: [["mkdir" "-p" "%{bin}%"] ["cp" "helper" "%{bin}%/test-helper"] ["chmod" "+x" "%{bin}%/test-helper"]]\n')
    write(base / "packages/test-helper/test-helper.1/files/helper", '#!/bin/sh\necho tested >> "$OX_TEST_REPORT"\n')
    report = root / "test-report"
    # Recipes deliberately use the file path explicitly: build env is isolated.
    library = pkg("library", f'''depends: ["external-lib" "test-helper" {{with-test}}]
install: [["mkdir" "-p" "%{{lib}}%/library"] ["sh" "-c" "echo built > %{{lib}}%/library/value"]]
run-test: [["sh" "-c" "test-helper" ]]
build-env: [OX_TEST_REPORT = "{report}"]
depexts: [["system-fixture"] {{os = "macos" | os = "linux"}}]
''')
    config = ["--repository", str(base), "--cache-dir", str(root / "cache"), "--data-dir", str(root / "data")]
    p = call(["build", *config, "library"])
    assert Path(p.stdout.strip(), "lib/library/value").read_text().strip() == "built"
    assert "Building external-lib.2" in p.stderr
    assert "test-helper" not in p.stderr
    p = call(["build", *config, "library"])
    assert "Using cached day10 layers" in p.stderr
    assert "Plan library.1" in call(["show", *config, "library"]).stderr
    assert call(["build", *config, "--depext", "library"]).stdout == "system-fixture\n"
    for _ in range(2):
        call(["test", *config, "library"])
    assert report.read_text().splitlines() == ["tested", "tested"]
    p = call(["exec", *config, "--with", "library", "--", "sh", "-c", 'cat "$OCAMLPATH/library/value"'])
    assert p.stdout == "built\n"
    assert "export OCAMLPATH=" in call(["env", *config, "--with", "library"]).stdout

    project = root / "project"
    project.mkdir()
    git(project, "init", "-q")
    write(project / ".gitignore", "_build/\n")
    write(project / "libs/local/dune-project", '(lang dune 3.21)\n(name local-lib)\n(version 1.0)\n')
    write(project / "libs/local/local-lib.opam", '''opam-version: "2.0"
depends: ["external-lib" {>= "2"} "test-helper" {with-test}]
''')
    write(project / "libs/local/value", "committed\n")
    git(project, "add", ".")
    git(project, "-c", "user.name=Ox test", "-c", "user.email=ox@example.invalid", "commit", "-qm", "snapshot")
    # Simulate a Dune frontend that reads the working tree and installed deps.
    dune = pkg("dune", 'depends: ["dune-own-tests-not-requested" {with-test}]\ninstall: [["mkdir" "-p" "%{bin}%"] ["cp" "dune" "%{bin}%/dune"] ["chmod" "+x" "%{bin}%/dune"]]\n')
    write(dune / "files/dune", '#!/bin/sh\nset -eu\ncat libs/local/value\ncat "$OCAMLPATH/external-lib/value"\nprintf "%s\\n" "$@"\n')
    write(project / "libs/local/value", "dirty\n")
    p = call(["build", *config], cwd=project)
    assert p.stdout.startswith("dirty\npatched\n"), p
    assert "@libs/local/all" in p.stdout
    assert "Building local-lib" not in p.stderr
    call(["build", *config, "--depext"], cwd=project)
    call(["build", *config, "--fetch"], cwd=project)
    p = call(["test", *config], cwd=project)
    assert "@libs/local/runtest" in p.stdout and "--force" in p.stdout
    p = call(["build", *config, "--local", "--deps-only", "local-lib"], cwd=project)
    assert Path(p.stdout.strip(), "lib/external-lib/value").exists()
    assert not Path(p.stdout.strip(), "lib/local-lib").exists()
    p = call(["build", *config, "--from", str(project), "local-lib"])
    assert "Building local-lib.1.0+ox.1." in p.stderr
    # An external package can depend on a local prerequisite. Only that local
    # prerequisite enters the immutable cache, and source-only edits rebuild it.
    write(project / "libs/local/local-lib.opam", '''opam-version: "2.0"
depends: ["external-lib" {>= "2"}]
install: [["mkdir" "-p" "%{lib}%/local-lib"] ["cp" "value" "%{lib}%/local-lib/value"]]
''')
    pkg("consumer", '''depends: ["local-lib" {>= "1.0"}]
install: [["mkdir" "-p" "%{lib}%/consumer"] ["cp" "%{lib}%/local-lib/value" "%{lib}%/consumer/value"]]
''')
    write(project / "apps/app/dune-project", '(lang dune 3.21)\n(name app)\n')
    write(project / "apps/app/app.opam", 'opam-version: "2.0"\ndepends: ["consumer"]\n')
    for value in ["first", "second"]:
        write(project / "libs/local/value", value + "\n")
        p = call(["build", *config, "--local", "--deps-only", "app"], cwd=project)
        assert Path(p.stdout.strip(), "lib/consumer/value").read_text() == value + "\n"
    # Root discovery excludes vendor roots, but keeps them available as deps.
    write(project / "vendor/broken/dune-project", '(lang dune 3.21)\n(name broken)\n')
    write(project / "vendor/broken/broken.opam", 'opam-version: "2.0"\ndepends: ["does-not-exist"]\n')
    call(["build", *config, "--dry-run"], cwd=project)
    # A fetch dry run must not try the missing source.
    pkg("missing-source", 'url { src: "file:///does-not-exist.tar.gz" }\n')
    call(["build", *config, "--fetch", "--dry-run", "missing-source"])
    assert not list((root / "cache").rglob(".opam-switch"))
print("ox workflows: libraries, repeated tests, environments, dirty workspace, external patch guards: OK")
