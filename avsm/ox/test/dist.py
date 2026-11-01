"""Build an exported source bundle offline, without ox or opam at build time."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile
import tempfile

OX = str(Path(sys.argv[1]).resolve())


def call(args, *, cwd=None, code=0, env=None):
    p = subprocess.run(args, cwd=cwd, env=env, text=True, capture_output=True)
    assert p.returncode == code, (args, p.returncode, p.stdout, p.stderr)
    return p


def write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


with tempfile.TemporaryDirectory(prefix="ox-dist-") as tmp:
    root = Path(tmp)
    repo = root / "repo"
    source = root / "source"
    source.mkdir()
    call(["git", "init", "-q", str(source)])
    write(repo / "repo", 'opam-version: "2.0"\n')
    compiler = repo / "packages/oxcaml/oxcaml.1"
    write(compiler / "opam", 'opam-version: "2.0"\n'
          'build: [["cp" "config" "oxcaml.config"]]\n'
          'setenv: [[OX_DIST_RELEASE = "%{_:release}%"]]\n')
    write(compiler / "files/config", 'opam-version: "2.0"\nvariables {\n'
          '  release: "test-release"\n}\n')
    lib = source / "lib"
    write(lib / "dune-project", '(lang dune 3.21)\n(name dist-lib)\n(version 1.2.0)\n')
    write(lib / "dist-lib.opam", '''opam-version: "2.0"
depends: ["oxcaml"]
build: [["cc" "-c" "answer.c"] ["ar" "rcs" "libanswer.a" "answer.o"]]
install: [["mkdir" "-p" "%{lib}%/dist-lib"]
          ["cp" "libanswer.a" "%{lib}%/dist-lib"]
          ["sh" "-c" "printf dependency-tool > '%{bin}%/dependency-tool'"]]
depexts: [["debian-fixture"] {os-distribution = "debian"}
          ["fedora-fixture"] {os-distribution = "fedora"}]
''')
    write(lib / "answer.c", 'int answer(void) { return 42; }\n')
    app = source / "app"
    write(app / "dune-project", '(lang dune 3.21)\n(name dist-app)\n(version 1.2.0)\n')
    write(app / "dune", '(executable (name main) (public_name greet) (package dist-app))\n')
    write(app / "dist-app.opam", '''opam-version: "2.0"
synopsis: "Distribution test application"
maintainer: "Builder <builder@example.org>"
license: "ISC"
depends: ["dist-lib"]
substs: ["settings.h"]
patches: ["answer.patch"]
build: [
  ["false"] {os != "linux"}
  ["test" "-f" "%{opamfile}%"]
  ["test" "-L" "link.txt"]
  ["test" "%{oxcaml:release}%" "=" "test-release"]
  ["sh" "-c" "test $OX_DIST_RELEASE = test-release"]
  ["sh" "-c" "test $OI_STATIC = 1"]
  ["sh" "-c" "grep -F 'CHOICE' settings.h"]
  ["test" "%{build}%" "=" "%{build}%"]
  ["cc" "main.c" "%{dist-lib:lib}%/libanswer.a" "-o" "greet"]
  ["sh" "-c" "printf 'bin: [ \\\"greet\\\" ]\\nshare: [ \\\"data.txt\\\" ]\\n' > dist-app.install"]
]
''')
    write(repo / "packages/bad-config/bad-config.1/opam",
          'opam-version: "2.0"\ndepends: ["oxcaml"]\n'
          'build: [["echo" "%{oxcaml:unknown-config}%"]]\n')
    write(app / "settings.h.in", '#define VERSION "%{version}%"\n'
          '#define FORMAT "%%s"\n'
          '#define ABSENT "%{absent:version}%"\n'
          '#define CHOICE "%{dist-lib:installed?yes:no}%"\n')
    write(app / "main.c", '#include <stdio.h>\n#include "settings.h"\n'
          'extern int answer(void);\nint main(void) { printf("unpatched %d %s\\n", answer(), VERSION); }\n')
    write(app / "answer.patch", '--- a/main.c\n+++ b/main.c\n@@ -4 +4 @@\n'
          '-int main(void) { printf("unpatched %d %s\\n", answer(), VERSION); }\n'
          '+int main(void) { printf("answer %d %s\\n", answer(), VERSION); }\n')
    write(app / "data.txt", "package data\n")
    # The source's symlink must survive export.
    (app / "link.txt").symlink_to("data.txt")
    call(["git", "-C", str(source), "add", "."])
    call(["git", "-C", str(source), "-c", "user.name=Ox test", "-c",
          "user.email=ox@example.org", "commit", "-qm", "snapshot"])
    commit = call(["git", "-C", str(source), "rev-parse", "HEAD"]).stdout.strip()
    call(["git", "-C", str(source), "branch", "minus39"])
    args = [OX, "dist", "pkg", "--from", source.as_uri() + "#minus39",
            "--repository", str(repo), "--data-dir", str(root / "data"),
            "--cache-dir", str(root / "cache"), "--distros", "debian-13,fedora-44",
            "--maintainer", "Test <test@example.org>"]
    output = root / "output"
    call(args + ["-o", str(output), "--", "greet"])
    call(args + ["-o", str(output), "greet"], code=124)
    call(args + ["--distros", "no-such-distro", "-o", str(root / "bad"), "greet"], code=124)
    version = "1.2.0+ox.1." + commit[:12]
    assert "platform: linux/amd64" in (output / "compose.yaml").read_text()
    for tag, dependency in [("debian-13", "debian-fixture"), ("fedora-44", "fedora-fixture")]:
        archive = next((output / "bundle" / tag).glob("*.tar.gz"))
        assert archive.name == f"dist-app-{version}.tar.gz"
        assert (archive.with_suffix(archive.suffix + ".sha256")).read_text().startswith(
            hashlib.sha256(archive.read_bytes()).hexdigest())
        sidecar = json.loads(archive.with_name(f"dist-app-{version}.osdist.json").read_text())
        assert sidecar["depexts"] == {tag: [dependency]}
        assert sidecar["maintainer"] == "Test <test@example.org>"
        assert dependency in (output / tag / "Dockerfile").read_text()
        assert archive.read_bytes() == (output / tag / archive.name).read_bytes()
    # Unknown generated variables fail at build time with a useful diagnostic.
    failed = root / "bad-config"
    call(args + ["--with", "bad-config", "-o", str(failed), "greet"])
    bad_tree = root / "bad-unpacked"
    bad_tree.mkdir()
    with tarfile.open(next((failed / "bundle/debian-13").glob("*.tar.gz"))) as tar:
        tar.extractall(bad_tree)
    result = call(["sh", "build.sh", "build"], cwd=next(bad_tree.iterdir()), code=2)
    assert "unknown-config" in result.stderr
    # The driver tries each target and reports failure even if the last succeeds.
    fake = root / "fake-docker"
    log = root / "docker.log"
    write(fake / "docker", '#!/bin/sh\nprintf "%s\\n" "$*" >> "$DOCKER_LOG"\n'
          'case "$*" in *build*debian-13*) exit 1;; esac\n')
    (fake / "docker").chmod(0o755)
    driver_env = dict(os.environ, PATH=str(fake) + os.pathsep + os.environ["PATH"],
                      DOCKER_LOG=str(log))
    call(["sh", str(output / "build.sh")], env=driver_env, code=1)
    calls = log.read_text().splitlines()
    assert len(calls) == 3, calls
    assert calls[-1].endswith("run --rm fedora-44"), calls
    # Export twice: metadata, archive ordering and gzip headers are stable.
    again = root / "again"
    call(args + ["-o", str(again), "greet"])
    for archive in (output / "bundle").rglob("*.tar.gz"):
        assert archive.read_bytes() == (again / archive.relative_to(output)).read_bytes()
    # Standalone build in a different directory after deleting the source inputs.
    unpack = root / "unpacked & #"
    unpack.mkdir()
    archive = next((output / "bundle/debian-13").glob("*.tar.gz"))
    with tarfile.open(archive) as tar:
        tar.extractall(unpack)
    tree = next(unpack.iterdir())
    plan = json.loads((tree / "plan.json").read_text())
    assert plan.get("external_layers", []) == []
    for path in (tree / "recipes").iterdir():
        assert str(root) not in path.read_text(), path
    shutil.rmtree(source)
    shutil.rmtree(root / "cache")
    shutil.rmtree(root / "data")
    forbidden = root / "forbidden"
    for exe in ["ox", "opam"]:
        write(forbidden / exe, '#!/bin/sh\nexit 97\n')
        (forbidden / exe).chmod(0o755)
    env = dict(os.environ, PATH=str(forbidden) + os.pathsep + os.environ["PATH"], OI_STATIC="1")
    call(["sh", "build.sh", "build", "2"], cwd=tree, env=env)
    assert call([str(tree / "dest/bin/greet")]).stdout == f"answer 42 {version}\n"
    assert not (tree / "dest/bin/dependency-tool").exists()
    dest = root / "installed"
    call(["sh", "build.sh", "install", "/usr", str(dest)], cwd=tree, env=env)
    assert call([str(dest / "usr/bin/greet")]).stdout == f"answer 42 {version}\n"
    assert (dest / "usr/share/dist-app/data.txt").read_text() == "package data\n"
    assert not (dest / "usr/bin/dependency-tool").exists()
    assert not (dest / "usr/lib").exists()
    call(["make", "clean"], cwd=tree, env=env)
    assert (tree / "sources").exists()
    substituted = next(p for p in (tree / "sources").rglob("settings.h.in"))
    text = substituted.read_text()
    assert '#define FORMAT "%s"' in text
    assert '#define ABSENT ""' in text
    assert '#define CHOICE "yes"' in text
print("ox distribution bundle tests passed")
