"""Exercise source snapshots and day10 builds with an unusable opam CLI."""
import concurrent.futures
import os
from pathlib import Path
import shutil
import signal
import json
import subprocess
import sys
import tempfile

OX = str(Path(sys.argv[1]).resolve())
COMPILER = os.environ.get("OX_TEST_COMPILER_PREFIX") or str(
    Path(shutil.which("ocamlc")).resolve().parent.parent
)


def call(args, *, cwd=None, code=0):
    p = subprocess.run(args, cwd=cwd, text=True, capture_output=True)
    assert p.returncode == code, (args, p.returncode, p.stdout, p.stderr)
    return p


def write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


def commit(path, message="snapshot"):
    call(["git", "-C", str(path), "add", "."])
    call(["git", "-C", str(path), "-c", "user.name=Ox test",
          "-c", "user.email=ox@example.invalid", "commit", "-qm", message])
    return call(["git", "-C", str(path), "rev-parse", "HEAD"]).stdout.strip()


def init(path):
    path.mkdir()
    call(["git", "init", "-q", str(path)])


def package(path, name, deps, build, install):
    write(path / (name + ".opam"), 'opam-version: "2.0"\n'
          'synopsis: "Ox integration fixture"\n'
          f'depends: [{deps}]\nbuild: [{build}]\ninstall: [{install}]\n')


def library(path, name, module, text):
    write(path / "dune-project", f'(lang dune 3.21)\n(name {name})\n(version 1.2.0)\n')
    write(path / (module + ".ml"), f'let text = "{text}"\n')
    package(path, name, '"ocaml" {>= "5.2"}',
            f'["ocamlc" "-c" "{module}.ml"] '
            f'["ocamlc" "-a" "{module}.cmo" "-o" "{module}.cma"] '
            f'["ocamlopt" "-c" "{module}.ml"] '
            f'["ocamlopt" "-a" "{module}.cmx" "-o" "{module}.cmxa"]',
            f'["mkdir" "-p" "%{{lib}}%/{name}"] '
            f'["cp" "{module}.cmi" "{module}.cma" "{module}.cmx" '
            f'"{module}.cmxa" "{module}.a" "%{{lib}}%/{name}"]')


with tempfile.TemporaryDirectory(prefix="ox-integration-") as temp:
    root = Path(temp)
    forbidden = root / "forbidden"
    write(forbidden / "opam", '#!/bin/sh\necho "unexpected opam CLI invocation" >&2\nexit 97\n')
    (forbidden / "opam").chmod(0o755)
    os.environ["PATH"] = str(forbidden) + os.pathsep + os.environ["PATH"]
    os.environ["HOME"] = str(root / "home")
    (root / "home").mkdir()
    os.environ.pop("OPAMROOT", None)
    os.environ.pop("OPAM_SWITCH_PREFIX", None)
    source = root / "source"
    external = root / "external"
    base = root / "base"
    init(source)
    init(external)
    library(external, "external-lib", "outside", "external Git dependency")
    ext_commit = commit(external)
    library(source / "lib", "hello-lib", "message", "cloned dependency")
    # The dependency also supplies a C stub, exercised by both executable modes.
    write(source / "lib/message.ml", 'external text_value : unit -> string = "ox_message"\nlet text = text_value ()\n')
    write(source / "lib/message_stubs.c", '#include <caml/mlvalues.h>\n#include <caml/alloc.h>\nCAMLprim value ox_message(value unit) { (void)unit; return caml_copy_string("cloned dependency"); }\n')
    package(source / "lib", "hello-lib", '"ocaml" {>= "5.2"}',
            '["ocamlc" "-c" "message.ml"] ["ocamlopt" "-c" "message.ml"] '
            '["ocamlc" "-c" "message_stubs.c"] '
            '["ocamlmklib" "-o" "message" "message.cmo" "message.cmx" "message_stubs.o"]',
            '["mkdir" "-p" "%{lib}%/hello-lib" "%{lib}%/stublibs"] '
            '["cp" "message.cmi" "message.cma" "message.cmx" "message.cmxa" '
            '"message.a" "libmessage.a" "%{lib}%/hello-lib"] '
            '["cp" "dllmessage.so" "%{lib}%/stublibs"]')
    app = source / "app"
    write(app / "dune-project", '(lang dune 3.21)\n(name hello-app)\n(version 1.2.0)\n')
    write(app / "dune", '(executables (names main mainbyte) '
          '(public_names greet greet-byte) (package hello-app))\n')
    write(app / "main.ml", '''let () =
  Printf.printf "%s|%s|%s|%s\\n%!" Message.text Outside.text (Sys.getcwd ())
    (String.concat "|" (List.tl (Array.to_list Sys.argv)));
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "exit7" then exit 7;
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "wait" then while true do () done
''')
    dirs = '"-I" "%{hello-lib:lib}%" "-I" "%{external-lib:lib}%" '
    package(app, "hello-app", '"ocaml" "hello-lib" {= version & >= "1.2.0"} "external-lib"',
            f'["ocamlopt" {dirs} "message.cmxa" "outside.cmxa" "main.ml" "-o" "greet"] '
            f'["ocamlc" {dirs} "message.cma" "outside.cma" "main.ml" "-o" "greet-byte"]',
            '["cp" "greet" "greet-byte" "%{bin}%"]')
    # Empty files mark Dune package ownership but provide no opam build recipe.
    write(source / "empty/dune-project", '(lang dune 3.21)\n(name placeholder)\n')
    write(source / "empty/placeholder.opam", "")
    original = commit(source)
    overlay = root / "stamped"
    p = call([OX, "stamp", str(source), "--output", str(overlay)])
    assert "Skipping empty opam placeholder" in p.stderr
    assert len(p.stdout.splitlines()) == 2
    version = "1.2.0+ox.1." + original[:12]
    app_opam = (overlay / "packages/hello-app" / ("hello-app." + version) / "opam").read_text()
    assert original in app_opam and 'subpath: "app"' in app_opam
    assert f'= "{version}"' in app_opam
    assert 'x-ox-binaries: ["greet" "greet-byte"]' in app_opam
    call([OX, "stamp", str(source), "--output", str(overlay)], code=124)
    # Dirty source does not affect a committed snapshot. A new commit does.
    write(source / "lib/message.ml", 'let text = "changed fork"\n')
    dirty = root / "dirty"
    call([OX, "stamp", str(source), "--output", str(dirty)])
    assert (dirty / "packages/hello-app" / ("hello-app." + version) / "opam").read_text() == app_opam
    next_commit = commit(source, "fork edit")
    changed = root / "changed"
    p = call([OX, "stamp", str(source), "--output", str(changed)])
    assert "1.2.0+ox.2." + next_commit[:12] in p.stdout
    # Keep the run on the first revision to exercise --ref after cloning.
    write(base / "repo", 'opam-version: "2.0"\n')
    ext_opam = (external / "external-lib.opam").read_text()
    write(base / "packages/external-lib/external-lib.1.0/opam", ext_opam +
          f'url {{ src: "git+file://{external}#{ext_commit}" }}\n')
    # Preserve upstream patch guards, while local snapshots carry their own patches.
    guard_defs = {
        "oxcaml-patch-guards.ox": 'depends: ["oxcaml-hello-lib" ("oxcaml-external-lib" | "oxcaml-external-lib-patches")]\n',
        "oxcaml-hello-lib.guard": 'conflicts: ["hello-lib"]\n',
        "oxcaml-external-lib.guard": 'conflicts: ["external-lib" "oxcaml-external-lib-patches"]\n',
        "oxcaml-external-lib-patches.enabled": 'depends: ["external-lib" {= "1.0"}] conflicts: ["oxcaml-external-lib"]\n',
    }
    for nv, definition in guard_defs.items():
        write(base / "packages" / nv.split(".")[0] / nv / "opam",
              'opam-version: "2.0"\n' + definition)
    write(base / "packages/external-lib/external-lib.2.0/opam",
          'opam-version: "2.0"\nbuild: [["false"]]\n')
    # This higher upstream version must not replace the selected fork snapshot.
    write(base / "packages/hello-app/hello-app.99/opam",
          'opam-version: "2.0"\nbuild: [["false"]]\n')
    args = [OX, "run", "--from", "file://" + str(source), "--ref", original,
            "--repository", str(base), "--cache-dir", str(root / "cache"),
            "--data-dir", str(root / "data"), "--compiler-prefix", COMPILER]
    command = args + ["greet", "--", "hello", "two words", "--literal"]
    dry = call(args + ["--dry-run", "greet"], cwd=root)
    assert "hello-app" in dry.stderr
    assert not (root / "cache/layers").exists()
    assert not (root / "cache/prefixes").exists()
    assert not (root / "cache/runs").exists()
    # Both processes race for an incomplete environment left by the dry run. Only one may publish it.
    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
        results = list(pool.map(lambda _: call(command, cwd=root), range(2)))
    expected = f"cloned dependency|external Git dependency|{root.resolve()}|hello|two words|--literal\n"
    assert all(p.stdout == expected for p in results), [p.stdout for p in results]
    assert sum("Using cached day10 layers" in p.stderr for p in results) == 1
    receipts = list((root / "cache/requests").glob("*.sexp"))
    assert len(receipts) == 1
    assert not list((root / "cache").rglob(".opam-switch"))
    layers = list((root / "cache/layers").glob("*/*/layer.json"))
    metadata = [(p, json.loads(p.read_text())) for p in layers]
    for package_name in ["hello-app.", "hello-lib.", "external-lib."]:
        assert any(m["package"].startswith(package_name) for _, m in metadata)
    app_layer = next(p.parent for p, m in metadata if m["package"].startswith("hello-app."))
    assert (app_layer / "recipe.json").exists()
    assert (app_layer / "fs/bin/greet").exists()
    external_layer = next(p.parent for p, m in metadata if m["package"].startswith("external-lib."))
    external_digest = (external_layer / "fs/lib/external-lib/outside.cma").read_bytes()
    # Reconstruct an application prefix and the run prefix from actual day10 layers.
    shutil.rmtree(root / "cache/prefixes" / app_layer.parent.name / app_layer.name)
    shutil.rmtree(root / "cache/runs")
    # Remove both upstream source repositories. Warm execution must stay offline.
    source.rename(root / "source-offline")
    external.rename(root / "external-offline")
    p = call(command, cwd=root)
    assert p.stdout == expected and "Using cached day10 layers" in p.stderr
    p = call(args + ["greet-byte", "--", "hello", "two words", "--literal"], cwd=root)
    assert p.stdout == expected and "Using cached day10 layers" in p.stderr
    p = call(args + ["greet", "--", "exit7"], cwd=root, code=7)
    assert "Using cached day10 layers" in p.stderr
    # An explicit provider can expose another binary in the same cached closure.
    p = call(args + ["--with", "hello-app", "greet-byte"], cwd=root)
    assert "Using cached day10 layers" in p.stderr
    p = call(args + ["--with", "hello-app", "absent"], code=124)
    assert "no binary absent" in p.stderr
    child = subprocess.Popen(args + ["greet", "--", "wait"], cwd=root,
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    try:
        assert "|wait" in child.stdout.readline()
        child.terminate()
        child.communicate(timeout=10)
        assert child.returncode == -signal.SIGTERM
    finally:
        if child.poll() is None:
            child.kill()
            child.communicate()
    # Refresh observes a new source commit. Failed builds cannot publish a cache.
    (root / "source-offline").rename(source)
    (root / "external-offline").rename(external)
    sentinel = root / "allow-build"
    recipe = app / "hello-app.opam"
    recipe.write_text(recipe.read_text().replace('build: [',
                      f'build: [["test" "-f" "{sentinel}"] '))
    commit(source, "test retry")
    refreshed = args.copy()
    refreshed[refreshed.index("--ref") + 1] = "HEAD"
    refreshed += ["--refresh", "greet"]
    p = call(refreshed, cwd=root, code=124)
    assert "Building hello-app" in p.stderr
    assert len(list((root / "cache/requests").glob("*.sexp"))) == 1
    sentinel.touch()
    p = call(refreshed, cwd=root)
    assert p.stdout.startswith("changed fork|external Git dependency|")
    assert len(list((root / "cache/requests").glob("*.sexp"))) == 2
    assert (external_layer / "fs/lib/external-lib/outside.cma").read_bytes() == external_digest
    assert sum(json.loads(p.read_text())["package"].startswith("external-lib.")
               for p in (root / "cache/layers").glob("*/*/layer.json")) == 1
    # A damaged receipt is rejected, rather than passed to opam exec.
    receipts[0].write_text("invalid\n")
    p = call(command, code=124)
    assert "Invalid cache receipt" in p.stderr
print("ox integration: stamps, fork precedence, Git dependencies, concurrent builds, offline native/bytecode, exit status: OK")
