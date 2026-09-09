#!/usr/bin/env python3
"""Compare the OxCaml TOML port with a pristine ../ocaml-codec checkout."""
import argparse
from pathlib import Path
import shutil
import subprocess
import tempfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('upstream', type=Path)
parser.add_argument('--upstream-switch', default='5.5.0')
parser.add_argument('--port-switch', default='5.2.0+ox')
args = parser.parse_args()
root = Path(__file__).resolve().parents[2]
source = args.upstream.resolve()
probe = root / 'bleeding/mqttz/test/toml_probe.ml'
subprocess.run(['opam', 'exec', '--switch=' + args.port_switch, '--',
                'dune', 'build', '--profile', 'release-check',
                'bleeding/mqttz/test/toml_probe.exe'], cwd=root, check=True)
ported = subprocess.check_output(
    [root / '_build/default/bleeding/mqttz/test/toml_probe.exe'], cwd=root)
with tempfile.TemporaryDirectory(prefix='mqttz-toml-pristine-') as name:
    pristine = Path(name)
    shutil.copy2(root / 'vendor/ocaml-codec/dune-project', pristine)
    (pristine / 'dune-workspace').write_text('(lang dune 3.21)\n')
    for library in ['ascii', 'utf8', 'loc', 'toml']:
        destination = pristine / 'lib' / library
        destination.mkdir(parents=True)
        for file in (source / 'lib' / library).iterdir():
            if file.suffix in ['.ml', '.mli']:
                shutil.copy2(file, destination / file.name)
        # Only build metadata is borrowed. All ML/MLI files are pristine.
        shutil.copy2(root / 'vendor/ocaml-codec/lib' / library / 'dune', destination)
    (pristine / 'test').mkdir()
    shutil.copy2(probe, pristine / 'test/probe.ml')
    (pristine / 'test/dune').write_text(
        '(executable (name probe) (libraries codec.toml))\n')
    subprocess.run(['opam', 'exec', '--switch=' + args.upstream_switch, '--',
                    'dune', 'build', '--root', str(pristine), 'test/probe.exe'],
                   cwd=pristine, check=True)
    upstream = subprocess.check_output([pristine / '_build/default/test/probe.exe'])
    if ported != upstream:
        raise SystemExit('TOML port differs from pristine source')
    print(f'PASS TOML differential: {len(ported.splitlines())} cases')
