#!/usr/bin/env python3
"""Build the sibling Tangled knot/appview and run the local CI integration stack."""
import argparse
import shutil
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent
REPO = ROOT.parents[2]
STATE = ROOT / '.state'


def run(*args, **kwargs):
    return subprocess.run(args, check=True, **kwargs)


def base58(data):
    number = int.from_bytes(data, 'big')
    value = ''
    alphabet = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
    while number:
        number, digit = divmod(number, 58)
        value = alphabet[digit] + value
    return value


def prepare(core):
    for program in ('knot', 'appview'):
        run('go', 'build', '-o', str(STATE / program), '-mod=readonly',
            './cmd/' + program, cwd=core)
    revision = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=core,
                                       text=True).strip()
    (STATE / 'tangled-revision').write_text(revision + '\n')
    for part in ('templates', 'static', 'legal'):
        shutil.copytree(core / 'appview/pages' / part,
                        STATE / 'appview-pages' / part, dirs_exist_ok=True)
    secret = STATE / 'appview.env'
    if not secret.exists():
        with tempfile.TemporaryDirectory() as temporary:
            key = Path(temporary) / 'key.pem'
            run('openssl', 'genpkey', '-algorithm', 'EC', '-pkeyopt',
                'ec_paramgen_curve:prime256v1', '-out', str(key),
                capture_output=True)
            der = subprocess.check_output(['openssl', 'ec', '-in', str(key),
                '-outform', 'DER'], stderr=subprocess.DEVNULL)
            assert der[2:7] == b'\x02\x01\x01\x04\x20'
            secret.write_text('TANGLED_OAUTH_CLIENT_SECRET=z' +
                base58(b'\x86\x26' + der[7:39]) + '\nTANGLED_OAUTH_CLIENT_KID=local\n')
            secret.chmod(0o600)
    key = STATE / 'id_ed25519'
    if not key.exists():
        run('ssh-keygen', '-t', 'ed25519', '-N', '', '-f', str(key), '-q')


def compose(*args):
    return run('docker', 'compose', '--env-file', str(STATE / 'compose.env'),
               '-f', str(ROOT / 'compose.yml'), '-f', str(ROOT / 'parity.yml'),
               *args)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', choices=('up', 'test', 'down'))
    parser.add_argument('--tangled-core', type=Path, default=REPO.parent / 'tangled-core')
    args = parser.parse_args()
    if args.command == 'down':
        compose('down')
        return
    run('python3', str(ROOT / 'run.py'), 'up', cwd=REPO)
    prepare(args.tangled_core)
    compose('build', 'knot')
    compose('up', '-d', 'redis', 'appview', 'knot', 'tangled-gateway', 'spindle')
    if args.command == 'test':
        run('python3', str(ROOT / 'tangled.py'), cwd=REPO)
    print('Local Tangled: appview http://127.0.0.1:3000; '
          'spindle http://127.0.0.1:9000; knot SSH 127.0.0.1:2222', flush=True)


if __name__ == '__main__':
    main()
