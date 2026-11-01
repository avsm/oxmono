#!/usr/bin/env python3
"""Build and exercise disposable Proffer DAV exports with an independent client.

Run from the repository root after building dav/test/fixture.exe. No production
credentials, directories or remote DAV servers are used. Docker requires Linux
host networking. Temporary stores and diagnostic logs remain on failure.
"""
import argparse
import hashlib
import http.client
import os
from pathlib import Path
import shutil
import socket
import ssl
import subprocess
import tempfile
import time

here = Path(__file__).resolve().parent
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--fixture', type=Path, default=Path('_build/default') /
                    here.relative_to(Path.cwd()) / 'fixture.exe')
parser.add_argument('--fetch-client', type=Path, default=Path('_build/default') /
                    here.parents[2].relative_to(Path.cwd()) /
                    'fetch/dav/integration/test_proffer.exe')
parser.add_argument('--image' , default='proffer-dav-client:test')
parser.add_argument('--skip-build', action='store_true')
parser.add_argument('--keep', action='store_true')
args = parser.parse_args()
fixture = args.fixture.resolve()
if not args.skip_build:
    subprocess.run(['docker', 'build', '-t', args.image, str(here / 'docker')], check=True)
work = Path(tempfile.mkdtemp(prefix='proffer-dav-'))
print(f'Disposable DAV test storage: {work}', flush=True)
folder = work / 'ro'
(folder / 'unsafe').mkdir(parents=True)
store = work / 'store'
store.mkdir(mode=0o700)
(folder / 'readme.txt').write_bytes(b'read-only fixture\n')
(work / 'outside').write_bytes(b'outside authority\n')
(folder / 'unsafe' / 'symlink').symlink_to(work / 'outside')
os.link(work / 'outside', folder / 'unsafe' / 'hardlink')
os.mkfifo(folder / 'unsafe' / 'fifo')
with socket.socket() as socket_:
    socket_.bind(('127.0.0.1', 0))
    port = socket_.getsockname()[1]
server = None
log = None
run_number = 0
success = False


def command(enabled=True, create=False, root=store, tls=False, require_tls=False):
    return [str(fixture), '--port', str(port), '--folder', str(folder),
            '--store', str(root)] + (['--enable-dav'] if enabled else []) + (
                ['--create'] if create else []) + (['--require-tls'] if require_tls else []) + (['--cert', str(work / 'cert.pem'),
                '--key', str(work / 'key.pem')] if tls else [])


def start(enabled=True, create=False, tls=False, require_tls=False):
    global server, log, run_number
    run_number += 1
    log = (work / f'server-{run_number}.log').open('wb')
    server = subprocess.Popen(command(enabled, create, tls=tls, require_tls=require_tls), stdout=log, stderr=log)
    for _ in range(100):
        if server.poll() is not None:
            raise RuntimeError(f'fixture exited, see {log.name}')
        try:
            conn = (http.client.HTTPSConnection('127.0.0.1', port, timeout=.5,
                    context=ssl.create_default_context(cafile=str(work / 'cert.pem')))
                    if tls else http.client.HTTPConnection('127.0.0.1', port, timeout=.2))
            conn.request('OPTIONS', '/rw/')
            result = conn.getresponse()
            result.read()
            conn.close()
            if result.status == (403 if require_tls else 401 if enabled else 404):
                return
        except (OSError, http.client.HTTPException):
            pass
        time.sleep(.05)
    raise RuntimeError('fixture did not become ready')


def stop():
    global server, log
    if server is not None:
        server.terminate()
        try:
            server.wait(timeout=10)
        except subprocess.TimeoutExpired:
            server.kill()
            server.wait()
        server = None
    if log is not None:
        log.close()
        log = None


def client(phase):
    subprocess.run(['docker', 'run', '--rm', '--network', 'host', '--read-only',
        '--tmpfs', '/tmp:rw,noexec,nosuid,size=16m', '--cap-drop', 'ALL',
        '--security-opt', 'no-new-privileges'] +
        (['--mount', f'type=bind,src={work / "cert.pem"},dst=/ca.pem,readonly']
         if phase == 'tls' else []) + [args.image,
        '--port', str(port), '--phase', phase] +
        (['--ca', '/ca.pem'] if phase == 'tls' else []), check=True)


def refuses_start(root):
    result = subprocess.run(command(root=root), stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, timeout=10)
    assert result.returncode != 0, 'unsafe store was accepted'
    assert b'Running at' not in result.stdout, 'unsafe store opened a listener'


try:
    start(enabled=False)
    client('disabled')
    assert not list(store.iterdir()), 'disabled DAV opened the store'
    stop()
    start(create=True, require_tls=True)
    for target in ['/ro/readme.txt', '/rw/forged-transport']:
        conn = http.client.HTTPConnection('127.0.0.1', port, timeout=2)
        conn.request('PUT', target, body=b'forged', headers={
            'Authorization': 'Basic YWxpY2U6dGVzdC1hbGljZQ==',
            'Forwarded': 'proto=https;host=127.0.0.1',
            'X-Forwarded-Proto': 'https'})
        result = conn.getresponse()
        result.read()
        assert result.status == 403, 'headers granted false TLS provenance'
        conn.close()
    stop()
    print('2 forged transport checks passed', flush=True)
    start()
    refuses_start(store)  # No second writer can use the private store.
    client('full')
    subprocess.run([str(args.fetch_client.resolve()), f'http://127.0.0.1:{port}/rw/'],
                   check=True)
    stop()
    orphan = store / ('object-' + 'a' * 32)
    orphan.write_bytes(b'interrupted private upload')
    manifest_temp = store / ('manifest-' + 'b' * 32)
    manifest_temp.write_bytes(b'interrupted metadata transaction')
    start()
    assert not orphan.exists() and not manifest_temp.exists(), 'recovery leaked staging'
    client('persist')
    stop()
    subprocess.run(['openssl', 'req', '-x509', '-newkey', 'rsa:2048', '-nodes',
        '-keyout', str(work / 'key.pem'), '-out', str(work / 'cert.pem'),
        '-days', '1', '-subj', '/CN=localhost',
        '-addext', 'subjectAltName=IP:127.0.0.1,DNS:localhost'],
        check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    start(tls=True)
    client('tls')
    stop()
    assert (folder / 'readme.txt').read_bytes() == b'read-only fixture\n'
    assert (work / 'outside').read_bytes() == b'outside authority\n'
    # Unknown files, corrupt metadata and corrupt object contents fail closed.
    for variant in ['unknown', 'manifest', 'object']:
        bad = work / ('bad-' + variant)
        shutil.copytree(store, bad)
        if variant == 'unknown':
            (bad / 'unexpected').write_bytes(b'not ours')
        elif variant == 'manifest':
            (bad / 'manifest.xml').write_bytes(b'<broken')
        else:
            next(bad.glob('object-*')).write_bytes(b'corrupt')
        before = {p.name: hashlib.sha256(p.read_bytes()).digest()
                  for p in bad.iterdir() if p.is_file()}
        refuses_start(bad)
        # The corrupt store's committed files must not be silently repaired.
        after = {p.name: hashlib.sha256(p.read_bytes()).digest()
                 for p in bad.iterdir() if p.is_file()}
        assert before == after, 'recovery changed corrupt committed data'
    print('Exclusive writer, orphan recovery, corruption refusal and confinement passed',
          flush=True)
    success = True
finally:
    stop()
    if success and not args.keep:
        shutil.rmtree(work)
    else:
        print(f'Retained diagnostics: {work}', flush=True)
