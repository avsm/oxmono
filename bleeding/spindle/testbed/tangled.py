#!/usr/bin/env python3
"""Exercise real local PDS records, knot SSH pushes and Tangled CI pages."""
import gzip
import json
import os
from pathlib import Path
import shlex
import sqlite3
import subprocess
import time
import urllib.error
import urllib.parse
import urllib.request

from smoke import request, logs
from parity import compose
import smoke

ROOT = Path(__file__).resolve().parent
STATE = ROOT / '.state'
PDS = 'http://127.0.0.1:2583/xrpc/'
KNOT = 'http://127.0.0.1:5555/xrpc/'
CI = 'http://127.0.0.1:9000/xrpc/'
smoke.SPINDLE = CI
smoke.PDS = PDS


def tid():
    number = int(time.time() * 1e6) << 10
    alphabet = '234567abcdefghijklmnopqrstuvwxyz'
    out = ''
    for _ in range(13):
        number, digit = divmod(number, 32)
        out = alphabet[digit] + out
    return out


def now():
    return time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())


def eventually(f, timeout=60):
    deadline = time.monotonic() + timeout
    error = None
    while time.monotonic() < deadline:
        try:
            result = f()
            if result:
                return result
        except (AssertionError, OSError, urllib.error.URLError) as ex:
            error = ex
        time.sleep(.5)
    raise AssertionError(('condition timed out', error))


def put(session, collection, rkey, value):
    return request(PDS + 'com.atproto.repo.putRecord', {
        'repo': session['did'], 'collection': collection, 'rkey': rkey,
        'record': dict(value, **{'$type': collection})}, session['accessJwt'])


def delete(session, collection, rkey):
    return request(PDS + 'com.atproto.repo.deleteRecord', {
        'repo': session['did'], 'collection': collection, 'rkey': rkey},
        session['accessJwt'])


def token(session, method='sh.tangled.ci.triggerPipeline', service='spindle'):
    return smoke.auth(session, method, 'did:web:' + service + '.tangled.test')


def query(repo, **kwargs):
    return request(CI + 'sh.tangled.ci.queryPipelines?' +
                   urllib.parse.urlencode(dict(repo=repo, **kwargs)))['pipelines']


def stored(namespace, key):
    with sqlite3.connect(STATE / 'data/spindle.db') as db:
        row = db.execute('SELECT value FROM kv WHERE namespace=? AND key=?',
                         (namespace, key)).fetchone()
    return row[0] if row else None


def repo_create(session, name):
    rkey = tid()
    result = request(KNOT + 'sh.tangled.repo.create',
        {'rkey': rkey, 'name': name, 'defaultBranch': 'main'},
        token(session, 'sh.tangled.repo.create', 'knot'))
    repo = result['repoDid']
    value = {'repoDid': repo, 'name': name, 'knot': 'knot.tangled.test',
             'spindle': 'spindle.tangled.test', 'createdAt': now()}
    put(session, 'sh.tangled.repo', rkey, value)
    return repo, rkey, value


def git(path, *args):
    env = dict(os.environ, GIT_SSH_COMMAND='ssh -i ' +
        shlex.quote(str(STATE / 'id_ed25519')) +
        ' -o IdentitiesOnly=yes -o StrictHostKeyChecking=no'
        ' -o UserKnownHostsFile=/dev/null')
    return subprocess.check_output(['git', '-C', str(path), *args], env=env,
                                   stderr=subprocess.STDOUT, text=True)


def push(path, repo, ref='main', option=None):
    argv = ['push', '--force']
    if option:
        argv += ['-o', option]
    argv += ['ssh://git@127.0.0.1:2222/' + repo, 'HEAD:refs/heads/' + ref]
    return git(path, *argv)


def commit(path, name):
    (path / name).write_text(now() + '\n' + tid() + '\n')
    git(path, 'add', name)
    git(path, '-c', 'user.name=Spindle Test', '-c',
        'user.email=spindle@tangled.test', 'commit', '-m', name)
    return git(path, 'rev-parse', 'HEAD').strip()


def pipeline(repo, commit, kind='push'):
    values = query(repo, commits=commit, kinds=kind)
    if not values:
        return None
    value = values[0]
    if any(w['status'] in ('pending', 'running') for w in value['workflows']):
        return None
    assert all(w['status'] == 'success' for w in value['workflows']), value
    return value


def main():
    eventually(lambda: request(KNOT + 'sh.tangled.owner'))
    alice, bob = smoke.login('alice'), smoke.login('bob')
    key = (STATE / 'id_ed25519.pub').read_text().strip()
    put(alice, 'sh.tangled.publicKey', tid(),
        {'key': key, 'name': 'spindle Docker test', 'createdAt': now()})
    put(alice, 'sh.tangled.spindle', 'spindle.tangled.test', {'createdAt': now()})
    name = 'spindle-' + tid()
    repo, rkey, value = repo_create(alice, name)
    eventually(lambda: stored('sh.tangled.repo', alice['did'] + '/' + rkey))
    eventually(lambda: stored('cursor', 'https://knot.tangled.test'))
    fixture = STATE / 'fixture'
    sha = commit(fixture, 'automatic-push.txt')
    def initial_push():
        try:
            push(fixture, repo)
            return True
        except subprocess.CalledProcessError:
            return False
    eventually(initial_push)
    first = eventually(lambda: pipeline(repo, sha))
    events, _ = logs(first['id'], ('inspect', 'tracked-files'))
    assert {event['workflow'] for event in events} == {'inspect', 'tracked-files'}
    text = ''.join(e['content'] for e in events if e['type'] == 'data')
    assert 'automatic-push.txt' in text
    metadata = next(json.loads(e['content']) for e in events
                    if e['type'] == 'data' and e['step'] == 1)
    assert metadata['actor'] == alice['did']
    assert metadata['request']['repo'] == repo
    assert metadata['request']['trigger']['newSha'] == sha
    print('PASS: real SSH push -> knot WebSocket -> OCaml checkout and logs', flush=True)

    skipped = commit(fixture, 'skip-ci.txt')
    push(fixture, repo, option='skip-ci')
    time.sleep(2)
    assert not query(repo, commits=skipped)
    revision = commit(fixture, 'pull-request.txt')
    push(fixture, repo, ref='feature')
    eventually(lambda: pipeline(repo, revision))
    patch = git(fixture, 'format-patch', '-1', '--stdout').encode()
    blob = request(PDS + 'com.atproto.repo.uploadBlob', token=alice['accessJwt'],
                   raw=gzip.compress(patch), content_type="application/gzip")['blob']
    # The PDS sniffs application/gzip from the body.
    assert blob['mimeType'] == 'application/gzip', blob
    pull_rkey = tid()
    pull = {'title': 'OCaml automatic PR', 'createdAt': now(),
            'target': {'repo': repo, 'branch': 'main'},
            'source': {'branch': 'feature'},
            'rounds': [{'createdAt': now(), 'patchBlob': blob}]}
    put(alice, 'sh.tangled.repo.pull', pull_rkey, pull)
    pr = eventually(lambda: pipeline(repo, revision, 'pull_request'))
    put(alice, 'sh.tangled.repo.pull', pull_rkey, dict(pull, title='Renamed PR'))
    time.sleep(2)
    assert [p['id'] for p in query(repo, kinds='pull_request')] == [pr['id']]
    print('PASS: skip-ci and branch pull-request updates with revision deduplication', flush=True)

    body = {'repo': repo, 'trigger': {'$type': 'sh.tangled.ci.trigger#manual',
                                     'sha': revision}}
    proxy = request(PDS + smoke.TRIGGER, body, alice['accessJwt'], extra_headers={
        'atproto-proxy': 'did:web:spindle.tangled.test#tangled_spindle'})
    assert proxy['pipeline'].startswith('at://did:web:spindle.tangled.test/')
    print('PASS: PDS service proxy discovers the spindle DID document', flush=True)
    request(CI + smoke.TRIGGER, body, token(bob), status=401)
    add = 'sh.tangled.repo.addCollaborator'
    remove = 'sh.tangled.repo.removeCollaborator'
    request(KNOT + add, {'subject': bob['did'], 'repo': repo},
            token(alice, add, 'knot'))
    def collaborator_run():
        return request(CI + smoke.TRIGGER, body, token(bob))['pipeline']
    manual = eventually(collaborator_run)
    eventually(lambda: pipeline(repo, revision, 'manual'))
    request(KNOT + remove, {'subject': bob['did'], 'repo': repo},
            token(alice, remove, 'knot'))
    eventually(lambda: request(CI + smoke.TRIGGER, body, token(bob), status=401))
    print('PASS: collaborator grants and revocations enforced by canonical knot', flush=True)

    member = tid()
    add_member = 'sh.tangled.knot.addMember'
    request(KNOT + add_member, {'subject': bob['did']},
            token(alice, add_member, 'knot'))
    put(alice, 'sh.tangled.spindle.member', member,
        {'subject': bob['did'], 'instance': 'spindle.tangled.test',
         'createdAt': now()})
    eventually(lambda: stored('sh.tangled.spindle.member', alice['did'] + '/' + member))
    fork, fork_rkey, _ = repo_create(bob, 'member-' + tid())
    eventually(lambda: stored('sh.tangled.repo', bob['did'] + '/' + fork_rkey))
    request(KNOT + add, {'subject': alice['did'], 'repo': fork},
            token(bob, add, 'knot'))
    push(fixture, fork)
    eventually(lambda: pipeline(fork, revision))
    private = dict(body, repo=fork,
                   trigger=dict(body['trigger'], sourceRepo=smoke.REPO))
    request(CI + smoke.TRIGGER, private, token(bob), status=401)
    foreign = dict(body, trigger=dict(body['trigger'], sourceRepo=fork))
    foreign_id = request(CI + smoke.TRIGGER, foreign, token(alice))['pipeline']
    result = smoke.wait_pipeline(foreign_id.rsplit('/', 1)[1])
    assert result['sourceRepo'] == fork and result['workflows'][0]['status'] == 'success'
    delete(alice, 'sh.tangled.spindle.member', member)
    eventually(lambda: stored('sh.tangled.spindle.member', alice['did'] + '/' + member) is None)
    revoked = commit(fixture, 'revoked-member.txt')
    push(fixture, fork)
    time.sleep(2)
    assert not query(fork, commits=revoked)
    request(CI + smoke.TRIGGER, dict(body, repo=fork), token(bob), status=400)
    print('PASS: membership, revocation and independently verified fork checkout', flush=True)

    used = token(alice)
    request(CI + smoke.TRIGGER, body, used)
    compose('stop', 'spindle')
    offline = commit(fixture, 'offline-push.txt')
    push(fixture, repo)
    compose('start', 'spindle')
    eventually(lambda: pipeline(repo, offline))
    request(CI + smoke.TRIGGER, body, used, status=401)
    assert len(query(repo, commits=sha, kinds='push')) == 1
    print('PASS: offline push replay, persistent cursors and JWT replay rejection', flush=True)

    # Tangled appview calls the spindle CI API to render this page.
    page_url = 'http://127.0.0.1:3000/' + alice['handle'] + '/' + name + '/pipelines'
    def appview_page():
        with urllib.request.urlopen(page_url, timeout=15) as response:
            page = response.read().decode()
        assert first['id'] in page and 'inspect' in page, page[:300]
        return page
    eventually(appview_page)
    (STATE / 'parity-result.json').write_text(json.dumps({
        'repo': repo, 'rkey': rkey, 'name': name, 'owner': alice['did'],
        'push': first['id'], 'pull': pr['id'], 'manual': manual,
        'tangled_revision': (STATE / 'tangled-revision').read_text().strip()}, indent=2))
    print('PASS: real Tangled appview renders OCaml pipeline history', flush=True)


if __name__ == '__main__':
    main()
