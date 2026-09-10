#!/usr/bin/env python3
"""Fault-inject the local spindle's readiness, retention and replay recovery."""
import json
import sqlite3
import subprocess
import time
import tempfile
from pathlib import Path

from parity import compose
from smoke import request, logs, login, auth, TRIGGER
from tangled import CI, STATE, commit, eventually, pipeline, push, query, stored, tid, git


def ready():
    value = request(CI + '_ready')
    assert value['ready'], value
    return value


def ref_recovery(repo):
    remote = 'ssh://git@127.0.0.1:2222/' + repo
    tag = 'review-' + tid()
    notes = 'refs/notes/' + tag
    blocked = 'did:plc:bbbbbbbbbbbbbbbbbbbbbbbb/sh.tangled.repo'

    def schedule():
        with sqlite3.connect(STATE / 'data/spindle.db', timeout=15) as db:
            db.execute("INSERT OR REPLACE INTO kv VALUES('recover',?,?)", (repo, tid()))

    def recovered():
        return stored('recover', repo) is None

    with tempfile.TemporaryDirectory(prefix='spindle-refs-') as temporary:
        fixture = Path(temporary) / 'fixture'
        subprocess.run(['git', 'clone', '-q', '--no-hardlinks',
                        str(STATE / 'fixture'), str(fixture)], check=True)
        try:
            git(fixture, '-c', 'user.name=Test', '-c', 'user.email=test@example.test',
                'tag', '-a', tag, '-m', tag)
            tag_sha = git(fixture, 'rev-parse', tag).strip()
            git(fixture, 'push', remote, 'refs/tags/' + tag)
            def tag_runs():
                return [p for p in query(repo) if p['trigger'].get('ref') == 'refs/tags/' + tag]
            first = eventually(tag_runs)
            assert len(first) == 1 and first[0]['commit'] == tag_sha, first
            schedule()
            eventually(recovered)
            assert [p['id'] for p in tag_runs()] == [first[0]['id']]
            git(fixture, '-c', 'user.name=Test', '-c', 'user.email=test@example.test',
                'notes', '--ref=' + notes, 'add', '-m', 'non-CI namespace')
            git(fixture, 'push', remote, notes)
            with sqlite3.connect(STATE / 'data/spindle.db', timeout=15) as db:
                db.execute("INSERT INTO kv VALUES('reconcile',?,?)", (blocked, tid()))
                db.execute("INSERT INTO retry VALUES('reconcile',?,6,?)", (blocked, time.time() + 300))
            schedule()
            eventually(recovered)
            assert stored('reconcile', blocked) is not None
            print('PASS: real annotated tags, notes and recovery during unrelated catalog retry', flush=True)
        finally:
            with sqlite3.connect(STATE / 'data/spindle.db', timeout=15) as db:
                db.execute("DELETE FROM kv WHERE namespace='reconcile' AND key=?", (blocked,))
                db.execute("DELETE FROM retry WHERE namespace='reconcile' AND key=?", (blocked,))
            git(fixture, 'push', remote, ':' + notes, ':refs/tags/' + tag)
            schedule()
            eventually(recovered)
    eventually(ready, timeout=300)


def crash_logs(repo, revision):
    alice = login('alice')
    body = {'repo': repo, 'workflows': ['stream-probe'], 'trigger': {
        '$type': 'sh.tangled.ci.trigger#manual', 'sha': revision}}
    run = request(CI + TRIGGER, body, auth(alice))['pipeline'].rsplit('/', 1)[1]

    def complete_output(events):
        output = ''.join(e['content'] for e in events if e['type'] == 'data'
                         and e['step'] == 1 and e['stream'] == 'stdout')
        errors = ''.join(e['content'] for e in events if e['type'] == 'data'
                         and e['step'] == 1 and e['stream'] == 'stderr')
        return output == 'x' * 70000 and errors == 'durable-partial'

    observed, frames = logs(run, ('stream-probe',), until=complete_output)
    assert complete_output(observed)
    state = request(CI + 'sh.tangled.ci.getPipeline?pipeline=' + run)
    assert state['workflows'][0]['status'] == 'running', state
    try:
        compose('kill', '-s', 'SIGKILL', 'spindle')
    finally:
        compose('start', 'spindle')
    eventually(lambda: request(CI + '_health'))
    restored, restored_frames = logs(run, ('stream-probe',))
    assert restored_frames == frames, (len(frames), len(restored_frames))
    state = request(CI + 'sh.tangled.ci.getPipeline?pipeline=' + run)
    assert state['workflows'][0]['status'] == 'failed', state
    assert 'restarted' in state['workflows'][0]['error'], state
    assert complete_output(restored)
    eventually(ready, timeout=300)
    print('PASS: published 70000-byte and partial-line logs survive SIGKILL byte-for-byte', flush=True)


def main():
    result = json.loads((STATE / 'parity-result.json').read_text())
    repo = result['repo']
    eventually(ready, timeout=300)
    ref_recovery(repo)
    revision = git(STATE / 'fixture', 'rev-parse', 'HEAD').strip()
    crash_logs(repo, revision)

    # Losing Jetstream must change readiness while liveness remains readable.
    subprocess.run(['docker', 'stop', 'oxmono-atp-jetstream-1'], check=True,
                   stdout=subprocess.DEVNULL)
    try:
        def disconnected():
            value = request(CI + '_health')
            stream = next(x for x in value['observers'] if x['source'] == 'jetstream')
            assert not stream['connected'], stream
            assert not request(CI + '_ready', status=503)['ready']
            return True
        eventually(disconnected)
    finally:
        subprocess.run(['docker', 'start', 'oxmono-atp-jetstream-1'], check=True,
                       stdout=subprocess.DEVNULL)
    eventually(ready, timeout=300)
    print('PASS: observer disconnection degrades readiness and reconnect restores it', flush=True)

    # Remove the specific missed push from the actual knot journal. The spindle
    # can only discover this SHA through current Git refs, not event replay.
    compose('stop', 'spindle')
    try:
        revision = commit(STATE / 'fixture', 'replay-recovery.txt')
        push(STATE / 'fixture', repo)
        script = '''import sqlite3,sys
with sqlite3.connect('/home/git/knot.db', timeout=30) as db:
    count = db.execute("DELETE FROM events WHERE nsid='sh.tangled.git.refUpdate' AND json_extract(event,'$.repo')=? AND json_extract(event,'$.newSha')=?",sys.argv[1:]).rowcount
    assert count == 1, count
'''
        compose('exec', '-T', '-u', 'git', 'knot', 'python3', '-c', script, repo, revision)
        with sqlite3.connect(STATE / 'data/spindle.db') as db:
            for source, factor in [('jetstream', 1e6), ('https://knot.tangled.test', 1e9)]:
                old = str(int((time.time() - 48 * 3600) * factor))
                db.execute("UPDATE kv SET value=? WHERE namespace='cursor' AND key=?", (old, source))
    finally:
        compose('start', 'spindle')
    recovered = eventually(lambda: pipeline(repo, revision), timeout=300)
    events, _ = logs(recovered['id'], ('inspect', 'tracked-files'))
    metadata = next(json.loads(x['content']) for x in events if x['type'] == 'data' and x['step'] == 1)
    assert metadata['actor'] == 'did:web:spindle.tangled.test', metadata
    assert metadata['request']['recovery']['mode'] == 'current_refs', metadata
    assert metadata['request']['recovery']['committerKnown'] is False
    value = eventually(ready, timeout=300)
    gaps = {x['source']: x for x in value['replay']}
    assert gaps['jetstream']['status'] == 'reconciled', gaps
    assert gaps['https://knot.tangled.test']['status'] == 'reconciled', gaps
    assert not gaps['https://knot.tangled.test']['historicalEventsComplete']
    before = [p['id'] for p in query(repo) if p['commit'] == revision]
    compose('restart', 'spindle')
    eventually(ready, timeout=300)
    after = [p['id'] for p in query(repo) if p['commit'] == revision]
    assert before == after == [recovered['id']], (before, after)
    print('PASS: missing upstream push recovered from Git refs without duplicate dispatch after restart', flush=True)

    # Expire synthetic history using the same schema as completed real runs.
    expired = tid()
    time.sleep(.001)
    pending = tid()
    with sqlite3.connect(STATE / 'data/spindle.db') as db:
        raw = db.execute("SELECT value FROM kv WHERE namespace='pipeline' AND key=?", (recovered['id'],)).fetchone()[0]
        snapshot = json.loads(raw)
        view = snapshot['pipeline']
        for key, status in [(expired, 'success'), (pending, 'pending')]:
            view = dict(view, id=key, workflows=[dict(w, status=status) for w in view['workflows']])
            db.execute('INSERT INTO kv VALUES(?,?,?)', ('pipeline', key, raw))
            db.execute('INSERT INTO kv VALUES(?,?,?)', ('pipeline-view', key, json.dumps(view)))
            db.execute("UPDATE age SET updated=? WHERE namespace IN ('pipeline','pipeline-view') AND key=?",
                       (time.time() - 40 * 86400, key))
    try:
        eventually(lambda: stored('pipeline', expired) is None)
        assert stored('pipeline-view', expired) is None
        assert stored('pipeline', pending) is not None
        assert stored('pipeline-view', pending) is not None
        assert stored('pipeline', recovered['id']) is not None
    finally:
        with sqlite3.connect(STATE / 'data/spindle.db') as db:
            db.execute("DELETE FROM kv WHERE namespace IN ('pipeline','pipeline-view') AND key IN (?,?)", (expired, pending))
    print('PASS: automatic maintenance expires completed history and preserves pending and recent work', flush=True)
    (STATE / 'operations-result.json').write_text(json.dumps({'repo': repo, 'recovery': recovered['id'], 'commit': revision}, indent=2) + '\n')


if __name__ == '__main__':
    main()
