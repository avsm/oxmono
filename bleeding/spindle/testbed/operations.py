#!/usr/bin/env python3
"""Fault-inject the local spindle's readiness, retention and replay recovery."""
import json
import sqlite3
import subprocess
import time

from parity import compose
from smoke import request, logs
from tangled import CI, STATE, commit, eventually, pipeline, push, query, stored, tid


def ready():
    value = request(CI + '_ready')
    assert value['ready'], value
    return value


def main():
    result = json.loads((STATE / 'parity-result.json').read_text())
    repo = result['repo']
    eventually(ready, timeout=300)

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
