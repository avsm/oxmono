#!/usr/bin/env python3
"""Exercise actual tutorial executables with private local fixture profiles."""
import base64
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import urllib.parse
import urllib.request

fixture = json.loads(Path(os.environ['ZULIP_TEST_FIXTURES']).read_text())
site = os.environ['ZULIP_TEST_SERVER']
bot = fixture['users']['bots']['echo']
alice = fixture['users']['alice']
echo, preflight = map(lambda s: str(Path(s).resolve()), sys.argv[1:])

def query(user, method, path, params):
    body = urllib.parse.urlencode(params).encode()
    url = site + '/api/v1/' + path
    if method == 'GET':
        url += '?' + body.decode()
        body = None
    token = base64.b64encode((user['email'] + ':' + user['api_key']).encode()).decode()
    request = urllib.request.Request(url, body, method=method, headers={
        'Authorization': 'Basic ' + token,
        'Content-Type': 'application/x-www-form-urlencoded',
    })
    with urllib.request.urlopen(request, timeout=10) as response:
        result = json.load(response)
    assert result['result'] == 'success', result.get('msg')
    return result

with tempfile.TemporaryDirectory(prefix='ocaml-zulip-cli-') as directory:
    env = dict(os.environ, XDG_CONFIG_HOME=directory+'/config', XDG_DATA_HOME=directory+'/data')
    for name in ['ZULIP_SITE', 'ZULIP_EMAIL', 'ZULIP_API_KEY']:
        env.pop(name, None)
    config = Path(directory, 'zuliprc')
    config.write_text('[api]\nsite='+site+'\nemail='+bot['email']+'\nkey='+bot['api_key']+'\n')
    config.chmod(0o600)
    result = subprocess.run([preflight, '--profile', 'fixture', '--zuliprc', str(config), '--allow-insecure-http'],
                            env=env, text=True, capture_output=True, timeout=30)
    assert result.returncode == 0, result.stderr
    assert 'bot: true' in result.stdout and site in result.stdout, result.stdout
    profile = Path(directory, 'config/zulip/profiles/fixture.json')
    assert profile.stat().st_mode & 0o077 == 0
    # HTTP must require explicit opt-in even when the saved profile has that URL.
    refused = subprocess.run([preflight, '--profile', 'fixture'], env=env,
                             text=True, capture_output=True, timeout=15)
    assert refused.returncode != 0, 'CLI accepted cleartext credentials without opt-in'
    log_path = Path(directory, 'echo.log')
    with log_path.open('w') as log:
        process = subprocess.Popen([echo, '--profile', 'fixture', '--allow-insecure-http'],
                                   env=env, stdout=log, stderr=log)
        try:
            deadline = time.monotonic()+30
            while 'connected as' not in log_path.read_text():
                assert process.poll() is None, log_path.read_text()
                assert time.monotonic() < deadline, 'CLI did not register its queue'
                time.sleep(.1)
            marker = 'cli-probe-' + str(time.time_ns())
            query(alice, 'POST', 'messages', {'type':'direct','to':json.dumps([bot['id']]),'content':marker})
            deadline = time.monotonic()+20
            while True:
                page = query(alice, 'GET', 'messages', {'anchor':'newest','num_before':20,'num_after':0,
                    'apply_markdown':'false', 'narrow':json.dumps([{'operator':'dm','operand':[bot['id']]}])})
                if any(m['sender_id']==bot['id'] and m['content']=='You said: '+marker for m in page['messages']):
                    break
                assert process.poll() is None, log_path.read_text()
                assert time.monotonic()<deadline, 'CLI reply did not arrive'
                time.sleep(.1)
            process.send_signal(signal.SIGTERM)
            assert process.wait(timeout=10) == 0, log_path.read_text()
        finally:
            if process.poll() is None:
                process.kill()
                process.wait(timeout=5)
    bad = dict(env, ZULIP_API_KEY='invalid-fixture-key')
    result = subprocess.run([echo, '--profile','fixture','--allow-insecure-http'], env=bad,
                            text=True,capture_output=True,timeout=15)
    assert result.returncode != 0, 'CLI returned success with invalid credentials'
print('Tutorial CLI: preflight, private import, HTTP opt-in, live echo, SIGTERM and invalid-key exit passed.')
