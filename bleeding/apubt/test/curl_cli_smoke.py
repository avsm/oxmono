"""Local CLI federation smoke test, with independent wire-signature verification."""
import os, base64, hashlib, http.server, json, pathlib, re, subprocess, tempfile, threading

with tempfile.TemporaryDirectory(prefix='apubt-audit-') as td:
    td = pathlib.Path(td)
    key, pub = td/'key.pem', td/'pub.pem'
    subprocess.run(['openssl', 'genpkey', '-algorithm', 'RSA', '-pkeyopt', 'rsa_keygen_bits:2048', '-out', str(key)], check=True, capture_output=True)
    subprocess.run(['openssl', 'pkey', '-in', str(key), '-pubout', '-out', str(pub)], check=True, capture_output=True)
    received = []
    class Handler(http.server.BaseHTTPRequestHandler):
        def log_message(self, *args): pass
        def do_GET(self):
            received.append(('GET', self.path, dict(self.headers.items()), b''))
            actor = {'id': origin+self.path, 'type': 'Person', 'inbox': origin+'/inbox', 'outbox': origin+'/outbox'}
            actor['publicKey'] = {'id': origin+self.path+'#main-key', 'owner': origin+self.path, 'publicKeyPem': pub.read_text()}
            body = json.dumps(actor).encode()
            self.send_response(200)
            self.send_header('Content-Type', 'application/activity+json')
            self.send_header('Content-Length', str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        def do_POST(self):
            body = self.rfile.read(int(self.headers['Content-Length']))
            received.append(('POST', self.path, dict(self.headers.items()), body))
            self.send_response(202)
            self.send_header('Content-Length', '0')
            self.end_headers()
    server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)
    origin = 'http://127.0.0.1:'+str(server.server_port)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    try:
        for signature_format in ('rfc9421', 'cavage'):
            received.clear()
            config = td/signature_format
            result = subprocess.run(['_build/default/bleeding/apubt/bin/apub.exe', 'follow', '--actor', origin+'/alice', '--key-file', str(key), '--key-id', origin+'/alice#main-key', '--signature-format', signature_format, origin+'/bob'], capture_output=True, text=True, timeout=15, env={**os.environ, 'XDG_CONFIG_HOME': str(config)})
            if result.returncode:
                raise RuntimeError(result.stderr)
            assert 'Sent follow request.' in result.stdout
            assert sum(method == 'POST' for method, *_ in received) == 1
            assert any(method == 'GET' for method, *_ in received)
            for method, path, headers, body in received:
                headers = {k.lower(): v for k, v in headers.items()}
                digest = base64.b64encode(hashlib.sha256(body).digest()).decode()
                if signature_format == 'rfc9421':
                    params = headers['signature-input'].split('=', 1)[1]
                    assert 'alg="rsa-v1_5-sha256"' in params
                    assert headers['content-digest'] == 'sha-256=:'+digest+':'
                    components = re.findall(r'"([^\"]+)"', params.split(')',1)[0])
                    values = {'@method':method, '@target-uri':origin+path, **headers}
                    sigbase = '\n'.join('"'+k+'": '+values[k] for k in components)+'\n"@signature-params": '+params
                    signature = headers['signature'].split(':')[1]
                else:
                    params = dict(re.findall(r'(\w+)="([^\"]*)"', headers['signature']))
                    assert params['algorithm'] == 'rsa-sha256'
                    components = params['headers'].split()
                    assert components == ['(request-target)', 'host', 'date'] + (['digest'] if method == 'POST' else [])
                    values = {'(request-target)':method.lower()+' '+path, **headers}
                    if method == 'POST':
                        assert headers['digest'] == 'SHA-256='+digest
                    sigbase = '\n'.join(k+': '+values[k] for k in components)
                    signature = params['signature']
                (td/'base').write_bytes(sigbase.encode())
                (td/'sig').write_bytes(base64.b64decode(signature))
                subprocess.run(['openssl','dgst','-sha256','-verify',str(pub),'-signature',str(td/'sig'),str(td/'base')], check=True, capture_output=True)
                if method == 'POST':
                    assert headers['content-type'] == 'application/activity+json'
                    activity = json.loads(body)
                    assert activity['type'] == 'Follow' and activity['id']
                    saved = list(config.rglob('*.json'))
                    assert len(saved) == 1 and json.loads(saved[0].read_text())['id'] == activity['id']
            print('PASS:', signature_format, 'curl CLI Follow, persisted identity, signed GET/POST verified independently with OpenSSL')
    finally:
        server.shutdown()
        server.server_close()
