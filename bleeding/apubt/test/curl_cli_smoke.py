import base64, hashlib, http.server, json, pathlib, re, subprocess, tempfile, threading

with tempfile.TemporaryDirectory(prefix='apubt-audit-') as td:
    td = pathlib.Path(td)
    key, pub = td/'key.pem', td/'pub.pem'
    subprocess.run(['openssl', 'genpkey', '-algorithm', 'RSA', '-pkeyopt', 'rsa_keygen_bits:2048', '-out', str(key)], check=True, capture_output=True)
    subprocess.run(['openssl', 'pkey', '-in', str(key), '-pubout', '-out', str(pub)], check=True, capture_output=True)
    received = []
    class Handler(http.server.BaseHTTPRequestHandler):
        def log_message(self, *args): pass
        def do_GET(self):
            actor = {'id': origin+self.path, 'type': 'Person', 'inbox': origin+'/inbox', 'outbox': origin+'/outbox'}
            body = json.dumps(actor).encode()
            self.send_response(200)
            self.send_header('Content-Type', 'application/activity+json')
            self.send_header('Content-Length', str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        def do_POST(self):
            body = self.rfile.read(int(self.headers['Content-Length']))
            received.append((self.path, dict(self.headers.items()), body))
            self.send_response(202)
            self.send_header('Content-Length', '0')
            self.end_headers()
    server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)
    origin = 'http://127.0.0.1:'+str(server.server_port)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        result = subprocess.run(['_build/default/bleeding/apubt/bin/apub.exe', 'follow', '--actor', origin+'/alice', '--key-file', str(key), '--key-id', origin+'/alice#main-key', origin+'/bob'], capture_output=True, text=True, timeout=15)
        if result.returncode:
            raise RuntimeError(result.stderr)
        assert 'Sent follow request.' in result.stdout
        assert len(received) == 1
        path, headers, body = received[0]
        headers = {k.lower(): v for k, v in headers.items()}
        params = headers['signature-input'].split('=', 1)[1]
        assert 'alg="rsa-v1_5-sha256"' in params
        assert headers['content-type'] == 'application/activity+json'
        assert headers['content-digest'] == 'sha-256=:'+base64.b64encode(hashlib.sha256(body).digest()).decode()+':'
        components = re.findall(r'"([^\"]+)"', params.split(')',1)[0])
        values = {'@method':'POST', '@target-uri':origin+path, **headers}
        sigbase = '\n'.join('"'+k+'": '+values[k] for k in components)+'\n"@signature-params": '+params
        (td/'base').write_bytes(sigbase.encode())
        (td/'sig').write_bytes(base64.b64decode(headers['signature'].split(':')[1]))
        subprocess.run(['openssl','dgst','-sha256','-verify',str(pub),'-signature',str(td/'sig'),str(td/'base')], check=True, capture_output=True)
        assert json.loads(body)['type'] == 'Follow'
        print('PASS: curl CLI Follow, RSA blinding initialization, HTTP body digest, independent OpenSSL signature verification, clean CLI exit')
    finally:
        server.shutdown()
        server.server_close()
