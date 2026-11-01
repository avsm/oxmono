"""Independent webdavclient3 and raw HTTP oracles for the loopback fixture."""
import argparse
import base64
import concurrent.futures
import http.client
import io
import socket
import ssl
import tempfile
import time
import xml.etree.ElementTree as ET
from pathlib import Path
from webdav3.client import Client

parser = argparse.ArgumentParser()
parser.add_argument('--port', type=int, default=18765)
parser.add_argument('--phase', choices=['full', 'persist', 'disabled', 'tls'], default='full')
parser.add_argument('--ca')
args = parser.parse_args()
scheme = 'https' if args.ca else 'http'
origin = f'{scheme}://127.0.0.1:{args.port}'
checks = 0


def check(name, condition):
    global checks
    assert condition, name
    checks += 1


def auth(user):
    return 'Basic ' + base64.b64encode(f'{user}:test-{user}'.encode()).decode()


def request(method, path, body=None, headers=None, user='alice', expected=None):
    conn = (http.client.HTTPSConnection('127.0.0.1', args.port, timeout=10,
            context=ssl.create_default_context(cafile=args.ca)) if args.ca else
            http.client.HTTPConnection('127.0.0.1', args.port, timeout=10))
    hs = {'Authorization': auth(user)} if user else {}
    hs.update(headers or {})
    conn.request(method, path, body=body, headers=hs)
    response = conn.getresponse()
    data = response.read()
    status = response.status
    result = status, dict((k.lower(), v) for k, v in response.getheaders()), data
    conn.close()
    if expected is not None:
        check(f'{method} {path}: expected {expected}, got {status}: {data[:200]!r}',
              status == expected)
    return result


def raw(head, body=b''):
    with socket.create_connection(('127.0.0.1', args.port), timeout=10) as sock:
        sock.sendall(head.encode() + body)
        response = http.client.HTTPResponse(sock)
        response.begin()
        data = response.read()
        return response.status, dict(response.getheaders()), data


xml = {'Content-Type': 'application/xml; charset=utf-8'}
lock_body = b'''<d:lockinfo xmlns:d="DAV:"><d:lockscope><d:exclusive/></d:lockscope>
<d:locktype><d:write/></d:locktype><d:owner><d:href>urn:test:alice</d:href>
</d:owner></d:lockinfo>'''
ns = {'d': 'DAV:', 'p': 'urn:test'}


def lock(path, depth='0', seconds='Second-600', expected=200):
    _, headers, body = request('LOCK', path, lock_body,
        {**xml, 'Depth': depth, 'Timeout': seconds}, expected=expected)
    root = ET.fromstring(body)
    token = headers['lock-token']
    check('LOCK token and discovery agree',
        root.find('.//d:locktoken/d:href', ns).text == token[1:-1])
    check('LOCK root matches resource',
        root.find('.//d:lockroot/d:href', ns).text.rstrip('/') == path.rstrip('/'))
    return token


if args.phase == 'disabled':
    for path in ['/ro/', '/rw/']:
        for method in ['GET', 'PUT', 'PROPFIND', 'LOCK', 'OPTIONS']:
            request(method, path, expected=404)
    print(f'{checks} disabled-by-default checks passed')
    raise SystemExit(0)

if args.phase == 'tls':
    request('OPTIONS', '/rw/', user=None, expected=401)
    request('PUT', '/rw/tls', b'tls verified', expected=201)
    _, _, data = request('GET', '/rw/tls', expected=200)
    check('TLS payload', data == b'tls verified')
    request('PUT', '/ro/no', b'bad', expected=403)
    request('DELETE', '/rw/tls', expected=204)
    client = Client({'webdav_hostname': origin, 'webdav_login': 'alice',
                     'webdav_password': 'test-alice'})
    client.verify = args.ca
    check('independent client verifies TLS', isinstance(client.list('/rw/'), list))
    print(f'{checks} verified TLS checks passed')
    raise SystemExit(0)

if args.phase == 'persist':
    _, _, data = request('GET', '/rw/persist', expected=200)
    check('restart preserves committed bytes', data == b'persistent data')
    _, _, data = request('PROPFIND', '/rw/persist', headers={'Depth': '0'}, expected=207)
    root = ET.fromstring(data)
    check('restart preserves dead property', root.find('.//p:value', ns).text == 'durable')
    token = root.find('.//d:locktoken/d:href', ns).text
    request('PUT', '/rw/persist', b'lost', expected=423)
    request('PUT', '/rw/persist', b'new', {'If': f'(<{token}>)'}, expected=204)
    request('UNLOCK', '/rw/persist', headers={'Lock-Token': f'<{token}>'}, expected=204)
    request('DELETE', '/rw/persist', expected=204)
    print(f'{checks} persistence checks passed')
    raise SystemExit(0)

# Library-generated XML, URL encoding and methods, independent of HTTPz codecs.
client = Client({'webdav_hostname': origin, 'webdav_login': 'alice',
                 'webdav_password': 'test-alice'})
check('client root listing', client.list('/rw/') == [])
check('client mkdir', client.mkdir('/rw/python'))
with tempfile.TemporaryDirectory() as work:
    source = Path(work) / 'source'
    target = Path(work) / 'target'
    payload = bytes(range(256)) * 1024
    source.write_bytes(payload)
    remote = '/rw/python/space % café.bin'
    client.upload_sync(remote_path=remote, local_path=str(source))
    check('client check', client.check(remote))
    check('client info size', int(client.info(remote)['size']) == len(payload))
    client.download_sync(remote_path=remote, local_path=str(target))
    check('client upload/download beyond parser window', target.read_bytes() == payload)
    client.copy(remote, '/rw/python/copied')
    client.move('/rw/python/copied', '/rw/python/moved')
    check('client move destination', client.check('/rw/python/moved'))
    check('client move removed source', not client.check('/rw/python/copied'))
    client.clean('/rw/python')
check('client recursive delete', not client.check('/rw/python'))

# Admission before disclosure, body intake and 100 Continue.
for method in ['OPTIONS', 'PROPFIND', 'PUT', 'DELETE', 'LOCK', 'REPORT']:
    request(method, '/rw/secret', user=None, expected=401)
request('PUT', '/rw/no', b'bad', user='reader', expected=403)
for method in ['PUT', 'DELETE', 'MKCOL', 'COPY', 'MOVE', 'PROPPATCH', 'LOCK', 'UNLOCK']:
    request(method, '/ro/readme.txt', b'bad', expected=403)
status, headers, data = request('GET', '/ro/readme.txt', expected=200)
check('read-only fixture bytes', data == b'read-only fixture\n')
check('outer response wrapper', headers.get('x-dav-fixture') == 'yes')
check('filesystem reader uses weak ETag', headers['etag'].startswith('W/'))
for user, path, expected in [(None, '/rw/no', 401), ('alice', '/ro/no', 403)]:
    authorization = f'Authorization: {auth(user)}\r\n' if user else ''
    status, _, _ = raw(f'PUT {path} HTTP/1.1\r\nHost: 127.0.0.1:{args.port}\r\n'
        f'{authorization}Content-Length: 1000000\r\nExpect: 100-continue\r\n\r\n')
    check('deny before Continue/body', status == expected)

# Canonical authority, once-only decoding and traversal policy.
for path in ['/rw/%2e%2e/ro/readme.txt', '/rw/a%2fb', '/rw/a%5cb',
             '/rw/a//b', '/rw/%00', '/rw/%ff', '/rw/file?x', '/rw/file#x']:
    request('GET', path, expected=400)
request('GET', '/rw/', headers={'Host': 'evil.test'}, expected=400)
for name in ['symlink', 'hardlink', 'fifo']:
    request('GET', '/ro/unsafe/' + name, expected=403)
request('PROPFIND', '/ro/unsafe/', headers={'Depth': '1'}, expected=403)
for method in ['COPY', 'MOVE']:
    for dst in ['http://evil.test/no', '/ro/readme.txt', '/rw/../escape',
                'http://alice@127.0.0.1/no', '/rw/x?query']:
        status, _, _ = request(method, '/rw/a', headers={'Destination': dst})
        check('destination rejected before lookup', status in [400, 403])

# HTTP and DAV request conditions and mutation outcomes.
_, created, _ = request('PUT', '/rw/a', b'alpha', {'If-None-Match': '*'}, expected=201)
tag = created['etag']
check('PUT returns strong ETag', tag.startswith('"'))
request('PUT', '/rw/a', b'wrong', {'If-None-Match': '*'}, expected=412)
request('PUT', '/rw/a', b'wrong', {'If-None-Match': 'W/' + tag}, expected=412)
request('PUT', '/rw/a', b'wrong', {'If-Match': 'W/' + tag}, expected=412)
request('PUT', '/rw/a', b'beta', {'If-Match': tag}, expected=204)
_, headers, data = request('GET', '/rw/a', expected=200)
check('precondition failures preserve bytes', data == b'beta')
request('GET', '/rw/a', headers={'If-None-Match': 'W/' + headers['etag']}, expected=304)
_, head, data = request('HEAD', '/rw/a', expected=200)
check('HEAD suppresses body and keeps length', data == b'' and head['content-length'] == '4')
request('MKCOL', '/rw/dir', expected=201)
request('MKCOL', '/rw/dir', expected=405)
request('PUT', '/rw/missing/child', b'x', expected=409)
request('COPY', '/rw/a', headers={'Destination': '/rw/b', 'Overwrite': 'F'}, expected=201)
request('COPY', '/rw/a', headers={'Destination': '/rw/b', 'Overwrite': 'F'}, expected=412)
request('MOVE', '/rw/b', headers={'Destination': '/rw/c'}, expected=201)
request('GET', '/rw/b', expected=404)
request('COPY', '/rw/dir', headers={'Destination': '/rw/dir/sub'}, expected=403)

# XML namespace preservation, atomic ordered property changes, missing props.
patch = b'''<d:propertyupdate xmlns:d="DAV:" xmlns:p="urn:test"><d:set><d:prop>
<p:value xml:lang="en">a<p:child x="1"/>b</p:value></d:prop></d:set>
</d:propertyupdate>'''
request('PROPPATCH', '/rw/a', patch, xml, expected=207)
query = b'''<d:propfind xmlns:d="DAV:" xmlns:p="urn:test"><d:prop>
<p:value/><p:missing/><d:getetag/></d:prop></d:propfind>'''
_, _, data = request('PROPFIND', '/rw/a', query, {**xml, 'Depth': '0'}, expected=207)
root = ET.fromstring(data)
check('dead property mixed content', root.find('.//p:value', ns).text == 'a' and
      root.find('.//p:child', ns).tail == 'b')
check('missing property is individual 404', any('404' in e.text for e in root.findall('.//d:status', ns)))
protected = b'''<propertyupdate xmlns="DAV:" xmlns:p="urn:test"><set><prop>
<getetag>bad</getetag><p:other>bad</p:other></prop></set></propertyupdate>'''
_, _, data = request('PROPPATCH', '/rw/a', protected, xml, expected=207)
check('protected property fails atomically', b'403' in data and b'424' in data)
request('PROPFIND', '/rw/', headers={'Depth': 'infinity'}, expected=403)
request('PROPFIND', '/rw/', headers={'Depth': 'wrong'}, expected=400)
for data in [b'<propfind xmlns="DAV:">', b'<!DOCTYPE x [<!ENTITY x "boom">]><x>&x;</x>',
             b'<propfind xmlns="DAV:"><propname/><allprop/></propfind>']:
    request('PROPFIND', '/rw/', data, {**xml, 'Depth': '0'}, expected=400)
request('PROPFIND', '/rw/', b'x' * 65537, {**xml, 'Depth': '0'}, expected=413)
request('PROPFIND', '/rw/', b'<propfind xmlns="DAV:"><propname/></propfind>',
        {'Depth': '0', 'Content-Type': 'text/plain'}, expected=415)
utf16 = '<propfind xmlns="DAV:"><propname/></propfind>'.encode('utf-16le')
request('PROPFIND', '/rw/', utf16,
        {'Depth': '0', 'Content-Type': 'application/xml; charset=utf-16le'}, expected=207)

# Exclusive locks, owner binding, refresh, parent and descendant coverage.
token = lock('/rw/a')
request('PUT', '/rw/a', b'wrong', expected=423)
request('PUT', '/rw/a', b'wrong', {'If': f'({token})'}, user='bob', expected=423)
request('PUT', '/rw/a', b'locked', {'If': f'({token})'}, expected=204)
_, refreshed, _ = request('LOCK', '/rw/a',
    headers={'If': f'({token})', 'Timeout': 'Second-30'}, expected=200)
check('refresh omits Lock-Token response header', 'lock-token' not in refreshed)
request('GET', '/rw/a', headers={'If': '(["wrong"])'}, expected=412)
request('UNLOCK', '/rw/a', headers={'Lock-Token': token}, user='bob', expected=409)
# A false tagged list does not defeat a different true tagged list (RFC 4918 10.4.3).
request('PUT', '/rw/a', b'or', {'If': f'<{origin}/rw/a> ({token}) '
        f'<{origin}/rw/c> (["wrong"])'}, expected=204)
request('COPY', '/rw/c', headers={'Destination': '/rw/a', 'If': f'<{origin}/rw/a> ({token})'}, expected=204)
request('PUT', '/rw/a', b'wrong', expected=423)
request('UNLOCK', '/rw/a', headers={'Lock-Token': token}, expected=204)
parent = lock('/rw/dir', depth='0')
request('PUT', '/rw/dir/new', b'x', expected=423)
request('LOCK', '/rw/dir/new', lock_body, xml, expected=423)
request('PUT', '/rw/dir/new', b'x', {'If': f'<{origin}/rw/dir/> ({parent})'}, expected=201)
request('UNLOCK', '/rw/dir', headers={'Lock-Token': parent}, expected=204)
child = lock('/rw/dir/new')
request('DELETE', '/rw/dir', expected=423)
request('DELETE', '/rw/dir', headers={'If': f'<{origin}/rw/dir/new> ({child})'}, expected=204)
new_token = lock('/rw/locked-new', expected=201)
request('GET', '/rw/locked-new', expected=200)
request('UNLOCK', '/rw/locked-new', headers={'Lock-Token': new_token}, expected=204)
short = lock('/rw/c', seconds='Second-1')
time.sleep(1.2)
request('PUT', '/rw/c', b'expired', expected=204)

# A single chunk much larger than the parser window, then a framed trailer.
payload = b'chunked!' * 40000
head = (f'PUT /rw/chunked HTTP/1.1\r\nHost: 127.0.0.1:{args.port}\r\n'
        f'Authorization: {auth("alice")}\r\nTransfer-Encoding: chunked\r\n'
        'Connection: close\r\n\r\n')
status, _, _ = raw(head, f'{len(payload):x}\r\n'.encode() + payload +
                   b'\r\n0\r\nX-Test: trailer\r\n\r\n')
check('stream a large single chunk', status == 201)
_, _, data = request('GET', '/rw/chunked', expected=200)
check('chunked bytes unchanged', data == payload)
status, _, _ = raw(head.replace('/rw/chunked', '/rw/bad-chunk'), b'3\r\nabc!\n0\r\n\r\n')
check('malformed chunk cannot commit', status == 400)
request('GET', '/rw/bad-chunk', expected=404)
status, _, _ = raw(f'PUT /rw/too-large HTTP/1.1\r\n'
    f'Host: 127.0.0.1:{args.port}\r\nAuthorization: {auth("alice")}\r\n'
    'Content-Length: 4194305\r\nExpect: 100-continue\r\n\r\n')
check('oversized length rejected before Continue', status == 413)
request('GET', '/rw/too-large', expected=404)

# Concurrent create-only requests have one winner, without lost updates.
def create_once(i):
    return request('PUT', '/rw/race', str(i).encode(), {'If-None-Match': '*'})[0]
with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
    codes = list(pool.map(create_once, range(8)))
check('atomic no-replace under concurrency', sorted(codes) == [201] + [412] * 7)

# State deliberately left for the orchestrator's restart phase.
request('PUT', '/rw/persist', b'persistent data', expected=201)
request('PROPPATCH', '/rw/persist', b'''<propertyupdate xmlns="DAV:" xmlns:p="urn:test">
<set><prop><p:value>durable</p:value></prop></set></propertyupdate>''', xml, expected=207)
lock('/rw/persist')
print(f'{checks} independent Python DAV checks passed')
