#!/usr/bin/env python3
"""Start isolated Apache DAV, verify it independently, run a client, clean up."""

import argparse
import base64
import http.client
import json
import os
from pathlib import Path
import re
import signal
import ssl
import subprocess
import sys
import time
import traceback
from urllib.parse import quote, urlsplit
import uuid
import xml.etree.ElementTree as ET


HERE = Path(__file__).resolve().parent
STACK_ROOT = HERE.parents[2]
REPO_ROOT = STACK_ROOT.parent if STACK_ROOT.name == "bleeding" else STACK_ROOT
DAV = "{DAV:}"
TEST = "{urn:fetch-webdav:test}"
PROPERTIES = b'''<d:propfind xmlns:d="DAV:" xmlns:t="urn:fetch-webdav:test">
<d:prop><d:resourcetype/><d:getetag/><d:getcontentlength/>
<t:colour/><t:missing/></d:prop></d:propfind>'''


class Probe:
    def __init__(self, origin, ca_file, artifacts, label):
        self.origin = origin
        self.ca_file = ca_file
        self.artifacts = artifacts
        self.label = label
        self.count = 0

    def request(self, method, path, expected, body=None, headers=None,
                user="alice", password=None, chunked=False):
        url = urlsplit(self.origin)
        fields = dict(headers or {})
        if user is not None:
            password = password if password is not None else user + "-password"
            encoded = base64.b64encode((user + ":" + password).encode()).decode()
            fields["Authorization"] = "Basic " + encoded
        if url.scheme == "https":
            conn = http.client.HTTPSConnection(
                url.hostname, url.port, timeout=10,
                context=ssl.create_default_context(cafile=self.ca_file))
        else:
            conn = http.client.HTTPConnection(url.hostname, url.port, timeout=10)
        try:
            conn.request(method, path, body=body, headers=fields,
                         encode_chunked=chunked)
            response = conn.getresponse()
            data = response.read(2 * 1024 * 1024 + 1)
            assert len(data) <= 2 * 1024 * 1024, "oversized fixture response"
            status = response.status
            result_headers = {k.lower(): v for k, v in response.getheaders()}
            for field in ["dav", "allow"]:
                values = response.headers.get_all(field)
                if values:
                    result_headers[field] = ", ".join(values)
        finally:
            conn.close()
        self.count += 1
        stem = f"{self.label}-{self.count:03}-{method}"
        (self.artifacts / (stem + ".body")).write_bytes(data)
        with (self.artifacts / "requests.jsonl").open("a") as trace:
            trace.write(json.dumps({
                "method": method, "url": self.origin + path, "status": status,
                "response_headers": result_headers, "body_file": stem + ".body",
            }) + "\n")
        assert status == expected, (
            f"{method} {path}: expected {expected}, got {status}: {data[:500]!r}")
        return result_headers, data

    def props(self, path, depth="0"):
        _, data = self.request("PROPFIND", path, 207, PROPERTIES,
                               {"Depth": depth, "Content-Type": "application/xml"})
        return ET.fromstring(data)


def property_statuses(tree):
    result = {}
    for group in tree.iter(DAV + "propstat"):
        status = int(group.findtext(DAV + "status").split()[1])
        for prop in group.find(DAV + "prop"):
            result[prop.tag] = status, prop
    return result


def smoke(probe):
    """Assert behavior we need before treating this server as a client oracle."""
    root = "/dav/probe-" + uuid.uuid4().hex + "/"
    probe.request("PROPFIND", "/dav/", 401, headers={"Depth": "0"}, user=None)
    probe.request("PROPFIND", "/dav/", 401, headers={"Depth": "0"}, password="wrong")
    headers, _ = probe.request("OPTIONS", "/dav/", 200)
    assert {"1", "2"} <= set(x.strip() for x in headers["dav"].split(","))
    assert "PROPFIND" in headers["allow"] and "LOCK" in headers["allow"]
    probe.request("MKCOL", root, 201)
    probe.request("MKCOL", root, 405)
    probe.request("MKCOL", root + "absent/child/", 409)
    probe.request("MKCOL", root + "sub/", 201)

    path = root + "file.txt"
    content = b"WebDAV fixture\n"
    probe.request("PUT", path, 201, content)
    headers, data = probe.request("GET", path, 200)
    assert data == content
    etag = headers["etag"]
    headers, data = probe.request("HEAD", path, 200)
    assert not data and int(headers["content-length"]) == len(content)
    _, data = probe.request("GET", path, 206, headers={"Range": "bytes=0-3"})
    assert data == content[:4]
    probe.request("GET", path, 304, headers={"If-None-Match": etag})
    probe.request("PUT", path, 412, b"stale", {"If-Match": '"stale"'})
    probe.request("PUT", path, 412, b"replace", {"If-None-Match": "*"})
    # Apache can emit a weak validator for a file modified in this second.
    # Weak validators cannot satisfy If-Match; wait for the server's strong
    # value rather than changing the spelling of the validator ourselves.
    deadline = time.monotonic() + 5
    while etag.startswith("W/"):
        assert time.monotonic() < deadline, "Apache did not issue a strong ETag"
        time.sleep(0.1)
        headers, _ = probe.request("HEAD", path, 200)
        etag = headers["etag"]
    probe.request("PUT", path, 204, b"updated fixture\n", {"If-Match": etag})

    tree = probe.props(path)
    props = property_statuses(tree)
    assert props[DAV + "getetag"][0] == 200
    assert props[TEST + "missing"][0] == 404
    patch = b'''<d:propertyupdate xmlns:d="DAV:" xmlns:t="urn:fetch-webdav:test">
<d:set><d:prop><t:colour>blue &amp; green</t:colour></d:prop></d:set>
</d:propertyupdate>'''
    _, body = probe.request("PROPPATCH", path, 207, patch,
                            {"Content-Type": "application/xml"})
    assert property_statuses(ET.fromstring(body))[TEST + "colour"][0] == 200
    assert property_statuses(probe.props(path))[TEST + "colour"][1].text == "blue & green"
    rollback = b'''<d:propertyupdate xmlns:d="DAV:" xmlns:t="urn:fetch-webdav:test">
<d:set><d:prop><t:colour>rolled back</t:colour></d:prop></d:set>
<d:set><d:prop><d:getetag>protected</d:getetag></d:prop></d:set>
</d:propertyupdate>'''
    _, body = probe.request("PROPPATCH", path, 207, rollback,
                            {"Content-Type": "application/xml"})
    statuses = property_statuses(ET.fromstring(body))
    # This pinned mod_dav_fs reports 409 for a read-only live property;
    # RFC 4918 recommends 403/cannot-modify-protected-property. Keep this
    # interoperability case explicit rather than treating Apache as the RFC.
    assert statuses[DAV + "getetag"][0] == 409
    assert statuses[TEST + "colour"][0] == 424
    assert property_statuses(probe.props(path))[TEST + "colour"][1].text == "blue & green"

    encoded = root + quote("space % café.txt", safe="")
    probe.request("PUT", encoded, 201, b"encoded name")
    _, body = probe.request("GET", encoded, 200)
    assert body == b"encoded name"
    tree = probe.props(root, "1")
    # Escape hex digits are case-insensitive, while literal path bytes are not.
    hrefs = [re.sub(r"%[0-9a-fA-F]{2}", lambda m: m[0].upper(),
                    urlsplit(e.text).path) for e in tree.iter(DAV + "href")]
    assert set(hrefs) == {root, path, encoded, root + "sub/"}, hrefs
    tree = probe.props(root)
    assert len(list(tree.iter(DAV + "response"))) == 1
    _, body = probe.request("PROPFIND", root, 403, PROPERTIES,
                            {"Depth": "infinity", "Content-Type": "application/xml"})
    # mod_dav returns an HTML diagnostic here, not a DAV:error document.
    assert b"infinity" in body and b"not allowed" in body

    copied = root + "copy.txt"
    destination = {"Destination": probe.origin + copied, "Overwrite": "F"}
    probe.request("COPY", path, 201, headers=destination)
    probe.request("COPY", path, 412, headers=destination)
    probe.request("COPY", path, 204, headers={**destination, "Overwrite": "T"})
    assert property_statuses(probe.props(copied))[TEST + "colour"][1].text == "blue & green"
    moved = root + "moved.txt"
    probe.request("MOVE", copied, 201,
                  headers={"Destination": probe.origin + moved, "Overwrite": "F"})
    probe.request("GET", copied, 404)
    _, body = probe.request("GET", moved, 200)
    assert body == b"updated fixture\n"

    lock = b'''<d:lockinfo xmlns:d="DAV:"><d:lockscope><d:exclusive/>
</d:lockscope><d:locktype><d:write/></d:locktype><d:owner>fixture</d:owner>
</d:lockinfo>'''
    headers, body = probe.request("LOCK", path, 200, lock,
                                  {"Content-Type": "application/xml", "Depth": "0",
                                   "Timeout": "Second-60"})
    token = headers["lock-token"]
    assert token.startswith("<") and token.endswith(">")
    active = ET.fromstring(body).find(".//" + DAV + "activelock")
    assert active.findtext(DAV + "timeout").startswith("Second-")
    assert active.findtext(DAV + "locktoken/" + DAV + "href") == token[1:-1]
    probe.request("PUT", path, 423, b"without token")
    probe.request("PUT", path, 423, b"other user", user="bob")
    probe.request("PUT", path, 204, b"with token", {"If": "(" + token + ")"})
    probe.request("LOCK", path, 200, b"",
                  {"If": "(" + token + ")", "Timeout": "Second-60"})
    probe.request("UNLOCK", path, 204, headers={"Lock-Token": token})
    probe.request("PUT", path, 204, b"after unlock")

    probe.request("PUT", root + "chunked", 201, iter([b"one", b"two"]), chunked=True)
    _, body = probe.request("GET", root + "chunked", 200)
    assert body == b"onetwo"
    probe.request("DELETE", moved, 204)
    probe.request("DELETE", moved, 404)
    probe.request("DELETE", root, 204)
    print(f"{probe.label}: {probe.count} WebDAV exchanges checked", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifacts", type=Path, help="directory for logs and response fixtures")
    parser.add_argument("command", nargs=argparse.REMAINDER, help="-- client command to run")
    args = parser.parse_args()
    project = "fetch-dav-" + uuid.uuid4().hex[:12]
    artifacts = (args.artifacts or REPO_ROOT / "_build/webdav-docker" / project).resolve()
    artifacts.mkdir(parents=True, exist_ok=False)
    print(f"Fixture: {project}\nArtifacts: {artifacts}", flush=True)
    compose = ["docker", "compose", "-f", str(HERE / "compose.yaml"), "-p", project]

    def docker(*arguments):
        return subprocess.check_output([*compose, *arguments], text=True).strip()

    try:
        with (artifacts / "startup.log").open("w") as log:
            subprocess.run([*compose, "up", "--build", "-d"], check=True,
                           stdout=log, stderr=subprocess.STDOUT)
        container = docker("ps", "-q", "dav")
        assert container, "Compose did not start the DAV service"
        ca_file = artifacts / "ca.pem"
        subprocess.run(["docker", "cp", container + ":/usr/local/apache2/conf/test-ca.pem",
                        str(ca_file)], check=True, stdout=subprocess.DEVNULL)
        http_origin = "http://" + docker("port", "dav", "8080")
        https_origin = "https://" + docker("port", "dav", "8443")
        deadline = time.monotonic() + 30
        while True:
            url = urlsplit(http_origin)
            conn = http.client.HTTPConnection(url.hostname, url.port, timeout=1)
            try:
                conn.request("GET", "/health.txt")
                response = conn.getresponse()
                if response.status == 200 and response.read() == b"ready\n":
                    break
            except (OSError, http.client.HTTPException):
                pass
            finally:
                conn.close()
            if time.monotonic() >= deadline:
                raise RuntimeError("DAV server did not become ready within 30s")
            time.sleep(0.1)
        print(f"HTTP: {http_origin}/dav/\nHTTPS: {https_origin}/dav/", flush=True)
        tls_url = urlsplit(https_origin)
        conn = http.client.HTTPSConnection(tls_url.hostname, tls_url.port, timeout=5)
        try:
            conn.request("GET", "/health.txt")
        except ssl.SSLCertVerificationError:
            pass
        else:
            raise AssertionError("fixture certificate unexpectedly trusted without its CA")
        finally:
            conn.close()
        for label, origin in [("http", http_origin), ("https", https_origin)]:
            smoke(Probe(origin, str(ca_file), artifacts, label))
        environment = {
            "WEBDAV_URL": https_origin + "/dav/",
            "WEBDAV_HTTP_URL": http_origin + "/dav/",
            "WEBDAV_CA_FILE": str(ca_file),
            "WEBDAV_USER": "alice", "WEBDAV_PASSWORD": "alice-password",
            "WEBDAV_OTHER_USER": "bob", "WEBDAV_OTHER_PASSWORD": "bob-password",
        }
        (artifacts / "environment.json").write_text(json.dumps(environment, indent=2) + "\n")
        command = args.command
        if command[:1] == ["--"]:
            command = command[1:]
        if command:
            subprocess.run(command, env={**os.environ, **environment}, check=True)
    finally:
        with (artifacts / "server.log").open("w") as log:
            subprocess.run([*compose, "logs", "--no-color"], stdout=log, stderr=log)
        with (artifacts / "cleanup.log").open("w") as log:
            cleanup = subprocess.run(
                [*compose, "down", "--volumes", "--remove-orphans", "--rmi", "local"],
                stdout=log, stderr=log)
        if cleanup.returncode:
            print(f"Fixture cleanup failed; see {artifacts / 'cleanup.log'}", file=sys.stderr)
            if sys.exc_info()[0] is None:
                cleanup.check_returncode()
        print(f"Artifacts: {artifacts}", flush=True)


if __name__ == "__main__":
    def interrupted(_signum, _frame):
        raise KeyboardInterrupt

    signal.signal(signal.SIGTERM, interrupted)
    try:
        main()
    except (AssertionError, RuntimeError, subprocess.CalledProcessError) as exc:
        traceback.print_exc()
        print(f"WebDAV fixture failed: {exc}", file=sys.stderr)
        sys.exit(1)
