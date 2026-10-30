#!/usr/bin/env python3
"""Exercise the built CLI against loopback PDS, PLC, knot and spindle fixtures."""
import base64
import hashlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
from pathlib import Path
import ssl
import struct
import subprocess
import sys
import tempfile
import threading
import time
from urllib.parse import urlparse, parse_qs

CLI = str(Path(sys.argv[1]).resolve())
DID = "did:plc:alice"
BOB = "did:plc:bob"
REPO = "did:plc:repository"
PIPELINE = "3m7cdthhgz222"
SHA = "a" * 40
records = {}
calls = []
service_tokens = {}
fault = None
urls = {}
pipeline = {"id": PIPELINE, "repo": REPO, "commit": SHA,
            "trigger": {"$type": "org.tangled.ci.trigger.manual", "repo": REPO,
                        "commit": SHA, "inputs": [{"key": "message", "value": "hello"}]},
            "workflows": [{"id": "inspect", "name": "inspect", "status": "success",
                           "definition": {"id": "inspect", "triggers": [],
                                          "source": {"$type": "org.tangled.ci.defs#externalSource",
                                                     "link": "https://example.com"}}}]}


def web_did(url):
    return "did:web:" + urlparse(url).netloc.replace(":", "%3A")


def cid(value):
    return "bafyrei" + hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()[:52]


def jwt(expiry=None):
    def b64(v):
        return base64.urlsafe_b64encode(json.dumps(v).encode()).decode().rstrip("=")
    return b64({"alg": "HS256", "typ": "JWT"}) + "." + b64(
        {"sub": DID, "exp": expiry if expiry is not None else int(time.time()) + 3600}) + ".c2ln"


def cbor(value):
    def head(kind, n):
        if n < 24:
            return bytes([kind * 32 + n])
        if n < 256:
            return bytes([kind * 32 + 24, n])
        return bytes([kind * 32 + 25]) + struct.pack(">H", n)
    if isinstance(value, int):
        return head(0, value)
    if isinstance(value, str):
        b = value.encode()
        return head(3, len(b)) + b
    if isinstance(value, dict):
        return head(5, len(value)) + b"".join(cbor(k) + cbor(v) for k, v in value.items())
    raise AssertionError(value)


# The audience each service expects for a procedure. Repository-account writes
# on a knot are addressed to the repository DID, everything else to the
# service's own DID.
def audience(role, method, data):
    if role == "knot" and data.get("repo") == REPO and (
            method.startswith("com.atproto.repo.")
            or method == "org.tangled.review.submitPatch"):
        return REPO
    return web_did(urls[role])


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *_):
        pass

    def json(self, value, status=200):
        body = json.dumps(value).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        self.handle_request()

    def do_POST(self):
        self.handle_request()

    def record_reply(self, did, coll, key):
        value = records.get((did, coll, key))
        if value is None:
            return self.json({"error": "RecordNotFound"}, 400)
        return self.json({"uri": f"at://{did}/{coll}/{key}", "cid": cid(value), "value": value})

    def handle_request(self):
        role = self.server.role
        url = urlparse(self.path)
        method = url.path.removeprefix("/xrpc/")
        query = parse_qs(url.query)
        body = self.rfile.read(int(self.headers.get("Content-Length", 0)))
        data = json.loads(body) if body else None
        auth = self.headers.get("Authorization")
        calls.append((role, method, query, data, auth))

        if role == "plc":
            did = url.path.lstrip("/")
            if did == REPO:
                return self.json({"id": REPO, "service": [
                    {"id": "#atproto_pds", "type": "AtprotoPersonalDataServer",
                     "serviceEndpoint": urls["knot"]},
                    {"id": "#tangled_knot", "type": "TangledKnot",
                     "serviceEndpoint": urls["knot"] + "/repo/abc"}]})
            return self.json({"id": did, "service": [
                {"id": "#atproto_pds", "type": "AtprotoPersonalDataServer",
                 "serviceEndpoint": urls["pds"]}]})
        if url.path == "/.well-known/did.json":
            assert role == "spindle"
            did = web_did(urls["spindle"])
            return self.json({"id": did, "service": [
                {"id": "#tangled_spindle", "type": "TangledSpindle",
                 "serviceEndpoint": urls["spindle"]}]})

        if role != "pds" and self.command == "POST":
            token = auth.removeprefix("Bearer ") if auth else None
            assert token in service_tokens, (method, "PDS credential leaked or service auth missing")
            aud, scope = service_tokens.pop(token)
            assert scope == method, (scope, method)
            assert aud == audience(role, method, data), (method, aud)

        if method == "com.atproto.server.createSession":
            return self.json({"did": DID, "handle": "alice.test",
                              "accessJwt": jwt(), "refreshJwt": jwt()})
        if method == "com.atproto.server.getServiceAuth":
            token = "service-" + str(len(calls))
            service_tokens[token] = (query["aud"][0], query["lxm"][0])
            return self.json({"token": token})
        if method == "com.atproto.identity.resolveHandle":
            return self.json({"did": BOB if query["handle"] == ["bob.test"] else DID})
        if method == "com.atproto.repo.listRecords":
            if fault == "list-error":
                return self.json({"error": "Unavailable"}, 503)
            did, collection = query["repo"][0], query["collection"][0]
            values = [{"uri": f"at://{did}/{collection}/{key}", "cid": cid(value), "value": value}
                      for (d, coll, key), value in records.items() if (d, coll) == (did, collection)]
            index = int(query.get("cursor", ["0"])[0])
            out = {"records": values[index:index + 1]}
            if index + 1 < len(values):
                out["cursor"] = str(index + 1)
            elif role == "knot":
                out["cursor"] = None  # knot2 writes an absent cursor as null
            if fault == "repeat-cursor":
                out["cursor"] = "1"
            return self.json(out)
        if method == "com.atproto.repo.getRecord":
            if fault == "get-error":
                return self.json({"error": "Unavailable"}, 503)
            return self.record_reply(query["repo"][0], query["collection"][0], query["rkey"][0])
        if method == "com.atproto.repo.createRecord":
            coll = data["collection"]
            assert data.get("validate") is None
            assert data["record"]["$type"] == coll
            assert data["record"]["createdAt"].endswith("Z"), data["record"]
            if role == "knot" and coll == "org.tangled.track.ticket":
                assert "rkey" not in data, "the knot mints ticket keys"
                assert data["record"]["state"] == "open"
                key = str(1 + sum(1 for (_, c, _) in records if c == coll))
            elif role == "knot":
                key = data["rkey"]
                assert key.startswith("did:"), "grants are keyed by subject DID"
            else:
                key = data.get("rkey", "3k" + str(len(records)))
            records[data["repo"], coll, key] = data["record"]
            if role == "knot" and key.startswith("did:"):
                return self.json({})
            return self.json({"uri": f"at://{data['repo']}/{coll}/{key}", "cid": cid(data["record"])})
        if method == "com.atproto.repo.putRecord":
            k = data["repo"], data["collection"], data["rkey"]
            assert data["swapRecord"] == cid(records[k]), "stale swapRecord"
            assert not any(f.startswith("x-tngl-") for f in data["record"]), "stamp written back"
            records[k] = data["record"]
            return self.json({"uri": "at://{}/{}/{}".format(*k), "cid": cid(data["record"])})
        if method == "com.atproto.repo.deleteRecord":
            k = data["repo"], data["collection"], data["rkey"]
            if role == "knot" or data.get("swapRecord") is not None:
                assert data["swapRecord"] == cid(records[k])
            records.pop(k)
            return self.json({})
        if method == "org.tangled.knot.describeKnot":
            assert auth is None
            return self.json({"did": web_did(urls["knot"]), "version": "fixture"})
        if method == "org.tangled.repo.createRepo":
            assert data["name"] == "demo"
            assert (DID, "org.tangled.repo.declaration", "demo") not in records
            records[REPO, "org.tangled.repo.manifest", "self"] = {
                "$type": "org.tangled.repo.manifest",
                "declaration": {"owner": DID, "name": "demo"},
                "createdAt": data["createdAt"], "spindle": data.get("spindle"),
                "x-tngl-editor": DID}
            return self.json({"did": REPO, "key": "zQ3sfixture"})
        if method == "org.tangled.repo.deleteRepo":
            assert data["repo"] == REPO
            assert (DID, "org.tangled.repo.declaration", "demo") not in records
            self.send_response(200)
            self.send_header("Content-Length", "0")
            return self.end_headers()
        if method == "org.tangled.review.submitPatch":
            patch = data["patch"]
            assert patch == {"ticket": "2", "base": {"oid": SHA},
                             "head": {"oid": "b" * 40}, "sourceRepo": REPO,
                             "sourceBranch": "feature"}, patch
            return self.json({"uri": f"at://{REPO}/org.tangled.review.patch/x",
                              "cid": cid(patch)})
        if method == "org.tangled.spindle.describeSpindle":
            return self.json({"did": web_did(urls["spindle"]), "version": "fixture"})
        if method.startswith("org.tangled.temp.spindle.allowList."):
            allowed = self.server.allowed
            if method.endswith(".list"):
                return self.json({"dids": sorted(allowed)})
            (allowed.add if method.endswith(".add") else allowed.discard)(data["did"])
            return self.json({})
        if method == "org.tangled.ci.queryPipelines":
            assert query["repo"] == [REPO]
            assert auth is None
            return self.json({"pipelines": [pipeline], "total": 1, "cursor": "next"})
        if method == "org.tangled.ci.getPipeline":
            assert query["repo"] == [REPO]
            return self.json(pipeline)
        if method == "org.tangled.ci.listWorkflowDefinitions":
            return self.json({"workflows": [pipeline["workflows"][0]["definition"]]})
        if method == "org.tangled.event.notifyEvent":
            event = data["event"]
            assert data["repo"] == REPO and event["repo"] == REPO
            assert event["$type"] == "org.tangled.ci.trigger.manual"
            assert event["commit"] == SHA
            return self.json({"ci": {"pipelines": [PIPELINE], "message": "started"}})
        if method == "org.tangled.ci.retryPipeline":
            return self.json({"pipeline": "3m7cdthhgz333"})
        if method in ("org.tangled.ci.cancelPipeline", "org.tangled.ci.approvePipeline"):
            assert data["repo"] == REPO and data["pipeline"] == PIPELINE
            return self.json({})
        if method == "org.tangled.ci.subscribePipelineLogs":
            assert query["repo"] == [REPO]
            accept = base64.b64encode(hashlib.sha1(
                (self.headers["Sec-WebSocket-Key"] + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").encode()).digest())
            self.send_response(101)
            self.send_header("Upgrade", "websocket")
            self.send_header("Connection", "Upgrade")
            self.send_header("Sec-WebSocket-Accept", accept.decode())
            self.end_headers()
            payload = cbor({"op": 1, "t": "#data"}) + cbor(
                {"time": "2026-01-01T00:00:00Z", "workflow": "inspect",
                 "step": 1, "stream": "stdout", "content": "partial\nmore"})
            header = bytes([0x82, len(payload)]) if len(payload) < 126 else bytes([0x82, 126]) + struct.pack(">H", len(payload))
            self.wfile.write(header + payload)
            self.wfile.write(b"\x88\x02\x03\xe8")
            self.wfile.flush()
            self.rfile.read(2)
            return
        return self.json({"error": "UnexpectedMethod", "message": method}, 400)


with tempfile.TemporaryDirectory(prefix="tangled-cli-") as directory:
    env = dict(os.environ, XDG_CONFIG_HOME=directory, XDG_STATE_HOME=directory,
               XDG_DATA_HOME=directory, XDG_CACHE_HOME=directory)
    env.pop("TANGLED_CONFIG_DIR", None)
    certificate = Path(directory) / "ca.pem"
    key = Path(directory) / "key.pem"
    subprocess.run(["openssl", "req", "-x509", "-newkey", "rsa:2048",
                    "-nodes", "-days", "1", "-subj", "/CN=localhost",
                    "-addext", "subjectAltName=DNS:localhost",
                    "-keyout", str(key), "-out", str(certificate)],
                   check=True, capture_output=True)
    env["SSL_CERT_FILE"] = str(certificate)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.load_cert_chain(certificate, key)
    servers = []

    def server(role):
        server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        server.role = role
        server.allowed = set()
        server.socket = context.wrap_socket(server.socket, server_side=True)
        servers.append(server)
        threading.Thread(target=server.serve_forever, daemon=True).start()
        urls[role] = f"https://localhost:{server.server_port}"
        return urls[role]

    pds, knot, spindle, plc = (server(r) for r in ("pds", "knot", "spindle", "plc"))
    spindle_host = urlparse(spindle).netloc
    env["TANGLED_PDS"] = pds
    env["TANGLED_PLC"] = plc

    def run(*args, ok=True, stdin=None):
        result = subprocess.run([CLI, *args], env=env, input=stdin, capture_output=True,
                                text=True, timeout=20)
        assert (result.returncode == 0) == ok, (args, result.stdout, result.stderr)
        return result.stdout

    def manifest():
        return records[REPO, "org.tangled.repo.manifest", "self"]

    try:
        assert len(run("api", "list").splitlines()) == 418
        run("api", "schema", "org.tangled.temp.spindle.quota.set")
        run("knot", "describe", knot)
        run("auth", "login", "--handle", "alice.test", "--password", "fixture")
        out = run("repo", "create", "demo", "--knot", knot, "--spindle", spindle_host)
        assert REPO in out and f"{knot}/{REPO}" in out, out
        assert records[DID, "org.tangled.repo.declaration", "demo"]["repo"] == REPO
        assert manifest()["spindle"] == web_did(spindle)
        run("repo", "create", "Demo", "--knot", knot, ok=False)
        records[DID, "org.tangled.repo.declaration", "another"] = {
            "$type": "org.tangled.repo.declaration", "repo": "did:plc:other",
            "createdAt": "2026-01-01T00:00:00Z"}
        assert len(json.loads(run("repo", "list", "--json"))) == 2
        fault = "repeat-cursor"
        run("repo", "list", ok=False)
        fault = "get-error"
        run("repo", "info", "demo", ok=False)
        fault = None
        run("repo", "info", "demo")
        run("repo", "info", "alice.test/demo")
        run("repo", "info", f"at://{DID}/org.tangled.repo.declaration/demo")
        run("repo", "info", REPO)
        # A declaration under another name must not claim the repository.
        records[DID, "org.tangled.repo.declaration", "evil"] = dict(
            records[DID, "org.tangled.repo.declaration", "demo"])
        run("repo", "info", "evil", ok=False)
        del records[DID, "org.tangled.repo.declaration", "evil"]
        manifest()["futureField"] = {"preserve": True}
        run("repo", "spindle", "demo", "other.test")
        assert manifest()["futureField"] == {"preserve": True}
        assert manifest()["spindle"] == "did:web:other.test"
        assert "x-tngl-editor" not in manifest()
        run("repo", "spindle", "demo")
        assert "spindle" not in manifest()
        run("repo", "spindle", "demo", spindle_host)
        run("repo", "add-collaborator", "demo", "bob.test")
        assert run("repo", "collaborators", "demo").split() == [BOB]
        run("repo", "remove-collaborator", "demo", BOB)
        assert run("repo", "collaborators", "demo").split() == []
        knot_did = web_did(knot)
        run("knot", "add-member", knot, BOB)
        assert (knot_did, "org.tangled.knot.member", BOB) in records
        assert run("knot", "members", knot).split() == [BOB]
        run("knot", "remove-member", knot, BOB)
        run("spindle", "describe", spindle_host)
        run("spindle", "add-member", spindle_host, BOB)
        assert json.loads(run("spindle", "members", spindle_host))["dids"] == [BOB]
        run("spindle", "remove-member", spindle_host, BOB)
        run("star", "add", REPO)
        run("star", "add", REPO)
        stars = json.loads(run("star", "list"))
        assert [s["subject"] for s in stars] == [
            {"$type": "org.tangled.feed.star#repo", "identity": REPO}], stars
        run("star", "remove", REPO)
        assert json.loads(run("star", "list")) == []
        issue = json.loads(run("issue", "create", "demo", "--title", "CLI issue"))["uri"]
        assert issue == f"at://{REPO}/org.tangled.track.ticket/1"
        ticket = records[REPO, "org.tangled.track.ticket", "1"]
        assert ticket["title"] == {"$type": "org.tangled.markup.markdown#inline", "text": "CLI issue"}
        run("issue", "close", "demo", "1")
        assert records[REPO, "org.tangled.track.ticket", "1"]["state"] == "closed"
        run("issue", "reopen", "demo", "1")
        run("pull", "create", "demo", "--title", "A change", "--target", "main")
        assert run("issue", "list", "demo").split()[0] == "1"
        assert run("pull", "list", "demo").split()[0] == "2"
        run("pull", "submit", "demo", "2", "--base", SHA, "--head", "b" * 40,
            "--branch", "feature")
        assert json.loads(run("issue", "show", "demo", "1"))["state"] == "open"
        key_file = Path(directory) / "id.pub"
        key_file.write_text("ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMockFixtureOnly fixture")
        key_uri = json.loads(run("key", "add", str(key_file), "--name", "fixture"))["uri"]
        key_rkey = key_uri.split("/")[-1]
        assert records[DID, "org.tangled.key.sshKey", key_rkey]["usages"] == ["auth"]
        run("key", "list")
        run("key", "remove", key_rkey)
        key_file.write_text("-----BEGIN OPENSSH PRIVATE KEY-----")
        before = len(calls)
        run("key", "add", str(key_file), "--name", "private", ok=False)
        assert len(calls) == before
        run("pipeline", "list", "--repo", REPO, "--spindle", spindle_host, "--json",
            "--commit", SHA, "--trigger", "org.tangled.ci.trigger.manual",
            "--limit", "1", "--cursor", "old")
        run("pipeline", "list", "--repo", "demo")
        run("pipeline", "show", PIPELINE, "--repo", "demo")
        assert run("pipeline", "trigger", "--repo", "demo", "--sha", SHA,
                   "--input", "message=hello").split() == [PIPELINE]
        assert calls[-1][3]["event"]["inputs"] == [{"key": "message", "value": "hello"}]
        assert run("pipeline", "retry", PIPELINE, "--repo", "demo").strip() == "3m7cdthhgz333"
        run("pipeline", "cancel", PIPELINE, "--repo", "demo", "-w", "inspect")
        assert calls[-1][3]["workflows"] == ["inspect"]
        run("pipeline", "approve", PIPELINE, "--repo", "demo")
        run("pipeline", "definitions", "--repo", "demo")
        assert run("pipeline", "logs", PIPELINE, "--repo", REPO,
                   "--spindle", spindle_host) == "partial\nmore"
        before = len(calls)
        run("api", "call", "org.tangled.ci.queryPipelines", "--service", spindle,
            "-q", "repo=did:plc:repo", "-q", "limit=999", ok=False)
        assert len(calls) == before
        run("api", "call", "org.tangled.event.notifyEvent", "--service", spindle,
            "--auth", "service", "--input", "-", stdin="{}", ok=False)
        assert len(calls) == before
        run("repo", "delete", "demo", "--force")
        assert calls[-1][1] == "org.tangled.repo.deleteRepo"
        assert (DID, "org.tangled.repo.declaration", "demo") not in records
        for path in Path(directory).rglob("session.json"):
            saved = json.loads(path.read_text())
            saved["access_jwt"] = jwt(1)
            path.write_text(json.dumps(saved))
        before = len(calls)
        run("knot", "describe", knot)
        run("pipeline", "show", PIPELINE, "--repo", REPO, "--spindle", spindle_host)
        assert [call[1] for call in calls[before:] if call[0] != "plc"
                and not call[1].startswith("/")] == [
            "org.tangled.knot.describeKnot", "org.tangled.ci.getPipeline"], calls[before:]
        print("PASS: CLI repository identity, declaration checks, repo-account writes, "
              "token audiences, ACLs, tickets, CI and WebSocket logs")
    finally:
        for server in servers:
            server.shutdown()
            server.server_close()
