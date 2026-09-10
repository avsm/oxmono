#!/usr/bin/env python3
"""Exercise the built CLI using loopback-only PDS, knot and spindle fixtures."""
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
REPO = "did:plc:repository"
PIPELINE = "3m7cdthhgz222"
SHA = "a" * 40
CID = "bafyreiaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
records = {}
calls = []
service_tokens = {}
fault = None
pipeline = {"id": PIPELINE, "repo": REPO, "commit": SHA,
            "trigger": {"$type": "sh.tangled.ci.trigger#manual", "sha": SHA,
                        "inputs": [{"key": "message", "value": "hello"}]},
            "workflows": [{"id": "inspect", "name": "inspect", "status": "success"}]}

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

    def handle_request(self):
        global fault
        url = urlparse(self.path)
        method = url.path.removeprefix("/xrpc/")
        query = parse_qs(url.query)
        body = self.rfile.read(int(self.headers.get("Content-Length", 0)))
        data = json.loads(body) if body else None
        auth = self.headers.get("Authorization")
        calls.append((self.server.role, method, query, data, auth))
        if self.server.role != "pds" and self.command == "POST":
            token = auth.removeprefix("Bearer ") if auth else None
            assert token in service_tokens, (method, "PDS credential leaked or service auth missing")
            audience, scope = service_tokens.pop(token)
            assert audience == "did:web:test.local" and scope == method

        if method == "com.atproto.server.createSession":
            return self.json({"did": DID, "handle": "alice.test",
                              "accessJwt": jwt(), "refreshJwt": jwt()})
        if method == "com.atproto.server.getServiceAuth":
            token = "service-" + str(len(calls))
            service_tokens[token] = (query["aud"][0], query["lxm"][0])
            return self.json({"token": token})
        if method == "com.atproto.identity.resolveHandle":
            return self.json({"did": DID})
        if method == "com.atproto.repo.listRecords":
            if fault == "list-error":
                return self.json({"error": "Unavailable"}, 503)
            collection = query["collection"][0]
            values = [{"uri": f"at://{DID}/{collection}/{key}", "cid": CID, "value": value}
                      for (coll, key), value in records.items() if coll == collection]
            index = int(query.get("cursor", ["0"])[0])
            out = {"records": values[index:index + 1]}
            if index + 1 < len(values):
                out["cursor"] = str(index + 1)
            if fault == "repeat-cursor":
                out["cursor"] = "1"
            return self.json(out)
        if method == "com.atproto.repo.getRecord":
            if fault == "get-error":
                return self.json({"error": "Unavailable"}, 503)
            coll, key = query["collection"][0], query["rkey"][0]
            value = records.get((coll, key))
            if value is None:
                return self.json({"error": "RecordNotFound"}, 400)
            return self.json({"uri": f"at://{DID}/{coll}/{key}", "cid": CID, "value": value})
        if method in ("com.atproto.repo.createRecord", "com.atproto.repo.putRecord"):
            coll = data["collection"]
            key = data.get("rkey", str(len(records)))
            assert data.get("validate") is None
            if method.endswith("putRecord"):
                assert data["swapRecord"] == CID
            records[coll, key] = data["record"]
            return self.json({"uri": f"at://{DID}/{coll}/{key}", "cid": CID})
        if method == "com.atproto.repo.deleteRecord":
            assert data["swapRecord"] == CID
            records.pop((data["collection"], data["rkey"]))
            return self.json({})
        if method == "sh.tangled.repo.create":
            assert data["name"] == "demo" and data["rkey"] == "demo"
            assert ("sh.tangled.repo", "demo") not in records
            return self.json({"repoDid": REPO})
        if method == "sh.tangled.repo.delete":
            assert data["repo"] == REPO
            assert ("sh.tangled.repo", "demo") not in records
            self.send_response(204)
            return self.end_headers()
        if method == "sh.tangled.repo.describeRepo":
            return self.json({"repoDid": REPO, "ownerDid": DID, "rkey": "demo"})
        if method.endswith(("addMember", "removeMember", "addCollaborator", "removeCollaborator", "cancelPipeline")):
            self.send_response(204)
            return self.end_headers()
        if method in ("sh.tangled.knot.listMembers", "sh.tangled.repo.listCollaborators"):
            return self.json({"items": [], "cursor": "next"})
        if method == "sh.tangled.knot.version":
            assert auth is None
            return self.json({"version": "fixture", "capabilities": ["knot-acl", "repo-did-input"]})
        if method == "sh.tangled.ci.queryPipelines":
            assert query["repo"] == [REPO]
            assert auth is None
            return self.json({"pipelines": [pipeline], "total": 1, "cursor": "next"})
        if method == "sh.tangled.ci.getPipeline":
            return self.json(pipeline)
        if method == "sh.tangled.ci.triggerPipeline":
            assert data["repo"] == REPO
            assert data["trigger"]["sha"] == SHA
            return self.json({"pipeline": f"at://did:web:test.local/sh.tangled.pipeline/{PIPELINE}"})
        if method == "sh.tangled.ci.describeWorkflowDefinition":
            return self.json({"derived": False})
        if method == "sh.tangled.ci.subscribePipelineLogs":
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
    env = dict(os.environ, XDG_CONFIG_HOME=directory, XDG_STATE_HOME=directory, XDG_DATA_HOME=directory, XDG_CACHE_HOME=directory)
    env.pop("TANGLED_CONFIG_DIR", None)
    certificate = Path(directory) / "ca.pem"
    key = Path(directory) / "key.pem"
    subprocess.run(["openssl", "req", "-x509", "-newkey", "rsa:2048",
                    "-nodes", "-days", "1", "-subj", "/CN=127.0.0.1",
                    "-addext", "subjectAltName=IP:127.0.0.1",
                    "-keyout", str(key), "-out", str(certificate)],
                   check=True, capture_output=True)
    env["SSL_CERT_FILE"] = str(certificate)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.load_cert_chain(certificate, key)
    servers = []
    def server(role):
        server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        server.role = role
        server.socket = context.wrap_socket(server.socket, server_side=True)
        servers.append(server)
        threading.Thread(target=server.serve_forever, daemon=True).start()
        return f"https://127.0.0.1:{server.server_port}"
    pds, knot, spindle = server("pds"), server("knot"), server("spindle")
    env["TANGLED_PDS"] = pds
    def run(*args, ok=True, stdin=None):
        result = subprocess.run([CLI, *args], env=env, input=stdin, capture_output=True,
                                text=True, timeout=20)
        assert (result.returncode == 0) == ok, (args, result.stdout, result.stderr)
        return result.stdout
    try:
        assert len(run("api", "list").splitlines()) == 231
        run("api", "schema", "org.tangled.temp.spindle.quota.set")
        run("knot", "version", knot)
        run("auth", "login", "--handle", "alice.test", "--password", "fixture")
        run("repo", "create", "demo", "--knot", knot, "--audience", "did:web:test.local")
        records["sh.tangled.repo", "another"] = dict(records["sh.tangled.repo", "demo"], name="another")
        assert len(json.loads(run("repo", "list", "--json"))) == 2
        fault = "repeat-cursor"
        run("repo", "list", ok=False)
        fault = "get-error"
        run("repo", "info", "at://did:plc:alice/sh.tangled.repo/demo", ok=False)
        fault = None
        run("repo", "info", REPO, "--knot", knot)
        records["sh.tangled.repo", "demo"]["futureField"] = {"preserve": True}
        run("repo", "spindle", "demo", "spindle.test", "--knot", knot)
        assert records["sh.tangled.repo", "demo"]["futureField"] == {"preserve": True}
        run("repo", "add-collaborator", "demo", "did:plc:bob", "--knot", knot,
            "--audience", "did:web:test.local")
        run("knot", "add-member", knot, "did:plc:bob", "--audience", "did:web:test.local")
        run("spindle", "add-member", "spindle.test", "did:plc:bob")
        run("spindle", "add-member", "spindle.test", "did:plc:bob")
        assert len(json.loads(run("spindle", "members", "spindle.test"))) == 1
        run("spindle", "remove-member", "spindle.test", "did:plc:bob")
        run("star", "add", REPO)
        run("star", "list")
        run("star", "remove", REPO)
        issue = json.loads(run("issue", "create", REPO, "--title", "CLI issue"))["uri"]
        run("issue", "close", issue)
        run("issue", "reopen", issue)
        run("record", "get", "sh.tangled.repo.issue", issue.split("/")[-1])
        run("record", "delete", "sh.tangled.repo.issue", issue.split("/")[-1])
        key_file = Path(directory) / "id.pub"
        key_file.write_text("ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMockFixtureOnly fixture")
        key_uri = json.loads(run("key", "add", str(key_file), "--name", "fixture"))["uri"]
        run("key", "list")
        run("key", "remove", key_uri.split("/")[-1])
        key_file.write_text("-----BEGIN OPENSSH PRIVATE KEY-----")
        before = len(calls)
        run("key", "add", str(key_file), "--name", "private", ok=False)
        assert len(calls) == before
        run("pipeline", "list", "--repo", REPO, "--spindle", spindle, "--json",
            "--commit", SHA, "--kind", "manual", "--limit", "1", "--cursor", "old")
        run("pipeline", "show", PIPELINE, "--spindle", spindle)
        run("pipeline", "trigger", "--repo", REPO, "--spindle", spindle,
            "--sha", SHA, "--input", "message=hello", "--audience", "did:web:test.local")
        run("pipeline", "retry", PIPELINE, "--spindle", spindle,
            "--audience", "did:web:test.local")
        assert calls[-1][3]["trigger"]["inputs"] == [{"key": "message", "value": "hello"}]
        assert calls[-1][3]["workflows"] == ["inspect"]
        run("pipeline", "cancel", PIPELINE, "--repo", REPO, "--spindle", spindle,
            "--audience", "did:web:test.local")
        assert run("pipeline", "logs", PIPELINE, "--spindle", spindle) == "partial\nmore"
        before = len(calls)
        run("api", "call", "sh.tangled.ci.queryPipelines", "--service", spindle,
            "-q", "repo=did:plc:repo", "-q", "limit=999", ok=False)
        assert len(calls) == before
        run("api", "call", "sh.tangled.ci.triggerPipeline", "--service", spindle,
            "--auth", "service", "--input", "-", stdin="{}", ok=False)
        assert len(calls) == before
        run("api", "call", "sh.tangled.knot.version", "--service", knot, "--auth", "pds", ok=False)
        run("repo", "delete", "demo", "--knot", knot, "--audience", "did:web:test.local", "--force")
        assert calls[-1][1] == "sh.tangled.repo.delete"
        for path in Path(directory).rglob("session.json"):
            saved = json.loads(path.read_text())
            saved["access_jwt"] = jwt(1)
            path.write_text(json.dumps(saved))
        before = len(calls)
        run("knot", "version", knot)
        run("pipeline", "show", PIPELINE, "--spindle", spindle)
        assert [call[1] for call in calls[before:]] == [
            "sh.tangled.knot.version", "sh.tangled.ci.getPipeline"]
        print("PASS: CLI discovery, pagination, errors, stable repo identity, safe updates, JWT scope, ACLs, CI and WebSocket logs")
    finally:
        for server in servers:
            server.shutdown()
            server.server_close()
