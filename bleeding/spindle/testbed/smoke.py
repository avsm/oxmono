#!/usr/bin/env python3
"""Dispatch a local PDS-authenticated job and read Tangled CBOR log frames."""

import base64
import hashlib
import json
import os
from pathlib import Path
import socket
import struct
import sys
import time
import urllib.error
import urllib.parse
import urllib.request


PDS = "http://oxmono-atp-pds-1:3000/xrpc/"
SPINDLE = "http://spindle.tangled.test:9000/xrpc/"
if "--lifecycle" in sys.argv or "--interrupted" in sys.argv:
    SPINDLE = "http://slow:9000/xrpc/"
REPO = "did:web:repo.tangled.test"
AUDIENCE = "did:web:spindle.tangled.test"
TRIGGER = "sh.tangled.ci.triggerPipeline"


def request(url, data=None, token=None, status=200, raw=None):
    headers = {"Content-Type": "application/json"}
    if token:
        headers["Authorization"] = "Bearer " + token
    body = raw if raw is not None else (
        None if data is None else json.dumps(data).encode())
    req = urllib.request.Request(url, body, headers)
    try:
        response = urllib.request.urlopen(req, timeout=10)
    except urllib.error.HTTPError as error:
        response = error
    with response:
        payload = response.read()
        assert response.status == status, (response.status, payload, status)
        return json.loads(payload) if payload else None


def get(method, **query):
    return request(SPINDLE + method + "?" + urllib.parse.urlencode(query))


def login(name):
    return request(PDS + "com.atproto.server.createSession", {
        "identifier": name + ".pds.tangled.test", "password": "local-password"})


def auth(session, method=TRIGGER, audience=AUDIENCE):
    query = urllib.parse.urlencode({"aud": audience, "lxm": method})
    return request(PDS + "com.atproto.server.getServiceAuth?" + query,
                   token=session["accessJwt"])["token"]


def wait_health():
    deadline = time.monotonic() + 30
    while True:
        try:
            return request(SPINDLE + "_health")
        except (OSError, urllib.error.URLError):
            if time.monotonic() >= deadline:
                raise
            time.sleep(0.1)


def wait_pipeline(pipeline):
    deadline = time.monotonic() + 30
    while True:
        result = get("sh.tangled.ci.getPipeline", pipeline=pipeline)
        if result["workflows"][0]["status"] not in ("pending", "running"):
            return result
        assert time.monotonic() < deadline, result
        time.sleep(0.05)


def cbor(data, offset=0):
    first = data[offset]
    offset += 1
    major, length = first >> 5, first & 31
    if length >= 24:
        size = {24: 1, 25: 2, 26: 4, 27: 8}[length]
        length = int.from_bytes(data[offset:offset + size], "big")
        offset += size
    if major == 0:
        return length, offset
    if major == 1:
        return -1 - length, offset
    if major == 3:
        return data[offset:offset + length].decode(), offset + length
    if major == 5:
        result = {}
        for _ in range(length):
            key, offset = cbor(data, offset)
            value, offset = cbor(data, offset)
            assert key not in result
            result[key] = value
        return result, offset
    raise AssertionError((major, length))


def read_exact(reader, length):
    data = reader.read(length)
    assert len(data) == length, "truncated frame"
    return data


def logs(pipeline):
    endpoint = urllib.parse.urlsplit(SPINDLE)
    sock = socket.create_connection((endpoint.hostname, endpoint.port), 10)
    sock.settimeout(20)
    key = base64.b64encode(os.urandom(16)).decode()
    path = "/xrpc/sh.tangled.ci.subscribePipelineLogs?pipeline=" + pipeline
    sock.sendall((f"GET {path} HTTP/1.1\r\nHost: {endpoint.netloc}\r\n"
                  "Connection: Upgrade\r\nUpgrade: websocket\r\n"
                  f"Sec-WebSocket-Key: {key}\r\n"
                  "Sec-WebSocket-Version: 13\r\n\r\n").encode())
    reader = sock.makefile("rb")
    try:
        assert reader.readline().split()[1] == b"101"
        headers = {}
        while True:
            line = reader.readline()
            if line == b"\r\n":
                break
            assert line, "truncated upgrade"
            name, value = line.decode().split(":", 1)
            headers[name.lower()] = value.strip()
        expected = base64.b64encode(hashlib.sha1(
            (key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").encode()).digest())
        assert headers["sec-websocket-accept"] == expected.decode()
        events = []
        frames = []
        while True:
            first, second = read_exact(reader, 2)
            assert first & 0x80 and not second & 0x80
            length = second & 127
            if length == 126:
                length = struct.unpack("!H", read_exact(reader, 2))[0]
            elif length == 127:
                length = struct.unpack("!Q", read_exact(reader, 8))[0]
            assert length <= 1024 * 1024
            payload = read_exact(reader, length)
            if first & 15 == 8:
                assert payload[:2] == struct.pack("!H", 1000)
                break
            assert first & 15 == 2
            header, offset = cbor(payload)
            event, offset = cbor(payload, offset)
            assert offset == len(payload)
            assert header["op"] == 1
            assert header["t"] in ("#control", "#data")
            assert event["workflow"] == "inspect"
            event["type"] = header["t"][1:]
            events.append(event)
            frames.append(payload.hex())
        return events, frames
    finally:
        reader.close()
        sock.close()


def main():
    wait_health()
    state = Path("/state")
    if "--interrupted" in sys.argv:
        pipeline = (state / "interrupted-run").read_text()
        result = wait_pipeline(pipeline)
        workflow = result["workflows"][0]
        assert workflow["status"] == "failed", result
        assert "restarted" in workflow["error"], result
        print("PASS: interrupted job marked failed after restart")
        return
    if "--lifecycle" in sys.argv:
        alice = login("alice")
        trigger_token = auth(alice)
        cancel_method = "sh.tangled.ci.cancelPipeline"
        cancel_token = auth(alice, cancel_method)
        body = {"repo": REPO, "trigger": {
            "$type": "sh.tangled.ci.trigger#manual",
            "sha": (state / "commit").read_text().strip()}}

        def sleeping_job():
            uri = request(SPINDLE + TRIGGER, body, trigger_token)["pipeline"]
            pipeline = uri.rsplit("/", 1)[1]
            deadline = time.monotonic() + 15
            path = state / "slow-data" / (pipeline + ".json")
            while True:
                stored = json.loads(path.read_text())
                if any(e.get("command") == "sleep 30" and
                       e.get("status") == "start" for e in stored["events"]):
                    return pipeline
                assert time.monotonic() < deadline, stored
                time.sleep(0.05)

        pipeline = sleeping_job()
        request(SPINDLE + cancel_method,
                {"repo": REPO, "pipeline": pipeline}, cancel_token)
        result = wait_pipeline(pipeline)
        assert result["workflows"][0]["status"] == "cancelled", result
        (state / "interrupted-run").write_text(sleeping_job())
        print("PASS: running child process cancelled through service-auth XRPC")
        return
    if "--recovery" in sys.argv:
        previous = json.loads((state / "last-run.json").read_text())
        result = wait_pipeline(previous["id"])
        assert result["workflows"][0]["status"] == "success", result
        events, _ = logs(previous["id"])
        assert events == previous["events"]
        print("PASS: pipeline state and complete CBOR logs survived restart")
        return

    alice = login("alice")
    assert get("sh.tangled.owner")["owner"] == alice["did"]
    token = auth(alice)
    sha = (state / "commit").read_text().strip()
    body = {"repo": REPO, "trigger": {
        "$type": "sh.tangled.ci.trigger#manual", "sha": sha,
        "ref": "refs/heads/main", "inputs": [
            {"key": "note", "value": "hello $(touch /tmp/should-not-exist)"}]}}

    request(SPINDLE + TRIGGER, body, status=401)
    request(SPINDLE + TRIGGER, status=405)
    request(SPINDLE + TRIGGER, body, auth(login("bob")), status=401)
    request(SPINDLE + TRIGGER, body,
            auth(alice, audience="did:web:wrong.tangled.test"), status=401)
    request(SPINDLE + TRIGGER, body,
            auth(alice, method="sh.tangled.ci.cancelPipeline"), status=401)
    parts = token.split(".")
    forged = bytearray(base64.urlsafe_b64decode(parts[2] + "=="))
    forged[0] ^= 1
    parts[2] = base64.urlsafe_b64encode(forged).decode().rstrip("=")
    request(SPINDLE + TRIGGER, body, ".".join(parts), status=401)
    request(SPINDLE + TRIGGER, token=token, raw=b'{"repo":1,"repo":2}',
            status=400)
    request(SPINDLE + TRIGGER, dict(body, workflows=["missing"]), token,
            status=400)
    request(SPINDLE + TRIGGER, dict(body, repo="did:web:unconfigured.test"),
            token, status=403)
    malformed = dict(body, trigger=dict(body["trigger"], sha="--help"))
    request(SPINDLE + TRIGGER, malformed, token, status=400)
    definition = get("sh.tangled.ci.describeWorkflowDefinition", repo=REPO,
                     sha=sha)
    assert definition == {"derived": False, "workflows": ["inspect"]}
    dispatched = request(SPINDLE + TRIGGER, body, token)
    pipeline = dispatched["pipeline"].rsplit("/", 1)[1]
    events, frames = logs(pipeline)
    result = wait_pipeline(pipeline)
    assert result["workflows"][0]["status"] == "success", result
    assert result["commit"] == sha
    text = "".join(e["content"] for e in events if e["type"] == "data")
    assert "spindle-marker.txt" in text, text
    echoed = next(json.loads(e["content"]) for e in events
                  if e["type"] == "data" and e["step"] == 1)
    assert echoed["request"] == body
    assert echoed["actor"] == alice["did"]
    assert token not in text
    starts = [e["step"] for e in events
              if e["type"] == "control" and e["status"] == "start"]
    ends = [e["step"] for e in events
            if e["type"] == "control" and e["status"] == "end"]
    assert starts == ends == [0, 1, 2]
    queried = get("sh.tangled.ci.queryPipelines", repo=REPO, commits=sha)
    assert len(queried["pipelines"]) == 1
    assert queried["pipelines"][0]["id"] == pipeline
    (state / "last-run.json").write_text(json.dumps(
        {"id": pipeline, "events": events, "frames": frames}, indent=2))
    bad = dict(body, trigger=dict(body["trigger"], sha="0" * 40))
    failed = request(SPINDLE + TRIGGER, bad, token)["pipeline"].rsplit("/", 1)[1]
    assert wait_pipeline(failed)["workflows"][0]["status"] == "failed"
    page = get("sh.tangled.ci.queryPipelines", repo=REPO, limit=1)
    assert "cursor" in page
    next_page = get("sh.tangled.ci.queryPipelines", repo=REPO, limit=1,
                    cursor=page["cursor"])
    assert next_page["pipelines"][0]["id"] != page["pipelines"][0]["id"]
    cancel_method = "sh.tangled.ci.cancelPipeline"
    cancellation = {"repo": REPO, "pipeline": pipeline}
    request(SPINDLE + cancel_method, cancellation, token, status=401)
    request(SPINDLE + cancel_method, cancellation, auth(alice, cancel_method))
    assert wait_pipeline(pipeline)["workflows"][0]["status"] == "success"
    print(f"PASS: authenticated {pipeline}, checkout, metadata, ls, CBOR logs")
    print("PASS: forged/unauthorized dispatches, invalid input, failed checkout,")
    print("      definition discovery, filtering and pagination")


if __name__ == "__main__":
    main()
