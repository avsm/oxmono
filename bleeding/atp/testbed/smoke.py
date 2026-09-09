#!/usr/bin/env python3
"""Exercise the real local PDS, PLC, HTTPS discovery and Jetstream."""

import base64
import datetime
import hashlib
import json
import os
from pathlib import Path
import socket
import ssl
import struct
import time
import urllib.error
import urllib.request


TLS = ssl.create_default_context(cafile="/certs/ca.crt")
PDS = "https://pds.tangled.test"
PASSWORD = "local-password"


def request(url, data=None, token=None):
    headers = {"Content-Type": "application/json"}
    if token:
        headers["Authorization"] = "Bearer " + token
    req = urllib.request.Request(
        url, data=None if data is None else json.dumps(data).encode(),
        headers=headers)
    with urllib.request.urlopen(req, context=TLS, timeout=15) as response:
        return json.load(response)


def rpc(method, data, token=None):
    return request(PDS + "/xrpc/" + method, data, token)


def account(name):
    handle = name + ".pds.tangled.test"
    try:
        rpc("com.atproto.server.createAccount", {
            "handle": handle, "email": name + "@tangled.test", "password": PASSWORD})
    except urllib.error.HTTPError as error:
        body = json.load(error)
        taken = body.get("error") == "HandleNotAvailable" or (
            body.get("error") == "InvalidRequest" and
            body.get("message") == "Handle already taken: " + handle)
        if not taken:
            raise RuntimeError(f"create {handle}: {body}") from error
    session = rpc("com.atproto.server.createSession",
                  {"identifier": handle, "password": PASSWORD})
    did = session["did"]
    document = request("https://plc.tangled.test/" + did)
    assert document["id"] == did
    assert any(s["serviceEndpoint"] == PDS for s in document["service"])
    with urllib.request.urlopen("https://" + handle + "/.well-known/atproto-did",
                                context=TLS, timeout=10) as response:
        assert response.read().decode().strip() == did
    print(f"{name}: {did} (PLC and HTTPS handle discovery verified)", flush=True)
    return session


def open_stream():
    raw = socket.create_connection(("jetstream.tangled.test", 443), 10)
    sock = TLS.wrap_socket(raw, server_hostname="jetstream.tangled.test")
    sock.settimeout(20)
    key = base64.b64encode(os.urandom(16)).decode()
    path = "/subscribe?wantedCollections=sh.tangled.repo&cursor=" + str(
        int((time.time() - 10) * 1_000_000))
    sock.sendall((f"GET {path} HTTP/1.1\r\nHost: jetstream.tangled.test\r\n"
                  "Connection: Upgrade\r\nUpgrade: websocket\r\n"
                  f"Sec-WebSocket-Key: {key}\r\nSec-WebSocket-Version: 13\r\n\r\n").encode())
    reader = sock.makefile("rb")
    assert reader.readline().split()[1] == b"101"
    headers = {}
    while True:
        line = reader.readline()
        if line == b"\r\n":
            break
        if not line:
            raise EOFError("truncated WebSocket handshake")
        name, value = line.decode().split(":", 1)
        headers[name.lower()] = value.strip()
    accept = base64.b64encode(hashlib.sha1(
        (key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").encode()).digest()).decode()
    assert headers["sec-websocket-accept"] == accept
    return sock, reader


def control(sock, opcode, payload=b""):
    mask = os.urandom(4)
    sock.sendall(bytes([128 | opcode, 128 | len(payload)]) + mask +
                 bytes(b ^ mask[i % 4] for i, b in enumerate(payload)))


def event(sock, reader):
    message = bytearray()
    while True:
        header = reader.read(2)
        if len(header) != 2:
            raise EOFError("Jetstream ended")
        first, second = header
        assert not first & 112 and not second & 128
        length = second & 127
        if length == 126:
            length = struct.unpack("!H", reader.read(2))[0]
        elif length == 127:
            length = struct.unpack("!Q", reader.read(8))[0]
        assert length <= 16 * 1024 * 1024 - len(message)
        payload = reader.read(length)
        assert len(payload) == length
        opcode = first & 15
        if opcode == 9:
            control(sock, 10, payload)
        elif opcode == 10:
            continue
        elif opcode == 8:
            raise EOFError("Jetstream closed")
        else:
            assert opcode in (0, 1)
            message.extend(payload)
            if first & 128:
                return json.loads(message)


def main():
    alice = account("alice")
    account("bob")
    Path("/state/owner-did").write_text(alice["did"])
    service_auth = request(
        PDS + "/xrpc/com.atproto.server.getServiceAuth"
        "?aud=did%3Aweb%3Aspindle.tangled.test&lxm=sh.tangled.ci.triggerPipeline",
        token=alice["accessJwt"])
    # Record the algorithm the real PDS requires a spindle to verify. This is
    # inspection of an issued token, not a substitute signature verifier.
    header = service_auth["token"].split(".")[0]
    algorithm = json.loads(base64.urlsafe_b64decode(header + "=" * (-len(header) % 4)))["alg"]
    print("Local PDS service-auth signing algorithm: " + algorithm, flush=True)
    sock, reader = open_stream()
    try:
        created = datetime.datetime.now(datetime.timezone.utc).isoformat()
        record = {"$type": "sh.tangled.repo", "name": "local-smoke",
                  "knot": "knot.tangled.test", "spindle": "spindle.tangled.test",
                  "createdAt": created}
        result = rpc("com.atproto.repo.putRecord", {
            "repo": alice["did"], "collection": "sh.tangled.repo",
            "rkey": "local-smoke", "record": record, "validate": False},
            alice["accessJwt"])
        deadline = time.monotonic() + 30
        while time.monotonic() < deadline:
            item = event(sock, reader)
            commit = item.get("commit", {})
            if (item.get("did") == alice["did"] and
                    commit.get("collection") == "sh.tangled.repo" and
                    commit.get("rkey") == "local-smoke" and
                    commit.get("record", {}).get("createdAt") == created):
                print("PASS: local record write reached Jetstream: " + result["uri"])
                return
        raise TimeoutError("local record did not reach Jetstream")
    finally:
        try:
            control(sock, 8)
        finally:
            reader.close()
            sock.close()


if __name__ == "__main__":
    main()
