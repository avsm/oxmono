#!/usr/bin/env python3
"""Provision only synthetic data in the disposable Stalwart v0.16 fixture."""

import base64
import json
import sys
import time
import urllib.error
import urllib.request

BASE = "http://127.0.0.1:" + sys.argv[2]
AUTH = "Basic " + base64.b64encode(b"admin:imap-fixture-bootstrap").decode()
USING = ["urn:ietf:params:jmap:core", "urn:stalwart:jmap"]


def call(method, arguments):
    body = json.dumps({"using": USING, "methodCalls": [[method, arguments, "fixture"]]}).encode()
    request = urllib.request.Request(
        BASE + "/jmap/", data=body,
        headers={"Authorization": AUTH, "Content-Type": "application/json"},
    )
    for attempt in range(50):
        try:
            with urllib.request.urlopen(request, timeout=2) as response:
                result = json.load(response)
            break
        except (OSError, urllib.error.HTTPError):
            if attempt == 49:
                raise
            time.sleep(0.2)
    name, value, _ = result["methodResponses"][0]
    if name != method or "error" in value or value.get("notUpdated") or value.get("notCreated"):
        raise RuntimeError(f"{method} failed: {result}")
    return value


if sys.argv[1] == "bootstrap":
    singleton = call("x:Bootstrap/get", {})["list"][0]["id"]
    result = call("x:Bootstrap/set", {"update": {singleton: {
        "serverHostname": "mail.example.org",
        "defaultDomain": "example.org",
        "requestTlsCertificate": False,
        "generateDkimKeys": False,
    }}})
    if singleton not in result.get("updated", {}):
        raise RuntimeError("bootstrap update was not acknowledged")
elif sys.argv[1] == "account":
    domains = call("x:Domain/get", {})["list"]
    domain_id = next(row["id"] for row in domains if row["name"] == "example.org")
    result = call("x:Account/set", {"create": {"fixture": {
        "@type": "User",
        "name": "imap-test-user",
        "domainId": domain_id,
        "credentials": {"0": {"@type": "Password", "secret": "imap-test-password"}},
        "roles": {"@type": "User"},
        "permissions": {"@type": "Inherit"},
        "encryptionAtRest": {"@type": "Disabled"},
        "memberGroupIds": {}, "aliases": {}, "quotas": {},
    }}})
    if "fixture" not in result.get("created", {}):
        raise RuntimeError("fixture account was not created")
else:
    raise SystemExit("usage: provision.py bootstrap|account PORT")
