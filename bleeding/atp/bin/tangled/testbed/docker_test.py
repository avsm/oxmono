#!/usr/bin/env python3
import json
import os
from pathlib import Path
import subprocess
import tempfile
import time

KNOT = "https://knot.tangled.test"
SPINDLE = "https://spindle.tangled.test"
ALICE = "did:plc:m6vusx6sxocpt245rrds6y3y"
BOB = "did:plc:gerqe7lcsdfod2orsksexdio"

with tempfile.TemporaryDirectory() as directory:
    env = dict(os.environ, XDG_CONFIG_HOME=directory, XDG_STATE_HOME=directory, XDG_DATA_HOME=directory, XDG_CACHE_HOME=directory,
               TANGLED_PDS="https://pds.tangled.test",
               GIT_SSL_CAINFO="/certs/ca.crt",
               GIT_SSH_COMMAND="ssh -i /test/id_ed25519 -o IdentitiesOnly=yes "
                               "-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null")

    def cli(*args, ok=True):
        result = subprocess.run(["/test/tangled", *args], env=env, capture_output=True,
                                text=True, timeout=60)
        if (result.returncode == 0) != ok:
            raise AssertionError((args, result.stdout, result.stderr))
        return result.stdout

    def eventually(f):
        deadline = time.monotonic() + 60
        while True:
            try:
                value = f()
                if value:
                    return value
            except (AssertionError, subprocess.CalledProcessError):
                if time.monotonic() >= deadline:
                    raise
            if time.monotonic() >= deadline:
                raise AssertionError("Timed out waiting for local observer")
            time.sleep(0.3)

    cli("knot", "version", KNOT)
    cli("auth", "login", "--handle", "alice.pds.tangled.test", "--password", "local-password")
    name = "cli-" + str(time.time_ns())
    created = cli("repo", "create", name, "--knot", KNOT)
    repo = next(line.removeprefix("Repository DID: ") for line in created.splitlines()
                if line.startswith("Repository DID: "))
    record_uri = next(line.removeprefix("AT URI: ") for line in created.splitlines()
                      if line.startswith("AT URI: "))
    assert json.loads(cli("repo", "info", repo, "--knot", KNOT))["repoDid"] == repo
    cli("repo", "spindle", record_uri, SPINDLE)
    print("PASS: CLI creates repo DID, publishes PDS record and assigns spindle", flush=True)

    cli("repo", "add-collaborator", record_uri, BOB)
    members = json.loads(cli("repo", "collaborators", record_uri))["items"]
    assert any(m["subject"] == BOB for m in members)
    cli("repo", "remove-collaborator", record_uri, BOB)
    assert not any(m["subject"] == BOB for m in json.loads(
        cli("repo", "collaborators", record_uri))["items"])
    # The fixture already grants Bob spindle access. Leave its pre-existing grants alone.
    members = json.loads(cli("spindle", "members", SPINDLE))
    new_subject = "did:web:cli-member.tangled.test"
    cli("spindle", "add-member", SPINDLE, new_subject)
    assert any(m["subject"] == new_subject for m in json.loads(cli("spindle", "members", SPINDLE)))
    cli("spindle", "remove-member", SPINDLE, new_subject)
    cli("star", "add", repo)
    assert any(s["subject"].get("did") == repo for s in json.loads(cli("star", "list")))
    cli("star", "remove", repo)
    issue = json.loads(cli("issue", "create", repo, "--title", "CLI integration issue"))["uri"]
    cli("issue", "close", issue)
    cli("record", "get", "sh.tangled.repo.issue", issue.split("/")[-1])
    cli("record", "delete", "sh.tangled.repo.issue", issue.split("/")[-1])
    print("PASS: knot collaborator JWTs, owner-PDS spindle membership and stars", flush=True)

    fixture = Path(directory) / "fixture"
    subprocess.run(["git", "clone", "/fixture", str(fixture)], env=env,
                   capture_output=True, check=True)
    sha = subprocess.check_output(["git", "-C", str(fixture), "rev-parse", "HEAD"], text=True).strip()
    eventually(lambda: subprocess.run(
        ["git", "-C", str(fixture), "push", "ssh://git@knot:22/" + repo, "HEAD:refs/heads/main"],
        env=env, capture_output=True, check=True))
    cloned = Path(directory) / "cloned"
    cli("repo", "clone", record_uri, str(cloned))
    assert subprocess.check_output(["git", "-C", str(cloned), "rev-parse", "HEAD"], text=True).strip() == sha

    def query():
        return json.loads(cli("pipeline", "list", "--repo", repo, "--spindle", SPINDLE, "--json"))["pipelines"]
    eventually(query)
    triggered = json.loads(cli("pipeline", "trigger", "--repo", repo, "--spindle", SPINDLE,
                              "--sha", sha, "--workflow", "inspect",
                              "--input", "source=tangled-cli"))["pipeline"].split("/")[-1]
    def completed(pipeline):
        value = json.loads(cli("pipeline", "show", pipeline, "--spindle", SPINDLE, "--json"))
        if all(w["status"] not in ("pending", "running") for w in value["workflows"]):
            assert all(w["status"] == "success" for w in value["workflows"]), value
            return value
    eventually(lambda: completed(triggered))
    output = cli("pipeline", "logs", triggered, "--spindle", SPINDLE)
    assert "README" in output and repo in output and ALICE in output
    for _ in range(10):
        assert cli("pipeline", "logs", triggered, "--spindle", SPINDLE) == output
    retried = json.loads(cli("pipeline", "retry", triggered, "--spindle", SPINDLE))["pipeline"].split("/")[-1]
    eventually(lambda: completed(retried))
    cli("pipeline", "cancel", retried, "--repo", repo, "--spindle", SPINDLE)
    cli("pipeline", "definition", "--repo", repo, "--spindle", SPINDLE, "--sha", sha)
    print("PASS: Git clone, push CI, manual JWT trigger, metadata/log stream, retry and cancel", flush=True)

    cli("api", "call", "sh.tangled.ci.queryPipelines", "--service", SPINDLE, "-q", "repo=" + repo)
    cli("repo", "delete", record_uri, "--force")
    cli("record", "get", "sh.tangled.repo", name, ok=False)
    print("PASS: generic API call and PDS-before-knot repository deletion", flush=True)
