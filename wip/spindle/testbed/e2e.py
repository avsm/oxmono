#!/usr/bin/env python3
"""Drive the Tangled CLI against knot2 and the OCaml spindle.

Runs in the client container of stack.yml. Every service is local: the PDS,
PLC directory, knot, its Jetstream and the spindle."""

import hashlib
import hmac
import http.cookiejar
import http.server
import json
import os
from pathlib import Path
import secrets
import shutil
import subprocess
import sys
import threading
import time
import urllib.parse
import urllib.request

KNOT = "knot.tangled.test"
# The gateway serves the knot's HTTPS name. SSH goes to the container.
KNOT_SSH = "knot"
SPINDLE = "spindle.tangled.test"
WORK = Path("/tmp/e2e")
HANDLES = {"alice": "alice.pds.tangled.test", "bob": "bob.pds.tangled.test"}
# The spindle reaches this container under this name for webhooks.
HOOKS = "http://hooks.tangled.test:8099/hook"
SECRET = "hunter2-e2e"
HOOK_KEY = "shh-e2e"
WORKFLOW = """\
when:
  - event: ["push", "pull_request"]
    branch: main
engine: nixery
environment:
  GREETING: hello
steps:
  - name: show the build
    command: |
      echo "token=$TOKEN greeting=$GREETING ref=$TANGLED_REF_NAME"
      printf '%s' "$TOKEN" | base64
"""
received = []


class Hook(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        body = self.rfile.read(int(self.headers["Content-Length"]))
        received.append((dict(self.headers), body))
        self.send_response(204)
        self.end_headers()

    def log_message(self, *_):
        pass


def hooks(event):
    return [(h, json.loads(b)) for h, b in received
            if h.get("X-Tangled-Event") == event]


def step(message):
    print(f"--- {message}", flush=True)


def user_env(user):
    return dict(os.environ, XDG_CONFIG_HOME=str(WORK / user / "config"),
                XDG_STATE_HOME=str(WORK / user / "state"),
                XDG_DATA_HOME=str(WORK / user / "data"),
                TANGLED_PASSWORD="local-password")


def tangled(*args, user="alice", ok=True, stdin=None, cwd=None):
    env = user_env(user)
    result = subprocess.run(["/test/tangled", *args], env=env, input=stdin,
                            capture_output=True, text=True, timeout=60,
                            cwd=cwd)
    if (result.returncode == 0) != ok:
        raise SystemExit(f"tangled {' '.join(args)} as {user}: "
                         f"exit {result.returncode}\n{result.stdout}{result.stderr}")
    return result.stdout


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args):
        return None


def oauth_login(user, handle):
    """Run tangled auth login --oauth and play the browser: sign in and
    consent through the PDS's authorization UI API, then follow the
    redirect to the CLI's loopback listener."""
    env = user_env(user)
    del env["TANGLED_PASSWORD"]
    cli = subprocess.Popen(
        ["/test/tangled", "auth", "login", "--oauth", "--loopback",
         "--no-browser", "--handle", handle], env=env, stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT, text=True)
    url = None
    for line in cli.stdout:
        if line.startswith("  https://"):
            url = line.strip()
            break
    if url is None:
        raise SystemExit("tangled printed no authorization URL")
    origin = "{0.scheme}://{0.netloc}".format(urllib.parse.urlsplit(url))
    jar = http.cookiejar.CookieJar()
    browser = urllib.request.build_opener(
        urllib.request.HTTPCookieProcessor(jar), NoRedirect)
    browser.open(urllib.request.Request(url, headers={
        "Sec-Fetch-Mode": "navigate", "Sec-Fetch-Site": "none",
        "Sec-Fetch-Dest": "document"})).read()
    csrf = next(c.value for c in jar if c.name == "csrf-token")

    def api(endpoint, body):
        request = urllib.request.Request(
            origin + "/@atproto/oauth-provider/~api" + endpoint,
            data=json.dumps(body).encode(), method="POST", headers={
                "Content-Type": "application/json", "Origin": origin,
                "Referer": url, "Sec-Fetch-Mode": "same-origin",
                "Sec-Fetch-Site": "same-origin", "x-csrf-token": csrf})
        return json.loads(browser.open(request).read())

    account = api("/sign-in", {"locale": "en", "username": handle,
                               "password": "local-password",
                               "remember": True})["account"]
    did = account.get("sub") or account["did"]
    # The UI sends back the scope it showed, which the client ID declares.
    client_id = urllib.parse.parse_qs(urllib.parse.urlsplit(url).query)[
        "client_id"][0]
    scope = urllib.parse.parse_qs(urllib.parse.urlsplit(client_id).query)[
        "scope"][0]
    redirect = api("/consent", {"did": did, "scope": scope})["url"]
    try:
        browser.open(urllib.request.Request(redirect, headers={
            "Referer": url, "Sec-Fetch-Mode": "navigate",
            "Sec-Fetch-Site": "same-origin", "Sec-Fetch-Dest": "document"}))
        raise SystemExit("authorization server did not redirect")
    except urllib.error.HTTPError as e:
        if e.code != 303:
            raise
        callback = e.headers["Location"]
    assert callback.startswith("http://127.0.0.1:"), callback
    urllib.request.urlopen(callback).read()
    output = cli.stdout.read()
    if cli.wait(timeout=60) != 0:
        raise SystemExit(f"tangled auth login --oauth: {output}")
    return output


def git(*args, cwd=None):
    result = subprocess.run(["git", *args], cwd=cwd, text=True,
                            capture_output=True)
    if result.returncode != 0:
        raise SystemExit(f"git {' '.join(args)}: {result.stderr}")
    return result.stdout.strip()


def wait(what, check, timeout=90):
    deadline = time.time() + timeout
    while time.time() < deadline:
        value = check()
        if value:
            return value
        time.sleep(1)
    raise SystemExit(f"timed out waiting for {what}")


def pipelines(repo, user="alice"):
    return json.loads(tangled("pipeline", "list", "--repo", repo, "--json",
                              user=user))["pipelines"]


def finished(repo, commit, trigger, user="alice"):
    def check():
        for p in pipelines(repo, user):
            if p["commit"] == commit and p["trigger"]["$type"] == trigger:
                states = {w["status"] for w in p["workflows"]}
                if states <= {"success", "failed", "timeout", "cancelled"}:
                    return p
        return None
    return wait(f"{trigger} pipeline at {commit}", check)


def main():
    shutil.rmtree(WORK, ignore_errors=True)
    WORK.mkdir()
    key = WORK / "id_ed25519"
    shutil.copyfile("/test/id_ed25519", key)
    key.chmod(0o600)
    os.environ["GIT_SSH_COMMAND"] = (
        f"ssh -i {key} -p 2222 -o IdentitiesOnly=yes "
        "-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null "
        "-o LogLevel=ERROR")
    os.environ.update(GIT_AUTHOR_NAME="E2E", GIT_AUTHOR_EMAIL="e2e@tangled.test",
                      GIT_COMMITTER_NAME="E2E",
                      GIT_COMMITTER_EMAIL="e2e@tangled.test")
    owner = Path("/atp/owner-did").read_text().strip()
    name = "e2e-" + secrets.token_hex(3)

    step("log alice in with OAuth and bob with a password")
    assert "Logged in as " + HANDLES["alice"] in oauth_login(
        "alice", HANDLES["alice"])
    assert "Login: OAuth via https://pds.tangled.test" in tangled(
        "auth", "status")
    tangled("auth", "login", "--pds", "https://pds.tangled.test",
            "--handle", HANDLES["bob"], user="bob")

    step("an expired OAuth token is refreshed and the rotation saved")
    saved = next((WORK / "alice" / "config").glob(
        "tangled/profiles/*/session.json"))
    session = json.loads(saved.read_text())
    session["oauth"]["expires_at"] = "2000-01-01T00:00:00Z"
    saved.write_text(json.dumps(session))
    tangled("repo", "list", "--json")
    refreshed = json.loads(saved.read_text())
    assert refreshed["refresh_jwt"] != session["refresh_jwt"]
    assert refreshed["oauth"]["expires_at"] > "2001"

    step("describe the services")
    assert json.loads(tangled("knot", "describe", KNOT))["did"] == \
        "did:web:" + KNOT
    assert json.loads(tangled("spindle", "describe", SPINDLE))["did"] == \
        "did:web:" + SPINDLE

    step("publish an SSH key")
    public = Path("/test/id_ed25519.pub").read_text().strip()
    keys = json.loads(tangled("key", "list"))
    if not any(k["value"]["publicKey"] == public for k in keys):
        tangled("key", "add", "/test/id_ed25519.pub", "--name", "e2e")

    step(f"create {name} on the knot with the spindle assigned")
    tangled("repo", "create", name, "--knot", KNOT, "--spindle", SPINDLE,
            "--description", "end-to-end test")
    info = tangled("repo", "info", name)
    did = next(line.split()[1] for line in info.splitlines()
               if line.startswith("DID:"))
    assert f"Spindle: did:web:{SPINDLE}" in info, info
    assert any(d["repo"] == did for d in json.loads(
        tangled("repo", "list", "--json")))
    assert json.loads(tangled("repo", "info", did, "--json"))[
        "declaration"]["owner"] == owner

    step("a secret, a signed webhook and a repository workflow")
    server = http.server.ThreadingHTTPServer(("0.0.0.0", 8099), Hook)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    tangled("pipeline", "secret", "set", "TOKEN", "-r", name, "--value", SECRET)
    assert tangled("pipeline", "secret", "list", "-r", name).split() == \
        ["TOKEN"]
    tangled("pipeline", "secret", "set", "1BAD", "-r", name, "--value", "x",
            ok=False)
    hook = tangled("repo", "webhook", "add", HOOKS, "-r", name, "--secret",
                   HOOK_KEY, "--event", "push", "--event",
                   "pull_request:created", "--event", "pull_request:closed",
                   "--event", "pull_request:reopened").split()[0]
    listed = json.loads(tangled("repo", "webhook", "list", "-r", name,
                                "--json"))
    assert [w["id"] for w in listed] == [hook], listed
    assert "secret" not in listed[0]

    step("clone over HTTPS and push a commit over SSH")
    checkout = WORK / name
    tangled("repo", "clone", name, str(checkout))
    (checkout / "README.md").write_text("Pushed by the end-to-end test.\n")
    (checkout / ".tangled/workflows").mkdir(parents=True)
    (checkout / ".tangled/workflows/build.yml").write_text(WORKFLOW)
    git("add", "README.md", ".tangled", cwd=checkout)
    git("commit", "-m", "First commit", cwd=checkout)
    git("push", f"ssh://git@{KNOT_SSH}/{did}", "HEAD:refs/heads/main",
        cwd=checkout)
    head = git("rev-parse", "HEAD", cwd=checkout)

    step("the knot notifies the spindle, which runs the push")
    pushed = finished(name, head, "org.tangled.event.push")
    assert pushed["repo"] == did
    assert {w["id"]: w["status"] for w in pushed["workflows"]} == {
        "inspect": "success", "build.yml": "success"}, pushed
    assert pushed["trigger"]["ref"] == "refs/heads/main"
    # The default executor describes each workflow instead of running it.
    log = tangled("pipeline", "logs", pushed["id"], "--repo", name)
    for shown in ("Workflow    build.yml", "TOKEN=<secret>", "GREETING=hello",
                  "TANGLED_REF_NAME=main", "engine: nixery",
                  'echo "token=$TOKEN greeting=$GREETING'):
        assert shown in log, (shown, log)
    assert SECRET not in log, log
    build = next(w for w in pushed["workflows"] if w["id"] == "build.yml")
    assert build["definition"]["source"]["path"] == \
        ".tangled/workflows/build.yml", build

    step("the push is delivered to the webhook, signed")
    wait("the push webhook", lambda: hooks("push"))
    headers, payload = hooks("push")[0]
    raw = next(b for h, b in received if h.get("X-Tangled-Event") == "push")
    assert headers["X-Tangled-Signature-256"] == "sha256=" + hmac.new(
        HOOK_KEY.encode(), raw, hashlib.sha256).hexdigest(), headers
    assert headers["X-Tangled-Hook-ID"] == hook
    assert payload["ref"] == "refs/heads/main" and payload["after"] == head
    assert payload["repository"]["full_name"] == f"{owner}/{name}", payload
    deliveries = json.loads(tangled("repo", "webhook", "deliveries", hook,
                                    "-r", name, "--json"))
    assert [d["success"] for d in deliveries] == [True], deliveries
    tangled("repo", "webhook", "redeliver", hook, deliveries[0]["id"],
            "-r", name)
    wait("the redelivery", lambda: len(hooks("push")) == 2)
    tangled("repo", "webhook", "edit", hook, "-r", name, "--disable")
    assert json.loads(tangled("repo", "webhook", "list", "-r", name,
                              "--json"))[0]["active"] is False
    tangled("repo", "webhook", "edit", hook, "-r", name, "--enable")
    shown = json.loads(tangled("pipeline", "show", pushed["id"], "--repo",
                               name, "--json"))
    assert shown["id"] == pushed["id"]

    step("manual runs, retry, cancellation and definitions")
    out = tangled("pipeline", "trigger", "--repo", name, "--commit", head,
                  "--ref", "refs/heads/main", "--input", "reason=e2e")
    manual = out.split()[0]
    run = finished(name, head, "org.tangled.ci.trigger.manual")
    assert run["id"] == manual
    assert run["trigger"]["inputs"] == [{"key": "reason", "value": "e2e"}]
    only = json.loads(tangled("pipeline", "list", "--repo", name, "--json",
                              "--trigger", "org.tangled.ci.trigger.manual"))
    assert [p["id"] for p in only["pipelines"]] == [manual], only
    retried = tangled("pipeline", "retry", pushed["id"], "--repo", name).strip()
    assert retried not in (pushed["id"], manual)
    wait("the retried pipeline", lambda: any(
        p["id"] == retried and p["workflows"][0]["status"] == "success"
        for p in pipelines(name)))
    tangled("pipeline", "cancel", retried, "--repo", name)
    tangled("pipeline", "approve", retried, "--repo", name, ok=False)
    definitions = json.loads(tangled("pipeline", "workflows", "--repo", name))
    assert [w["id"] for w in definitions["workflows"]] == [
        "inspect", "build.yml"], definitions

    step("collaborators may run CI, others may not")
    tangled("pipeline", "trigger", "--repo", f"{HANDLES['alice']}/{name}",
            "--commit", head, user="bob", ok=False)
    tangled("repo", "collaborator", "add", HANDLES["bob"], "-r", name)
    assert tangled("repo", "collaborator", "list", "-r", name).split()
    tangled("pipeline", "trigger", "--repo", f"{HANDLES['alice']}/{name}",
            "--commit", head, user="bob")
    tangled("repo", "collaborator", "remove", HANDLES["bob"], "-r", name)
    tangled("pipeline", "trigger", "--repo", f"{HANDLES['alice']}/{name}",
            "--commit", head, user="bob", ok=False)

    step("issues and a pull request revision")
    issue = json.loads(tangled("issue", "create", "-r", name, "--title",
                               "Tracked by the end-to-end test"))["uri"]
    number = issue.rsplit("/", 1)[1]
    tangled("issue", "close", "-r", name, number)
    assert json.loads(tangled("issue", "show", "-r", name, number))[
        "record"]["state"] == "closed"
    tangled("issue", "reopen", "-r", name, number)
    tangled("issue", "comment", "-r", name, number, "--body", "Noted.")
    shown = json.loads(tangled("issue", "show", "-r", name, number))
    assert [c["record"]["body"]["text"] for c in shown["comments"]] == \
        ["Noted."], shown
    tangled("repo", "label", "add", "-r", name, "Needs review", "#e11d48")
    tangled("issue", "edit", "-r", name, number, "--label", "needs review")
    assert json.loads(tangled("issue", "show", "-r", name, number))[
        "labels"] == ["Needs review"]
    assert number in tangled("issue", "list", "-r", name, "--label",
                             "needs-review")
    # Removing a label in use leaves the issue editable.
    tangled("repo", "label", "remove", "-r", name, "Needs review")
    tangled("issue", "close", "-r", name, number)
    tangled("issue", "reopen", "-r", name, number)
    assert json.loads(tangled("issue", "show", "-r", name, number))[
        "labels"] == []
    tangled("repo", "label", "add", "-r", name, "Needs review", ok=False)
    git("checkout", "-b", "feature", cwd=checkout)
    (checkout / "feature.txt").write_text("A change under review.\n")
    git("add", "feature.txt", cwd=checkout)
    git("commit", "-m", "Add a feature", cwd=checkout)
    git("push", f"ssh://git@{KNOT_SSH}/{did}", "HEAD:refs/heads/feature",
        cwd=checkout)
    feature = git("rev-parse", "HEAD", cwd=checkout)
    # In the clone, create proposes the current branch into the default one.
    pull = json.loads(tangled("pull", "create", "--title", "Feature",
                              cwd=checkout))["uri"].rsplit("/", 1)[1]
    assert pull in tangled("pull", "list", cwd=checkout)
    assert pull not in tangled("issue", "list", "-r", name)
    assert "+A change under review." in tangled("pull", "diff", pull, "-r", name)
    reviewed = finished(name, feature, "org.tangled.event.pullRequest")
    assert reviewed["trigger"]["targetBranch"] == "main", reviewed
    assert reviewed["trigger"]["pull"].endswith("/" + pull), reviewed
    assert {w["status"] for w in reviewed["workflows"]} == {"success"}, reviewed
    assert "TOKEN=<secret>" in tangled("pipeline", "logs", reviewed["id"],
                                       "--repo", name)
    wait("the pull request webhook", lambda: hooks("pull_request:created"))
    created = hooks("pull_request:created")[0][1]
    assert created["action"] == "created", created
    assert created["pull_request"]["target_branch"] == "main", created

    step("closing and reopening a pull request notifies and reruns")
    tangled("pull", "close", pull, "-r", name)
    wait("the closed webhook", lambda: hooks("pull_request:closed"))
    time.sleep(3)
    # Nothing here asks to run on a closed pull request.
    assert not [p for p in pipelines(name)
                if p["trigger"].get("action") == "closed"]
    tangled("pull", "reopen", pull, "-r", name)
    wait("the reopened webhook", lambda: hooks("pull_request:reopened"))
    wait("the reopened pull request's pipeline", lambda: [
        p for p in pipelines(name)
        if p["trigger"].get("action") == "reopened"])

    step("a revision from someone without push access waits for approval")
    git("checkout", "-b", "outside", cwd=checkout)
    (checkout / "outside.txt").write_text("From a contributor.\n")
    git("add", "outside.txt", cwd=checkout)
    git("commit", "-m", "Contribute", cwd=checkout)
    git("push", f"ssh://git@{KNOT_SSH}/{did}", "HEAD:refs/heads/outside",
        cwd=checkout)
    outside = git("rev-parse", "HEAD", cwd=checkout)
    target = f"{HANDLES['alice']}/{name}"
    tangled("pull", "create", "-r", target, "--title", "Outside", "--target",
            "main", "--base", head, "--head", outside, "--branch", "outside",
            user="bob")

    def blocked():
        for p in pipelines(name):
            if (p["commit"] == outside and p["trigger"]["$type"]
                    == "org.tangled.event.pullRequest"):
                return p
        return None
    waiting = wait("the contributed revision", blocked)
    assert {w["status"] for w in waiting["workflows"]} == {"blocked"}, waiting
    tangled("pipeline", "approve", waiting["id"], "--repo", target,
            user="bob", ok=False)
    tangled("pipeline", "approve", waiting["id"], "--repo", name)
    wait("the approved revision", lambda: any(
        p["id"] == waiting["id"]
        and {w["status"] for w in p["workflows"]} == {"success"}
        for p in pipelines(name)))
    # An approved contribution gets no secrets.
    approved = tangled("pipeline", "logs", waiting["id"], "--repo", name)
    assert "GREETING=hello" in approved, approved
    assert "TOKEN=<secret>" not in approved, approved

    step("a closed spindle serves allow-listed owners only")
    bob_key = WORK / "bob_ed25519"
    subprocess.run(["ssh-keygen", "-t", "ed25519", "-N", "", "-q", "-f",
                    str(bob_key)], check=True)
    tangled("key", "add", str(bob_key) + ".pub", "--name", "e2e-bob",
            user="bob")
    bob_ssh = os.environ["GIT_SSH_COMMAND"].replace(str(key), str(bob_key))
    theirs = "e2e-bob-" + secrets.token_hex(3)
    tangled("repo", "create", theirs, "--knot", KNOT, "--spindle", SPINDLE,
            user="bob")
    their_did = next(line.split()[1] for line in tangled(
        "repo", "info", theirs, user="bob").splitlines()
        if line.startswith("DID:"))

    def push_as_bob(message):
        (checkout / "bob.txt").write_text(message + "\n")
        git("add", "bob.txt", cwd=checkout)
        git("commit", "-m", message, cwd=checkout)
        env = dict(os.environ, GIT_SSH_COMMAND=bob_ssh)

        # The knot may not yet accept a key or repository created moments
        # ago.
        def push():
            result = subprocess.run(
                ["git", "push", "-f", f"ssh://git@{KNOT_SSH}/{their_did}",
                 "HEAD:refs/heads/main"], cwd=checkout, env=env,
                text=True, capture_output=True)
            push.error = result.stderr
            return result.returncode == 0
        try:
            wait("bob's push", push, timeout=30)
        except SystemExit:
            raise SystemExit(f"bob's push failed: {push.error}")
        return git("rev-parse", "HEAD", cwd=checkout)

    # The stack keeps its state between runs, so start bob unadmitted.
    tangled("spindle", "member", "remove", SPINDLE, HANDLES["bob"])
    refused = push_as_bob("Before admission")
    time.sleep(5)
    # An unadmitted owner's repository is not served, so the spindle knows it
    # as RepoNotFound, as the reference spindle does.
    try:
        assert not [p for p in pipelines(theirs, "bob")
                    if p["commit"] == refused]
    except SystemExit as error:
        assert "RepoNotFound" in str(error), error
    tangled("spindle", "member", "add", SPINDLE, HANDLES["bob"])
    members = json.loads(tangled("spindle", "member", "list", SPINDLE))["dids"]
    assert len(members) == 1, members
    tangled("spindle", "member", "list", SPINDLE, user="bob", ok=False)
    admitted = push_as_bob("After admission")
    finished(theirs, admitted, "org.tangled.event.push", user="bob")
    tangled("spindle", "member", "remove", SPINDLE, HANDLES["bob"])
    assert json.loads(tangled("spindle", "member", "list", SPINDLE))[
        "dids"] == []

    step("invite codes, quotas and bans")
    code = tangled("spindle", "invite", SPINDLE, "--for",
                   HANDLES["bob"]).strip()
    tangled("spindle", "invite", SPINDLE, user="bob", ok=False)
    tangled("spindle", "join", SPINDLE, code, user="bob")
    tangled("spindle", "join", SPINDLE, code, user="bob", ok=False)
    assert len(json.loads(tangled("spindle", "member", "list", SPINDLE))[
        "dids"]) == 1
    invited = push_as_bob("After an invitation")
    finished(theirs, invited, "org.tangled.event.push", user="bob")
    tangled("spindle", "quota", "set", SPINDLE, f"{HANDLES['bob']}/{theirs}",
            "workflows", "1")
    limits = json.loads(tangled("spindle", "quota", "list", SPINDLE))
    assert limits["limits"] == [{"did": their_did, "resource": "workflows",
                                 "limit": 1}], limits
    usage = json.loads(tangled("spindle", "quota", "usage", SPINDLE))
    assert any(u["did"] == their_did and u["resource"] ==
               "ci_minutes_monthly" for u in usage["usages"]), usage
    tangled("spindle", "quota", "unset", SPINDLE, their_did, "workflows")
    tangled("spindle", "ban", "add", SPINDLE, HANDLES["bob"])
    bans = json.loads(tangled("spindle", "ban", "list", SPINDLE))
    assert [b["did"] for b in bans["bans"]] != [], bans
    # A ban wipes the owner's pipelines and runs nothing new.
    assert pipelines(theirs, "bob") == []
    banned = push_as_bob("While banned")
    time.sleep(5)
    assert pipelines(theirs, "bob") == []
    tangled("pipeline", "list", "--repo", theirs, user="bob")
    bob_did = json.loads(tangled("spindle", "ban", "list", SPINDLE))[
        "bans"][0]["did"]
    tangled("spindle", "ban", "remove", SPINDLE, bob_did)
    tangled("spindle", "ban", "remove", SPINDLE, bob_did, ok=False)
    tangled("spindle", "member", "remove", SPINDLE, HANDLES["bob"])
    assert banned

    step("skip-ci suppresses a push")
    git("checkout", "--detach", head, cwd=checkout)
    (checkout / "skipped.txt").write_text("No CI for this.\n")
    git("add", "skipped.txt", cwd=checkout)
    git("commit", "-m", "Skip CI", cwd=checkout)
    git("push", "-o", "skip-ci", f"ssh://git@{KNOT_SSH}/{did}",
        "HEAD:refs/heads/main", cwd=checkout)
    skipped = git("rev-parse", "HEAD", cwd=checkout)
    time.sleep(5)
    assert not [p for p in pipelines(name) if p["commit"] == skipped]

    step("delete the repositories")
    tangled("repo", "delete", name, "--force")
    tangled("repo", "delete", theirs, "--force", user="bob")
    tangled("repo", "info", name, ok=False)

    server.shutdown()
    print("PASS: knot2 and the OCaml spindle: pushes, repository workflows, "
          "secrets, webhooks, manual runs, retry, collaborators, admission, "
          "invites, quotas, bans, issues, labels, pull requests and their "
          "lifecycle, approval and deletion", flush=True)


if __name__ == "__main__":
    main()
