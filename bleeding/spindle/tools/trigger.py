#!/usr/bin/env python3
"""Request a PDS service JWT and dispatch one manual spindle inspection."""

import argparse
import getpass
import json
import os
import urllib.error
import urllib.parse
import urllib.request


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def request(url, *, body=None, token=None):
    headers = {"Content-Type": "application/json"}
    if token:
        headers["Authorization"] = "Bearer " + token
    data = None if body is None else json.dumps(body).encode()
    req = urllib.request.Request(url, data=data, headers=headers)
    opener = urllib.request.build_opener(NoRedirect)
    with opener.open(req, timeout=30) as response:
        return json.load(response)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pds", required=True, help="Owner's PDS origin")
    parser.add_argument("--spindle", required=True, help="Spindle HTTP(S) origin")
    parser.add_argument("--hostname", required=True,
                        help="Server's configured did:web hostname")
    parser.add_argument("--identifier", required=True, help="Owner handle or DID")
    parser.add_argument("--repo", required=True, help="Configured repository DID")
    parser.add_argument("--sha", required=True, help="Full Git commit SHA")
    parser.add_argument("--ref", default="refs/heads/main")
    parser.add_argument("--password-env",
                        help="Read app password from this environment variable")
    args = parser.parse_args()
    for origin in (args.pds, args.spindle):
        url = urllib.parse.urlsplit(origin)
        if (url.scheme not in ("http", "https") or not url.netloc
                or url.username is not None or url.password is not None
                or url.path not in ("", "/") or url.query or url.fragment):
            parser.error("PDS and spindle URLs must be HTTP(S) origins")
    password = (os.environ.pop(args.password_env) if args.password_env
                else getpass.getpass("PDS app password: "))
    pds = args.pds.rstrip("/") + "/xrpc/"
    spindle = args.spindle.rstrip("/") + "/xrpc/"
    session = request(pds + "com.atproto.server.createSession", body={
        "identifier": args.identifier, "password": password})
    method = "sh.tangled.ci.triggerPipeline"
    query = urllib.parse.urlencode({
        "aud": "did:web:" + args.hostname, "lxm": method})
    auth = request(pds + "com.atproto.server.getServiceAuth?" + query,
                   token=session["accessJwt"])
    result = request(spindle + method, token=auth["token"], body={
        "repo": args.repo,
        "trigger": {"$type": "sh.tangled.ci.trigger#manual",
                    "sha": args.sha, "ref": args.ref}})
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    try:
        main()
    except urllib.error.HTTPError as error:
        raise SystemExit(f"Request failed: HTTP {error.code}") from None
    except (urllib.error.URLError, TimeoutError) as error:
        raise SystemExit(f"Connection failed: {error}") from None
