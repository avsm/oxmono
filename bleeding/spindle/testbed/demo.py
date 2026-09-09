#!/usr/bin/env python3
"""Run the inspection job and print its streamed output from the host."""

from pathlib import Path
import smoke


def main():
    smoke.PDS = "http://127.0.0.1:2583/xrpc/"
    smoke.SPINDLE = "http://127.0.0.1:9000/xrpc/"
    sha = (Path(__file__).parent / ".state/commit").read_text().strip()
    token = smoke.auth(smoke.login("alice"))
    result = smoke.request(smoke.SPINDLE + smoke.TRIGGER, {
        "repo": smoke.REPO, "trigger": {
            "$type": "sh.tangled.ci.trigger#manual", "sha": sha,
            "ref": "refs/heads/main", "inputs": [
                {"key": "message", "value": "Hello from OCaml spindle"}]}},
        token)
    pipeline = result["pipeline"].rsplit("/", 1)[1]
    print(result["pipeline"], flush=True)
    events, _ = smoke.logs(pipeline)
    for event in events:
        if event["type"] == "data":
            print(event["content"], end="")
    status = smoke.wait_pipeline(pipeline)["workflows"][0]["status"]
    print("Status:", status)
    if status != "success":
        raise SystemExit(1)


if __name__ == "__main__":
    main()
