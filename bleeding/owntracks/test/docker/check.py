#!/usr/bin/env python3
"""Exercise the compiled CLI against Mosquitto and a Recorder HTTP fixture."""
import base64
import http.server
import json
import os
from pathlib import Path
import subprocess
import tempfile
import threading
import urllib.parse

ROOT = Path.cwd()
CLI = ROOT / "_build/default/bleeding/owntracks/bin/main.exe"
COMPOSE = ["docker", "compose", "-p", os.environ["MQTTZ_COMPOSE_PROJECT"],
           "-f", os.environ["MQTTZ_COMPOSE_FILE"], "exec", "-T", "broker"]


def run(*args, ok=True, env=None):
    result = subprocess.run([str(arg) for arg in args], capture_output=True,
                            text=True, timeout=20, env=env)
    if (result.returncode == 0) != ok:
        raise AssertionError(f"{args}: exit {result.returncode}\n{result.stderr}")
    return result


def publish(topic, message):
    run(*COMPOSE, "mosquitto_pub", "-V", "mqttv5", "-q", "1", "-r",
        "-t", topic, "-m", json.dumps(message))


class Recorder(http.server.BaseHTTPRequestHandler):
    seen = []

    def log_message(self, *_args):
        pass

    def do_GET(self):
        parsed = urllib.parse.urlsplit(self.path)
        query = urllib.parse.parse_qs(parsed.query)
        self.seen.append((parsed.path, query, self.headers.get("Authorization")))
        status = 200
        if parsed.path == "/denied/api/0/list":
            status, body = 403, {"error": "denied"}
        elif parsed.path == "/bad/api/0/list":
            body = {"error": "unexpected response"}
        elif parsed.path == "/api/0/list":
            body = {"results": ["phone"] if "user" in query else ["alice"]}
        elif parsed.path == "/api/0/locations":
            body = {"data": [{"lat": 51.5, "lon": 2, "tst": 2},
                             {"lat": 51.5, "lon": 1, "tst": 1}]}
        else:
            status, body = 404, {}
        data = json.dumps(body).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        # Split at arbitrary boundaries to exercise streaming decoding.
        try:
            for byte in data:
                self.wfile.write(bytes([byte]))
        except (BrokenPipeError, ConnectionResetError):
            # Error responses may be discarded without reading their body.
            pass


print(run(ROOT / "_build/default/bleeding/owntracks/test/integration.exe").stdout, end="")
with tempfile.TemporaryDirectory(prefix="owntracks-test-") as tmp:
    config = Path(tmp) / "owntracks.toml"
    run(CLI, "init", "--config", config)
    assert config.stat().st_mode & 0o777 == 0o600
    run(CLI, "init", "--config", config, ok=False)
    # --help does not read or validate an application's configuration.
    config.write_text("this is invalid TOML")
    run(CLI, "listen", "--config", config, "--help=plain")
    run(CLI, "devices", "--config", config, ok=False)
    run(CLI, "init", "--config", config, "--force")
    config.write_text(config.read_text() + f'port = {os.environ["MQTTZ_PORT"]}\n')
    common = ["--config", str(config)]
    first = {"_type": "location", "lat": 51.5, "lon": -0.1, "tst": 1,
             "topic": "spoof"}
    publish("owntracks/cli/phone", first)
    result = run(CLI, "geojson", *common, "-t", "owntracks/cli/phone", "--duration", "2")
    point = json.loads(result.stdout)
    assert point["geometry"]["coordinates"] == [-0.1, 51.5]
    result = run(CLI, "listen", *common, "-t", "owntracks/cli/phone", "--count", "1")
    assert "owntracks/cli/phone" in result.stdout and "spoof" not in result.stdout
    # The same device ID under different users must stay in separate tracks.
    publish("owntracks/other/phone", {**first, "lon": 2})
    result = run(CLI, "geojson", *common, "--track", "--duration", "0.3")
    tracks = json.loads(result.stdout)
    assert tracks["type"] == "FeatureCollection" and len(tracks["features"]) == 2
    assert all(f["geometry"]["type"] == "Point" for f in tracks["features"])
    run(CLI, "geojson", *common, "-t", "owntracks/absent/#", "--duration", "0.05", ok=False)
    run(CLI, "geojson", *common, "--track", "--max-points", "1", "--duration", "1", ok=False)
    run(CLI, "geojson", *common, "--duration", "nan", ok=False)
    run(CLI, "geojson", *common, "--from", "2026-02-30", ok=False)
    no_home = {key: val for key, val in os.environ.items() if key != "HOME"}
    run(CLI, "devices", *common, env=no_home)

    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Recorder)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        url = f"http://127.0.0.1:{server.server_port}"
        result = run(CLI, "recorder", *common, "--recorder-url", url)
        assert result.stdout.strip() == "alice"
        run(CLI, "recorder", *common, "--recorder-url", url, "--user", "a&b",
            "--recorder-user", "api", "--recorder-password", "secret")
        assert Recorder.seen[-1][1] == {"user": ["a&b"]}
        assert Recorder.seen[-1][2] == "Basic " + base64.b64encode(b"api:secret").decode()
        result = run(CLI, "geojson", *common, "--recorder-url", url,
                     "--from", "2026-01-01", "--to", "2026-01-02",
                     "--user", "a&b", "--device", "phone/one")
        line = json.loads(result.stdout)
        assert line["geometry"]["type"] == "LineString"
        assert line["geometry"]["coordinates"] == [[1, 51.5], [2, 51.5]]
        assert Recorder.seen[-1][1]["device"] == ["phone/one"]
        run(CLI, "recorder", *common, "--recorder-url", url + "/denied", ok=False)
        run(CLI, "recorder", *common, "--recorder-url", url + "/bad", ok=False)
    finally:
        server.shutdown()
        server.server_close()
        thread.join()
print("PASS OwnTracks CLI: config, retained messages, per-user tracks, bounds and Recorder HTTP")
