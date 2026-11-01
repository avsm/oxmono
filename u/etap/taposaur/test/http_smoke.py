"""Exercise the CLI and real HTTP backend against synthetic loopback responses."""
import http.server
import json
import pathlib
import subprocess
import tempfile
import threading

BINARY = pathlib.Path(__file__).resolve().parents[4] / "_build/default/u/etap/taposaur/bin/main.exe"
PDF = b"%PDF-1.4\n0123456789\n%%EOF\n"


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        body = PDF if self.path in ("/valid", "/truncated") else b"<html>login</html>"
        self.send_response(200)
        self.send_header("Content-Type", "application/pdf")
        length = len(body) + (100 if self.path == "/truncated" else 0)
        self.send_header("Content-Length", str(length))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, *_args):
        pass


def main():
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        with tempfile.TemporaryDirectory(prefix="taposaur-http-smoke-") as tmp:
            base = [str(BINARY), "fetch", "10.1000/http-smoke", "--staging", tmp,
                    "--timeout", "5", "--url"]
            url = f"http://127.0.0.1:{server.server_port}"
            for endpoint, expected in [("/valid", 0), ("/valid", 0),
                                       ("/invalid", 1), ("/truncated", 1)]:
                result = subprocess.run(base + [url + endpoint], text=True,
                                        capture_output=True, timeout=15)
                print(endpoint, "exit", result.returncode,
                      result.stdout.strip(), result.stderr.strip())
                assert (result.returncode == 0) == (expected == 0)
            manifests = list(pathlib.Path(tmp).rglob("paper.json"))
            assert len(manifests) == 1
            manifest = json.loads(manifests[0].read_text())
            assert len(manifest["artifacts"]) == 1
            events = [json.loads(line) for line in manifests[0].with_name(
                "events.jsonl").read_text().splitlines()]
            assert len(events) == 3
            print("Real HTTP backend: transfer, deduplication, invalid content "
                  "and truncated body rejection passed.")
    finally:
        server.shutdown()
        server.server_close()
        thread.join()


if __name__ == "__main__":
    main()
