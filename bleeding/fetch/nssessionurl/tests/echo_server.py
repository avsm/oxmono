"""Local test server: POST /echo echoes the body back; GET /big/<n> streams
n bytes of a repeating pattern; GET /header/<name> returns that request
header's value; GET /slow/<secs> sleeps before responding; GET /redirect
302s to /big/16.  Serves argv[1] requests, then exits."""
import sys
import time
from http.server import BaseHTTPRequestHandler, HTTPServer

PATTERN = b"0123456789abcdef"

class Handler(BaseHTTPRequestHandler):
    def read_body(self):
        if "chunked" in self.headers.get("Transfer-Encoding", "").lower():
            out = []
            while True:
                n = int(self.rfile.readline().split(b";")[0], 16)
                if n == 0:
                    self.rfile.readline()
                    return b"".join(out)
                out.append(self.rfile.read(n))
                self.rfile.readline()
        return self.rfile.read(int(self.headers.get("Content-Length", 0)))

    def do_POST(self):
        body = self.read_body()
        self.send_response(200)
        self.send_header("Content-Type", "application/octet-stream")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def send_bytes(self, body):
        self.send_response(200)
        self.send_header("Content-Type", "application/octet-stream")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        kind, _, arg = self.path.strip("/").partition("/")
        if kind == "big":
            n = int(arg)
            self.send_response(200)
            self.send_header("Content-Type", "application/octet-stream")
            self.send_header("Content-Length", str(n))
            self.end_headers()
            sent = 0
            while sent < n:
                take = min(len(PATTERN), n - sent)
                self.wfile.write(PATTERN[:take])
                sent += take
        elif kind == "header":
            self.send_bytes(self.headers.get(arg, "").encode())
        elif kind == "slow":
            time.sleep(float(arg))
            self.send_bytes(b"finally")
        elif kind == "redirect":
            self.send_response(302)
            self.send_header("Location", "/big/16")
            self.send_header("Content-Length", "0")
            self.end_headers()
        else:
            self.send_error(404)

    def log_message(self, *args):
        pass

class QuietServer(HTTPServer):
    # Clients time out and hang up mid-response; don't spew tracebacks.
    def handle_error(self, request, client_address):
        pass

srv = QuietServer(("127.0.0.1", 0), Handler)
print(srv.server_address[1], flush=True)
for _ in range(int(sys.argv[1]) if len(sys.argv) > 1 else 1):
    srv.handle_request()
