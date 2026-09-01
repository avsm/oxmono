#!/usr/bin/env python3
"""Small real TLS/HTTP2 peer for the curl transport trailer tests."""

import datetime
import socket
import ssl
import struct
import sys
import tempfile
from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.x509.oid import NameOID


def frame(kind, flags, stream, payload=b""):
    n = len(payload)
    return n.to_bytes(3, "big") + bytes((kind, flags)) + struct.pack(">I", stream) + payload


def literal(name, value):
    name = name.encode()
    value = value.encode()
    assert len(name) < 127 and len(value) < 127
    return b"\x00" + bytes((len(name),)) + name + bytes((len(value),)) + value


def response(mode):
    body = b"" if mode == "empty" else b"a"
    trailers = literal("x-checksum", "ok")
    if mode == "forbidden":
        trailers += literal("set-cookie", "sid=trailer")
    elif mode == "large":
        trailers = literal("x-long", "x" * 80)
    return body, trailers


def read_exact(sock, n):
    out = b""
    while len(out) < n:
        piece = sock.recv(n - len(out))
        if not piece:
            raise EOFError
        out += piece
    return out


def send_response(sock, stream, mode):
    body, trailers = response(mode)
    sock.sendall(frame(1, 4, stream, b"\x88"))
    if body:
        sock.sendall(frame(0, 0, stream, body))
    sock.sendall(frame(1, 5, stream, trailers))


def serve_connection(raw, context, mode):
    with context.wrap_socket(raw, server_side=True) as sock:
        if sock.selected_alpn_protocol() != "h2":
            return
        if read_exact(sock, 24) != b"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n":
            return
        sock.sendall(frame(4, 0, 0))
        continued = set()
        streams = []
        held_second = None
        while True:
            header = read_exact(sock, 9)
            length = int.from_bytes(header[:3], "big")
            kind, flags = header[3], header[4]
            stream = struct.unpack(">I", header[5:])[0] & 0x7FFFFFFF
            read_exact(sock, length)
            if kind == 4 and not (flags & 1):
                sock.sendall(frame(4, 1, 0))
            if kind == 1:
                if flags & 4:
                    streams.append(stream)
                    if mode != "multiplex":
                        send_response(sock, stream, mode)
                    elif len(streams) == 1:
                        # Publish the first response head, then leave its body
                        # pending until the client cancels this stream.
                        sock.sendall(frame(1, 4, stream, b"\x88"))
                    else:
                        # The second response starts on the same connection but
                        # cannot finish until cancellation of the first stream.
                        sock.sendall(frame(1, 4, stream, b"\x88"))
                        sock.sendall(frame(0, 0, stream, b"b"))
                        held_second = stream
                else:
                    continued.add(stream)
            elif kind == 9 and stream in continued and flags & 4:
                continued.remove(stream)
                streams.append(stream)
                if mode != "multiplex":
                    send_response(sock, stream, mode)
                elif len(streams) == 1:
                    sock.sendall(frame(1, 4, stream, b"\x88"))
                else:
                    sock.sendall(frame(1, 4, stream, b"\x88"))
                    sock.sendall(frame(0, 0, stream, b"b"))
                    held_second = stream
            elif kind == 3 and mode == "multiplex" and stream == streams[0]:
                if held_second is not None:
                    sock.sendall(frame(0, 1, held_second, b"c"))
                    held_second = None


def certificate(directory):
    key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    subject = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "localhost")])
    now = datetime.datetime.now(datetime.timezone.utc)
    cert = (
        x509.CertificateBuilder()
        .subject_name(subject)
        .issuer_name(subject)
        .public_key(key.public_key())
        .serial_number(1)
        .not_valid_before(now - datetime.timedelta(days=1))
        .not_valid_after(now + datetime.timedelta(days=1))
        .add_extension(x509.SubjectAlternativeName([x509.DNSName("localhost")]), False)
        .sign(key, hashes.SHA256())
    )
    cert_path = directory + "/cert.pem"
    key_path = directory + "/key.pem"
    with open(cert_path, "wb") as f:
        f.write(cert.public_bytes(serialization.Encoding.PEM))
    with open(key_path, "wb") as f:
        f.write(key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption()))
    return cert_path, key_path


def main():
    port, mode = int(sys.argv[1]), sys.argv[2]
    with tempfile.TemporaryDirectory() as directory:
        cert, key = certificate(directory)
        context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        context.load_cert_chain(cert, key)
        context.set_alpn_protocols(["h2"])
        listener = socket.socket()
        listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        listener.bind(("127.0.0.1", port))
        listener.listen(16)
        while True:
            raw, _ = listener.accept()
            try:
                serve_connection(raw, context, mode)
            except (EOFError, ConnectionError, ssl.SSLError):
                raw.close()


if __name__ == "__main__":
    main()
