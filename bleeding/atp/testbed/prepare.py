#!/usr/bin/env python3
"""Create persistent development secrets and certificates, without network I/O."""

import os
from pathlib import Path
import secrets
import subprocess


def openssl(*args):
    subprocess.run(["openssl", *args], check=True, capture_output=True)


def main():
    os.umask(0o077)
    state = Path(__file__).resolve().parent / ".state"
    certs = state / "certs"
    certs.mkdir(parents=True, exist_ok=True)
    env = state / "pds.env"
    if not env.exists():
        env.write_text(
            f"PDS_JWT_SECRET={secrets.token_hex(32)}\n"
            f"PDS_ADMIN_PASSWORD={secrets.token_hex(24)}\n"
            f"PDS_PLC_ROTATION_KEY_K256_PRIVATE_KEY_HEX={secrets.token_hex(32)}\n")
    if not (certs / "ca.crt").exists():
        openssl("req", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "3650",
                "-subj", "/CN=oxmono local ATP CA",
                "-addext", "basicConstraints=critical,CA:TRUE",
                "-addext", "keyUsage=critical,keyCertSign,cRLSign",
                "-keyout", str(certs / "ca.key"), "-out", str(certs / "ca.crt"))
    if not (certs / "server.crt").exists():
        openssl("req", "-new", "-newkey", "rsa:2048", "-nodes",
                "-subj", "/CN=pds.tangled.test", "-keyout", str(certs / "server.key"),
                "-out", str(certs / "server.csr"))
        extensions = certs / "server.ext"
        extensions.write_text(
            "basicConstraints=critical,CA:FALSE\n"
            "keyUsage=critical,digitalSignature,keyEncipherment\n"
            "extendedKeyUsage=serverAuth\n"
            "subjectAltName=DNS:pds.tangled.test,DNS:*.pds.tangled.test,"
            "DNS:plc.tangled.test,DNS:jetstream.tangled.test,DNS:knot.tangled.test,"
            "DNS:spindle.tangled.test,DNS:appview.tangled.test\n")
        openssl("x509", "-req", "-days", "365", "-in", str(certs / "server.csr"),
                "-CA", str(certs / "ca.crt"), "-CAkey", str(certs / "ca.key"),
                "-CAcreateserial", "-extfile", str(extensions),
                "-out", str(certs / "server.crt"))
    # Public certificates must be readable by non-root image users.
    os.chmod(certs / "ca.crt", 0o644)
    os.chmod(certs / "server.crt", 0o644)
    print(f"Development configuration ready in {state}")


if __name__ == "__main__":
    main()
