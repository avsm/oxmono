#!/usr/bin/env python3
"""Generate public-only CWT fixtures with OpenSSL 3 and a small CBOR writer."""
import base64
import json
from pathlib import Path
import subprocess
import tempfile


def b64(value):
    return base64.urlsafe_b64encode(value).decode().rstrip("=")


def run(*args, data=None):
    return subprocess.check_output(args, input=data, stderr=subprocess.DEVNULL)


def der_integer(data, pos):
    assert data[pos] == 2
    size = data[pos + 1]
    return int.from_bytes(data[pos + 2:pos + 2 + size], "big"), pos + 2 + size


def cbor(value):
    def head(major, n):
        if n < 24:
            return bytes([(major << 5) | n])
        for width, extra in [(1, 24), (2, 25), (4, 26), (8, 27)]:
            if n < 1 << (8 * width):
                return bytes([(major << 5) | extra]) + n.to_bytes(width, "big")
        raise ValueError("integer too large")
    if isinstance(value, int):
        return head(0, value) if value >= 0 else head(1, -1 - value)
    if isinstance(value, bytes):
        return head(2, len(value)) + value
    if isinstance(value, str):
        data = value.encode()
        return head(3, len(data)) + data
    if isinstance(value, list):
        return head(4, len(value)) + b"".join(map(cbor, value))
    if isinstance(value, dict):
        return head(5, len(value)) + b"".join(
            cbor(k) + cbor(v) for k, v in value.items())
    raise ValueError(type(value))


def main():
    fixtures = []
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        key = root / "key.pem"
        message = root / "message"
        for alg, curve, width in [
            ("ES256", "prime256v1", 32), ("ES384", "secp384r1", 48),
            ("ES512", "secp521r1", 66),
            ("EdDSA", None, 32),
        ]:
            if curve:
                run("openssl", "genpkey", "-algorithm", "EC", "-pkeyopt",
                    f"ec_paramgen_curve:{curve}", "-out", str(key))
                public = run("openssl", "pkey", "-in", str(key), "-pubout",
                             "-outform", "DER")[-(1 + 2 * width):]
                assert public[0] == 4
            else:
                run("openssl", "genpkey", "-algorithm", "ED25519",
                    "-out", str(key))
                public = run("openssl", "pkey", "-in", str(key), "-pubout",
                             "-outform", "DER")[-32:]
            algorithm = {"ES256": -7, "ES384": -35,
                         "ES512": -36, "EdDSA": -8}[alg]
            protected = cbor({1: algorithm})
            payload = cbor({1: "fixture", 4: 1060, 3: "service"})
            text = cbor(["Signature1", protected, b"", payload])
            if curve:
                digest = {"ES384": "-sha384", "ES512": "-sha512"}.get(
                    alg, "-sha256")
                der = run("openssl", "dgst", digest, "-sign", str(key),
                          data=text)
                assert der[0] == 0x30
                pos = 2 if der[1] < 128 else 2 + (der[1] & 127)
                r, pos = der_integer(der, pos)
                s, pos = der_integer(der, pos)
                assert pos == len(der)
                signature = r.to_bytes(width, "big") + s.to_bytes(width, "big")
            else:
                message.write_bytes(text)
                signature = run("openssl", "pkeyutl", "-sign", "-rawin",
                                "-inkey", str(key), "-in", str(message))
            if curve:
                curve_id = {"ES256": 1, "ES384": 2, "ES512": 3}[alg]
                key_map = {1: 2, 3: algorithm, -1: curve_id,
                           -2: public[1:1 + width], -3: public[1 + width:]}
            else:
                key_map = {1: 1, 3: algorithm, -1: 6, -2: public}
            token = b"\xd8\x3d\xd2" + cbor(
                [protected, {}, payload, signature])
            fixtures.append({"alg": algorithm, "key": b64(cbor(key_map)),
                             "token": b64(token)})
    Path(__file__).with_name("cwt-fixtures.json").write_text(
        json.dumps(fixtures, indent=2) + "\n")


if __name__ == "__main__":
    main()
