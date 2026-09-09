#!/usr/bin/env python3
"""Generate public-only JWT fixtures with OpenSSL 3, independently of JSONWT."""
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


def main():
    fixtures = []
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        key = root / "key.pem"
        message = root / "message"
        for alg, curve, width in [
            ("ES256", "prime256v1", 32), ("ES384", "secp384r1", 48),
            ("ES512", "secp521r1", 66), ("ES256K", "secp256k1", 32),
            ("EdDSA", None, 32),
        ]:
            if curve:
                run("openssl", "genpkey", "-algorithm", "EC", "-pkeyopt",
                    f"ec_paramgen_curve:{curve}", "-out", str(key))
                public = run("openssl", "pkey", "-in", str(key), "-pubout",
                             "-outform", "DER")[-(1 + 2 * width):]
                assert public[0] == 4
                name = {"ES256": "P-256", "ES384": "P-384", "ES512": "P-521",
                        "ES256K": "secp256k1"}[alg]
                jwk = {"kty": "EC", "crv": name, "alg": alg,
                       "x": b64(public[1:1 + width]),
                       "y": b64(public[1 + width:])}
                compressed = run("openssl", "pkey", "-in", str(key),
                                 "-pubout", "-outform", "DER",
                                 "-ec_conv_form", "compressed")[-(width + 1):]
            else:
                run("openssl", "genpkey", "-algorithm", "ED25519",
                    "-out", str(key))
                public = run("openssl", "pkey", "-in", str(key), "-pubout",
                             "-outform", "DER")[-32:]
                jwk = {"kty": "OKP", "crv": "Ed25519", "alg": alg,
                       "x": b64(public)}
                compressed = public
            header = json.dumps({"alg": alg, "typ": "JWT"}).encode()
            claims = json.dumps({"iss": "did:plc:fixture", "exp": 1060,
                                 "aud": "did:web:spindle.test"}).encode()
            text = (b64(header) + "." + b64(claims)).encode()
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
            fixtures.append({"alg": alg, "jwk": jwk, "public": b64(public),
                             "compressed": b64(compressed),
                             "token": text.decode() + "." + b64(signature)})
    Path(__file__).with_name("fixtures.json").write_text(
        json.dumps(fixtures, indent=2) + "\n")


if __name__ == "__main__":
    main()
