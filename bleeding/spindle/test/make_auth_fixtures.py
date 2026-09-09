#!/usr/bin/env python3
"""Generate public-only JOSE fixtures using OpenSSL, with fixed claim times."""

import base64
import json
from pathlib import Path
import subprocess
import tempfile


def b64(data):
    return base64.urlsafe_b64encode(data).decode().rstrip("=")


def base58(data):
    alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    number = int.from_bytes(data, "big")
    result = ""
    while number:
        number, digit = divmod(number, 58)
        result = alphabet[digit] + result
    return result


def main():
    actor = "did:plc:aaaaaaaaaaaaaaaaaaaaaaaa"
    claims = {"iss": actor, "aud": "did:web:spindle.test", "iat": 990,
              "exp": 1060, "lxm": "sh.tangled.ci.triggerPipeline"}
    with tempfile.TemporaryDirectory() as directory:
        key = Path(directory) / "key.pem"
        subprocess.run(["openssl", "genpkey", "-algorithm", "EC", "-pkeyopt",
                        "ec_paramgen_curve:secp256k1", "-out", str(key)],
                       check=True, capture_output=True)
        public = subprocess.check_output(["openssl", "pkey", "-in", str(key),
            "-pubout", "-outform", "DER", "-ec_conv_form", "compressed"])[-33:]
        document = {"id": actor, "verificationMethod": [{
            "id": actor + "#atproto", "controller": actor, "type": "Multikey",
            "publicKeyMultibase": "z" + base58(b"\xe7\x01" + public)}]}

        def sign(payload, header=None):
            if header is None:
                header = {"alg": "ES256K", "typ": "JWT"}
            text = b64(json.dumps(header).encode()) + "." + b64(payload.encode())
            der = subprocess.check_output(["openssl", "dgst", "-sha256",
                "-sign", str(key)], input=text.encode())
            assert der[0] == 0x30 and der[2] == 2
            rlen = der[3]
            r = int.from_bytes(der[4:4 + rlen], "big")
            assert der[4 + rlen] == 2
            slen = der[5 + rlen]
            s = int.from_bytes(der[6 + rlen:6 + rlen + slen], "big")
            return text + "." + b64(r.to_bytes(32, "big") + s.to_bytes(32, "big"))

        tests = [{"name": "valid", "valid": True,
                  "token": sign(json.dumps(claims))}]
        for name, updates in [
            ("expired", {"exp": 999}), ("expiry boundary", {"exp": 1000}),
            ("future issuance", {"iat": 1100}),
            ("future not-before", {"nbf": 1001}),
            ("excessive lifetime", {"exp": 4601}),
            ("fractional expiry", {"exp": 1060.5}),
            ("wrong audience", {"aud": "did:web:wrong.test"}),
            ("wrong issuer", {"iss": "did:plc:bbbbbbbbbbbbbbbbbbbbbbbb"}),
            ("wrong method", {"lxm": "sh.tangled.ci.cancelPipeline"})]:
            tests.append({"name": name, "valid": False,
                          "token": sign(json.dumps(dict(claims, **updates)))})
        duplicate = json.dumps(claims)[:-1] + ', "exp": 1060}'
        for name, payload, header in [
            ("duplicate claim", duplicate, None),
            ("unsigned algorithm", json.dumps(claims), {"alg": "none"}),
            ("unencoded payload", json.dumps(claims),
             {"alg": "ES256K", "b64": False}),
            ("critical extension", json.dumps(claims),
             {"alg": "ES256K", "crit": ["b64"], "b64": False})]:
            tests.append({"name": name, "valid": False,
                          "token": sign(payload, header)})
        valid = tests[0]["token"]
        for name, token in [
            ("missing signature", valid.rsplit(".", 1)[0] + "."),
            ("wrong signature", valid.rsplit(".", 1)[0] + "." + b64(b"\0" * 64)),
            ("extra component", valid + ".x"), ("oversized", "x" * 8193)]:
            tests.append({"name": name, "valid": False, "token": token})
    path = Path(__file__).with_name("auth-fixtures.json")
    path.write_text(json.dumps({"actor": actor, "document": json.dumps(document),
                               "tests": tests}, indent=2) + "\n")


if __name__ == "__main__":
    main()
