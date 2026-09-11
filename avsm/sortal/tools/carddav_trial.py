#!/usr/bin/env python3
"""Inspect or seed a CardDAV address book and verify every new card.

This is an initial-sync trial, not the future two-way merger. Existing contacts
are kept intact; uncertain matches are held for review. Credentials are read
from a file at runtime and are never written to the report or logged.
"""

import argparse
import base64
from collections import Counter
from concurrent.futures import ThreadPoolExecutor, wait, FIRST_COMPLETED
import hashlib
import http.client
import json
from pathlib import Path
import ssl
import sys
import unicodedata
from urllib.parse import urljoin, urlsplit
import xml.etree.ElementTree as ET

import yaml

import carddav_export as export
import sortal_vcard as mapping


D = "{DAV:}"
C = "{urn:ietf:params:xml:ns:carddav}"
ROOT = "https://carddav.fastmail.com/"


class Dav:
    def __init__(self, username, password, root=ROOT, readonly=False):
        self.root = root
        self.origin = self.url_origin(root)
        if urlsplit(root).query:
            raise ValueError("server URL must not contain a query")
        self.readonly = readonly
        self.authorization = "Basic " + base64.b64encode((username + ":" + password).encode()).decode()
        self.context = ssl.create_default_context()

    @staticmethod
    def url_origin(url):
        u = urlsplit(url)
        if u.scheme != "https" or not u.hostname or u.username is not None or u.password is not None or u.fragment:
            raise ValueError("CardDAV URLs must use HTTPS without embedded credentials or fragments")
        port = 443 if u.port is None else u.port
        if port < 1:
            raise ValueError("invalid CardDAV port")
        return u.scheme, u.hostname, port

    def validate_url(self, url):
        if self.url_origin(url) != self.origin:
            raise ValueError("refusing to send credentials outside the configured CardDAV origin")

    def request(self, method, url, body=None, headers=None):
        method = method.upper()
        if self.readonly and method not in {"GET", "HEAD", "OPTIONS", "PROPFIND", "REPORT"}:
            raise ValueError(f"dry-run transport forbids {method}")
        for _ in range(5):
            self.validate_url(url)
            u = urlsplit(url)
            conn = http.client.HTTPSConnection(u.hostname, port=u.port, context=self.context, timeout=40)
            try:
                target = u.path or "/"
                if u.query:
                    target += "?" + u.query
                conn.request(method, target, body=body, headers={
                    "Authorization": self.authorization, "User-Agent": "Sortal-CardDAV-trial/1",
                    **(headers or {})})
                response = conn.getresponse()
                status = response.status
                result_headers = {key.lower(): value for key, value in response.getheaders()}
                data = response.read(64 * 1024 * 1024 + 1)
                if len(data) > 64 * 1024 * 1024:
                    raise ValueError("CardDAV response exceeds trial size limit")
            finally:
                conn.close()
            if status in {301, 302, 307, 308}:
                url = urljoin(url, result_headers["location"])
                continue
            return status, result_headers, data
        raise ValueError("too many CardDAV redirects")

    def xml(self, method, url, body, depth):
        status, _, data = self.request(method, url, body.encode(), {
            "Content-Type": "application/xml; charset=utf-8", "Depth": str(depth)})
        if status != 207:
            raise ValueError(f"{method} failed with HTTP {status}")
        return responses(data, url)


def responses(data, base):
    root = ET.fromstring(data)
    if root.tag != D + "multistatus" or root.find(D + "error") is not None:
        raise ValueError("invalid or incomplete DAV multistatus")
    result = []
    for response in root.findall(D + "response"):
        href = response.findtext(D + "href")
        if not href:
            raise ValueError("DAV response has no href")
        status = response.findtext(D + "status")
        if status is not None and (len(status.split()) < 2 or status.split()[1] != "200"):
            raise ValueError("DAV listing contains a failed resource response")
        if response.find(D + "error") is not None:
            raise ValueError("DAV listing contains an error")
        props = {}
        for propstat in response.findall(D + "propstat"):
            status = propstat.findtext(D + "status", "").split()
            if len(status) < 2 or status[1] != "200":
                continue
            prop = propstat.find(D + "prop")
            if prop is not None:
                props.update({p.tag: p for p in prop})
        result.append((urljoin(base, href), props))
    return result


def find_href(rows, prop):
    values = [urljoin(url, href.text) for url, props in rows
              if (element := props.get(prop)) is not None
              for href in element.findall(D + "href") if href.text]
    if len(values) != 1:
        raise ValueError(f"expected one {prop} during discovery")
    return values[0]


def discover(dav, collection=None):
    def query(prop):
        return '<?xml version="1.0"?><d:propfind xmlns:d="DAV:" xmlns:c="urn:ietf:params:xml:ns:carddav"><d:prop>' + prop + '</d:prop></d:propfind>'
    entry = (urljoin(dav.root, ".well-known/carddav")
             if urlsplit(dav.root).path in {"", "/"} else dav.root)
    principal = find_href(dav.xml("PROPFIND", entry, query("<d:current-user-principal/>"), 0), D + "current-user-principal")
    home = find_href(dav.xml("PROPFIND", principal, query("<c:addressbook-home-set/>"), 0), C + "addressbook-home-set")
    rows = dav.xml("PROPFIND", home, query("<d:resourcetype/><d:displayname/><c:max-resource-size/><c:supported-address-data/>"), 1)
    books = []
    for href, props in rows:
        resource = props.get(D + "resourcetype")
        if resource is None or resource.find(C + "addressbook") is None:
            continue
        size = props.get(C + "max-resource-size")
        supported = props.get(C + "supported-address-data")
        books.append({"href": href, "name": props.get(D + "displayname").text if D + "displayname" in props else "",
                      "max_resource_size": int(size.text) if size is not None and size.text else None,
                      "supported_address_data": [dict(p.attrib) for p in supported] if supported is not None else None})
    if collection:
        dav.validate_url(collection)
        selected = [b for b in books if b["href"].rstrip("/") == collection.rstrip("/")]
        if len(selected) != 1:
            raise ValueError("specified collection was not uniquely found during discovery")
        return selected[0]
    defaults = [b for b in books if urlsplit(b["href"]).path.rstrip("/").endswith("/Default")]
    selected = defaults if len(defaults) == 1 else books
    if len(selected) != 1:
        raise ValueError("address-book selection is ambiguous; use --collection with its full URL")
    return selected[0]


def fetch_all(dav, book):
    body = ('<?xml version="1.0"?><c:addressbook-query xmlns:d="DAV:" xmlns:c="urn:ietf:params:xml:ns:carddav">'
            '<d:prop><d:getetag/><c:address-data/></d:prop></c:addressbook-query>')
    result = []
    seen = set()
    for href, props in dav.xml("REPORT", book["href"], body, 1):
        address = props.get(C + "address-data")
        if address is None or address.text is None:
            raise ValueError("incomplete address-book listing; refusing to plan creations")
        data = address.text.encode()
        parsed = mapping.parse(data)
        uid = mapping.untext(mapping.only(parsed, "UID")["value"])
        if uid in seen:
            raise ValueError("duplicate destination UID; refusing to plan creations")
        seen.add(uid)
        result.append({"uid": uid, "href": href,
                       "etag": props.get(D + "getetag").text if D + "getetag" in props else None,
                       "data": data, "props": parsed})
    return result


def normalize_name(value):
    return " ".join(unicodedata.normalize("NFKC", value).casefold().split())


def identifiers(props):
    names, emails, urls = set(), set(), set()
    for p in props:
        name, value = p["name"], mapping.untext(p["value"])
        if name in {"FN", "X-SORTAL-ALT-NAME"}:
            names.add(normalize_name(value))
        elif name == "NICKNAME":
            names.update(normalize_name(v) for v in re_split_list(p["value"]))
        elif name == "N":
            parts = mapping.components(p["value"])
            names.add(normalize_name(" ".join(parts)))
            if len(parts) >= 2:
                names.add(normalize_name(" ".join([parts[1], parts[0], *parts[2:]])))
        elif name == "EMAIL":
            local, sep, domain = value.strip().rpartition("@")
            emails.add(local + sep + domain.casefold())
        elif name in {"URL", "SOCIALPROFILE", "X-SOCIALPROFILE", "IMPP", "X-ATPROTO"}:
            urls.add(value.strip())
    return {"names": names - {""}, "emails": emails - {""}, "urls": urls - {""}}


def re_split_list(value):
    # The structured splitter already handles escaping; replace only unescaped
    # list separators so nickname commas remain part of the name when escaped.
    result, start, i = [], 0, 0
    while i < len(value):
        if value[i] == "\\":
            i += 2
        elif value[i] == ",":
            result.append(mapping.untext(value[start:i]))
            start, i = i + 1, i + 1
        else:
            i += 1
    return result + [mapping.untext(value[start:])]


def assert_contact(data, bundle, entry):
    props = mapping.parse(data)
    if mapping.untext(mapping.only(props, "UID")["value"]) != entry["uid"]:
        raise ValueError("server changed UID")
    expected_store = json.loads((bundle / "manifest.json").read_text())["store_id"]
    if mapping.untext(mapping.only(props, "X-SORTAL-STORE")["value"]) != expected_store:
        raise ValueError("server changed Sortal store identity")
    decoded, photos = mapping.decode(data)
    original = yaml.load(export.safe_path(bundle / "originals", entry["source"]).read_bytes(), Loader=export.UniqueLoader)
    if decoded != mapping.canonical(original):
        raise ValueError("server fields do not reconstruct the source contact")
    for filename, photo in photos.items():
        if photo != export.safe_path(bundle / "originals", filename).read_bytes():
            raise ValueError("server changed photo bytes")
    # Reconstruction alone cannot detect removal of display fallbacks such as
    # N or the ordinary social URLs. Retain every emitted property as well.
    # This intentionally conservative trial check is not a general vCard merge.
    def signatures(properties):
        def parameters(p):
            result = []
            for key, value in p["params"].items():
                if key in {"TYPE", "VALUE", "ENCODING"}:
                    value = ",".join(sorted(value.lower().split(",")))
                result.append((key, value))
            return tuple(sorted(result))
        return Counter((p["group"], p["name"], parameters(p), p["value"])
                       for p in properties if p["name"] != "X-SORTAL-MAPPING")
    expected = mapping.parse(export.safe_path(bundle, entry["card"]).read_bytes())
    if signatures(expected) - signatures(props):
        raise ValueError("server removed or transformed an emitted property; review the readback")


def plan(bundle, manifest, book, remote):
    source = []
    for entry in manifest["contacts"]:
        data = export.safe_path(bundle, entry["card"]).read_bytes()
        source.append((entry, data, identifiers(mapping.parse(data))))
    index = [(r, identifiers(r["props"]), {
        key: [mapping.untext(p["value"]) for p in r["props"] if p["name"] == key]
        for key in ("X-SORTAL-ID", "X-SORTAL-STORE")}) for r in remote]
    result = []
    for entry, data, ids in source:
        strong = []
        for r, _, values in index:
            if (r["uid"] == entry["uid"] or
                    (values.get("X-SORTAL-ID") == [entry["handle"]]
                     and values.get("X-SORTAL-STORE") == [manifest["store_id"]])):
                strong.append(r)
        row = {"handle": entry["handle"], "uid": entry["uid"], "card": entry["card"],
               "source": entry["source"], "action": "create",
               "href": urljoin(book["href"].rstrip("/") + "/", entry["uid"] + ".vcf")}
        if strong:
            row["action"] = "review"
            row["reason"] = "existing identity needs reconciliation"
            row["candidates"] = [r["href"] for r in strong]
            if len(strong) == 1:
                try:
                    assert_contact(strong[0]["data"], bundle, entry)
                    row.update(action="unchanged", href=strong[0]["href"], etag=strong[0]["etag"])
                    row.pop("reason")
                except (ValueError, KeyError, TypeError):
                    pass
        else:
            candidates = [r["href"] for r, other, _ in index if any(ids[k] & other[k] for k in ids)]
            local = [e["handle"] for e, _, other in source
                     if e["uid"] != entry["uid"] and ids["names"] & other["names"]]
            if candidates or local:
                row.update(action="review", reason="possible duplicate name/email/account", candidates=candidates, source_candidates=local)
            elif book["max_resource_size"] and len(data) > book["max_resource_size"]:
                row.update(action="review", reason="card exceeds the destination resource size limit")
        result.append(row)
    return result


def save_json(path, value):
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n")
    temporary.replace(path)


def seed_one(dav, bundle, row, directory):
    data = export.safe_path(bundle, row["card"]).read_bytes()
    status, _, _ = dav.request("PUT", row["href"], data, {
        "Content-Type": "text/vcard; charset=utf-8", "If-None-Match": "*"})
    if status not in {201, 204}:
        raise ValueError(f"PUT returned HTTP {status}")
    status, headers, stored = dav.request("GET", row["href"])
    if status != 200:
        raise ValueError(f"readback returned HTTP {status}")
    # Save returned data before checking so any transformation is reviewable.
    (directory / (row["uid"] + ".vcf")).write_bytes(stored)
    assert_contact(stored, bundle, row)
    if not headers.get("etag") or headers["etag"].startswith("W/"):
        raise ValueError("server did not return a strong ETag for later conditional writes")
    return {"uid": row["uid"], "handle": row["handle"], "href": row["href"],
            "etag": headers.get("etag"), "status": "verified", "sha256": export.digest(stored)}


def run(args):
    if getattr(args, "dry_run", False) and args.apply:
        raise ValueError("--dry-run and --apply cannot be combined")
    if args.report.exists() or args.report.is_symlink():
        raise ValueError("report directory must be new")
    manifest = export.verify(args.bundle)
    protected = [Path(manifest["source"]).resolve(), (args.bundle / "originals").resolve(),
                 (args.bundle / "cards").resolve()]
    if any(args.report.resolve().is_relative_to(path) for path in protected):
        raise ValueError("report must be outside the source and saved contact trees")
    password = args.password_file.read_text().strip()
    if not password or "\n" in password or "\r" in password:
        raise ValueError("password file must contain one nonempty password")
    dav = Dav(args.username, password, root=getattr(args, "server", ROOT), readonly=not args.apply)
    book = discover(dav, getattr(args, "collection", None))
    supported = book["supported_address_data"]
    if supported is not None and not any(p.get("content-type", "").lower() == "text/vcard"
                                         and p.get("version") == "3.0" for p in supported):
        raise ValueError("destination does not advertise vCard 3.0 support")
    remote = fetch_all(dav, book)
    rows = plan(args.bundle, manifest, book, remote)
    args.report.mkdir(mode=0o700)
    before, after = args.report / "before", args.report / "after"
    before.mkdir(mode=0o700)
    after.mkdir(mode=0o700)
    for r in remote:
        name = hashlib.sha256(r["uid"].encode()).hexdigest() + ".vcf"
        (before / name).write_bytes(r["data"])
    report = {"account": args.username, "book": book, "existing_contacts": len(remote),
              "plan": rows, "results": [], "applied": args.apply, "dry_run": not args.apply}
    save_json(args.report / "report.json", report)
    counts = {action: sum(r["action"] == action for r in rows) for action in ("create", "unchanged", "review")}
    print(json.dumps({"address_book": book, "existing": len(remote), "plan": counts}), flush=True)
    if not args.apply:
        return report
    creates = sorted((r for r in rows if r["action"] == "create"), key=lambda r: (r["handle"] != "avsm", r["handle"]))
    if not creates:
        return
    # One representative real contact must pass before the rest are submitted.
    try:
        report["results"].append(seed_one(dav, args.bundle, creates[0], after))
    except Exception as error:
        report["results"].append({"uid": creates[0]["uid"], "status": "failed", "error": str(error)})
        save_json(args.report / "report.json", report)
        raise ValueError("first contact did not verify; further uploads stopped") from error
    save_json(args.report / "report.json", report)
    print("First contact passed PUT/GET and complete field/photo recovery.", flush=True)
    todo = iter(creates[1:])
    with ThreadPoolExecutor(max_workers=2) as pool:
        pending = {}
        stopped = False
        while True:
            while len(pending) < 2 and not stopped:
                row = next(todo, None)
                if row is None:
                    break
                pending[pool.submit(seed_one, dav, args.bundle, row, after)] = row
            if not pending:
                break
            done, _ = wait(pending, return_when=FIRST_COMPLETED)
            for future in done:
                row = pending.pop(future)
                try:
                    report["results"].append(future.result())
                except Exception as error:
                    report["results"].append({"uid": row["uid"], "handle": row["handle"], "status": "failed", "error": str(error)})
                    stopped = True
                save_json(args.report / "report.json", report)
                if len(report["results"]) % 25 == 0 or stopped:
                    print(f"Completed {len(report['results'])}/{len(creates)} uploads; stopped={stopped}", flush=True)
    failures = sum(r["status"] == "failed" for r in report["results"])
    print(f"Verified {len(report['results']) - failures} created contacts; {failures} failures; {counts['review']} held for review.", flush=True)
    if failures:
        raise ValueError("some uploads did not verify; see the local report")


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("bundle", type=Path)
    p.add_argument("--username", required=True)
    p.add_argument("--password-file", type=Path, required=True)
    p.add_argument("--report", type=Path, required=True, help="new private output directory")
    p.add_argument("--server", default=ROOT, help="HTTPS CardDAV origin or discovery endpoint")
    p.add_argument("--collection", help="full address-book URL when discovery finds multiple collections")
    mode = p.add_mutually_exclusive_group()
    mode.add_argument("--dry-run", action="store_true", help="preview only; HTTP writes are blocked (the default)")
    mode.add_argument("--apply", action="store_true", help="create unmatched contacts, then verify their readback")
    args = p.parse_args()
    try:
        run(args)
    except Exception as error:
        print(f"Trial failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
