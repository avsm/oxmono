"""Field-level Sortal V2 <-> vCard mapping for the offline migration tool.

No serialized contact payload. X-SORTAL-PATH locates a property in the source
record, preserving collection membership/order independently of wire order.
This decoder is for this export profile, not a general remote-contact merger.
"""

import base64
import datetime
import re
from urllib.parse import quote, urlsplit


def text(value):
    return (value.replace("\\", "\\\\").replace("\r\n", "\n")
            .replace("\r", "\n").replace("\n", "\\n")
            .replace(";", "\\;").replace(",", "\\,"))


def untext(value):
    return re.sub(r"\\([nN,;\\])", lambda m: "\n" if m[1] in "nN" else m[1], value)


def fold(line):
    parts, current, size = [], [], 0
    for char in line:
        width = len(char.encode("utf-8"))
        if size + width > 75:
            parts.append("".join(current))
            current, size = [" "], 1
        current.append(char)
        size += width
    parts.append("".join(current))
    return "\r\n".join(parts)


def split_outside(value, delimiter):
    """Split vCard headers without splitting quoted parameter values."""
    parts, start, quoted = [], 0, False
    for i, char in enumerate(value):
        if char == '"':
            quoted = not quoted
        elif char == delimiter and not quoted:
            parts.append(value[start:i])
            start = i + 1
    if quoted:
        raise ValueError("unclosed vCard parameter quote")
    return parts + [value[start:]]


def parse(data):
    unfolded = re.sub(r"\r?\n[ \t]", "", data.decode("utf-8"))
    props = []
    for line in unfolded.replace("\r\n", "\n").split("\n"):
        if not line:
            continue
        # Values may contain literal quotes. Stop at the first unquoted colon.
        quoted, boundary = False, None
        for i, char in enumerate(line):
            if char == '"':
                quoted = not quoted
            elif char == ":" and not quoted:
                boundary = i
                break
        if boundary is None:
            raise ValueError("vCard property has no value delimiter")
        head, value = line[:boundary], line[boundary + 1:]
        fields = split_outside(head, ";")
        group, _, name = fields[0].rpartition(".")
        params = {}
        for field in fields[1:]:
            key, val = field.split("=", 1)
            key = key.upper()
            if key in params:
                raise ValueError(f"duplicate parameter {key}")
            params[key] = val[1:-1] if val.startswith('"') and val.endswith('"') else val
        props.append({"group": group.lower(), "name": name.upper(), "params": params, "value": value})
    return props


def only(props, name):
    values = [p for p in props if p["name"] == name]
    if len(values) != 1:
        raise ValueError(f"expected exactly one {name}, found {len(values)}")
    return values[0]


def canonical(value):
    """YAML timestamp scalars represent Sortal's ISO date strings."""
    if isinstance(value, datetime.date):
        return value.isoformat()
    if isinstance(value, dict):
        return {k: canonical(v) for k, v in value.items()}
    if isinstance(value, list):
        return [canonical(v) for v in value]
    return value


def uri(value):
    return quote(value.strip(), safe=":/?#[]@!$&'()*+,;=%")


def check_keys(value, allowed, location):
    unknown = set(value) - set(allowed)
    if unknown:
        raise ValueError(f"unmapped Sortal fields at {location}: {sorted(unknown)}")


SIMPLE_URLS = {
    "github": "https://github.com/{}", "gitlab": "https://gitlab.com/{}",
    "codeberg": "https://codeberg.org/{}", "orcid": "https://orcid.org/{}",
    "scholar": "https://scholar.google.com/citations?user={}",
    "twitter": "https://twitter.com/{}", "linkedin": "https://www.linkedin.com/in/{}",
    "threads": "https://www.threads.com/@{}", "instagram": "https://www.instagram.com/{}",
    "flickr": "https://www.flickr.com/photos/{}",
}
FEDERATED_URLS = {
    "mastodon": "https://{host}/@{user}", "pixelfed": "https://{host}/@{user}",
    "peertube": "https://{host}/c/{user}/videos", "matrix": "https://matrix.to/#/@{user}:{host}",
    "discourse": "https://{host}/u/{user}", "zulip": "https://{host}",
}
FEED_MEDIA_TYPES = {"atom": "application/atom+xml", "rss": "application/rss+xml",
                    "json": "application/feed+json"}


def profile_url(platform, handle):
    if platform in SIMPLE_URLS:
        return SIMPLE_URLS[platform].format(quote(handle, safe=""))
    if platform in FEDERATED_URLS:
        user, host = handle.rsplit("@", 1)
        return FEDERATED_URLS[platform].format(user=quote(user, safe=""), host=host)
    raise ValueError(f"unmapped account platform: {platform}")


def app_url(app, handle):
    templates = {"bluesky": "https://bsky.app/profile/{}", "tangled": "https://tangled.org/@{}",
                 "standard-site": "https://{}"}
    if app not in templates:
        raise ValueError(f"unmapped AT Protocol app: {app}")
    return templates[app].format(quote(handle, safe=""))


class Writer:
    def __init__(self, version, warnings):
        if version not in {"3.0", "4.0"}:
            raise ValueError("unsupported vCard version")
        self.version, self.warnings = version, warnings
        self.lines = ["BEGIN:VCARD", "VERSION:" + version, "X-SORTAL-MAPPING:3"]
        self.number = 0

    def group(self):
        self.number += 1
        return f"item{self.number}"

    def add(self, name, value, path=None, group=None, params=None, raw=False):
        params = dict(params or {})
        if path is not None:
            params["X-SORTAL-PATH"] = path
        head = (group + "." if group else "") + name
        for key, val in params.items():
            val = str(val)
            # Simple parameters work identically in 3.0 and 4.0. Do not apply
            # 4.0-only RFC 6868 escapes to 3.0 cards. Free text belongs in
            # grouped properties when it cannot be represented here.
            if any(c in val for c in '\r\n"^'):
                raise ValueError(f"parameter {key} requires unsupported quoting")
            token_list = key == "TYPE" and re.fullmatch(r"[A-Za-z0-9-]+(?:,[A-Za-z0-9-]+)*", val)
            quoted = any(c in val for c in ":;, ") and not token_list
            head += ";" + key + "=" + ('"' + val + '"' if quoted else val)
        self.lines.append(head + ":" + (value if raw else text(value)))

    def url(self, name, value, path=None, group=None, params=None):
        group = group or self.group()
        normalized = uri(value)
        self.add(name, normalized, path, group, params, raw=True)
        if normalized != value and path is not None:
            self.add("X-SORTAL-ORIGINAL-URL", value, group=group)
            self.warnings.append("URL normalized; original retained in grouped X-SORTAL-ORIGINAL-URL")

    def empty(self, value, path):
        if value in ([], {}):
            self.add("X-SORTAL-EMPTY", "array" if isinstance(value, list) else "object", path)
            return True
        return False

    def social(self, platform, handle, path, group):
        name = "SOCIALPROFILE" if self.version == "4.0" else "X-SOCIALPROFILE"
        service = "SERVICE-TYPE" if self.version == "4.0" else "TYPE"
        username = "USERNAME" if self.version == "4.0" else "X-USER"
        params = {service: platform}
        if any(c in handle for c in '\r\n"^'):
            self.add("X-SORTAL-USERNAME", handle, group=group)
        else:
            params[username] = handle
        self.url(name, profile_url(platform, handle), path, group, params)
        # A client that ignores SOCIALPROFILE must still see a usable link.
        self.url("URL", profile_url(platform, handle), group=group,
                 params={"X-SORTAL-DERIVED": "profile"})

    def atproto(self, account, path, group):
        obj = isinstance(account, dict)
        if obj:
            check_keys(account, {"handle", "did", "apps"}, path)
        handle = account["handle"] if obj else account
        params = {"X-ATPROTO-HANDLE": handle}
        if obj:
            params["X-SORTAL-SHAPE"] = "object"
            if "did" in account:
                params["X-ATPROTO-DID"] = account["did"]
        identity = account.get("did", handle) if obj else handle
        self.add("X-ATPROTO", "at://" + identity, path, group, params, raw=True)
        if obj and "apps" in account and not self.empty(account["apps"], path + "/apps"):
            for i, app in enumerate(account["apps"]):
                app_group = self.group()
                self.add("X-ATPROTO-APP", app, f"{path}/apps/{i}", app_group)
                self.url("URL", app_url(app, handle), group=app_group)
                self.add("X-ABLabel", app, group=app_group)
        if not obj or not account.get("apps"):
            self.url("URL", app_url("bluesky", handle), group=group,
                     params={"X-SORTAL-DERIVED": "atproto-default"})
            self.add("X-ABLabel", "Bluesky", group=group)

    def passthrough(self, fields):
        """Native Sortal vcard map: unfolded property header -> wire value.

        EMAIL entries overlay the unique equal native email to retain its
        parameters without emitting a duplicate address. Other entries are
        additional properties. Keep the original header in a grouped marker
        so reverse conversion also retains its spelling and parameters.
        """
        if not isinstance(fields, dict):
            raise ValueError("vcard passthrough must be a property-header mapping")
        reserved = {"BEGIN", "END", "VERSION", "UID", "FN", "N", "KIND", "URL",
                    "PHOTO", "ORG", "TITLE", "ADR", "SOCIALPROFILE", "X-SOCIALPROFILE",
                    "X-ATPROTO", "X-ATPROTO-APP", "X-ADDRESSBOOKSERVER-KIND"}
        overlaid, group_bindings = set(), {}
        for header, value in fields.items():
            if not isinstance(header, str) or not isinstance(value, str) or any(c in header + value for c in "\r\n"):
                raise ValueError("vcard passthrough requires single-line wire strings")
            props = parse((header + ":" + value).encode())
            if len(props) != 1 or props[0]["value"] != value:
                raise ValueError("invalid vcard passthrough property")
            p = props[0]
            if p["name"] in reserved or p["name"].startswith("X-SORTAL-") or any(k.startswith("X-SORTAL-") for k in p["params"]):
                raise ValueError("vcard passthrough cannot override mapped identities or annotations")
            if p["group"]:
                if p["group"] not in group_bindings:
                    group_bindings[p["group"]] = self.group()
                group = group_bindings[p["group"]]
            else:
                group = self.group()
            path = None
            if p["name"] == "EMAIL":
                existing = parse(("\r\n".join(self.lines) + "\r\n").encode())
                matches = [(i, q) for i, q in enumerate(existing)
                           if q["name"] == "EMAIL" and untext(q["value"]) == untext(value)]
                if len(matches) != 1 or matches[0][0] in overlaid:
                    raise ValueError("EMAIL passthrough requires one matching native email")
                index, original = matches[0]
                overlaid.add(index)
                path = original["params"]["X-SORTAL-PATH"]
                self.add(p["name"], value, path, group, p["params"], raw=True)
                self.lines[index] = self.lines.pop()
            else:
                self.add(p["name"], value, group=group, params=p["params"], raw=True)
            key = quote(header, safe="")
            self.add("X-SORTAL-VCARD-KEY", header, "/vcard/" + key, group)


def encode(contact, uid, store_id, originals, safe_path, version="3.0", warnings=None):
    c = canonical(contact)
    check_keys(c, {"version", "kind", "handle", "names", "emails", "accounts", "links",
                   "affiliations", "photo", "feeds", "vcard"}, "/")
    if c.get("version") != 2:
        raise ValueError("this mapping requires Sortal V2")
    w = Writer(version, warnings if warnings is not None else [])
    w.add("UID", uid, params={"VALUE": "text"} if version == "4.0" else None)
    w.add("X-SORTAL-ID", c["handle"], "/handle")
    w.add("X-SORTAL-STORE", store_id)
    w.add("X-SORTAL-SCHEMA", "2", "/version")
    names = c["names"]
    if not names or any(not isinstance(n, str) or not n for n in names):
        raise ValueError("names must be nonempty strings")
    w.add("FN", names[0], "/names/0", params={"PREF": "1"} if version == "4.0" else None)
    w.add("N", text(names[0]) + ";;;;", raw=True)
    for i, name in enumerate(names[1:], 1):
        w.add("FN" if version == "4.0" else "X-SORTAL-ALT-NAME", name, f"/names/{i}")
    if "kind" in c:
        if c["kind"] not in {"person", "organization"}:
            raise ValueError("unsupported Sortal kind")
        w.add("KIND" if version == "4.0" else "X-ADDRESSBOOKSERVER-KIND",
              "individual" if c["kind"] == "person" else "org", "/kind")
    if c.get("kind") == "organization":
        w.add("X-ABShowAs", "COMPANY")
        w.add("ORG", names[0])
    for key in ("emails", "links", "accounts", "affiliations", "feeds", "vcard"):
        if key in c:
            w.empty(c[key], "/" + key)
    for i, email in enumerate(c.get("emails", [])):
        params = ({"PREF": "1"} if i == 0 else {}) if version == "4.0" else {
            "TYPE": "INTERNET,PREF" if i == 0 else "INTERNET"}
        w.add("EMAIL", email, f"/emails/{i}", params=params)
    for i, link in enumerate(c.get("links", [])):
        group, path = w.group(), f"/links/{i}"
        if isinstance(link, dict):
            check_keys(link, {"url", "label"}, path)
            w.url("URL", link["url"], path + "/url", group)
            if "label" in link:
                w.add("X-ABLabel", link["label"], path + "/label", group)
        else:
            w.url("URL", link, path, group)
    for platform, values in c.get("accounts", {}).items():
        path = "/accounts/" + platform
        if isinstance(values, list):
            w.empty(values, path)
            entries = [(f"{path}/{i}", value) for i, value in enumerate(values)]
        else:
            entries = [(path, values)]
        for path, value in entries:
            group = w.group()
            if platform == "atproto":
                w.atproto(value, path, group)
            else:
                w.social(platform, value, path, group)
                w.add("X-ABLabel", platform, group=group)
    for i, a in enumerate(c.get("affiliations", [])):
        path, group = f"/affiliations/{i}", w.group()
        check_keys(a, {"org", "department", "title", "url", "address", "from", "until"}, path)
        dates = {"X-VALID-" + key.upper(): a[key].replace("-", "") for key in ("from", "until") if key in a}
        components = [a["org"]] + ([a["department"]] if "department" in a else [])
        w.add("ORG", ";".join(text(v) for v in components), path, group, dates, raw=True)
        for key, prop in (("title", "TITLE"), ("address", "ADR"), ("url", "URL")):
            if key not in a:
                continue
            if key == "url":
                w.url(prop, a[key], path + "/" + key, group, dates)
            elif key == "address":
                w.add(prop, ";;" + text(a[key]) + ";;;;", path + "/" + key, group,
                      {**dates, "TYPE": "WORK"}, raw=True)
            else:
                w.add(prop, a[key], path + "/" + key, group, dates)
    for i, feed in enumerate(c.get("feeds", [])):
        path, group = f"/feeds/{i}", w.group()
        check_keys(feed, {"type", "url", "name", "hint", "paused"}, path)
        if feed["type"] not in {*FEED_MEDIA_TYPES, "manual"}:
            raise ValueError("unmapped feed type")
        params = {"X-FEED-TYPE": feed["type"]}
        if version == "4.0" and feed["type"] in FEED_MEDIA_TYPES:
            params["MEDIATYPE"] = FEED_MEDIA_TYPES[feed["type"]]
        w.url("URL", feed["url"], path, group, params)
        if "name" not in feed:
            w.add("X-ABLabel", feed["type"].upper() + " feed", group=group)
        for key, prop in (("name", "X-ABLabel"), ("hint", "X-FEED-HINT"), ("paused", "X-FEED-PAUSED")):
            if key in feed:
                value = ("TRUE" if feed[key] else "FALSE") if key == "paused" else feed[key]
                w.add(prop, value, path + "/" + key, group)
    if c.get("photo"):
        photo, group = c["photo"], w.group()
        if urlsplit(photo).scheme in {"http", "https"}:
            w.url("PHOTO", photo, "/photo", group, {"VALUE": "uri"})
        else:
            data = safe_path(originals, photo).read_bytes()
            if data.startswith(b"\xff\xd8\xff"):
                media = "jpeg"
            elif data.startswith(b"\x89PNG\r\n\x1a\n"):
                media = "png"
            else:
                raise ValueError("photo is neither JPEG nor PNG")
            encoded = base64.b64encode(data).decode("ascii")
            w.add("X-SORTAL-PHOTO-PATH", photo, group=group)
            if version == "3.0":
                w.add("PHOTO", encoded, "/photo", group, {"ENCODING": "b", "TYPE": media.upper()}, raw=True)
            else:
                w.add("PHOTO", f"data:image/{media};base64," + encoded, "/photo", group, raw=True)
    if c.get("vcard"):
        w.passthrough(c["vcard"])
    w.lines.append("END:VCARD")
    return ("\r\n".join(fold(line) for line in w.lines) + "\r\n").encode("utf-8")


def components(value):
    result, start, i = [], 0, 0
    while i < len(value):
        if value[i] == "\\":
            i += 2
        elif value[i] == ";":
            result.append(untext(value[start:i]))
            start = i + 1
            i += 1
        else:
            i += 1
    return result + [untext(value[start:])]


def decode(data):
    """Recover field values from our profile, independently of the snapshot.

    Unknown/unannotated remote fields are not imported by this offline check;
    a live sync must preserve and merge the complete remote property AST.
    """
    props = parse(data)
    if only(props, "X-SORTAL-MAPPING")["value"] not in {"1", "2", "3"}:
        raise ValueError("unsupported Sortal field mapping")
    groups = {}
    for p in props:
        groups.setdefault(p["group"], []).append(p)

    def sibling(p, name):
        values = [q for q in groups[p["group"]] if q["name"] == name]
        if len(values) > 1:
            raise ValueError(f"ambiguous grouped {name}")
        return untext(values[0]["value"]) if values else None

    def original_uri(p):
        raw = sibling(p, "X-SORTAL-ORIGINAL-URL")
        return raw if raw is not None and uri(raw) == p["value"] else p["value"]

    def date(value):
        if len(value) not in {4, 6, 8} or not value.isdecimal():
            raise ValueError("invalid temporal date")
        return "-".join([value[:4]] + ([value[4:6]] if len(value) >= 6 else []) + ([value[6:]] if len(value) == 8 else []))

    assignments, photos = [], {}
    for p in props:
        path = p["params"].get("X-SORTAL-PATH")
        if path is None:
            continue
        name, params, value = p["name"], p["params"], p["value"]
        if name == "X-SORTAL-VCARD-KEY":
            header = untext(value)
            path = "/vcard/" + header.replace("~", "~0").replace("/", "~1")
            original = parse((header + ":").encode())[0]
            def parameters(q):
                result = {}
                for key, val in q["params"].items():
                    if key.startswith("X-SORTAL-"):
                        continue
                    if key in {"TYPE", "VALUE", "ENCODING"}:
                        val = ",".join(sorted(val.lower().split(",")))
                    result[key] = val
                return result
            targets = [q for q in groups[p["group"]] if q["name"] == original["name"]
                       and parameters(q) == parameters(original)]
            if len(targets) != 1:
                raise ValueError("vcard passthrough lost its grouped property")
            value = targets[0]["value"]
        elif name == "X-SORTAL-SCHEMA":
            value = int(value)
        elif name in {"KIND", "X-ADDRESSBOOKSERVER-KIND"}:
            value = {"individual": "person", "org": "organization"}[value]
        elif name in {"SOCIALPROFILE", "X-SOCIALPROFILE"}:
            value = params.get("USERNAME", params.get("X-USER")) or sibling(p, "X-SORTAL-USERNAME")
            if value is None:
                raise ValueError("social profile has no recoverable username")
            platform = params.get("SERVICE-TYPE", params.get("TYPE"))
            if uri(profile_url(platform, value)) != p["value"]:
                raise ValueError("social profile URL/username conflict requires reconciliation")
            mirror = sibling(p, "URL")
            if mirror is not None and mirror != p["value"]:
                raise ValueError("social profile/visible URL conflict requires reconciliation")
        elif name == "X-ATPROTO":
            handle = params["X-ATPROTO-HANDLE"]
            value = handle
            if params.get("X-SORTAL-SHAPE") == "object":
                value = {"handle": handle}
                if "X-ATPROTO-DID" in params:
                    value["did"] = params["X-ATPROTO-DID"]
            if p["value"] != "at://" + params.get("X-ATPROTO-DID", handle):
                raise ValueError("AT Protocol identity conflict requires reconciliation")
            mirror = sibling(p, "URL")
            if mirror is not None and mirror != app_url("bluesky", handle):
                raise ValueError("AT Protocol visible URL conflict requires reconciliation")
        elif name == "ORG":
            cs = components(value)
            if len(cs) > 2:
                raise ValueError("additional organization components require passthrough")
            value = {"org": cs[0]}
            if len(cs) == 2:
                value["department"] = cs[1]
            for key in ("from", "until"):
                if "X-VALID-" + key.upper() in params:
                    value[key] = date(params["X-VALID-" + key.upper()])
        elif name == "ADR":
            cs = components(value)
            if len(cs) != 7 or any(v for i, v in enumerate(cs) if i != 2):
                raise ValueError("structured remote address requires passthrough")
            value = cs[2]
        elif name == "X-FEED" or (name == "URL" and "X-FEED-TYPE" in params):
            value = {"type": params["X-FEED-TYPE"], "url": original_uri(p)}
            expected_media = FEED_MEDIA_TYPES.get(value["type"])
            if "MEDIATYPE" in params and params["MEDIATYPE"].lower() != expected_media:
                raise ValueError("feed type/media type conflict requires reconciliation")
        elif name == "X-FEED-PAUSED":
            value = {"TRUE": True, "FALSE": False}[value.upper()]
        elif name == "URL":
            value = original_uri(p)
        elif name == "PHOTO":
            local = sibling(p, "X-SORTAL-PHOTO-PATH")
            if local is not None:
                encoded = value.split(",", 1)[1] if value.startswith("data:image/") else value
                photos[local] = base64.b64decode(encoded, validate=True)
                value = local
            else:
                value = original_uri(p)
        elif name == "X-SORTAL-EMPTY":
            value = {"array": [], "object": {}}[value]
        else:
            value = untext(value)
        assignments.append((path, value))

    result = {}
    seen = set()
    for path, value in sorted(assignments, key=lambda a: a[0].count("/")):
        if not path.startswith("/") or path in seen:
            raise ValueError("invalid or duplicate Sortal field path")
        seen.add(path)
        keys = [key.replace("~1", "/").replace("~0", "~") for key in path[1:].split("/")]
        target = result
        for i, key in enumerate(keys):
            key = int(key) if isinstance(target, list) else key
            if isinstance(target, list):
                if not isinstance(key, int) or key < 0 or key > 10000:
                    raise ValueError("invalid collection index")
                while len(target) <= key:
                    target.append(None)
            if i == len(keys) - 1:
                if isinstance(target, dict) and key in target:
                    raise ValueError("overlapping Sortal field paths")
                target[key] = value
            else:
                if (isinstance(target, dict) and key not in target) or (isinstance(target, list) and target[key] is None):
                    target[key] = [] if keys[i + 1].isdigit() else {}
                target = target[key]
    for p in props:
        if p["name"] == "X-ATPROTO-APP" and "X-SORTAL-PATH" in p["params"]:
            path = p["params"]["X-SORTAL-PATH"]
            target = result
            for key in path[1:].split("/")[:-2]:
                target = target[int(key)] if isinstance(target, list) else target[key]
            mirror = sibling(p, "URL")
            if mirror is not None and mirror != app_url(untext(p["value"]), target["handle"]):
                raise ValueError("AT Protocol app/visible URL conflict requires reconciliation")
    return result, photos
