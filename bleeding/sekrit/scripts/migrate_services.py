# /// script
# requires-python = ">=3.9"
# dependencies = ["tomlkit==0.13.3"]
# ///
"""Discover service credentials and migrate them through the Sekrit CLI."""
from __future__ import annotations

import argparse
import datetime
import fcntl
import hashlib
import hmac
import json
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import sys
import tempfile

import tomlkit

MAGIC = b"SEKRIT\x00\x01"
MAX_CONFIG = 1024 * 1024


class Refused(Exception):
    pass


def quoted(path):
    return json.dumps(str(path))


def absolute(path, base=None):
    path = Path(os.path.expanduser(str(path)))
    return Path(os.path.abspath(path if path.is_absolute() else (base or Path.cwd()) / path))


def env_path(name, default):
    value = os.environ.get(name) or str(default)
    if not Path(value).is_absolute():
        raise Refused(f"{name} must be absolute")
    return Path(value)


def read(path, private=False, limit=MAX_CONFIG):
    try:
        fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
        with os.fdopen(fd, "rb") as stream:
            info = os.fstat(stream.fileno())
            if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid():
                raise Refused(f"Expected an owned regular file: {quoted(path)}")
            if private and stat.S_IMODE(info.st_mode) & 0o077:
                raise Refused(f"Remove group/other permissions first: {quoted(path)}")
            value = stream.read(limit + 1)
            if len(value) > limit:
                raise Refused(f"File exceeds migration limit: {quoted(path)}")
            return value
    except OSError:
        raise Refused(f"Cannot read file safely: {quoted(path)}") from None


def line(value):
    if value.endswith(b"\n"):
        value = value[:-1]
        if value.endswith(b"\r"):
            value = value[:-1]
    if not value or any(c in value for c in (b"\n", b"\r", b"\x00")):
        raise Refused("A credential must be one nonempty line")
    return value


def private_dir(path):
    path.mkdir(mode=0o700, parents=True, exist_ok=True)
    check_directory(path)


def check_directory(path, private=True):
    info = path.lstat()
    if (not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid()
            or stat.S_IMODE(info.st_mode) & (0o077 if private else 0o022)):
        raise Refused(f"Expected an owned private directory: {quoted(path)}")


def sync_dir(path):
    fd = os.open(path, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)


def create(path, value):
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600)
    with os.fdopen(fd, "wb") as stream:
        stream.write(value)
        stream.flush()
        os.fsync(stream.fileno())


def replace(path, before, after):
    fd, temporary = tempfile.mkstemp(prefix=".sekrit-migrate-", dir=path.parent)
    try:
        with os.fdopen(fd, "wb") as stream:
            stream.write(after)
            stream.flush()
            os.fsync(stream.fileno())
        if read(path) != before:
            raise Refused(f"Configuration changed during migration: {quoted(path)}")
        os.replace(temporary, path)
        sync_dir(path.parent)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


def run(command, operation):
    result = subprocess.run(command, stdin=subprocess.DEVNULL,
                            stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    if result.returncode:
        # Child diagnostics can contain parsed credentials. Never print them.
        raise Refused(f"{operation} failed (exit {result.returncode}); no child output displayed")


def binary(explicit, name, relative):
    if explicit:
        result = shutil.which(explicit)
    else:
        parents = Path(__file__).resolve().parents
        local = parents[3] / "_build/default" / relative if len(parents) > 3 else None
        result = str(local) if local is not None and os.access(local, os.X_OK) else shutil.which(name)
    if result is None:
        raise Refused(f"Cannot find {name}; build/install the updated binary or pass --{name}-bin")
    return str(absolute(result))


class Plan:
    def __init__(self, args):
        self.args = args
        home = env_path("HOME", Path.home())
        self.config_home = env_path("XDG_CONFIG_HOME", home / ".config")
        self.state_home = env_path("XDG_STATE_HOME", home / ".local/state")
        self.key = env_path("SEKRIT_KEY_FILE", self.config_home / "sekrit/master.key")
        self.sources = {}
        self.edits = {}
        self.imports = {}
        self.references = {}
        self.crow = {}
        self.sealed_crow = False
        self.notes = []
        self.seen = set()

    def app_dir(self, app):
        return env_path(app.upper() + "_CONFIG_DIR", self.config_home) / app

    def remember(self, path, private=False, limit=MAX_CONFIG):
        value = read(path, private, limit)
        if path in self.sources and self.sources[path] != value:
            raise Refused(f"Source changed while scanning: {quoted(path)}")
        self.sources[path] = value
        return value

    def add_secret(self, path, app, role, value, limit=65536):
        value = line(value)
        if len(value) > limit:
            raise Refused(f"Credential exceeds {limit}-byte limit: {quoted(path)}")
        digest = hashlib.sha256(str(path).encode()).hexdigest()[:16]
        name = f"migration/{app}/{digest}/{role}"
        if name in self.imports and not hmac.compare_digest(self.imports[name], value):
            raise Refused("Conflicting migration secret names")
        self.imports[name] = value
        return "sekrit:" + name

    def reference(self, value, path, app, role, base, limit=65536):
        if not isinstance(value, str) or not value:
            raise Refused(f"Invalid credential reference: {quoted(path)}")
        if value.startswith("sekrit:"):
            name = value[7:]
            if (len(name) > 256 or not re.fullmatch(r"[A-Za-z0-9._/-]+", name)
                    or any(p in ("", ".", "..") for p in name.split("/"))):
                raise Refused(f"Invalid named reference: {quoted(path)}")
            self.references[name] = min(self.references.get(name, limit), limit)
            return value
        source = absolute(value, base)
        return self.add_secret(path, app, role,
                               self.remember(source, private=True, limit=limit), limit)

    def toml(self, app, path, required=False):
        path = absolute(path)
        if (app, path) in self.seen:
            return
        self.seen.add((app, path))
        if not path.exists() and not path.is_symlink() and not required:
            return
        raw = self.remember(path)
        try:
            document = tomlkit.parse(raw.decode("utf-8"))
        except Exception:
            raise Refused(f"Invalid TOML: {quoted(path)}") from None
        before = document.unwrap()
        if app == "dooit" and "webdav" in document:
            table = document["webdav"]
            keys = [k for k in ("password", "password_file", "secret") if k in table]
            if len(keys) != 1:
                raise Refused(f"Expected one WebDAV credential field: {quoted(path)}")
            key = keys[0]
            if key == "password":
                self.remember(path, private=True)
                value = self.add_secret(path, app, "webdav", table[key].encode())
            else:
                value = self.reference(table[key], path, app, "webdav", path.parent)
            if key != "secret":
                del table[key]
            table["secret"] = value
        elif app in ("sortal", "termanil"):
            table = document.get("carddav") if app == "sortal" else document.get("contacts", {}).get("carddav")
            if table is not None:
                default = str(self.app_dir("sortal") / "carddav-password") if app == "sortal" else None
                value = table.get("password_file", default)
                # Sortal resolves relative credentials against its working directory.
                base = absolute(self.args.sortal_cwd) if app == "sortal" else path.parent
                if app == "sortal" and not table.get("username"):
                    self.notes.append("Skipped unconfigured Sortal CardDAV: " + quoted(path))
                else:
                    table["password_file"] = self.reference(value, path, app, "carddav", base)
            if app == "termanil":
                linked = document.get("dooit", {}).get("config")
                if linked:
                    self.toml("dooit", absolute(linked, path.parent), required=True)
        elif app == "bushel":
            table = document.get("embeddings", {})
            if "api_key_file" in table:
                table["api_key_file"] = self.reference(table["api_key_file"], path, app,
                                                       "embeddings", path.parent, 16384)
        if before != document.unwrap():
            self.edits[path] = tomlkit.dumps(document).encode("utf-8")

    def jmap(self, directory):
        if not directory.exists():
            return
        check_directory(directory)
        for path in sorted(directory.iterdir()):
            if path.name in (".", "..") or not re.fullmatch(r"[A-Za-z0-9._-]+", path.name):
                continue
            raw = self.remember(path, private=True)
            fields = {}
            lines = raw.decode("utf-8").splitlines(keepends=True)
            for text in lines:
                key, sep, value = text.rstrip("\r\n").partition("=")
                key = key.strip()
                if sep and key in ("url", "auth", "user", "secret", "secret_ref"):
                    if key in fields:
                        raise Refused(f"Duplicate JMAP profile field: {quoted(path)}")
                    fields[key] = value
            if fields.get("auth", "").strip().lower() not in ("basic", "bearer") or not fields.get("url"):
                raise Refused(f"Invalid JMAP profile: {quoted(path)}")
            if ("secret" in fields) == ("secret_ref" in fields):
                raise Refused(f"Expected one JMAP credential field: {quoted(path)}")
            basic = fields["auth"].strip().lower() == "basic"
            if "secret_ref" in fields:
                reference = fields["secret_ref"].strip()
                if not reference.startswith("sekrit:") and not Path(reference).is_absolute():
                    raise Refused(f"JMAP file references must be absolute: {quoted(path)}")
                if basic and fields.get("user", "").strip():
                    raise Refused(f"JMAP basic secret_ref already includes its user: {quoted(path)}")
                value = self.reference(reference, path, "jmap", "credential", path.parent)
                if value == reference:
                    continue
            else:
                value = fields["secret"]
                line(value.encode())
                if basic:
                    user = fields.get("user", "").strip()
                    if not user or ":" in user:
                        raise Refused(f"Invalid JMAP basic-auth user: {quoted(path)}")
                    value = user + ":" + value
                value = self.add_secret(path, "jmap", "credential", value.encode())
            after = []
            for text in lines:
                key = text.partition("=")[0].strip()
                if key in ("secret", "secret_ref"):
                    after.append("secret_ref=" + value + "\n")
                elif basic and key == "user":
                    continue
                else:
                    after.append(text)
            self.edits[path] = "".join(after).encode()

    def discover(self):
        for app in ("dooit", "sortal", "termanil", "bushel"):
            self.toml(app, self.app_dir(app) / "config.toml")
            for path in getattr(self.args, app + "_config"):
                self.toml(app, path, required=True)
        directories = [self.config_home / "jmap/profiles"] + [absolute(p) for p in self.args.jmap_profiles]
        for directory in dict.fromkeys(directories):
            self.jmap(directory)
        root = self.app_dir("crowthebot") / "secrets"
        if root.exists():
            check_directory(root.parent, private=False)
            check_directory(root)
            for profile in sorted(root.iterdir()):
                if not profile.is_dir() or profile.is_symlink():
                    raise Refused(f"Unexpected Crow profile entry: {quoted(profile)}")
                check_directory(profile)
                files = []
                for path in sorted(profile.glob("*.json")):
                    raw = self.remember(path, private=True, limit=MAX_CONFIG + 36)
                    if raw.startswith(MAGIC):
                        self.sealed_crow = True
                    else:
                        try:
                            data = json.loads(raw)
                            names = [entry["name"] for entry in data["entries"]]
                            valid = (data["version"] == 1 and len(names) == len(set(names))
                                     and all("value" in entry for entry in data["entries"])
                                     and (data.get("selected") is None or data["selected"] in names))
                        except Exception:
                            valid = False
                        if not valid:
                            raise Refused(f"Invalid Crow secret configuration: {quoted(path)}")
                        files.append(path)
                if list(profile.glob("*.json")):
                    self.crow[profile.name] = files
        if (self.sealed_crow or self.references) and not self.key.exists():
            raise Refused("Existing encrypted data/references need their original master key; refusing initialization")
        self.notes.append("Matrix sessions and OwnTracks credentials are not migrated.")
        self.notes.append("Credentials supplied only through service arguments/environment are not discovered.")

    def unchanged(self):
        for path, original in self.sources.items():
            if read(path, limit=max(MAX_CONFIG, len(original))) != original:
                raise Refused(f"Source changed since discovery: {quoted(path)}")

    def apply(self):
        if not (self.imports or self.references or self.crow or self.edits
                or self.args.restart_systemd_user or self.args.restart_launchd):
            print("Nothing to migrate.")
            return
        sekrit = binary(self.args.sekrit_bin, "sekrit", "bleeding/sekrit/bin/main.exe")
        crow = binary(self.args.crowthebot_bin, "crowthebot", "avsm/crowthebot/bin/main.exe") if self.crow else None
        restarts = []
        for unit in self.args.restart_systemd_user:
            if not unit or unit.startswith("-"):
                raise Refused("Invalid systemd unit")
            command = shutil.which("systemctl")
            if not command:
                raise Refused("systemctl is unavailable")
            restarts.append([command, "--user", "try-restart", "--", unit])
        for target in self.args.restart_launchd:
            if not any(target.startswith(f"{domain}/{os.getuid()}/") for domain in ("gui", "user")):
                raise Refused("launchd target must belong to this user's gui/UID or user/UID domain")
            command = shutil.which("launchctl")
            if not command:
                raise Refused("launchctl is unavailable")
            restarts.append([command, "kickstart", "-k", target])
        state = self.state_home / "sekrit"
        private_dir(state)
        fd = os.open(state / ".migration.lock", os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
        with os.fdopen(fd, "r+b") as lock:
            info = os.fstat(lock.fileno())
            if info.st_uid != os.getuid() or info.st_mode & 0o077 or info.st_nlink != 1:
                raise Refused("Migration lock must be owned, private and unlinked elsewhere")
            try:
                fcntl.lockf(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except OSError:
                raise Refused("Another migration is running") from None
            self.unchanged()
            if self.imports or self.references or self.crow:
                run([sekrit, "init"], "Sekrit initialization")
            with tempfile.TemporaryDirectory(prefix=".migration-", dir=state) as temporary:
                temp = Path(temporary)
                def export(name):
                    output = temp / "export"
                    run([sekrit, "export", name, "--file", str(output)], "Secret verification")
                    try:
                        return read(output, private=True, limit=65536)
                    finally:
                        output.unlink(missing_ok=True)
                # Verify pre-existing references before rewriting any configuration.
                for name, limit in self.references.items():
                    value = line(export(name))
                    if len(value) > limit:
                        raise Refused("An existing named secret exceeds its consumer's size limit")
                listing = subprocess.run([sekrit, "list"], stdout=subprocess.PIPE,
                                         stderr=subprocess.PIPE, stdin=subprocess.DEVNULL) if self.imports else None
                if listing is not None and listing.returncode:
                    raise Refused("Cannot enumerate the Sekrit store")
                existing = set(listing.stdout.decode().splitlines()) if listing else set()
                for name, value in self.imports.items():
                    if name in existing:
                        if not hmac.compare_digest(export(name), value):
                            raise Refused("An existing migration secret differs; refusing to overwrite it")
                    else:
                        source = temp / "import"
                        create(source, value)
                        try:
                            run([sekrit, "put", name, "--file", str(source)], "Secret import")
                        finally:
                            source.unlink()
                        if not hmac.compare_digest(export(name), value):
                            raise Refused("Imported secret verification failed")
            self.unchanged()
            originals = list(self.edits) + [path for files in self.crow.values() for path in files]
            backup = None
            if originals:
                parent = state / "migrations"
                private_dir(parent)
                stamp = datetime.datetime.now(datetime.timezone.utc).strftime("%Y%m%dT%H%M%SZ-")
                backup = Path(tempfile.mkdtemp(prefix=stamp, dir=parent))
                manifest = []
                for index, path in enumerate(dict.fromkeys(originals)):
                    name = f"{index:04d}.backup"
                    create(backup / name, self.sources[path])
                    manifest.append({"path": str(path), "backup": name,
                                     "sha256": hashlib.sha256(self.sources[path]).hexdigest()})
                create(backup / "manifest.json", (json.dumps(manifest, indent=2) + "\n").encode())
                sync_dir(backup)
                sync_dir(parent)
                print("Private backups: " + quoted(backup), flush=True)
            for profile in self.crow:
                run([crow, "config", "migrate", "--profile=" + profile], "Crow profile migration")
                print("Migrated/verified Crow profile " + quoted(profile), flush=True)
            for path, after in self.edits.items():
                replace(path, self.sources[path], after)
                print("Updated " + quoted(path), flush=True)
            for command in restarts:
                run(command, "Service restart")
            print("Migration complete. Source credential files were retained.")
            if not restarts:
                print("Restart long-running consumers to reload credentials.")


def main(argv=None):
    parser = argparse.ArgumentParser(description="Migrate this user's configured services to Sekrit. Default: dry run.")
    action = parser.add_mutually_exclusive_group()
    action.add_argument("--apply", action="store_true", help="Import, back up and update configurations")
    action.add_argument("--dry-run", action="store_true", help="Preview without writing credentials or configuration (default)")
    for app in ("dooit", "sortal", "termanil", "bushel"):
        parser.add_argument(f"--{app}-config", action="append", default=[], metavar="PATH", help="Also migrate this configuration; repeatable")
    parser.add_argument("--jmap-profiles", action="append", default=[], metavar="DIR", help="Also migrate this profile directory; repeatable")
    parser.add_argument("--sortal-cwd", default=str(Path.cwd()), metavar="DIR", help="Working directory used by Sortal for relative password files")
    parser.add_argument("--sekrit-bin", metavar="PATH")
    parser.add_argument("--crowthebot-bin", metavar="PATH")
    parser.add_argument("--restart-systemd-user", action="append", default=[], metavar="UNIT", help="Try-restart this user unit after successful apply; repeatable")
    parser.add_argument("--restart-launchd", action="append", default=[], metavar="TARGET", help="Kickstart this gui/UID/LABEL or user/UID/LABEL after apply; repeatable")
    args = parser.parse_args(argv)
    try:
        plan = Plan(args)
        plan.discover()
        print(f"{'Apply' if args.apply else 'Dry run'}: {len(plan.imports)} secret imports, "
              f"{len(plan.edits)} config updates, {len(plan.crow)} Crow profiles, "
              f"{len(plan.references)} existing references to verify.")
        print("Master key: " + quoted(plan.key))
        for path in plan.edits:
            print("Update " + quoted(path))
        for profile, files in plan.crow.items():
            print(f"Crow {quoted(profile)}: {len(files)} plaintext files to encrypt")
        for note in plan.notes:
            print(note)
        if args.apply:
            plan.apply()
        else:
            print("No credentials or configuration changed. Rerun with --apply.")
        return 0
    except Refused as error:
        print("Error: " + str(error), file=sys.stderr)
    except Exception as error:
        # TOML, subprocess and filesystem errors must not print secret values.
        print(f"Error: migration failed ({type(error).__name__}); no diagnostic contents displayed", file=sys.stderr)
    print("Completed steps are retained. Fix the error and rerun the same command.", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
