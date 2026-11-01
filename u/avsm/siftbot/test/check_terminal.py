#!/usr/bin/env python3
"""Exercise terminal cleanup with synthetic configuration and stalled helpers.

Run after building @u/avsm/siftbot/all:
    python3 u/avsm/siftbot/test/check_terminal.py
"""
import fcntl
import os
from pathlib import Path
import pty
import select
import shutil
import struct
import subprocess
import sys
import tempfile
import termios
import time

ROOT = Path(__file__).resolve().parents[4]
BINARY = (
    Path(sys.argv[1]).resolve() if len(sys.argv) > 1
    else ROOT / "_build/default/u/avsm/siftbot/bin/siftbot_review.exe"
)


def run(binary, root, *, stalled=False, timeout=False, key=b"\x03", demo=False, switch=False):
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 18, 90, 0, 0))
    original = termios.tcgetattr(slave)
    env = dict(os.environ, TERM="xterm-256color")
    for variable, directory in [
        ("XDG_CONFIG_HOME", "config"),
        ("XDG_DATA_HOME", "data"),
        ("XDG_CACHE_HOME", "cache"),
        ("SEKRIT_STORE_DIR", "vault"),
    ]:
        env[variable] = str(root / directory)
    env["SEKRIT_KEY_FILE"] = str(root / "key")
    env["SIFT_TEST_PID"] = str(root / "helper.pid")
    process = subprocess.Popen(
        [str(binary), "--demo" if demo else "--setup", "--profile", "test"],
        stdin=slave, stdout=slave, stderr=slave, env=env,
    )
    output = bytearray()

    def wait_for(predicate, seconds=5):
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline:
            if select.select([master], [], [], 0.05)[0]:
                output.extend(os.read(master, 65536))
            if predicate():
                return
            if process.poll() is not None:
                raise AssertionError(f"UI exited early ({process.returncode}): {output!r}")
        raise AssertionError(f"UI did not respond; termios={termios.tcgetattr(slave)!r}: {output!r}")

    try:
        wait_for(lambda: b"Siftbot" in output)
        if stalled:
            wait_for(lambda: (root / "helper.pid").exists())
            if timeout:
                wait_for(lambda: b"timed out" in output, seconds=13)
        else:
            wait_for(lambda: b"Siftbot profile" in output if not demo else b"Revision" in output)
            if switch:
                os.write(master, b"p")
                wait_for(lambda: b"Siftbot profile" in output)
            if not demo:
                os.write(master, b"\t\t\t\x1b[200~synthetic-token\x1b[201~")
                wait_for(lambda: b"********" in output)
                assert b"synthetic-token" not in output, "token echoed to terminal"
        os.write(master, key)
        wait_for(lambda: process.poll() is not None)
        while select.select([master], [], [], 0)[0]:
            output.extend(os.read(master, 65536))
        assert process.returncode == 0, output
        restored = termios.tcgetattr(slave)
        # Darwin can set PENDIN while switching back to canonical input.
        restored[3] &= ~getattr(termios, "PENDIN", 0)
        original[3] &= ~getattr(termios, "PENDIN", 0)
        assert restored == original, "terminal settings not restored"
        assert b"\x1b[?1049l" in output, "alternate screen not released"
        assert b"\x1b[?2004l" in output, "bracketed paste not disabled"
        if stalled:
            pid = int((root / "helper.pid").read_text())
            try:
                os.kill(pid, 0)
            except ProcessLookupError:
                pass
            else:
                raise AssertionError("setup helper survived UI exit")
    finally:
        if process.poll() is None:
            process.kill()
            process.wait()
        if stalled and (root / "helper.pid").exists():
            try:
                os.kill(int((root / "helper.pid").read_text()), 9)
            except ProcessLookupError:
                pass
        os.close(master)
        os.close(slave)


with tempfile.TemporaryDirectory(prefix="sift-terminal-") as temp:
    root = Path(temp)
    run(BINARY, root)
    run(BINARY, root, demo=True)
    run(BINARY, root, demo=True, switch=True)
    fake = root / "bin"
    fake.mkdir()
    shutil.copy2(BINARY, fake / BINARY.name)
    helper = fake / "siftbot_config.exe"
    helper.write_text(
        "#!/usr/bin/env python3\n"
        "import os, time\n"
        "with open(os.environ['SIFT_TEST_PID'], 'w') as f:\n"
        "    f.write(str(os.getpid()))\n"
        "time.sleep(60)\n"
    )
    helper.chmod(0o700)
    for key, timeout in [(b"\x03", False), (b"\x1b", False), (b"\x03", True)]:
        (root / "helper.pid").unlink(missing_ok=True)
        run(fake / BINARY.name, root, stalled=True, key=key, timeout=timeout)
print("Terminal setup/review exit, restoration, helper cancellation and timeout passed")
