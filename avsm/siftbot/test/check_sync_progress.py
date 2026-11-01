"""Check the real progress renderer and SIGINT cleanup without mail access."""
import fcntl
import os
import pty
import select
import signal
import struct
import subprocess
import sys
import termios
import time
from pathlib import Path

binary = str(Path(sys.argv[1]).resolve())
def check_terminal(kind, interrupt=False, empty=False):
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 20, 100, 0, 0))
    original = termios.tcgetattr(slave)
    process = subprocess.Popen(
        [binary, kind] + (["interrupt"] if interrupt else []) + (["empty"] if empty else []),
        stdin=slave, stdout=slave, stderr=slave,
        env=dict(os.environ, TERM="xterm-256color"),
    )
    output = bytearray()
    sent = False
    deadline = time.monotonic() + 8
    try:
        while process.poll() is None:
            if time.monotonic() > deadline:
                raise AssertionError("progress process failed to stop")
            if select.select([master], [], [], 0.05)[0]:
                output.extend(os.read(master, 65536))
            label = b"Classifying emails" if kind == "classify" else b"Importing emails"
            if interrupt and not sent and label in output:
                process.send_signal(signal.SIGINT)
                sent = True
        while select.select([master], [], [], 0)[0]:
            output.extend(os.read(master, 65536))
        assert process.returncode == 0, output
        assert termios.tcgetattr(slave) == original
        if b"\x1b[?25l" in output:
            assert output.rfind(b"\x1b[?25h") > output.rfind(b"\x1b[?25l"), output
        if interrupt:
            assert b"Interrupted" in output, output
        else:
            label = b"Classifying emails" if kind == "classify" else b"Loading thread context"
            assert b"Complete" in output, output
            if empty:
                assert b"Classification up to date" in output, output
            else:
                assert label in output and b"%" in output, output
            if kind == "classify" and not empty:
                assert b"triage=relevant" in output, output
                assert b"delivery=direct" in output, output
                assert b"attention=needs-reply" in output, output
                assert b"Synthetic subject" in output, output
                assert b"\x1b[2J" not in output, output
    finally:
        if process.poll() is None:
            process.kill()
            process.wait()
        os.close(master)
        os.close(slave)

for kind in ("sync", "classify"):
    check_terminal(kind)
    check_terminal(kind, interrupt=True)
    plain = subprocess.run([binary, kind], capture_output=True, check=True, timeout=8)
    assert b"\x1b[" not in plain.stderr
    assert (b"Classified 2 emails" if kind == "classify" else b"Sync complete") in plain.stderr
    if kind == "classify":
        assert plain.stderr.count(b"triage=relevant") == 2, plain.stderr
        assert b"Synthetic subject" in plain.stderr, plain.stderr
check_terminal("classify", empty=True)
print("Sync and classification progress, plain output and interrupt cleanup passed")
