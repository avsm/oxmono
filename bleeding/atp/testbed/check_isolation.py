#!/usr/bin/env python3
"""Test Docker isolation using cached images and local canaries only.

This is a network prerequisite, not the Tangled integration testbed. It never
pulls images, resolves a real ATP hostname or connects to an external IP.
"""

import argparse
import json
import subprocess
import time
import uuid


def docker(*args, **kwargs):
    return subprocess.check_output(
        ["docker", *args], text=True, timeout=30, **kwargs).strip()


SERVER = """
import socket
s = socket.socket()
s.bind(('0.0.0.0', 8080))
s.listen()
print('ready', flush=True)
while True:
    c, _ = s.accept()
    with c:
        c.sendall(b'local canary\\n')
"""

PROBE = """
import json, socket, sys
from pathlib import Path
peer, canary = sys.argv[1:]
routes = Path('/proc/net/route').read_text().splitlines()[1:]
assert not any(row.split()[1] == '00000000' for row in routes), 'default route'
ipv6 = Path('/proc/net/ipv6_route').read_text().splitlines()
assert not any(row.split()[0] == '0' * 32 and row.split()[1] == '00'
               and row.split()[-1] != 'lo' for row in ipv6), 'IPv6 default route'
with socket.create_connection((peer, 8080), 2) as s:
    assert s.recv(100) == b'local canary\\n', 'same-network canary failed'
try:
    s = socket.create_connection((canary, 8080), 1)
except OSError:
    pass
else:
    s.close()
    raise AssertionError('cross-network canary reachable')
try:
    socket.getaddrinfo('must-not-resolve.invalid', 80)
except socket.gaierror:
    pass
else:
    raise AssertionError('unexpected external DNS resolution')
print(json.dumps({'local_canary': 'reachable', 'cross_network_canary': 'blocked',
                  'default_routes': 'absent', 'unknown_dns': 'blocked'}))
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--image", default="python:3.13-slim",
                        help="already-cached Python image (never pulled)")
    args = parser.parse_args()
    image = json.loads(docker("image", "inspect", args.image))[0]["Id"]
    prefix = "oxmono-atp-isolation-" + uuid.uuid4().hex[:12]
    networks = []
    containers = []
    try:
        for suffix in ("test", "canary"):
            name = prefix + "-" + suffix
            docker("network", "create", "--internal", "--driver", "bridge",
                   "--opt", "com.docker.network.bridge.gateway_mode_ipv4=isolated",
                   "--opt", "com.docker.network.bridge.gateway_mode_ipv6=isolated",
                   name)
            networks.append(name)
            model = json.loads(docker("network", "inspect", name))[0]
            assert model["Internal"] and not model["EnableIPv6"]
            assert model["Options"][
                "com.docker.network.bridge.gateway_mode_ipv4"] == "isolated"

        def create(suffix, network, script, *script_args):
            name = prefix + "-" + suffix
            docker("create", "--pull", "never", "--name", name,
                   "--network", network, "--dns", "127.0.0.1",
                   "--dns-option", "timeout:1", "--dns-option", "attempts:1",
                   "--cap-drop", "ALL", "--security-opt", "no-new-privileges",
                   "--read-only", "--pids-limit", "32", "--memory", "64m",
                   "--cpus", "1", "--env", "PYTHONDONTWRITEBYTECODE=1",
                   "--entrypoint", "python3", image, "-u", "-c", script,
                   *script_args)
            containers.append(name)
            model = json.loads(docker("inspect", name))[0]
            assert set(model["NetworkSettings"]["Networks"]) == {network}
            assert model["HostConfig"]["Dns"] == ["127.0.0.1"]
            assert not model["HostConfig"]["Privileged"]
            assert not model["HostConfig"]["PortBindings"]
            assert not model["Mounts"]
            return name

        addresses = []
        for suffix, network in zip(("peer", "outside"), networks):
            name = create(suffix, network, SERVER)
            docker("start", name)
            deadline = time.monotonic() + 10
            while "ready" not in docker("logs", name):
                if time.monotonic() > deadline:
                    raise TimeoutError("local canary did not start")
                time.sleep(0.1)
            model = json.loads(docker("inspect", name))[0]
            addresses.append(model["NetworkSettings"]["Networks"][network]["IPAddress"])
        probe = create("probe", networks[0], PROBE, *addresses)
        output = docker("start", "--attach", probe)
        model = json.loads(docker("inspect", probe))[0]
        if model["State"]["ExitCode"] != 0:
            raise RuntimeError("isolation probe failed")
        print(json.dumps({"image": image, "probe": json.loads(output)}, indent=2))
    finally:
        errors = []
        for name in reversed(containers):
            try:
                docker("rm", "--force", name)
            except (subprocess.SubprocessError, OSError) as error:
                errors.append(f"container {name}: {error}")
        for name in reversed(networks):
            try:
                docker("network", "rm", name)
            except (subprocess.SubprocessError, OSError) as error:
                errors.append(f"network {name}: {error}")
        if errors:
            raise RuntimeError("cleanup failed: " + "; ".join(errors))


if __name__ == "__main__":
    main()
