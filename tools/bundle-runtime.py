#!/usr/bin/env python3
"""Collect Linux ELF runtime libraries for a bundle built on the target image."""
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

root=Path(sys.argv[1]).resolve()
native=root/'lib/native'
native.mkdir(parents=True,exist_ok=True)
records=[]
for path in sorted(root.rglob('*')):
    if not path.is_file() or native in path.parents:
        continue
    with path.open('rb') as f:
        if f.read(4)!=b'\x7fELF':continue
    proc=subprocess.run(['ldd',str(path)],capture_output=True,text=True)
    if 'not found' in proc.stdout:
        raise SystemExit(f'Missing runtime library for {path}:\n{proc.stdout}')
    if proc.returncode and 'not a dynamic executable' not in proc.stderr and 'statically linked' not in proc.stdout:
        raise SystemExit(proc.stderr)
    libraries=[]
    for line in proc.stdout.splitlines():
        match=re.search(r'(?:=>\s+)?(/\S+)\s+\(',line)
        if not match:continue
        source=Path(match[1]);libraries.append(str(source))
        # The target image supplies the ELF interpreter and matching glibc.
        if source.name.startswith(('ld-linux','ld64')):continue
        target=native/source.name
        if target.exists() and target.read_bytes()!=source.read_bytes():
            raise SystemExit(f'Conflicting shared library: {source.name}')
        if not target.exists():shutil.copy2(source,target)
    records.append({'file':str(path.relative_to(root)),'libraries':libraries})
(root/'runtime-libraries.json').write_text(json.dumps(records,indent=2)+'\n')
