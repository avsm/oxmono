#!/usr/bin/env python3
"""Offline contract inventory guards; behavioral assertions live in OCaml tests."""
import ast
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
reference = json.loads((HERE / 'reference.json').read_text())
bindings = json.loads((HERE / 'bindings.json').read_text())
errors = []

def check(condition, message):
    if not condition:
        errors.append(message)

def normalized_path(path):
    path = path.removeprefix('/api/v1')
    return re.sub(r'\{[^}]+\}|%[ds]', '{}', path)

check(set(reference['helpers']) == set(bindings), 'Python helper inventory differs from binding inventory')
for helper, contract in reference['helpers'].items():
    if helper not in bindings:
        continue
    binding = bindings[helper]
    module = binding['entrypoint']['module']
    function = binding['entrypoint']['function']
    source = ROOT / 'lib/zulip_eio' / (module.lower() + '.ml')
    interface = source.with_suffix('.mli')
    check(interface.exists(), f'{helper}: missing interface {interface}')
    if not interface.exists():
        continue
    check(re.search(r'^val ' + re.escape(function) + r'\s*:', interface.read_text(), re.M),
          f'{helper}: missing public binding {module}.{function}')
    operation = binding['operation']
    check(operation == contract['operation'], f'{helper}: method/path changed without updating the reference')
    if operation is None:
        continue
    check(operation in reference['operations'], f'{helper}: unknown server operation {operation}')
    if operation not in reference['operations']:
        continue
    expected = set(reference['operations'][operation]['parameters'])
    check(expected == set(binding['parameters']),
          f'{helper}: unaccounted parameters {sorted(expected ^ set(binding["parameters"]))}')
    for param, evidence in binding['parameters'].items():
        evidence_file = ROOT / evidence['evidence']
        check(evidence_file.exists() and evidence['token'] in evidence_file.read_text(),
              f'{helper}.{param}: missing source evidence ({evidence["evidence"]})')
    # This guards literal route changes, including formatted path components.
    # Shared helpers may assemble the method/path separately; request mocks test
    # the actual association and serialization at runtime.
    method, path = operation.split(' ', 1)
    text = source.read_text()
    route_literals = re.findall(r'"([^"\s]*)"', text)
    normalized = [normalized_path('/' + x.lstrip('/')) for x in route_literals]
    parts = re.split(r'\{[^}]+\}', path)
    check(normalized_path(path) in normalized or all(not part or normalized_path(part) in normalized for part in parts),
          f'{helper}: expected route components absent from {source.name}: {path}')
    if method != 'POST' or 'multipart' not in text:
        check('`' + method in text, f'{helper}: expected method absent from {source.name}: {method}')

for name in ['me_pointer', 'update_me_pointer', 'get_settings', 'get_muted_users']:
    check(not re.search(r'^val '+name+r'\s*:', (ROOT/'lib/zulip_eio/users.mli').read_text(), re.M),
          f'obsolete Users.{name} route reintroduced')

# Optional development check against a different Python checkout. Ordinary
# dune runtest uses only the frozen inventory and Python's standard library.
if len(sys.argv) == 2:
    tree = ast.parse(Path(sys.argv[1]).read_text())
    client = next(n for n in tree.body if isinstance(n, ast.ClassDef) and n.name == 'Client')
    methods = {n.name for n in client.body if isinstance(n, ast.FunctionDef) and not n.name.startswith('_')}
    check(methods == set(reference['helpers']), 'Python checkout helper names differ from frozen reference')

if errors:
    print('\n'.join(errors), file=sys.stderr)
    sys.exit(1)
operations = {entry['operation'] for entry in bindings.values()} - {None}
parameters = sum(len(reference['operations'][op]['parameters']) for op in operations)
print(f'Parity inventory: {len(bindings)} Python helpers, {len(operations)} canonical operations, {parameters} wire parameters accounted for.')
