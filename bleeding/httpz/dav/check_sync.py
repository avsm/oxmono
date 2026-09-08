#!/usr/bin/env python3
"""Read-only parity check for DAV support in OxMono and both HTTPz siblings."""
import argparse
from pathlib import Path
import re

TREES = ('httpz/dav', 'fetch/dav', 'proffer/dav', 'fetch/test/webdav')
FILES = ('fetch/WEBDAV.md',)


def stock_source(relative, text):
    """Only the syntax differences used by these DAV modules, not a general port."""
    if relative.name == 'dune':
        text = text.replace(' stdlib_stable', '').replace(
            ' stdlib_upstream_compatible', '')
        return '\n'.join(line.rstrip() for line in text.split('\n'))
    if relative.suffix not in ('.ml', '.mli'):
        return text
    text = re.sub(r'@@\s*(portable|global)(?:\s+contended)?', '', text)
    text = re.sub(r'@\s*(local|portable|global)(?:\s+contended)?', '', text)
    text = re.sub(r'\b(local_|global_|exclave_|stack_)\s*', '', text)
    text = text.replace(' : value mod portable contended', '')
    text = text.replace(' : immutable_data', '')
    text = text.replace('Hashtbl.MakePortable', 'Hashtbl.Make')
    text = re.sub(r'\[@(?:zero_alloc|inline)[^]]*\]', '', text)
    text = text.replace('X509.Authenticator.chain_of_trust_no_crl',
                        'X509.Authenticator.chain_of_trust')
    if relative.name in ('httpz_dav.ml', 'fetch_dav.ml', 'proffer_dav.ml',
                          'proffer_dav_eio.ml'):
        text = re.sub(r'\bNull\b', 'None', text)
        text = re.sub(r'\bThis\b', 'Some', text)
    if relative.name == 'proffer_dav.ml':
        text = text.replace('let #(valid, time)', 'let (valid, time)')
        text = text.replace('(Stdlib_stable.Int16_u.of_int 0)', '0')
        text = text.replace('(Stdlib_stable.Int16_u.of_int (String.length s))',
                            '(String.length s)')
        text = text.replace('(Stdlib_upstream_compatible.Float_u.to_float time)',
                            'time')
        text = text.replace(
            '(Stdlib_upstream_compatible.Float_u.of_float entry.modified)',
            'entry.modified')
        text = text.replace('module U = Httpz_uri', '''module U = struct
  include Httpz_uri
  let has_userinfo u = encoded_userinfo u <> None
  let has_query u = encoded_query u <> None
  let has_fragment u = encoded_fragment u <> None
end''')
    if relative.name == 'fixture.ml':
        text = text.replace('Proffer_httpz.globalize_event event', 'event')
    return '\n'.join(line.rstrip() for line in text.split('\n'))


def files(stack):
    result = {Path(p) for p in FILES}
    for tree in TREES:
        for path in (stack / tree).rglob('*'):
            if path.is_file() and not any(p in ('__pycache__', '_build') for p in path.parts):
                result.add(path.relative_to(stack))
    return result


def main():
    stack = Path(__file__).resolve().parents[2]
    default_mono = stack.parent if stack.name == 'bleeding' else stack.parent / 'oxmono'
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--monorepo', type=Path, default=default_mono)
    parser.add_argument('--oxcaml', type=Path)
    parser.add_argument('--ocaml', type=Path)
    args = parser.parse_args()
    mono = args.monorepo.resolve()
    source = mono / 'bleeding'
    if not (source / 'httpz/dav/httpz_dav.ml').is_file():
        parser.error('--monorepo must name the OxMono checkout')
    expected = files(source)
    problems = []
    for label, target, stock in (
        ('OxCaml', args.oxcaml or mono.parent / 'oxcaml-httpz', False),
        ('OCaml', args.ocaml or mono.parent / 'ocaml-httpz', True),
    ):
        actual = files(target)
        for path in sorted(expected | actual):
            if path not in expected:
                problems.append(f'{label}: unexpected {path}')
            elif not (target / path).is_file():
                problems.append(f'{label}: missing {path}')
            else:
                text = (source / path).read_text()
                wanted = stock_source(path, text) if stock else text
                if (target / path).read_text() != wanted:
                    problems.append(f'{label}: differs {path}')
        print(f'{label}: checked {len(expected)} DAV files')
    for name in ('xmlm.ml', 'xmlm.mli'):
        if (source / 'httpz/dav' / name).read_bytes() != (mono / 'vendor/xmlm' / name).read_bytes():
            problems.append(f'OxMono: private {name} differs from vendor/xmlm')
    if problems:
        print('\n'.join(problems))
        raise SystemExit(1)
    print('DAV source, tests, fixtures, documentation and private XML codec are synchronized')


if __name__ == '__main__':
    main()
