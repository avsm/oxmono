# Frozen Python SDK parity audit

This directory contains the offline structural audit for the supported Python
SDK surface. Ordinary tests need neither a sibling Python checkout nor a
downloaded OpenAPI document.

## Frozen files

`reference.json` freezes three inputs:

- the 77 public methods of `zulip.Client` at Python SDK revision `b1723475`;
- the published REST operation inventory, with the two real profile-field routes
  omitted from 12.2 OpenAPI supplied from its server route table; and
- the documented Zulip Server 12.2 parameters for those operations. The helper
  mappings select 68 canonical operations from that inventory.

The 71 endpoint helpers collapse to 68 operations because
`get_users`/`get_members`, `get_subscriptions`/`list_subscriptions`, and
`update_message`/`move_topic` share canonical operations. The six remaining
helpers are transport or callback facilities. Parameters are counted once per
canonical operation, producing 294 wire parameters.

`bindings.json` maps every frozen helper to its public OCaml entry point. For an
endpoint helper it also records the canonical method/path, every accounted
parameter, a source file and token used as evidence, the result shape, and the
family notes file. This is the machine-readable source for the 77-row table in
`API_COVERAGE.md`.

`audit.py` reads only those frozen files and repository sources. The Dune rule
runs it as part of `@runtest`.

## What the audit checks

The audit fails when:

- the frozen helper set and binding set differ;
- the named public OCaml declaration is missing;
- a helper changes its canonical operation without updating the reference;
- a documented parameter is missing or an undeclared parameter is claimed;
- a parameter's declared source evidence token is absent;
- expected route components or HTTP method tokens disappear; or
- selected obsolete endpoint-shaped declarations are reintroduced.

On success it prints:

```text
Parity inventory: 77 Python helpers, 68 canonical operations, 294 wire parameters accounted for.
```

These checks are static guards. Token presence does not prove that a value is
serialized correctly, that a response codec models every semantic constraint,
that retries are safe, or that a live server accepts the request. The focused
Alcotest suites verify representative wire requests, responses, nullability,
extensions, and failures. The Docker integration suite verifies selected
operations against Zulip 12.2. Neither the audit nor `Client.request_json`
claims typed coverage of every REST endpoint.

## Running it

Run the frozen audit directly:

```sh
python3 test/parity/audit.py
```

It also runs with the ordinary offline suite:

```sh
dune runtest test
```

During maintenance, an optional argument compares the frozen helper names with
a Python source checkout:

```sh
python3 test/parity/audit.py ../python-zulip-api/zulip/zulip/__init__.py
```

That optional comparison checks method names only. The frozen operation and
parameter data still require review against the chosen Python revision, Zulip
12.2 OpenAPI schema, and server route table.

## Updating the inventory

When changing the reference revision or a binding:

1. Review the Python `Client` class, the Zulip 12.2 OpenAPI operation, and the
   actual 12.2 route together.
2. Update `reference.json` for the helper, canonical operation, and full
   parameter set.
3. Update `bindings.json` with the public entry point and concrete evidence for
   each parameter.
4. Update the relevant family coverage JSON with typing, response, extension,
   alias, and intentional-difference notes.
5. Add or update behavioral tests for encoding, decoding, errors, and any
   nullable or extension fields.
6. Regenerate the helper table and counts in `API_COVERAGE.md`, then run the
   offline suite and the applicable live scenarios.

Do not satisfy the audit with an unrelated token or count a generic raw request
as native coverage. Evidence should point to the code that actually constructs
or decodes the parameter, and the documentation should distinguish structural
accounting from tested behavior.
