# Changes

## Unreleased

- Add `oecosystems`, a command-line tool for registries, packages, versions,
  dependents, advisories, maintainers and keywords, with `--json` output.

- Patch the pinned spec to accept a null `Package.issue_metadata`.

- Patch the pinned spec where live responses violate it. The hunks are listed
  in the README.

- Add `Ecosystems_client.create`, which sets the base URL and a User-Agent and
  retries 429, 500, 502, 503 and 504 responses. It takes `?session` and
  `?max_response_bytes`.

- Add `Ecosystems_client.pages`, a lazy sequence over the pages of a listing.
  It ends at the first empty page.

- Add the `ecosystems` client, generated from the packages.ecosyste.ms OpenAPI
  spec.
