# Changes

## Unreleased

- Add decode tests that run every checked-in schema over recorded API
  responses.

- Patch the pinned spec where live responses violate it. The hunks are listed
  in the README.

- Add `Ecosystems_client.create`, which sets the base URL and a User-Agent.

- Add `Ecosystems_client.pages`, a lazy sequence over the pages of a listing.

- Add the `ecosystems` client, generated from the packages.ecosyste.ms OpenAPI
  spec.
