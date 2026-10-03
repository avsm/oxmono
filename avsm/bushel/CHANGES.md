# Changes

## Unreleased

- Show registered releases as one line each in the arod notes view, inside the
  month they were made, with links to the release page and to ecosyste.ms.

- Load `releases.yml` into the arod context.

- Add `bushel release list|discover|add|refresh`. A release is registered from
  its GitHub or tangled release, with the registries that carry it found
  through ecosyste.ms.

- Attach package registries to a release through ecosyste.ms.

- Parse GitHub releases and Tangled artifacts into release candidates.

- Add a `[releases]` section to the configuration.

- Replace `Bushel.Release` with a forge release that carries a one-line summary
  and the registries it reached.
