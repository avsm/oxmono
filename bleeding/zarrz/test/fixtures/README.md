# zarrz test fixtures

Golden data copied verbatim from the `zarrs` Rust workspace, the
behavioural oracle for this library.

- Source: <https://github.com/LDeakin/zarrs>, directory
  `zarrs/tests/data/`.
- Commit: `c17fe374b1fa7df8373b6c6f6eb3f1d33c3a3bd7`.
- Licence: MIT OR Apache-2.0, see `LICENCE-MIT` and `LICENCE-APACHE` in
  that repository.

Files:

- `array_metadata.json`, the array metadata example of
  `zarrs_metadata::v3::ArrayMetadataV3`.
- `group_metadata.json`, the group metadata example of
  `zarrs_metadata::v3::GroupMetadataV3`.

Re-vendoring: copy the file again from the same path in a newer `zarrs`
checkout and update the commit above. A test that fails afterwards means
the oracle changed, so change this library rather than the fixture.
