Empty Collection YAML Emission

These tests verify that empty sequences and mappings are correctly emitted
as [] and {} in YAML output.

Test: Empty sequence

  $ yamlcat empty_seq.yml
  empty_seq: []

Test: Empty mapping

  $ yamlcat empty_map.yml
  empty_map: {}

Test: Multiple empty collections

  $ yamlcat multiple_empty.yml
  seq: []
  map: {}
  data: value

Test: Nested empty collections

  $ yamlcat nested_empty.yml
  outer:
    inner_seq: []
    inner_map: {}

Test: Empty collection in sequence

  $ yamlcat empty_in_seq.yml
  items:
    - first
    - []
    - third

Test: Verify JSON output is correct (for comparison)

  $ yamlcat --json empty_both.yml
  {"empty_seq": [], "empty_map": {}}
