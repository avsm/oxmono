Test YAML directives and single document parsing

This test suite covers YAML directives (%YAML, %TAG) and various single document formats.
For multi-document stream tests, see multidoc.t.

Test 1: Basic YAML 1.2 directive
====================================

  $ yamlcat directives.yml
  version: '1.2'
  content: This document uses YAML 1.2

Test 2: YAML 1.1 directive
====================================

  $ yamlcat directives_yaml11.yml
  version: '1.1'
  content: This document uses YAML 1.1
  booleans:
    - true
    - false
    - true
    - false

Test 3: TAG directive with custom prefix
====================================

  $ yamlcat directives_tag.yml
  shape:
    radius: 5
    color: red
  point:
    x: 10
    y: 20

Test 4: Multiple TAG directives
====================================

  $ yamlcat directives_multiple_tags.yml
  user:
    name: Alice
    age: 30
  location:
    lat: 40.7128
    lon: -74.006
  config:
    debug: true
    timeout: 30

Test 5: Implicit document (no markers)
====================================

  $ yamlcat documents_single.yml
  key1: value1
  key2: value2
  nested:
    inner: data
  list:
    - item1
    - item2
    - item3

Test 6: Explicit start marker (---)
====================================

  $ yamlcat documents_single_explicit_start.yml
  key1: value1
  key2: value2
  nested:
    inner: data
  list:
    - item1
    - item2
    - item3

Test 7: Explicit start and end markers (--- ... )
====================================

  $ yamlcat documents_single_explicit_both.yml
  key1: value1
  key2: value2
  nested:
    inner: data
  list:
    - item1
    - item2
    - item3

Test 8: Document with YAML directive
====================================

  $ yamlcat documents_single_with_directive.yml
  key1: value1
  key2: value2
  nested:
    inner: data
  list:
    - item1
    - item2
    - item3

Test 9: Inline - implicit document (no markers)
====================================

  $ yamlcat person_john.yml
  name: John
  age: 30
  city: New York

Test 10: Inline - explicit start marker
====================================

  $ yamlcat person_jane.yml
  name: Jane
  age: 25

Test 11: Inline - explicit start and end markers
====================================

  $ yamlcat doc_with_end.yml
  title: Example
  content: data

Test 12: Inline - document with %YAML 1.2 directive
====================================

  $ yamlcat yaml12_version.yml
  version: 1.2
  enabled: true

Test 13: Inline - document with comment before content
====================================

  $ yamlcat doc_with_comments.yml
  name: Alice
  value: 42

Test 14: Inline - document with comment after directive
====================================

  $ yamlcat directive_with_comment.yml
  key: value

Test 15: Inline - explicit markers with comments
====================================

  $ yamlcat start_with_comment.yml
  data: content
  more: values

Test 16: Verify JSON roundtrip for directive file
====================================

  $ yamlcat --json directives.yml
  {"version": "1.2", "content": "This document uses YAML 1.2"}

Test 17: Verify JSON roundtrip for explicit markers
====================================

  $ yamlcat --json documents_single_explicit_both.yml
  {"key1": "value1", "key2": "value2", "nested": {"inner": "data"}, "list": ["item1", "item2", "item3"]}

Test 18: Empty document with explicit markers
====================================

  $ yamlcat empty_doc_markers.yml
  null

Test 19: Document with only whitespace and markers
====================================

  $ yamlcat empty_doc_with_blank.yml
  null

Test 20: Directive followed by content without explicit start
====================================

  $ yamlcat directive_without_start.yml
  Error: expected document start '---' at line 2, columns 1-1
  [1]
