Tag Support Tests

These tests verify YAML tag support including type coercion and
different tag formats.

Test: String tag shorthand

  $ printf '!!str 123' | yamlcat
  '123'

The !!str tag forces the value to be treated as a string.

Test: Integer tag shorthand

  $ printf '!!int "42"' | yamlcat
  42

The !!int tag coerces the quoted string to an integer.

Test: Boolean tag shorthand

  $ printf '!!bool "yes"' | yamlcat
  true

The !!bool tag coerces the string to a boolean.

Test: Null tag shorthand

  $ printf '!!null ""' | yamlcat
  null

The !!null tag coerces the value to null.

Test: Float tag shorthand

  $ printf '!!float 3.14' | yamlcat
  3.14

The !!float tag specifies a floating-point number.

Test: Tag shorthand in mapping value

  $ printf 'value: !!str 42' | yamlcat
  value: '42'

Tags work in mapping values and force type coercion.

Test: Local tags

  $ printf '!local_tag value' | yamlcat
  value

Local tags (single !) are treated as unknown and default to string type.

Test: Verbatim tags

  $ printf '!<tag:example.com:type> value' | yamlcat
  value

Verbatim tags (!<...>) are treated as unknown and default to string type.
