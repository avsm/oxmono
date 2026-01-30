Escape Sequence Issues (documentation of known edge cases)

These tests document escape sequence handling edge cases.

The primary issue is with \U (capital U) in double-quoted strings.
In YAML, \U is a 32-bit unicode escape that expects 8 hex digits.
When users write paths like "C:\Users" the \U is interpreted as
a unicode escape but "sers" are not valid hex digits.

Test: Capital U interpreted as unicode escape

  $ yamlcat --json failing_escapes.yml 2>&1
  Error: invalid hex escape:  at line 1, columns 12-12
  [1]

This fails because:
- YAML input: "C:\Users\Name"
- \U is a 32-bit unicode escape (expects \UHHHHHHHH)
- "sers" are not 8 hex digits, so it fails

Test: Lowercase u unicode escape works

  $ yamlcat --json lowercase_u_unicode.yml
  {"unicode": "A"}

Test: Uppercase U requires 8 hex digits

  $ yamlcat --json uppercase_U_unicode.yml
  {"unicode": "A"}

Test: Backslash escaping works for non-unicode

  $ yamlcat --json escaped_backslashes.yml
  {"escaped": "one\\two\\three"}

Test: Mixed valid escapes

  $ yamlcat --json mixed_escapes.yml
  {"text": "Tab:\t Newline:\n Quote:\\"}

Test: Backslash a is bell character

  $ yamlcat --json bell_character.yml
  {"text": "test\007"}
