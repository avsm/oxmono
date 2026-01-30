Test comments.yml file with various comment styles

  $ yamlcat comments.yml
  name: John Doe
  age: 30
  address:
    street: 123 Main St
    city: Springfield
    zip: 12345
  items:
    - apple
    - banana
    - cherry
    - date
  flow_seq:
    - 1
    - 2
    - 3
  flow_map:
    key1: value1
    key2: value2
  nested:
    level1:
      level2:
        value: deeply nested
  multi_comment_key: value
  special: 'value with # hash inside quotes'
  empty_value: null
  final_key: final_value

Test comments.yml roundtrip with JSON to verify parsed values

  $ yamlcat --json comments.yml
  {"name": "John Doe", "age": 30, "address": {"street": "123 Main St", "city": "Springfield", "zip": 12345}, "items": ["apple", "banana", "cherry", "date"], "flow_seq": [1, 2, 3], "flow_map": {"key1": "value1", "key2": "value2"}, "nested": {"level1": {"level2": {"value": "deeply nested"}}}, "multi_comment_key": "value", "special": "value with # hash inside quotes", "empty_value": null, "final_key": "final_value"}

Test full line comments are ignored

  $ echo '# This is a full line comment
  > name: Alice
  > # Another comment
  > age: 25' | yamlcat --json
  {"name": "Alice", "age": 25}

Test end of line comments after scalars

  $ echo 'name: Bob  # This is an end of line comment
  > age: 35  # Another end of line comment' | yamlcat --json
  {"name": "Bob", "age": 35}

Test comments after sequence items

  $ echo 'fruits:
  >   - apple  # First fruit
  >   - banana  # Second fruit
  >   - cherry  # Third fruit' | yamlcat --json
  {"fruits": ["apple", "banana", "cherry"]}

Test comments between sequence items

  $ echo 'numbers:
  >   - 1
  >   # Comment between items
  >   - 2
  >   # Another comment
  >   - 3' | yamlcat --json
  {"numbers": [1, 2, 3]}

Test comments after flow sequences

  $ echo 'flow: [1, 2, 3]  # Comment after flow sequence' | yamlcat --json
  {"flow": [1, 2, 3]}

Test comments after flow mappings

  $ echo 'flow: {a: 1, b: 2}  # Comment after flow mapping' | yamlcat --json
  {"flow": {"a": 1, "b": 2}}

Test comments between mapping entries

  $ echo 'first: value1
  > # Comment between entries
  > second: value2
  > # Another comment
  > third: value3' | yamlcat --json
  {"first": "value1", "second": "value2", "third": "value3"}

Test multiple consecutive comments

  $ echo '# First comment
  > # Second comment
  > # Third comment
  > key: value' | yamlcat --json
  {"key": "value"}

Test comments in nested structures

  $ echo 'outer:
  >   # Comment in nested level
  >   inner:
  >     # Comment in deeper level
  >     key: value  # End of line comment' | yamlcat --json
  {"outer": {"inner": {"key": "value"}}}

Test comments with special characters

  $ echo '# Comment with !@#$%^&*()
  > key: value' | yamlcat --json
  {"key": "value"}

Test that hash in quoted strings is not treated as comment

  $ echo 'text: "This # is not a comment"
  > other: '"'"'Also # not a comment'"'"'' | yamlcat --json
  {"text": "This # is not a comment", "other": "Also # not a comment"}

Test comment before empty value (null)

  $ echo 'key:  # Comment, value is null' | yamlcat --json
  {"key": null}

Test comments at document start

  $ echo '# Comment at very start
  > # Another at start
  > data: value' | yamlcat --json
  {"data": "value"}

Test comments at document end

  $ echo 'data: value
  > # Comment at end
  > # Another at end' | yamlcat --json
  {"data": "value"}

Test comments with various indentation levels

  $ echo 'level1:
  >     # Indented comment
  >     level2:
  >         # More indented comment
  >         value: data' | yamlcat --json
  {"level1": {"level2": {"value": "data"}}}

Test empty lines with comments

  $ echo 'first: 1
  > 
  > # Comment after empty line
  > 
  > second: 2' | yamlcat --json
  {"first": 1, "second": 2}

Test comment after sequence with nested mapping

  $ echo 'items:
  >   - name: item1  # Comment after nested value
  >     value: 10
  >   # Comment between sequence items
  >   - name: item2
  >     value: 20  # Another comment' | yamlcat --json
  {"items": [{"name": "item1", "value": 10}, {"name": "item2", "value": 20}]}

Test comment only lines between complex structures

  $ echo 'mapping1:
  >   key: value
  > # Comment between mappings
  > mapping2:
  >   key: value' | yamlcat --json
  {"mapping1": {"key": "value"}, "mapping2": {"key": "value"}}

Test comments do not affect block scalars

  $ echo 'literal: |
  >   # This looks like a comment
  >   but it is part of the literal text
  > key: value' | yamlcat --json
  {"literal": "# This looks like a comment\nbut it is part of the literal text\n", "key": "value"}

Test comments do not affect folded scalars

  $ echo 'folded: >
  >   # This also looks like a comment
  >   but is part of folded text
  > key: value' | yamlcat --json
  {"folded": "# This also looks like a comment but is part of folded text\n", "key": "value"}

Test whitespace preservation around comments

  $ echo 'key1:    value1   # Comment with spaces' | yamlcat --json
  {"key1": "value1"}

Test comment after colon but before value

  $ echo 'key: # Comment before value
  >   value' | yamlcat --json
  {"key": "value"}
