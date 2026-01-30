YAML Scalar Parsing Tests

This file tests various forms of YAML scalar values including plain, quoted, and block scalars.

================================================================================
PLAIN SCALARS
================================================================================

Simple plain scalars

  $ yamlcat scalar_plain_simple.yml
  key: value

  $ yamlcat scalar_plain_multi.yml
  name: Alice
  age: 30
  active: true

Plain scalars with special values

  $ yamlcat --json scalar_plain_types.yml
  {"null_val": null, "bool_true": true, "bool_false": false, "number": 42, "float": 3.14}

================================================================================
QUOTED SCALARS - SINGLE QUOTES
================================================================================

Single-quoted strings preserve literal text

  $ yamlcat scalar_single_simple.yml
  single: hello world

Single-quoted strings with embedded double quotes

  $ yamlcat scalar_single_dquote.yml
  quote: "He said \"hello\""

Single-quoted strings with escaped single quotes (doubled)

  $ yamlcat scalar_single_escaped.yml
  escaped: It's a test

Single-quoted multiline (newlines become spaces)

  $ yamlcat --json scalar_single_multiline.yml
  {"text": "This is a multi-line string"}

Empty single-quoted string

  $ yamlcat scalar_single_empty.yml
  empty: ''

================================================================================
QUOTED SCALARS - DOUBLE QUOTES
================================================================================

Simple double-quoted strings

  $ yamlcat scalar_double_simple.yml
  double: hello world

Double-quoted with escaped newline

  $ yamlcat --json escaped_newline.yml
  {"text": "Line one\nLine two"}

Double-quoted with escaped tab

  $ yamlcat --json tab_columns.yml
  {"text": "Col1\tCol2\tCol3"}

Double-quoted with backslash escape (Windows path)
Note: Using YAML file instead of echo to avoid shell interpretation
The echo command would interpret \\ as \ before yamlcat sees it

  $ yamlcat --json windows_path.yml
  {"path": "C:\\Users\\Name"}

Double-quoted with escaped quote

  $ yamlcat --json scalar_double_quote.yml
  {"text": "She said \"hello\""}

Double-quoted with multiple escape sequences
Note: Using YAML file instead of echo to avoid shell interpretation
The echo command interprets \t and \n before yamlcat sees them

  $ yamlcat --json multiple_escapes.yml
  {"text": "Tab:\t Newline:\n Quote:\" Backslash:\\\\"}

Empty double-quoted string

  $ yamlcat scalar_double_empty.yml
  empty: ''

================================================================================
BLOCK SCALARS - LITERAL STYLE (|)
================================================================================

Basic literal block scalar (preserves newlines)

  $ yamlcat --json scalar_literal_basic.yml
  {"text": "line one\nline two\nline three\n"}

Literal with indentation

  $ yamlcat --json scalar_literal_indent.yml
  {"text": "First line\n  Indented line\nBack to first\n"}

Literal with blank lines

  $ yamlcat --json scalar_literal_blank.yml
  {"text": "First paragraph\n\nSecond paragraph\n"}

================================================================================
BLOCK SCALARS - FOLDED STYLE (>)
================================================================================

Basic folded block scalar (newlines become spaces)

  $ yamlcat --json scalar_folded_basic.yml
  {"text": "This is a long paragraph that will be folded into a single line.\n"}

Folded with paragraph separation (blank line preserved)

  $ yamlcat --json scalar_folded_para.yml
  {"text": "First paragraph flows together.\nSecond paragraph also flows.\n"}

================================================================================
CHOMPING INDICATORS
================================================================================

Strip chomping (-) removes trailing newlines

  $ yamlcat --json scalar_strip_simple.yml
  {"text": "No trailing newline"}

  $ yamlcat --json scalar_strip_blank.yml
  {"text": "Text here"}

Folded with strip

  $ yamlcat --json scalar_folded_strip.yml
  {"text": "Folded text with stripped trailing newlines"}

Clip chomping (default) keeps single trailing newline

  $ yamlcat --json scalar_clip_literal.yml
  {"text": "One trailing newline\n"}

  $ yamlcat --json scalar_clip_folded.yml
  {"text": "Folded with one trailing newline\n"}

Keep chomping (+) preserves all trailing newlines

  $ yamlcat --json scalar_keep_literal.yml
  {"text": "Keeps trailing newlines\n\n\n\n"}

  $ yamlcat --json scalar_keep_folded.yml
  {"text": "Folded text keeps trailing\n\n\n\n"}

================================================================================
EXPLICIT INDENTATION INDICATORS
================================================================================

Literal with explicit 2-space indentation

  $ yamlcat --json scalar_indent2_literal.yml
  {"text": "  Two space base\n  Second line\n    Extra indent\n"}

Folded with explicit indentation

  $ yamlcat --json scalar_indent2_folded.yml
  {"text": "  Text with two space\n  base indentation that\n  will be folded.\n"}

Combined indentation and chomping indicators

  $ yamlcat --json scalar_indent2_strip.yml
  {"text": "  Indented by 2\n  No trailing newlines"}

  $ yamlcat --json scalar_indent2_keep.yml
  {"text": "  Indented by 2\n  Keeps trailing newlines\n\n\n\n"}

================================================================================
FILE TESTS - QUOTED SCALARS
================================================================================

Test parsing scalars_quoted.yml file

  $ yamlcat scalars_quoted.yml | head -20
  single_simple: hello world
  single_with_double: "He said \"hello\""
  single_escaped_quote: 'It''s a single quote: ''example'''
  single_multiline: This is a multi-line single quoted string
  double_simple: hello world
  double_with_single: It's easy
  double_escaped_quote: "She said \"hello\""
  escaped_newline: "Line one\nLine two\nLine three"
  escaped_tab: "Column1\tColumn2\tColumn3"
  escaped_backslash: "Path: C:\\Users\\Name"
  escaped_carriage: "Before\rAfter"
  escaped_bell: "Bell\x07"
  escaped_backspace: "Back\x08"
  escaped_formfeed: "Form\x0c"
  escaped_vertical: "Vertical\x0btab"
  unicode_16bit: 'Snowman: ☃'
  unicode_32bit: 'Emoji: 😀'
  unicode_hex: "Null byte: \x00"
  empty_single: ''
  empty_double: ''

Test JSON output for quoted scalars

  $ yamlcat --json scalars_quoted.yml | head -c 500
  {"single_simple": "hello world", "single_with_double": "He said \"hello\"", "single_escaped_quote": "It's a single quote: 'example'", "single_multiline": "This is a multi-line single quoted string", "double_simple": "hello world", "double_with_single": "It's easy", "double_escaped_quote": "She said \"hello\"", "escaped_newline": "Line one\nLine two\nLine three", "escaped_tab": "Column1\tColumn2\tColumn3", "escaped_backslash": "Path: C:\\Users\\Name", "escaped_carriage": "Before\rAfter", "escaped

Verify specific escape handling in JSON

  $ yamlcat --json scalars_quoted.yml | grep -o '"escaped_newline": "[^"]*"'
  "escaped_newline": "Line one\nLine two\nLine three"

  $ yamlcat --json scalars_quoted.yml | grep -o '"escaped_tab": "[^"]*"'
  "escaped_tab": "Column1\tColumn2\tColumn3"

Verify Unicode handling

  $ yamlcat --json scalars_quoted.yml | grep -o '"unicode_16bit": "[^"]*"'
  "unicode_16bit": "Snowman: \226\152\131"

  $ yamlcat --json scalars_quoted.yml | grep -o '"unicode_32bit": "[^"]*"'
  "unicode_32bit": "Emoji: \240\159\152\128"

Verify quoted strings preserve type indicators

  $ yamlcat --json scalars_quoted.yml | grep -o '"string_true": "[^"]*"'
  "string_true": "true"

  $ yamlcat --json scalars_quoted.yml | grep -o '"string_null": "[^"]*"'
  "string_null": "null"

  $ yamlcat --json scalars_quoted.yml | grep -o '"string_number": "[^"]*"'
  "string_number": "123"

================================================================================
FILE TESTS - BLOCK SCALARS
================================================================================

Test parsing scalars_block.yml file

  $ yamlcat scalars_block.yml | head -30
  literal_basic: "Line one\nLine two\nLine three\n"
  literal_with_indent: "First line\n  Indented line\n    More indented\n  Back to second level\nBack to first level\n"
  folded_basic: "This is a long paragraph that will be folded into a single line with the newlines converted to spaces.\n"
  folded_paragraph: "First paragraph flows together into a single line.\nSecond paragraph after blank line also flows together.\n"
  literal_strip: No trailing newline
  literal_strip_multiple: Text here
  folded_strip: Folded text with stripped trailing newlines
  literal_clip: "One trailing newline\n"
  literal_clip_explicit: "This is the default behavior\n"
  folded_clip: "Folded with one trailing newline\n"
  literal_keep: "Keeps trailing newlines\n\n\n"
  literal_keep_multiple: "Text here\n\n\n"
  folded_keep: "Folded text keeps trailing\n\n\n"
  literal_indent_2: "  Two space indentation\n  is preserved here\n    Extra indent\n  Back to two\n"
  literal_indent_4: "  Four space base indent\n  Second line\n    Extra indent\n  Back to base\n"
  folded_indent_2: "  Text with two space\n  base indentation that\n  will be folded.\n"
  folded_indent_3: "  Three space indent\n  for this folded\n  text block.\n"
  literal_indent_strip: "  Indented by 2\n  No trailing newlines"
  folded_indent_strip: "  Folded with indent\n  and stripped end"
  literal_indent_keep: "  Indented by 2\n  Keeps trailing newlines\n\n\n"
  folded_indent_keep: "  Folded indent 4\n  keeps all trailing\n\n\n"
  empty_literal: ''
  empty_folded: ''
  only_newlines_literal: ''
  only_newlines_folded: ''
  complex_literal: "First level\n  Second level\n    Third level\n  Back to second\nBack to first\n\nNew paragraph\n  With indent\n\nFinal paragraph\n"
  complex_folded: "This paragraph flows together.\nThis is separate.\n  This line starts more indented\n  and continues.\n\nFinal thoughts here.\n"
  special_chars_literal: "Special: @#$%^&*()\nQuotes: \"double\" 'single'\nBrackets: [array] {object}\nSymbols: | > & * ? : -\n"
  special_chars_folded: "All special chars are literal in block scalars: []{}|>*&\n"
  sequence_with_blocks:

Test JSON output for block scalars

  $ yamlcat --json scalars_block.yml | grep -o '"literal_basic": "[^"]*"'
  "literal_basic": "Line one\nLine two\nLine three\n"

  $ yamlcat --json scalars_block.yml | grep -o '"folded_basic": "[^"]*"' | head -c 100
  "folded_basic": "This is a long paragraph that will be folded into a single line with the newlines c

Verify strip chomping

  $ yamlcat --json scalars_block.yml | grep -o '"literal_strip": "[^"]*"'
  "literal_strip": "No trailing newline"

  $ yamlcat --json scalars_block.yml | grep -o '"folded_strip": "[^"]*"'
  "folded_strip": "Folded text with stripped trailing newlines"

Verify clip chomping (single newline)

  $ yamlcat --json scalars_block.yml | grep -o '"literal_clip": "[^"]*"'
  "literal_clip": "One trailing newline\n"

Verify keep chomping (all newlines)

  $ yamlcat --json scalars_block.yml | grep -o '"literal_keep": "[^"]*"'
  "literal_keep": "Keeps trailing newlines\n\n\n"

  $ yamlcat --json scalars_block.yml | grep -o '"folded_keep": "[^"]*"'
  "folded_keep": "Folded text keeps trailing\n\n\n"

Verify indentation handling

  $ yamlcat --json scalars_block.yml | grep -o '"literal_indent_2": "[^"]*"'
  "literal_indent_2": "  Two space indentation\n  is preserved here\n    Extra indent\n  Back to two\n"

Verify nested structures with block scalars

  $ yamlcat scalars_block.yml | tail -10
  special_chars_folded: "All special chars are literal in block scalars: []{}|>*&\n"
  sequence_with_blocks:
    - "First item\nliteral block\n"
    - "Second item folded block\n"
    - "Third item\nstripped"
    - "Fourth item\nkept\n\n\n"
  nested:
    description: "This is a folded description that spans multiple lines.\n"
    code: "def hello():\n    print(\"Hello, World!\")\n    return True\n"
    notes: "Final notes\nwith stripped end"

================================================================================
SPECIAL CASES AND EDGE CASES
================================================================================

Empty block scalars

  $ yamlcat --json scalar_empty_literal.yml
  {"empty_literal": ""}

  $ yamlcat --json scalar_empty_folded.yml
  {"empty_folded": ""}

Block scalars with special characters (no escaping needed)

  $ yamlcat --json scalar_special_chars.yml
  {"code": "Special: @#$%^&*()\nSymbols: <>?/\\|\nBrackets: []{}\n"}

Plain scalar vs quoted string for special values

  $ yamlcat --json scalar_bool_vs_string.yml
  {"unquoted_true": true, "quoted_true": "true"}

  $ yamlcat --json scalar_null_vs_string.yml
  {"unquoted_null": null, "quoted_null": "null"}

Strings that need quoting to preserve leading/trailing spaces

  $ yamlcat --json scalar_whitespace.yml
  {"leading": "  spaces", "trailing": "spaces  ", "both": "  both  "}

Block scalars in sequences

  $ yamlcat --json scalar_seq_blocks.yml
  {"items": ["First item\nmultiline\n", "Second item folded\n"]}

Block scalars in nested mappings

  $ yamlcat --json scalar_nested_blocks.yml
  {"outer": {"inner": {"description": "This is a folded description.\n", "code": "def test():\n    return True\n"}}}

Preserving indentation in literal blocks

  $ yamlcat --json scalar_code_sample.yml
  {"code": "def hello():\n    print(\"Hello\")\n    if True:\n        return 42\n"}

Folded scalars preserve more-indented lines

  $ yamlcat --json scalar_folded_indent_preserve.yml
  {"text": "Normal paragraph continues here.\n\n  Indented block\n  preserved.\n\nBack to normal.\n"}
