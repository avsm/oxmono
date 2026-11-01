JMAP Extended JSON Pointer Tests (RFC 8620 Section 3.7)

This tests the wildcard (*) extension to JSON Pointer for JMAP result references.

Parsing JMAP extended pointers:

Basic pointers (no wildcards, same as RFC 6901):
  $ ./test_pointer.exe jmap-parse ""
  OK: (root)
  $ ./test_pointer.exe jmap-parse "/foo"
  OK: /foo
  $ ./test_pointer.exe jmap-parse "/foo/0"
  OK: /foo/0
  $ ./test_pointer.exe jmap-parse "/a~1b"
  OK: /a~1b

Wildcard pointers:
  $ ./test_pointer.exe jmap-parse "/*"
  OK: /*
  $ ./test_pointer.exe jmap-parse "/list/*"
  OK: /list/*
  $ ./test_pointer.exe jmap-parse "/list/*/id"
  OK: /list/*/id
  $ ./test_pointer.exe jmap-parse "/list/*/emailIds"
  OK: /list/*/emailIds
  $ ./test_pointer.exe jmap-parse "/a/*/b/*/c"
  OK: /a/*/b/*/c

[-] uses ordinary JSON Pointer syntax and is accepted by the parser:
  $ ./test_pointer.exe jmap-parse "/-"
  OK: /-
  $ ./test_pointer.exe jmap-parse "/foo/-"
  OK: /foo/-

Error: invalid syntax:
  $ ./test_pointer.exe jmap-parse "foo"
  ERROR: Invalid JSON Pointer: must be empty or start with '/': foo
  $ ./test_pointer.exe jmap-parse "/~"
  ERROR: Invalid JSON Pointer: incomplete escape sequence at end

Evaluation without wildcards:

Root pointer:
  $ ./test_pointer.exe jmap-eval '{"foo":"bar"}' ""
  OK: {"foo":"bar"}

Simple member access:
  $ ./test_pointer.exe jmap-eval '{"foo":"bar"}' "/foo"
  OK: "bar"

Array index:
  $ ./test_pointer.exe jmap-eval '{"arr":[1,2,3]}' "/arr/1"
  OK: 2

Nested access:
  $ ./test_pointer.exe jmap-eval '{"a":{"b":{"c":"deep"}}}' "/a/b/c"
  OK: "deep"

Evaluation with wildcards (RFC 8620 examples):

Extract single field from each object in array:
  $ ./test_pointer.exe jmap-eval '{"list":[{"id":"a"},{"id":"b"},{"id":"c"}]}' "/list/*/id"
  OK: ["a","b","c"]

Extract threadId from Email/get response (RFC 8620 pattern):
  $ ./test_pointer.exe jmap-eval-file data/jmap_emails.json "/list/*/threadId"
  OK: ["trd194","trd114","trd99"]

Extract emailIds from Thread/get response (RFC 8620 pattern) - results flattened:
  $ ./test_pointer.exe jmap-eval-file data/jmap_threads.json "/list/*/emailIds"
  OK: ["msg1020","msg1021","msg1023","msg201","msg223","msg42"]

Extract nested field:
  $ ./test_pointer.exe jmap-eval '{"items":[{"data":{"value":1}},{"data":{"value":2}}]}' "/items/*/data/value"
  OK: [1,2]

Wildcard on empty array:
  $ ./test_pointer.exe jmap-eval '{"list":[]}' "/list/*/id"
  OK: []

Multiple wildcards (nested arrays):
  $ ./test_pointer.exe jmap-eval '{"a":[{"b":[{"c":1},{"c":2}]},{"b":[{"c":3}]}]}' "/a/*/b/*/c"
  OK: [1,2,3]

Wildcard returning non-arrays (no flattening needed):
  $ ./test_pointer.exe jmap-eval '{"list":[{"name":"alice"},{"name":"bob"}]}' "/list/*/name"
  OK: ["alice","bob"]

Flattening behavior - arrays of arrays become flat:
  $ ./test_pointer.exe jmap-eval '{"items":[{"tags":["a","b"]},{"tags":["c"]},{"tags":["d","e","f"]}]}' "/items/*/tags"
  OK: ["a","b","c","d","e","f"]

Error cases:

[*] is an ordinary member token on objects and cannot index primitives:
  $ ./test_pointer.exe jmap-eval '{"obj":{"a":1}}' "/obj/*"
  ERROR: JMAP Pointer: member '*' not found
  File "-":
  $ ./test_pointer.exe jmap-eval '"string"' "/*"
  ERROR: JMAP Pointer: cannot index into string with '*'
  File "-":

Member not found:
  $ ./test_pointer.exe jmap-eval '{"foo":"bar"}' "/baz"
  ERROR: JMAP Pointer: member 'baz' not found
  File "-":

Index out of bounds:
  $ ./test_pointer.exe jmap-eval '{"arr":[1,2]}' "/arr/5"
  ERROR: JMAP Pointer: index 5 out of bounds (array has 2 elements)
  File "-":

Member not found after wildcard:
  $ ./test_pointer.exe jmap-eval '{"list":[{"a":1},{"b":2}]}' "/list/*/a"
  ERROR: JMAP Pointer: member 'a' not found
  File "-":

Real JMAP patterns:

Get IDs from query response:
  $ ./test_pointer.exe jmap-eval '{"queryState":"abc","ids":["id1","id2","id3"]}' "/ids"
  OK: ["id1","id2","id3"]

Get created IDs from changes response:
  $ ./test_pointer.exe jmap-eval '{"oldState":"a","newState":"b","created":["f1","f4"],"updated":[],"destroyed":[]}' "/created"
  OK: ["f1","f4"]

Complex nested extraction:
  $ ./test_pointer.exe jmap-eval '{"results":[{"emails":[{"from":"a@b.com"},{"from":"c@d.com"}]},{"emails":[{"from":"e@f.com"}]}]}' "/results/*/emails/*/from"
  OK: ["a@b.com","c@d.com","e@f.com"]

Typed extraction with Jmap.path combinator:

Extract string list with wildcard:
  $ ./test_pointer.exe jmap-path-strings '{"list":[{"id":"a"},{"id":"b"},{"id":"c"}]}' "/list/*/id"
  OK: [a, b, c]

Extract IDs from JMAP-style response:
  $ ./test_pointer.exe jmap-path-strings '{"ids":["id1","id2","id3"]}' "/ids"
  OK: [id1, id2, id3]

Extract threadIds (JMAP Email/get pattern):
  $ ./test_pointer.exe jmap-path-strings '{"list":[{"threadId":"t1"},{"threadId":"t2"}]}' "/list/*/threadId"
  OK: [t1, t2]

Extract integers:
  $ ./test_pointer.exe jmap-path-ints '{"items":[{"count":10},{"count":20},{"count":30}]}' "/items/*/count"
  OK: [10, 20, 30]

Extract single string value:
  $ ./test_pointer.exe jmap-path-single '{"account":{"id":"acc123"}}' "/account/id"
  OK: acc123

Extract with absent default (path exists):
  $ ./test_pointer.exe jmap-path-absent '{"name":"alice"}' "/name" "default"
  OK: alice

Extract with absent default (path missing):
  $ ./test_pointer.exe jmap-path-absent '{"other":"value"}' "/name" "default"
  OK: default

Nested wildcard extraction:
  $ ./test_pointer.exe jmap-path-strings '{"a":[{"b":[{"c":"x"},{"c":"y"}]},{"b":[{"c":"z"}]}]}' "/a/*/b/*/c"
  OK: [x, y, z]

Empty array result:
  $ ./test_pointer.exe jmap-path-strings '{"list":[]}' "/list/*/id"
  OK: []

Bounded wildcard expansion:

The exact bound succeeds, while the next emitted result fails before a larger
array is constructed:
  $ ./test_pointer.exe jmap-eval-bounded '{"list":[{"id":"a"},{"id":"b"},{"id":"c"}]}' '/list/*/id' 3
  OK: ["a","b","c"]
  $ ./test_pointer.exe jmap-eval-bounded '{"list":[{"id":"a"},{"id":"b"},{"id":"c"}]}' '/list/*/id' 2
  ERROR: JMAP Pointer: wildcard result limit 2 exceeded
  File "-":

Nested wildcards share one final-result budget rather than counting the same
value once per wildcard:
  $ ./test_pointer.exe jmap-eval-bounded '{"a":[{"b":[{"c":1},{"c":2}]},{"b":[{"c":3}]}]}' '/a/*/b/*/c' 3
  OK: [1,2,3]
  $ ./test_pointer.exe jmap-eval-bounded '{"a":[{"b":[{"c":1},{"c":2}]},{"b":[{"c":3}]}]}' '/a/*/b/*/c' 2
  ERROR: JMAP Pointer: wildcard result limit 2 exceeded
  File "-":

A physically shared subtree is charged each time the pointer traverses it:
  $ ./test_pointer.exe jmap-shared-bounded 1000 1000 12
  ERROR: JMAP Pointer: wildcard result limit 12 exceeded

A pointer with no wildcard does not charge the expansion budget:
  $ ./test_pointer.exe jmap-eval-bounded '{"ids":[1,2,3]}' '/ids' 0
  OK: [1,2,3]

Type mismatch error (expecting strings, got ints):
  $ ./test_pointer.exe jmap-path-strings '{"list":[{"id":1},{"id":2}]}' "/list/*/id"
  ERROR: Expected string but found number
  File "-":
  File "-": at index 0 of
  File "-": array<string>
