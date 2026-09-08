Comprehensive JSON Pointer Tests (from json-pointer-js test suite)

Additional parsing tests:
  $ ./test_pointer.exe parse "/foo/bar"
  OK: ["foo"; "bar"]
  $ ./test_pointer.exe parse "/foo/-"
  OK: ["foo"; "-"]
  $ ./test_pointer.exe parse "/foo/1"
  OK: ["foo"; "1"]
  $ ./test_pointer.exe parse "/foo/~0"
  OK: ["foo"; "~"]
  $ ./test_pointer.exe parse "/foo/~1"
  OK: ["foo"; "/"]
  $ ./test_pointer.exe parse "/foo/~1~0"
  OK: ["foo"; "/~"]
  $ ./test_pointer.exe parse "/foo/~0~1"
  OK: ["foo"; "~/"]
  $ ./test_pointer.exe parse "/foo/~01"
  OK: ["foo"; "~1"]
  $ ./test_pointer.exe parse "/foo/~10"
  OK: ["foo"; "/0"]

Parse error tests:
  $ ./test_pointer.exe parse "foo/bar"
  ERROR: Invalid JSON Pointer: must be empty or start with '/': foo/bar
  $ ./test_pointer.exe parse "~foo"
  ERROR: Invalid JSON Pointer: must be empty or start with '/': ~foo
  $ ./test_pointer.exe parse "/~a"
  ERROR: Invalid JSON Pointer: invalid escape sequence ~a
  $ ./test_pointer.exe parse "~1/foo"
  ERROR: Invalid JSON Pointer: must be empty or start with '/': ~1/foo

Get value tests:
  $ ./test_pointer.exe eval data/rfc6901_example.json "/a~1b"
  OK: 1
  $ ./test_pointer.exe eval data/rfc6901_example.json "/c%d"
  OK: 2
  $ ./test_pointer.exe eval data/rfc6901_example.json "/e^f"
  OK: 3
  $ ./test_pointer.exe eval data/rfc6901_example.json "/g|h"
  OK: 4
  $ ./test_pointer.exe eval data/rfc6901_example.json '/i\j'
  OK: 5
  $ ./test_pointer.exe eval data/rfc6901_example.json '/k"l'
  OK: 6
  $ ./test_pointer.exe eval data/rfc6901_example.json "/ "
  OK: 7
  $ ./test_pointer.exe eval data/rfc6901_example.json "/m~0n"
  OK: 8
  $ ./test_pointer.exe eval data/rfc6901_example.json "/"
  OK: 0

Set tests (using add for replace/add semantics):
  $ ./test_pointer.exe add '{"bar":"foo"}' '/bar' '"baz"'
  {"bar":"baz"}
  $ ./test_pointer.exe add '{"bar":"foo"}' '/foo' '"baz"'
  {"bar":"foo","foo":"baz"}
  $ ./test_pointer.exe add '["foo"]' '/0' '"bar"'
  ["bar","foo"]
  $ ./test_pointer.exe add '["foo"]' '/-' '"bar"'
  ["foo","bar"]
  $ ./test_pointer.exe add '{"foo":["bar"]}' '/foo/0' '"baz"'
  {"foo":["baz","bar"]}

Replace tests using special characters:
  $ ./test_pointer.exe replace '{"a/b":"bar"}' '/a~1b' '"baz"'
  {"a/b":"baz"}
  $ ./test_pointer.exe replace '{"c%d":"bar"}' '/c%d' '"baz"'
  {"c%d":"baz"}
  $ ./test_pointer.exe replace '{"e^f":"bar"}' '/e^f' '"baz"'
  {"e^f":"baz"}
  $ ./test_pointer.exe replace '{"g|h":"bar"}' '/g|h' '"baz"'
  {"g|h":"baz"}
  $ ./test_pointer.exe replace '{" ":"bar"}' '/ ' '"baz"'
  {" ":"baz"}
  $ ./test_pointer.exe replace '{"m~n":"bar"}' '/m~0n' '"baz"'
  {"m~n":"baz"}
  $ ./test_pointer.exe replace '{"":"bar"}' '/' '"baz"'
  {"":"baz"}

Remove tests:
  $ ./test_pointer.exe remove '{"foo":"bar","baz":"qux"}' '/foo'
  {"baz":"qux"}
  $ ./test_pointer.exe remove '["foo","baz"]' '/1'
  ["foo"]
  $ ./test_pointer.exe remove '["foo","baz"]' '/0'
  ["baz"]
  $ ./test_pointer.exe remove '{"foo":["bar"]}' '/foo/0'
  {"foo":[]}

Copy tests:
  $ ./test_pointer.exe copy '{"foo":"bar"}' '/foo' '/baz'
  {"foo":"bar","baz":"bar"}

Test operation:
  $ ./test_pointer.exe test '{"foo":"bar"}' '/foo' '"bar"'
  true
  $ ./test_pointer.exe test '{"foo":"bar"}' '/foo' '"baz"'
  false
  $ ./test_pointer.exe test '{"foo":["bar","baz"]}' '/foo' '["bar","baz"]'
  true
  $ ./test_pointer.exe test '{"foo":"bar"}' '/baz' '"qux"'
  false

Equality tests (pointers that should be equal after roundtrip):
  $ ./test_pointer.exe roundtrip "/foo/~0"
  OK: /foo/~0
  $ ./test_pointer.exe roundtrip "/foo/~1"
  OK: /foo/~1

Has/exists tests (from json-pointer-js):
  $ ./test_pointer.exe has '{"foo":"bar"}' '/foo'
  true
  $ ./test_pointer.exe has '{"foo":"bar"}' '/bar'
  false
  $ ./test_pointer.exe has '{"foo":{"bar":"baz"}}' '/foo/bar'
  true
  $ ./test_pointer.exe has '{"foo":{"bar":"baz"}}' '/foo/qux'
  false
  $ ./test_pointer.exe has '["foo","bar"]' '/0'
  true
  $ ./test_pointer.exe has '["foo","bar"]' '/1'
  true
  $ ./test_pointer.exe has '["foo","bar"]' '/2'
  false
  $ ./test_pointer.exe has '{"foo":["bar"]}' '/foo/0'
  true
  $ ./test_pointer.exe has '{"foo":["bar"]}' '/foo/1'
  false
  $ ./test_pointer.exe has '{}' ''
  true
  $ ./test_pointer.exe has '[]' ''
  true
  $ ./test_pointer.exe has '{"":0}' '/'
  true
  $ ./test_pointer.exe has '{"a/b":1}' '/a~1b'
  true
  $ ./test_pointer.exe has '{"m~n":8}' '/m~0n'
  true

Has with null values:
  $ ./test_pointer.exe has '{"foo":null}' '/foo'
  true
  $ ./test_pointer.exe has 'null' ''
  true

An array [-] token does not resolve to an existing value:
  $ ./test_pointer.exe has '["foo"]' '/-'
  false
  $ ./test_pointer.exe has '[]' '/-'
  false
