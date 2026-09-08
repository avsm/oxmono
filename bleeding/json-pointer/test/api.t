Public API smoke tests

Pointers are token sequences; token interpretation is deferred until evaluation.

  $ ./test_pointer.exe root
  root = 
  is_root(root) = true
  $ ./test_pointer.exe is-root ""
  true
  $ ./test_pointer.exe is-root "/foo"
  false
  $ ./test_pointer.exe of-tokens "foo,0,a/b"
  /foo/0/a~1b
  $ ./test_pointer.exe tokens "/foo/0/a~1b/-"
  ["foo"; "0"; "a/b"; "-"]

Appending, concatenating, and inspecting tokens:

  $ ./test_pointer.exe append "" "foo"
  /foo
  $ ./test_pointer.exe append "/foo" "a/b"
  /foo/a~1b
  $ ./test_pointer.exe concat "/a/b" "/c/d"
  /a/b/c/d
  $ ./test_pointer.exe parent ""
  None
  $ ./test_pointer.exe parent "/a/b"
  Some(/a)
  $ ./test_pointer.exe parent "/"
  Some()
  $ ./test_pointer.exe last ""
  None
  $ ./test_pointer.exe last "/a~1b"
  Some("a/b")
  $ ./test_pointer.exe last "/"
  Some("")

Result-returning parsers reject malformed pointer and percent syntax:

  $ ./test_pointer.exe of-string-result "/foo/~0"
  Ok(/foo/~0)
  $ ./test_pointer.exe of-string-result "foo"
  Error(Invalid JSON Pointer: must be empty or start with '/': foo)
  $ ./test_pointer.exe of-string-result "/~2"
  Error(Invalid JSON Pointer: invalid escape sequence ~2)
  $ ./test_pointer.exe of-uri-fragment-result "/a%20b"
  Ok(/a b)
  $ ./test_pointer.exe of-uri-fragment-result "%2Ffoo%2F0"
  Ok(/foo/0)
  $ ./test_pointer.exe of-uri-fragment-result "/a%7E1b"
  Ok(/a~1b)
  $ ./test_pointer.exe of-uri-fragment-result "/%"
  Error(Incomplete percent-encoding at position 1)
  $ ./test_pointer.exe of-uri-fragment-result "/%GG"
  Error(Invalid percent-encoding at position 1)
  $ ./test_pointer.exe of-uri-fragment-result "/%FF"
  Error(Invalid JSON Pointer: input is not valid UTF-8)

Formatting, equality, ordering, and Jsont.Path conversion:

  $ ./test_pointer.exe pp "/a~1b/0"
  /a~1b/0
  $ ./test_pointer.exe equal "/foo" "/foo"
  true
  $ ./test_pointer.exe equal "/foo" "/foo/-"
  false
  $ ./test_pointer.exe compare "/foo" "/foo/bar"
  LT
  $ ./test_pointer.exe compare "/z" "/a"
  GT
  $ ./test_pointer.exe of-path
  /0/foo/1

Evaluation and JSON Patch operations use the containing JSON value to interpret
numeric and [-] tokens:

  $ ./test_pointer.exe get-result '{"0":"zero","-":"dash"}' '/0'
  Ok("zero")
  $ ./test_pointer.exe get-result '{"0":"zero","-":"dash"}' '/-'
  Ok("dash")
  $ ./test_pointer.exe get-result '{"-":{"x":1}}' '/-/x'
  Ok(1)
  $ ./test_pointer.exe add '{"-":1}' '/-' '2'
  {"-":2}
  $ ./test_pointer.exe add '[1]' '/-' '2'
  [1,2]
  $ ./test_pointer.exe remove '{"-":1,"x":2}' '/-'
  {"x":2}
  $ ./test_pointer.exe replace '{"-":1}' '/-' '2'
  {"-":2}

The move prefix rule forbids moving a value into its descendant, but permits
moving a child to an ancestor:

  $ ./test_pointer.exe move '{"a":{"b":1},"c":2}' '/a/b' '/a'
  {"a":1,"c":2}
  $ ./test_pointer.exe move '{"a":{"b":1}}' '/a' '/a/b'
  ERROR: JSON Pointer: move source is a proper prefix of its destination
  File "-":

Jsont codecs:

  $ ./test_pointer.exe jsont-codec ""
  ""
  $ ./test_pointer.exe jsont-codec "/foo/-"
  "/foo/-"
  $ ./test_pointer.exe jsont-uri-fragment "/a b/c%d"
  "/a%20b/c%25d"
  $ ./test_pointer.exe decode-jsont-codec '"not/a/pointer"'
  ERROR: Invalid JSON Pointer: must be empty or start with '/': not/a/pointer
  File "-":
  $ ./test_pointer.exe decode-jsont-uri-fragment '"/%"'
  ERROR: Incomplete percent-encoding at position 1
  File "-":

Query combinators:

  $ ./test_pointer.exe query-path '{"user":{"name":"alice"}}' '/user/name'
  OK: alice
  $ ./test_pointer.exe query-path-absent '{"user":{}}' '/user/name' 'unknown'
  OK: unknown
  $ ./test_pointer.exe query-path-absent '{"items":[]}' '/items/01' 'unknown'
  ERROR: JSON Pointer: invalid array index '01'
  File "-":
  $ ./test_pointer.exe query-path-absent '{"items":1}' '/items/name' 'unknown'
  ERROR: JSON Pointer: cannot index into number with 'name'
  File "-":
  $ ./test_pointer.exe set-path '{"name":"alice"}' '/name' 'bob'
  {"name":"bob"}
  $ ./test_pointer.exe set-path-absent '{"user":{}}' '/user/name' 'alice'
  {"user":{"name":"alice"}}
  $ ./test_pointer.exe set-path-absent '{"items":["a","b"]}' '/items/1' 'x'
  {"items":["a","x"]}
  $ ./test_pointer.exe set-path-absent '{"items":["a","b"]}' '/items/2' 'x'
  {"items":["a","b","x"]}
  $ ./test_pointer.exe update-path-absent '{"user":{}}' '/user/name' 'alice'
  {"user":{"name":"alice"}}
  $ ./test_pointer.exe delete-path '{"foo":1,"bar":2}' '/foo'
  {"bar":2}
  $ ./test_pointer.exe delete-path-absent '{"foo":1}' '/bar'
  {"foo":1}
  $ ./test_pointer.exe delete-path-absent '{"items":["a"]}' '/items/-'
  {"items":["a"]}
  $ ./test_pointer.exe delete-path-absent '{"items":["a"]}' '/items/01'
  ERROR: JSON Pointer: invalid array index '01'
  File "-":
  $ ./test_pointer.exe delete-path-absent '{"user":{}}' '/missing/name'
  ERROR: JSON Pointer: member 'missing' not found
  File "-":
  $ ./test_pointer.exe duplicate delete
  ERROR: JSON Pointer: member 'duplicate' is not unique

JMAP reuses ordinary pointers. A [*] token is a wildcard only for arrays and is
an ordinary member name for objects:

  $ ./test_pointer.exe jmap-eval '[[1,2],[3]]' '/*'
  OK: [1,2,3]
  $ ./test_pointer.exe jmap-eval '{"*":"star"}' '/*'
  OK: "star"
  $ ./test_pointer.exe jmap-eval '{"-":"dash"}' '/-'
  OK: "dash"
  $ ./test_pointer.exe jmap-get-result '{"list":[{"id":"a"},{"id":"b"}]}' '/list/*/id'
  Ok(["a","b"])
  $ ./test_pointer.exe jmap-find '{"list":[]}' '/list/*/id'
  Some([])
  $ ./test_pointer.exe jmap-find '{"list":1}' '/list/*/id'
  None
