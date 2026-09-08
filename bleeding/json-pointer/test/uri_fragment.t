URI Fragment Encoding Tests (RFC 6901 Section 6)

Roundtrip through URI fragment encoding:
  $ ./test_pointer.exe uri-fragment ""
  OK:  -> 
  $ ./test_pointer.exe uri-fragment "/foo"
  OK: /foo -> /foo
  $ ./test_pointer.exe uri-fragment "/foo/0"
  OK: /foo/0 -> /foo/0
  $ ./test_pointer.exe uri-fragment "/"
  OK: / -> /
  $ ./test_pointer.exe uri-fragment "/a~1b"
  OK: /a~1b -> /a~1b
  $ ./test_pointer.exe uri-fragment "/m~0n"
  OK: /m~0n -> /m~0n

Characters requiring percent-encoding:
  $ ./test_pointer.exe uri-fragment "/c%d"
  OK: /c%d -> /c%25d
  $ ./test_pointer.exe uri-fragment "/e^f"
  OK: /e^f -> /e%5Ef
  $ ./test_pointer.exe uri-fragment "/g|h"
  OK: /g|h -> /g%7Ch
  $ ./test_pointer.exe uri-fragment '/i\j'
  OK: /i\j -> /i%5Cj
  $ ./test_pointer.exe uri-fragment '/k"l'
  OK: /k"l -> /k%22l
  $ ./test_pointer.exe uri-fragment "/ "
  OK: /  -> /%20

Roundtrip tests:
  $ ./test_pointer.exe roundtrip ""
  OK: 
  $ ./test_pointer.exe roundtrip "/foo"
  OK: /foo
  $ ./test_pointer.exe roundtrip "/foo/0"
  OK: /foo/0
  $ ./test_pointer.exe roundtrip "/"
  OK: /
  $ ./test_pointer.exe roundtrip "/a~1b"
  OK: /a~1b
  $ ./test_pointer.exe roundtrip "/m~0n"
  OK: /m~0n
  $ ./test_pointer.exe roundtrip "/-"
  OK: /-
  $ ./test_pointer.exe roundtrip "/a/b/c"
  OK: /a/b/c

RFC 6901 Section 6 examples (URI fragment encoding):
Note: These test the full RFC 6901 Section 6 examples
  $ ./test_pointer.exe uri-fragment "/c%d"
  OK: /c%d -> /c%25d
  $ ./test_pointer.exe uri-fragment "/e^f"
  OK: /e^f -> /e%5Ef
  $ ./test_pointer.exe uri-fragment "/g|h"
  OK: /g|h -> /g%7Ch
  $ ./test_pointer.exe uri-fragment '/i\j'
  OK: /i\j -> /i%5Cj
  $ ./test_pointer.exe uri-fragment '/k"l'
  OK: /k"l -> /k%22l
  $ ./test_pointer.exe uri-fragment "/ "
  OK: /  -> /%20

Unicode in URI fragments:
  $ ./test_pointer.exe uri-fragment "/café"
  OK: /café -> /caf%C3%A9
  $ ./test_pointer.exe uri-fragment "/日本語"
  OK: /日本語 -> /%E6%97%A5%E6%9C%AC%E8%AA%9E

Combined escapes (tilde escape + URI encode):
Note: %25 in result is the URI encoding of %, and 100% is treated as member name
  $ ./test_pointer.exe uri-fragment "/100%"
  OK: /100% -> /100%25

Multiple special chars:
  $ ./test_pointer.exe uri-fragment "/a b^c|d"
  OK: /a b^c|d -> /a%20b%5Ec%7Cd

RFC 3986 fragment-safe reserved characters remain unescaped:
  $ ./test_pointer.exe uri-fragment "/&;+"
  OK: /&;+ -> /&;+
  $ ./test_pointer.exe uri-fragment "/:@/?"
  OK: /:@/? -> /:@/?

Fragment delimiters and brackets are escaped:
  $ ./test_pointer.exe uri-fragment "/#[]"
  OK: /#[] -> /%23%5B%5D
