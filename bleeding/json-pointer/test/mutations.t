JSON Pointer Mutation Tests (RFC 6902 JSON Patch operations)

Add to object:
  $ ./test_pointer.exe add '{"foo":"bar"}' '/baz' '"qux"'
  {"foo":"bar","baz":"qux"}

Add to array (insert before):
  $ ./test_pointer.exe add '{"foo":["bar","baz"]}' '/foo/1' '"qux"'
  {"foo":["bar","qux","baz"]}

Add to array at beginning:
  $ ./test_pointer.exe add '{"foo":["bar","baz"]}' '/foo/0' '"qux"'
  {"foo":["qux","bar","baz"]}

Add to array at end using -:
  $ ./test_pointer.exe add '{"foo":["bar"]}' '/foo/-' '"qux"'
  {"foo":["bar","qux"]}
  $ ./test_pointer.exe add '{"foo":["bar"]}' '/foo/1' '"qux"'
  {"foo":["bar","qux"]}

Add at the root replaces the document:
  $ ./test_pointer.exe add '{"foo":"bar"}' '' '[1,2]'
  [1,2]

Add replaces existing member:
  $ ./test_pointer.exe add '{"foo":"bar"}' '/foo' '"baz"'
  {"foo":"baz"}

Add rejects an ambiguous duplicate member target:
  $ ./test_pointer.exe duplicate add
  ERROR: JSON Pointer: member 'duplicate' is not unique

Remove from object:
  $ ./test_pointer.exe remove '{"foo":"bar","baz":"qux"}' '/baz'
  {"foo":"bar"}

Remove from array:
  $ ./test_pointer.exe remove '{"foo":["bar","qux","baz"]}' '/foo/1'
  {"foo":["bar","baz"]}

Remove first element:
  $ ./test_pointer.exe remove '{"foo":["bar","baz"]}' '/foo/0'
  {"foo":["baz"]}

Replace in object:
  $ ./test_pointer.exe replace '{"foo":"bar"}' '/foo' '"baz"'
  {"foo":"baz"}

Replace in array:
  $ ./test_pointer.exe replace '{"foo":["bar","baz"]}' '/foo/0' '"qux"'
  {"foo":["qux","baz"]}

Move within object:
  $ ./test_pointer.exe move '{"foo":{"bar":"baz"},"qux":{}}' '/foo/bar' '/qux/thud'
  {"foo":{},"qux":{"thud":"baz"}}

Move within array:
  $ ./test_pointer.exe move '{"foo":["a","b","c","d"]}' '/foo/1' '/foo/3'
  {"foo":["a","c","d","b"]}

Move to the same location is an exact no-op, including at the root:
  $ ./test_pointer.exe move '{"foo":"bar"}' '/foo' '/foo'
  {"foo":"bar"}
  $ ./test_pointer.exe move '{"foo":"bar"}' '' ''
  {"foo":"bar"}

Copy within object:
  $ ./test_pointer.exe copy '{"foo":{"bar":"baz"}}' '/foo/bar' '/foo/qux'
  {"foo":{"bar":"baz","qux":"baz"}}

Copy to new location:
  $ ./test_pointer.exe copy '{"foo":"bar"}' '/foo' '/baz'
  {"foo":"bar","baz":"bar"}

Test equality (true):
  $ ./test_pointer.exe test '{"foo":"bar"}' '/foo' '"bar"'
  true

Test equality (false):
  $ ./test_pointer.exe test '{"foo":"bar"}' '/foo' '"baz"'
  false

Test array equality:
  $ ./test_pointer.exe test '{"foo":["bar","baz"]}' '/foo' '["bar","baz"]'
  true

Test nonexistent path returns false:
  $ ./test_pointer.exe test '{"foo":"bar"}' '/baz' '"qux"'
  false

Error: remove root:
  $ ./test_pointer.exe remove '{"foo":"bar"}' ''
  ERROR: JSON Pointer: cannot remove root document
  File "-":

Error: remove nonexistent:
  $ ./test_pointer.exe remove '{"foo":"bar"}' '/baz'
  ERROR: JSON Pointer: member 'baz' not found for remove
  File "-":

Error: replace nonexistent:
  $ ./test_pointer.exe replace '{"foo":"bar"}' '/baz' '"qux"'
  ERROR: JSON Pointer: member 'baz' not found
  File "-":

Error: add to out of bounds index:
  $ ./test_pointer.exe add '{"foo":["bar"]}' '/foo/5' '"qux"'
  ERROR: JSON Pointer: index 5 out of bounds for add (array has 1 elements)
  File "-":
  $ ./test_pointer.exe add '{"foo":["bar"]}' '/foo/01' '"qux"'
  ERROR: JSON Pointer: invalid array index '01'
  File "-":

Error: add cannot invent an intermediate object (RFC 6902 A.12):
  $ ./test_pointer.exe add '{"foo":"bar"}' '/baz/bat' '"qux"'
  ERROR: JSON Pointer: member 'baz' not found
  File "-":

Add nested path (parent must exist):
  $ ./test_pointer.exe add '{"foo":{}}' '/foo/bar' '"baz"'
  {"foo":{"bar":"baz"}}

Add to root-level array:
  $ ./test_pointer.exe add '["a","b"]' '/1' '"x"'
  ["a","x","b"]
  $ ./test_pointer.exe add '["a","b"]' '/-' '"c"'
  ["a","b","c"]
  $ ./test_pointer.exe add '["a","b"]' '/0' '"x"'
  ["x","a","b"]

Replace root document:
  $ ./test_pointer.exe replace '{"foo":"bar"}' '' '"new root"'
  "new root"
  $ ./test_pointer.exe replace '["a","b"]' '' '{"replaced":true}'
  {"replaced":true}

Move from array to object:
  $ ./test_pointer.exe move '{"arr":["x","y"],"obj":{}}' '/arr/0' '/obj/item'
  {"arr":["y"],"obj":{"item":"x"}}

Copy array element:
  $ ./test_pointer.exe copy '{"arr":["x","y"]}' '/arr/0' '/arr/-'
  {"arr":["x","y","x"]}

Add an array as one array element (RFC 6902 A.16):
  $ ./test_pointer.exe add '{"foo":["bar"]}' '/foo/-' '["abc","def"]'
  {"foo":["bar",["abc","def"]]}

Test with numeric string member names (RFC 6901: context determines interpretation):
  $ ./test_pointer.exe add '{"0":"zero","1":"one"}' '/2' '"two"'
  {"0":"zero","1":"one","2":"two"}
  $ ./test_pointer.exe eval data/numeric_keys.json "/0"
  OK: "zero"

Special characters in member names:
  $ ./test_pointer.exe add '{}' '/a~1b' '"slash"'
  {"a/b":"slash"}
  $ ./test_pointer.exe add '{}' '/m~0n' '"tilde"'
  {"m~n":"tilde"}
  $ ./test_pointer.exe replace '{"a/b":"old"}' '/a~1b' '"new"'
  {"a/b":"new"}

Deep nested mutations:
  $ ./test_pointer.exe add '{"a":{"b":{"c":{}}}}' '/a/b/c/d' '"deep"'
  {"a":{"b":{"c":{"d":"deep"}}}}
  $ ./test_pointer.exe remove '{"a":{"b":{"c":{"d":"deep"}}}}' '/a/b/c/d'
  {"a":{"b":{"c":{}}}}

Error: remove from nonexistent array index:
  $ ./test_pointer.exe remove '{"foo":["bar"]}' '/foo/5'
  ERROR: JSON Pointer: index 5 out of bounds for remove
  File "-":

Error: move to descendant of source is forbidden before mutation:
  $ ./test_pointer.exe move '{"a":{"b":"c"}}' '/a' '/a/b'
  ERROR: JSON Pointer: move source is a proper prefix of its destination
  File "-":

Copy onto the same object member replaces it with an equal value:
  $ ./test_pointer.exe copy '{"foo":"bar"}' '/foo' '/foo'
  {"foo":"bar"}

Test value equality with different types:
  $ ./test_pointer.exe test '{"n":1}' '/n' '1'
  true
  $ ./test_pointer.exe test '{"n":1.0}' '/n' '1'
  true
  $ ./test_pointer.exe test '{"n":"1"}' '/n' '1'
  false
  $ ./test_pointer.exe test '{"b":true}' '/b' 'true'
  true
  $ ./test_pointer.exe test '{"b":false}' '/b' 'false'
  true
  $ ./test_pointer.exe test '{"n":null}' '/n' 'null'
  true

Test deep equality:
  $ ./test_pointer.exe test '{"obj":{"a":1,"b":2}}' '/obj' '{"a":1,"b":2}'
  true
  $ ./test_pointer.exe test '{"obj":{"a":1,"b":2}}' '/obj' '{"b":2,"a":1}'
  true
  $ ./test_pointer.exe test '{"arr":[1,2,3]}' '/arr' '[1,2,3]'
  true

Escape ordering in a test operation (RFC 6902 A.14):
  $ ./test_pointer.exe test '{"/":9,"~1":10}' '/~01' '10'
  true
