ppx_simple_xml_conv
===================

A ppx rewriter for generating XML serializers/deserializers from OCaml type definitions.
It relies on `Simple_xml` as the underlying XML library.

Overview
========

To derive XML serializers/deserializers on a type, you use the `xml` deriver.

<!--
```ocaml
open! Core

let print_xml xml =
  Element xml
  |> Simple_xml.to_string ~fmt:(`Indent 2)
  |> print_endline;
;;
```
-->

```ocaml
# module Xmlable : sig
    type t = int [@@deriving xml]
  end = struct
    type t = (int[@xml.leaf "count"]) [@@deriving xml]
  end
module Xmlable :
  sig
    type t = int
    val xml_of_t : t -> Simple_xml.element
    val t_of_xml_description : t Ppx_simple_xml_conv_lib.Of_xml.t
    val t_of_xml : Simple_xml.element -> t
  end
```

This will generate a xml parser description (`t_of_xml_description`)
which allows us to parse any number of xml elements whose tag is `count`.
Additionally, it generates a parse function (`t_of_xml`), and a serialize
function (`xml_of_t`).

```ocaml
# Xmlable.xml_of_t 15 |> print_xml
<?xml version="1.0" encoding="UTF-8"?>
<count>
  15
</count>
```

If you wish to only generate deserializers, you can use `of_xml`

```ocaml
# module Xmlable : sig
    type t [@@deriving of_xml]
  end = struct
    type t = (int[@xml.leaf "count"]) [@@deriving of_xml]
  end
module Xmlable :
  sig
    type t
    val t_of_xml_description : t Ppx_simple_xml_conv_lib.Of_xml.t
    val t_of_xml : Simple_xml.element -> t
  end
```
If you wish to only generate serializers, you can use `xml_of`

```ocaml
# module Xmlable : sig
    type t [@@deriving xml_of]
  end = struct
    type t = (int[@xml.leaf "count"]) [@@deriving xml_of]
  end
module Xmlable : sig type t val xml_of_t : t -> Simple_xml.element end
```

Conversion rules
================

In the following, we'll review the (de)serialization rules for different
OCaml types.

Basic types
-----------

For type `foo`, the ppx will for use `foo_of_xml_description` for the
deserializer, and `xml_of_foo`.
It has no default handling for any types. This is due to the fact that
XML has very little in the way of types, but at the same time requires
elements to carry tag names.

### Generic types

For type `(a, b) c`, the ppx will use
```ocaml skip
c_of_xml_description a_of_xml_description b_of_xml_description
```
for the deserializers, and
```ocaml skip
xml_of_c xml_of_a xml_of_b
```
for the serializers.


### Attributes

The ppx provides a helper attribute that you can attach to the type
that generates leaf elements for you:

```ocaml
type t = (float [@xml.leaf "tag-name"]) [@@deriving xml]
```

This will load the `of_string` definition of `ty`, and uses it to create
a leaf element parser whose tag is `tag-name`, as well as the `to_string`
definition, and use it to generate a leaf element serializer. This attribute
can take the following labelled arguments:

* `~of_string:_`: uses a custom `of_string` function.
* `~to_string:_`: uses a custom `to_string` function.
* `~ignore_attributes`: do not fail if the leaf element has attributes. Namespace attributes
  do not cause the parser to raise.
* `~preserve_space`: do not strip leading and trailing whitespace from the element content
  when parsing it.
* `~namespace:_`: On serialization, generate the element with the given namespace (note that
  you should provide the namespace URI and not the namespace prefix). On deserialization,
  only matches elements that have that namespace.
* `~assert_no_namespace`: This only matters on deserialization, only matches elements
  that have no namespace. This is as opposed to the default behavior which is to ignore
  the namespace.

For example:

```ocaml
type t =
  (Time_ns.t
   [@xml.leaf
     "time"
       ~to_string:Time_ns.to_string_utc
       ~of_string:Time_ns.of_string_with_utc_offset
       ~ignore_attributes])
[@@deriving xml]
```

```ocaml
# xml_of_t Time_ns.epoch |> print_xml
<?xml version="1.0" encoding="UTF-8"?>
<time>
  1970-01-01 00:00:00.000000000Z
</time>
```

Additionally, it provides you an empty attribute of the form `[@xml.empty "tag-name"]`.
This will match empty elements. The type has to be unit. If the element is not actually empty,
you can use the following attributes:

* `~ignore_children`: this will make sure the parser does not raise if the element has
  children.
* `~ignore_attributes`: this will make sure the parser does not raise if the element has
  attributes. The parser will not raise if the attributes are namespace prefix/default
  namespace attributes.
* `~namespace:_`: On serialization, generate the element with the given namespace (note that
  you should provide the namespace URI and not the namespace prefix). On deserialization,
  only matches elements that have that namespace.
* `~assert_no_namespace`: This only matters on deserialization, only matches elements
  that have no namespace. This is as opposed to the default behavior which is to ignore
  the namespace.

This attribute is useful when you want to provide an empty tag as an argument to a type,
e.g. `type t = (unit[@xml.empty "foo"]) Some_type.t`.

Records
-------

Records are used to define XML element structures.

```ocaml skip
type t =
 { a_field : A.t
 ; b_field : B.t
 ; c_field : C.t
 }
[@@deriving xml ~tag:"tag-name"]
```

If `A.t`'s xml parser parses elements whose tag is `<a>`, and similarly for `B.t` and `C.t`,
then this XML parser will parse an element of the format:

```xml
<tag-name>
  <a>...</a>
  <b>...</b>
  <c>...</c>
</tag-name>
```

**Note here that the tag name is specified in the parser, and that the field names
are completely ignored from the point of view of parsing.**

### Namespaces

By default, the ppx ignores namespaces, so that it neither generates nor checks namespaces
for the attributes and elements it handles. However, users of the PPX can opt into using
namespaces by providing the appropriate arguments as detailed below.

#### Serialization

You can specify that the generated namespace for the record has a particular value
by providing the namespace URI using the `~namespace` argument. Note that XML
does not allow you to specify the namespace for an element or attribute directly,
and thus you either have to declare a prefix in the element or in a surrounding element
that the serializer can use to reference the namespace. *Note that we cannot catch
undeclared namespaces at compile time*, so serializing the resulting XML element (e.g. 
via `Simple_xml.to_string`) will raise if a namespace is not bound to a prefix.
For example:

```ocaml skip
(* Note that namespace and prefixes can be any arbitrary expression as long as
   the resulting type is correct.
 *)
let foo_namespace = "http://foo.com"
let foo_prefixes = [ "foo", namespace ]

type t =
  { field : Field.t
    (* The namespace does not apply to [field] necessarily, but the declared prefixes will
       be valid when serializing [field] unless shadowed.
     *)
  }
[@@deriving of_xml ~tag:"tag-name" ~namespace:foo_namespace ~prefixes:foo_prefixes]
```

#### Deserialization

You can enforce that an element has a particular namespace when parsed using `~namespace`
(the same attribute used in the section above). Additionally, you can enforce that
an element does not have a particular namespace when parsed by passing `~assert_no_namespace`.
This is particularly useful when mixing multiple elements with the same tag but
different namespaces the same context, since you have to distinguish the parsers
and if you do not provide `~namespace` or `~assert_no_namespace` namespaces are
completely ignored.

### Attributes

If a field is an attribute, you can use the following attribute on the field:
```ocaml skip
[@xml.attribute "attribute-key"]
```

It will use the `of_string` definition of the field type to parse the attribute,
and the `to_string` definition of the field type to serialize the attribute.
It supports the following labelled arguments:

* `~of_string:_`: uses a custom `of_string` function.
* `~to_string:_`: uses a custom `to_string` function.
* `~optional`: expectes the type to be of the form `_ option`. On parsing, if the xml
  attribute is missing, the field is assigned the value `None`. On serialization, if the
  value is None, the xml attribute is dropped.
* `~default:_`: If the xml attribute is missing on parsing, the field is assigned the
  provided default value. This is incompatible with `~optional`.
* `~namespace:_`: On serialization, generate the attribute with the given namespace (note that
  you should provide the namespace URI and not the namespace prefix). On deserialization,
  only matches attributes that have that namespace.
* `~assert_no_namespace`: This only matters on deserialization, only matches attributes
  that have no namespace. This is as opposed to the default behavior which is to ignore
  the namespace.

### Element count

The ppx provides 3 element count modifiers which are mutually exclusive:

* **Optional elements:** If the element is optional, you can use the `[@xml.option]` attribute.
  This expects the type to be of the form `_ option`. On parsing, if the
  element is missing, the field is assigned the value `None`. On serialization,
  if the value is None, the element is dropped.
* **Repeated elements:** If the element can occur more than once, you can use the `[@xml.list]`
  attribute. This expects the type to be of the form `_ list`. The generated parser will
  populate the field as a list in the order the elements appear in.
* **Optional elements with default:** If the element is optional, you can `[@xml.default value]`
  to provide the default value. There are no constraints on the type of the element.
* **Boolean elements:** These are similar to optional element, but instead of (de)serializing
  a type of the form `[_ option]`, you can use `[@xml.bool "tag-name"]` to (de)serialize
  any type that has two values. By default, that type is assumed to be boolean.
  If you have another type, you need to pass a `~true_` and `~false_` argument to
  specify the expressions to use for when the element exists, and when the element is missing
  respectively. If you are trying to deserialize, the expression needs to be convertible
  to a pattern. Additionally, you can provide `~ignore_children` and `~ignore_attributes`
  to ignore any child elements or attributes on deserialization, and `~namespace` or
  `~assert_no_namespace` to specify a particular handling of namespaces. For example:
  ```ocaml
  type t =
    { foo : [ `Enabled | `Disabled ]
            [@xml.bool
              "foo" ~true_:`Enabled ~false_:`Disabled ~ignore_children ~ignore_attributes]
    }
  [@@deriving xml ~tag:"tag-name"]
  ```

### Ignoring extra elements and attributes

The XML parser is strict, which means that extra elements/attributes (except for namespace
prefix/default namespace attributes) will cause it to fail. If you want to allow extra
elements and extra attributes, you can derive `xml`/`of_xml` like this:
```ocaml skip
[@@deriving of_xml ~tag:"tag-name" ~allow_extra_elements ~allow_extra_attributes]
```
### Elements with attributes and text

If you wish to parse elements with attributes and text into a record and vice-versa, you can
use the `[@xml.text]` attribute. For example:

```ocaml
type t =
  { foo : string [@xml.attribute "foo"]
  ; content : int [@xml.text]
  }
[@@deriving xml ~tag:"bar"]
```

This uses `int_of_string` to parse the content. It assumes that the element only
contains text and no children elements.

```ocaml
# xml_of_t { foo = "baz"; content = 2 } |> print_xml
<?xml version="1.0" encoding="UTF-8"?>
<bar foo="baz">
  2
</bar>
```

### Inlined records

Sometimes, XML elements are structured in such a way that generic elements/attributes are embedded into
multiple different elements directly. For example, say you have a bunch of records, each
including a username, and a time of creation. What you can do is use the inlined parameter
to derive inlinable xml. For example:

```ocaml
# module Metadata : sig
    type t =
      { username : string
      ; created_at : Time_ns_unix.t
      }
    [@@deriving xml ~inlined]
  end = struct
    type t =
      { username : (string [@xml.leaf "username"])
      ; created_at : Time_ns_unix.t [@xml.attribute "created-at"]
      }
    [@@deriving xml ~inlined]
  end
module Metadata :
  sig
    type t = { username : string; created_at : t; }
    val inlined_xml_of_t : t Ppx_simple_xml_conv_lib.To_xml.inlined
    val t_of_xml_inlined : t Ppx_simple_xml_conv_lib.Of_xml.inlined
  end
```

```ocaml
module Foo = struct
  type t =
    { foo : (int [@xml.leaf "foo"])
    ; metadata : Metadata.t [@xml.inlined]
    }
  [@@deriving xml ~tag:"foo"]
end
```

```ocaml
# Foo.xml_of_t { foo = 1;  metadata = { username = "alice"; created_at = Time_ns.epoch } }
  |> print_xml
<?xml version="1.0" encoding="UTF-8"?>
<foo created-at="1969-12-31 19:00:00.000000000-05:00">
  <foo>
    1
  </foo>
  <username>
    alice
  </username>
</foo>
```

Note that `[@xml.text]` is not allowed inside inlined derivers.

Variants
--------

Variants can be used to group different alternatives. It can only handle variant
constructors with one argument or with 0 arguments for now. For example:

<!--
```ocaml
module A = struct
  type t = (string[@xml.leaf "a"])
  [@@deriving xml]
end
```
-->
```ocaml
type t =
  | A of A.t
  | B of (int[@xml.leaf "b"])
  | C [@xml.empty "c"]
[@@deriving xml]
```

The generated parser will now match whatever tags are handled by `A.t`'s parser and assign
it to constructor `A`, tag `b` and assign it to constructor `B`, or the empty element with
tag `c` and create the constructor `C`.

```ocaml
# xml_of_t (A "foo") |> print_xml
<?xml version="1.0" encoding="UTF-8"?>
<a>
  foo
</a>
# xml_of_t (B 15) |> print_xml
<?xml version="1.0" encoding="UTF-8"?>
<b>
  15
</b>
# xml_of_t C |> print_xml
<?xml version="1.0" encoding="UTF-8"?>
<c/>
```

### Constructors without attributes

An empty branch needs to at least have the `[@xml.empty "tag-name"]` attribute. This will
match empty elements. If the element is not actually empty, you can use the following
attributes:

* `~ignore_children`: this will make sure the parser does not raise if the element has
  children.
* `~ignore_attributes`: this will make sure the parser does not raise if the element has
  attributes. The parser will not raise if the attributes are namespace prefix/default
  namespace attributes.
* `~namespace:_`: On serialization, generate the attribute with the given namespace (note that
  you should provide the namespace URI and not the namespace prefix). On deserialization,
  only matches attributes that have that namespace.
* `~assert_no_namespace`: This only matters on deserialization, only matches attributes
  that have no namespace. This is as opposed to the default behavior which is to ignore
  the namespace.

### Extensions

If you want to generate a (de)serializer inside an expression, you can use the following
extensions:

```ocaml
# let xml_of = [%xml_of: t]
val xml_of : t -> Simple_xml.element = <fun>
# let of_xml = [%of_xml: t]
val of_xml : Simple_xml.element -> t = <fun>
# let of_xml_description = [%of_xml_description: t]
val of_xml_description : t Ppx_simple_xml_conv_lib.Of_xml.t =
  Ppx_simple_xml_conv_lib.Of_xml.Variant <lazy>
```

