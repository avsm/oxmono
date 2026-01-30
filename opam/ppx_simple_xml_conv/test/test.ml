open! Core

module _ = struct
  module A = struct
    type t = (int[@xml.leaf "a"]) [@@deriving sexp_of, xml]
  end

  module B = struct
    type t = (string[@xml.leaf "b"]) [@@deriving sexp_of, xml]
  end

  module C = struct
    type t = (int[@xml.leaf "c"]) [@@deriving sexp_of, xml]
  end

  module Foo = struct
    type t =
      | Int of A.t
      | String of B.t
      | Empty [@xml.empty "empty"]
    [@@deriving sexp_of, xml]
  end

  module _ = struct
    type t = { foos : Foo.t list [@xml.list] }
    [@@deriving sexp_of, xml ~tag:"foo" ~allow_extra_elements]

    let input_xml =
      Simple_xml.parse
        (`String
          {|<foo>
  <a>1</a>
  <b>foo</b>
  <a>2</a>
  <b>bar</b>
  <c>1</c>
  <empty/>
  <c>2</c>
</foo>|})
      |> t_of_xml
    ;;

    let%expect_test _ =
      print_s [%message (input_xml : t)];
      [%expect
        {| (input_xml ((foos ((Int 1) (String foo) (Int 2) (String bar) Empty)))) |}];
      xml_of_t input_xml
      |> Element
      |> Simple_xml.to_string ~fmt:(`Indent 2)
      |> print_endline;
      [%expect
        {|
        <?xml version="1.0" encoding="UTF-8"?>
        <foo>
          <a>
            1
          </a>
          <b>
            foo
          </b>
          <a>
            2
          </a>
          <b>
            bar
          </b>
          <empty/>
        </foo>
        |}]
    ;;
  end

  type t =
    { a : A.t
    ; b : B.t
    ; c : C.t list [@xml.list] [@sexp.list]
    ; d : t option [@xml.option] [@sexp.option]
    }
  [@@deriving sexp_of, xml ~tag:"tag"]

  let input_xml =
    Simple_xml.parse
      (`String
        {|<tag>
  <a>1</a>
  <b>foo</b>
  <c>1</c>
  <c>2</c>
  <tag>
    <a>1</a>
    <b>foo</b>
   </tag>
</tag>|})
    |> t_of_xml
  ;;

  let%expect_test _ =
    print_s [%message (input_xml : t)];
    [%expect {| (input_xml ((a 1) (b foo) (c (1 2)) (d ((a 1) (b foo))))) |}];
    xml_of_t input_xml
    |> Element
    |> Simple_xml.to_string ~fmt:(`Indent 2)
    |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <tag>
        <a>
          1
        </a>
        <b>
          foo
        </b>
        <c>
          1
        </c>
        <c>
          2
        </c>
        <tag>
          <a>
            1
          </a>
          <b>
            foo
          </b>
        </tag>
      </tag>
      |}]
  ;;
end

module _ = struct
  let time_ns_of_timestamp timestamp =
    Int63.of_string timestamp
    |> Time_ns.Span.of_int63_seconds
    |> Time_ns.of_span_since_epoch
  ;;

  let timestamp_of_time_ns time_ns =
    Time_ns.to_span_since_epoch time_ns
    |> Time_ns.Span.to_int63_seconds_round_down_exn
    |> Int63.to_string
  ;;

  type t =
    { name : (string[@xml.leaf "name" ~ignore_attributes])
    ; age : (int[@xml.leaf "age"])
    ; added_at : Time_ns.Alternate_sexp.t
         [@xml.attribute
           "added-at" ~of_string:time_ns_of_timestamp ~to_string:timestamp_of_time_ns]
    ; verified_by : string option [@xml.attribute "verified-by" ~optional]
    ; account_deleted : bool
         [@xml.attribute "deleted" ~default:false ~to_string:Bool.to_string]
    }
  [@@deriving sexp_of, xml ~tag:"user"]

  module Many : sig
    type nonrec t = { users : t list } [@@deriving sexp_of, xml]
  end = struct
    type nonrec t = { users : t list [@xml.list] } [@@deriving sexp_of, xml ~tag:"users"]
  end

  let input =
    {|<users>
  <user added-at="1388601907" verified-by="jdoe">
    <name type="first-name">Alice</name>
    <age>30</age>
  </user>
  <user added-at="1600378059" deleted="true">
    <name type="first-name">Bob</name>
    <age>23</age>
  </user>
</users>|}
  ;;

  let%expect_test _ =
    Simple_xml.parse (`String input) |> Many.t_of_xml |> Many.sexp_of_t |> print_s;
    [%expect
      {|
      ((users
        (((name Alice) (age 30) (added_at "2014-01-01 18:45:07Z")
          (verified_by (jdoe)) (account_deleted false))
         ((name Bob) (age 23) (added_at "2020-09-17 21:27:39Z") (verified_by ())
          (account_deleted true)))))
      |}];
    Simple_xml.parse (`String input)
    |> Many.t_of_xml
    |> Many.xml_of_t
    |> Element
    |> Simple_xml.to_string ~fmt:(`Indent 2)
    |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <users>
        <user added-at="1388601907" verified-by="jdoe" deleted="false">
          <name>
            Alice
          </name>
          <age>
            30
          </age>
        </user>
        <user added-at="1600378059" deleted="true">
          <name>
            Bob
          </name>
          <age>
            23
          </age>
        </user>
      </users>
      |}]
  ;;
end

module _ = struct
  module Generic_record : sig
    type ('a, 'b) t [@@deriving sexp_of, xml]
  end = struct
    type ('a, 'b) t =
      { name : (string[@xml.leaf "name"])
      ; a : 'a
      ; b : 'b
      }
    [@@deriving sexp_of, xml ~tag:"generic-record"]
  end

  module Generic_variant : sig
    type ('a, 'b) t [@@deriving sexp_of, xml]
  end = struct
    type ('a, 'b) t =
      | A of 'a
      | B of 'b
    [@@deriving sexp_of, xml]
  end

  module A = struct
    type t = (int[@xml.leaf "a"]) [@@deriving sexp_of, xml]
  end

  module B = struct
    type t = (int[@xml.leaf "b"]) [@@deriving sexp_of, xml]
  end

  type t =
    { record : (A.t, B.t) Generic_record.t
    ; variants : (A.t, B.t) Generic_variant.t list [@xml.list]
    }
  [@@deriving sexp_of, xml ~tag:"root"]

  let input =
    {|<root>
      <generic-record>
        <name>Alice</name>
        <a>1</a>
        <b>2</b>
      </generic-record>
      <a>3</a>
      <b>4</b>
      <a>5</a>
    </root>|}
  ;;

  let%expect_test _ =
    Simple_xml.parse (`String input) |> t_of_xml |> sexp_of_t |> print_s;
    [%expect {| ((record ((name Alice) (a 1) (b 2))) (variants ((A 3) (B 4) (A 5)))) |}];
    Simple_xml.parse (`String input)
    |> t_of_xml
    |> xml_of_t
    |> Element
    |> Simple_xml.to_string ~fmt:(`Indent 2)
    |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <root>
        <generic-record>
          <name>
            Alice
          </name>
          <a>
            1
          </a>
          <b>
            2
          </b>
        </generic-record>
        <a>
          3
        </a>
        <b>
          4
        </b>
        <a>
          5
        </a>
      </root>
      |}]
  ;;
end

module _ = struct
  module M = struct
    type t = (int[@xml.leaf "a"]) [@@deriving xml]
  end

  let inputs =
    [ {|<a xmlns="foo">1</a>|}
    ; {|<a xmlns:bar="foo">2</a>|}
    ; {|<a a="hello">3</a>|}
    ; {|<a xmlns:bar="foo" bar:a="hello">4</a>|}
    ]
  ;;

  let%expect_test "Ignore namespace attributes, complain about other attributes" =
    List.iter inputs ~f:(fun input ->
      Or_error.try_with (fun () -> Simple_xml.parse (`String input) |> M.t_of_xml)
      |> [%sexp_of: int Or_error.t]
      |> print_s);
    [%expect
      {|
      (Ok 1)
      (Ok 2)
      (Error (Failure "Ppx_simple_xml_conv, at [a]: extra attributes: a"))
      (Error
       (Failure "Ppx_simple_xml_conv, at [a]: extra attributes: a[namespace=foo]"))
      |}]
  ;;
end

module _ = struct
  module M = struct
    type t =
      { attribute : int [@xml.attribute "attr"]
      ; content : string [@xml.text]
      }
    [@@deriving sexp_of, xml ~tag:"foo"]
  end

  let input = {|<foo attr="2">Hello</foo>|}

  let%expect_test _ =
    Simple_xml.parse (`String input) |> M.t_of_xml |> M.sexp_of_t |> print_s;
    [%expect {| ((attribute 2) (content Hello)) |}];
    Simple_xml.parse (`String input)
    |> M.t_of_xml
    |> M.xml_of_t
    |> Element
    |> Simple_xml.to_string ~fmt:(`Indent 2)
    |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <foo attr="2">
        Hello
      </foo>
      |}]
  ;;
end

module _ = struct
  module M = struct
    module Fake_bool = struct
      type t =
        | A
        | B of unit
      [@@deriving sexp_of, enumerate]
    end

    type t =
      { a : bool [@xml.bool "a"]
      ; b : Fake_bool.t [@xml.bool "b" ~true_:(Fake_bool.B ()) ~false_:Fake_bool.A]
      }
    [@@deriving sexp_of, xml ~tag:"foo"]
  end

  let test_cases =
    let%bind.Sequence a = Sequence.of_list Bool.all in
    let%map.Sequence b = Sequence.of_list M.Fake_bool.all in
    { M.a; b }
  ;;

  let%expect_test _ =
    Sequence.iter test_cases ~f:(fun m ->
      print_endline "---------";
      M.xml_of_t m |> M.t_of_xml |> M.sexp_of_t |> print_s;
      M.xml_of_t m |> Element |> Simple_xml.to_string ~fmt:(`Indent 2) |> print_endline);
    [%expect
      {|
      ---------
      ((a false) (b A))
      <?xml version="1.0" encoding="UTF-8"?>
      <foo/>
      ---------
      ((a false) (b (B ())))
      <?xml version="1.0" encoding="UTF-8"?>
      <foo>
        <b/>
      </foo>
      ---------
      ((a true) (b A))
      <?xml version="1.0" encoding="UTF-8"?>
      <foo>
        <a/>
      </foo>
      ---------
      ((a true) (b (B ())))
      <?xml version="1.0" encoding="UTF-8"?>
      <foo>
        <a/>
        <b/>
      </foo>
      |}]
  ;;
end

(* Simple inlined example. *)
module _ = struct
  module Metadata = struct
    type t =
      { user : (string[@xml.leaf "username"])
      ; created_at : (Time_ns_unix.t[@xml.leaf "created-at"])
      }
    [@@deriving sexp_of, xml ~inlined]
  end

  module Foo = struct
    type t =
      { foo : (string[@xml.leaf "foo-name"])
      ; metadata : Metadata.t [@xml.inlined]
      }
    [@@deriving sexp_of, xml ~tag:"foo"]

    let example =
      { foo = "footastic"; metadata = { user = "alice"; created_at = Time_ns.epoch } }
    ;;
  end

  module Bar = struct
    type t =
      { bar : (string[@xml.leaf "bar-name"])
      ; metadata : Metadata.t [@xml.inlined]
      }
    [@@deriving sexp_of, xml ~tag:"bar"]

    let example =
      { bar = "bar-of-soap"
      ; metadata = { user = "bob"; created_at = Time_ns.(add epoch Span.minute) }
      }
    ;;
  end

  module type Test_case = sig
    type t [@@deriving sexp_of, xml]

    val example : t
  end

  let test (module M : Test_case) =
    print_endline "One way";
    print_endline "=======";
    M.xml_of_t M.example
    |> Element
    |> Simple_xml.to_string ~fmt:(`Indent 2)
    |> print_endline;
    print_endline "Round trip";
    print_endline "==========";
    M.xml_of_t M.example |> M.t_of_xml |> M.sexp_of_t |> print_s
  ;;

  let%expect_test _ =
    test (module Foo);
    [%expect
      {|
      One way
      =======
      <?xml version="1.0" encoding="UTF-8"?>
      <foo>
        <foo-name>
          footastic
        </foo-name>
        <username>
          alice
        </username>
        <created-at>
          1969-12-31 19:00:00.000000000-05:00
        </created-at>
      </foo>
      Round trip
      ==========
      ((foo footastic)
       (metadata ((user alice) (created_at (1969-12-31 19:00:00.000000000-05:00)))))
      |}]
  ;;

  let%expect_test _ =
    test (module Bar);
    [%expect
      {|
      One way
      =======
      <?xml version="1.0" encoding="UTF-8"?>
      <bar>
        <bar-name>
          bar-of-soap
        </bar-name>
        <username>
          bob
        </username>
        <created-at>
          1969-12-31 19:01:00.000000000-05:00
        </created-at>
      </bar>
      Round trip
      ==========
      ((bar bar-of-soap)
       (metadata ((user bob) (created_at (1969-12-31 19:01:00.000000000-05:00)))))
      |}]
  ;;
end

(* Complicated inlined example. *)
module _ = struct
  module Nested = struct
    type 'a t =
      { a : (int[@xml.leaf "a"])
      ; b : 'a
      }
    [@@deriving sexp_of, xml ~tag:"nested"]
  end

  module Inner_inlined = struct
    type 'a t =
      { a : (int[@xml.leaf "a"])
      ; b : 'a
      ; c : int [@xml.attribute "c"]
      }
    [@@deriving sexp_of, xml ~inlined]
  end

  module Inlined = struct
    type 'a t =
      { a : 'a Nested.t
      ; b : 'a Inner_inlined.t [@xml.inlined]
      }
    [@@deriving sexp_of, xml ~inlined]
  end

  module D = struct
    type t = { c : (int[@xml.leaf "number"]) Inlined.t [@xml.inlined] }
    [@@deriving sexp_of, xml ~tag:"hello"]
  end

  let sample : D.t = { c = { a = { a = 1; b = 2 }; b = { a = 3; b = 4; c = 5 } } }

  let%expect_test _ =
    D.xml_of_t sample |> Element |> Simple_xml.to_string ~fmt:(`Indent 2) |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <hello c="5">
        <nested>
          <a>
            1
          </a>
          <number>
            2
          </number>
        </nested>
        <a>
          3
        </a>
        <number>
          4
        </number>
      </hello>
      |}];
    D.xml_of_t sample |> D.t_of_xml |> D.sexp_of_t |> print_s;
    [%expect {| ((c ((a ((a 1) (b 2))) (b ((a 3) (b 4) (c 5)))))) |}]
  ;;
end

(* Extensions *)
module _ = struct
  module M = struct
    type t = { a : (int[@xml.leaf "a"]) } [@@deriving sexp_of, xml ~tag:"foo"]
  end

  module Bar = struct
    type t = { a : (int[@xml.leaf "a"]) } [@@deriving sexp_of, xml ~tag:"bar"]
  end

  module M1 = struct
    type 'a t = { a : 'a } [@@deriving sexp_of, xml ~tag:"foo"]
  end

  module M2 = struct
    type ('a, 'b) t =
      { a : 'a
      ; b : 'b
      }
    [@@deriving sexp_of, xml ~tag:"foo"]
  end

  let%expect_test "xml_of" =
    let xml_of_m : M.t -> Simple_xml.element = [%xml_of: M.t] in
    let xml_of_m1 : M.t M1.t -> Simple_xml.element = [%xml_of: M.t M1.t] in
    let xml_of_m2 : (M.t, Bar.t) M2.t -> Simple_xml.element =
      [%xml_of: (M.t, Bar.t) M2.t]
    in
    xml_of_m { a = 1 }
    |> Element
    |> Simple_xml.to_string ~fmt:(`Indent 2)
    |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <foo>
        <a>
          1
        </a>
      </foo>
      |}];
    xml_of_m1 { a = { a = 1 } }
    |> Element
    |> Simple_xml.to_string ~fmt:(`Indent 2)
    |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <foo>
        <foo>
          <a>
            1
          </a>
        </foo>
      </foo>
      |}];
    xml_of_m2 { a = { a = 1 }; b = { a = 2 } }
    |> Element
    |> Simple_xml.to_string ~fmt:(`Indent 2)
    |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <foo>
        <foo>
          <a>
            1
          </a>
        </foo>
        <bar>
          <a>
            2
          </a>
        </bar>
      </foo>
      |}]
  ;;

  let%expect_test "of_xml" =
    let _m_of_xml_description : M.t Ppx_simple_xml_conv_lib.Of_xml.t =
      [%of_xml_description: M.t]
    in
    let _m1_of_xml_description : M.t M1.t Ppx_simple_xml_conv_lib.Of_xml.t =
      [%of_xml_description: M.t M1.t]
    in
    let _m2_of_xml_description : (M.t, Bar.t) M2.t Ppx_simple_xml_conv_lib.Of_xml.t =
      [%of_xml_description: (M.t, Bar.t) M2.t]
    in
    let m_of_xml : Simple_xml.element -> M.t = [%of_xml: M.t] in
    let m1_of_xml : Simple_xml.element -> M.t M1.t = [%of_xml: M.t M1.t] in
    let m2_of_xml : Simple_xml.element -> (M.t, Bar.t) M2.t =
      [%of_xml: (M.t, Bar.t) M2.t]
    in
    `String
      {|
      <foo>
        <a>
          1
        </a>
      </foo> |}
    |> Simple_xml.parse
    |> m_of_xml
    |> [%sexp_of: M.t]
    |> print_s;
    [%expect {| ((a 1)) |}];
    `String
      {|
      <foo>
        <foo>
          <a>
            1
          </a>
        </foo>
      </foo> |}
    |> Simple_xml.parse
    |> m1_of_xml
    |> [%sexp_of: M.t M1.t]
    |> print_s;
    [%expect {| ((a ((a 1)))) |}];
    `String
      {|
      <foo>
        <foo>
          <a>
            1
          </a>
        </foo>
        <bar>
          <a>
            2
          </a>
        </bar>
      </foo> |}
    |> Simple_xml.parse
    |> m2_of_xml
    |> [%sexp_of: (M.t, Bar.t) M2.t]
    |> print_s;
    [%expect {| ((a ((a 1))) (b ((a 2)))) |}]
  ;;
end

(* tag name based on variable *)
module _ = struct
  module Make (M : sig
      val tag : string
    end) =
  struct
    type t =
      { foo : (string[@xml.leaf "foo-name"])
      ; bar : (string[@xml.leaf "bar-name"])
      }
    [@@deriving sexp_of, xml ~tag:M.tag]
  end

  module A = Make (struct
      let tag = "a"
    end)

  module B = Make (struct
      let tag = "b"
    end)

  let%expect_test _ =
    A.xml_of_t { foo = "footastic"; bar = "bar-of-soap" }
    |> Element
    |> Simple_xml.to_string ~fmt:(`Indent 2)
    |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <a>
        <foo-name>
          footastic
        </foo-name>
        <bar-name>
          bar-of-soap
        </bar-name>
      </a>
      |}]
  ;;

  let%expect_test _ =
    B.xml_of_t { foo = "footastic"; bar = "bar-of-soap" }
    |> Element
    |> Simple_xml.to_string ~fmt:(`Indent 2)
    |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <b>
        <foo-name>
          footastic
        </foo-name>
        <bar-name>
          bar-of-soap
        </bar-name>
      </b>
      |}]
  ;;
end

(* Empty on core types (and motivating example). *)
module _ = struct
  (* A common type used in a bunch of places. *)
  module With_properties = struct
    type 'a t =
      { value : 'a
      ; timestamp : int
      }

    (* Whatever custom logic on serialization/deserialization that applies to all
       serializations. *)
    let xml_of_t xml_of_value { value; timestamp } =
      let element : Simple_xml.element = xml_of_value value in
      { element with
        attributes =
          { ns = ""; key = "timestamp"; value = string_of_int timestamp }
          :: element.attributes
      }
    ;;
  end

  type t =
    { foo : (string[@xml.leaf "foo"]) With_properties.t
    ; bar : (unit[@xml.empty "bar"]) With_properties.t
    }
  [@@deriving xml_of ~tag:"t"]

  let%expect_test _ =
    { foo = { value = "foo"; timestamp = 1 }; bar = { value = (); timestamp = 2 } }
    |> xml_of_t
    |> Element
    |> Simple_xml.to_string ~fmt:(`Indent 2)
    |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <t>
        <foo timestamp="1">
          foo
        </foo>
        <bar timestamp="2"/>
      </t>
      |}]
  ;;
end

(* [preserve_space] flag on leaf. *)
module _ = struct
  type t =
    { foo : (string[@xml.leaf "foo"])
    ; bar : (string[@xml.leaf "bar" ~preserve_space])
    }
  [@@deriving sexp_of, of_xml ~tag:"t"]

  let input = {|<t><foo>  Hello  World  </foo><bar>  Hello  World  </bar></t>|}

  let%expect_test _ =
    Simple_xml.parse (`String input) |> t_of_xml |> sexp_of_t |> print_s;
    [%expect {| ((foo "Hello  World") (bar "  Hello  World  ")) |}]
  ;;
end

module%test Namespace_serialization = struct
  let namespace_1 = "http://namespace1"
  let namespace_2 = "http://namespace2"

  module Sub_record_with_namespace = struct
    type t = { a : (string[@xml.leaf "a"]) }
    [@@deriving sexp_of, xml ~tag:"sub_a" ~namespace:namespace_1]
  end

  module Sub_record_without_namespace = struct
    type t = { b : (string[@xml.leaf "b"]) }
    [@@deriving sexp_of, xml ~tag:"sub_b" ~assert_no_namespace]
  end

  module Sub_record_with_any_namespace = struct
    type t = { c : (string[@xml.leaf "c"]) } [@@deriving sexp_of, xml ~tag:"sub_c"]
  end

  type t =
    { attribute_with_namespace : string [@xml.attribute "a" ~namespace:namespace_1]
    ; attribute_without_namespace : string [@xml.attribute "b" ~assert_no_namespace]
    ; attribute_accepts_all_namespaces : string [@xml.attribute "c"]
    ; leaf_with_namespace : (string[@xml.leaf "d" ~namespace:namespace_2])
    ; leaf_without_namespace : (string[@xml.leaf "e" ~assert_no_namespace])
    ; leaf_accepts_all_namespaces : (string[@xml.leaf "f"])
    ; sub_record_with_namespace : Sub_record_with_namespace.t
    ; sub_record_without_namespace : Sub_record_without_namespace.t
    ; sub_record_with_any_namespace : Sub_record_with_any_namespace.t
    }
  [@@deriving
    sexp_of, xml ~tag:"foo" ~prefixes:[ "ns1", namespace_1; "ns2", namespace_2 ]]

  let%expect_test "Serialize" =
    let t =
      { attribute_with_namespace = "with-namespace"
      ; attribute_without_namespace = "no-namespace"
      ; attribute_accepts_all_namespaces = "any-namespace"
      ; leaf_with_namespace = "with-namespace"
      ; leaf_without_namespace = "no-namespace"
      ; leaf_accepts_all_namespaces = "any-namespace"
      ; sub_record_with_namespace = { a = "with-namespace" }
      ; sub_record_without_namespace = { b = "no-namespace" }
      ; sub_record_with_any_namespace = { c = "any-namespace" }
      }
    in
    t |> xml_of_t |> Element |> Simple_xml.to_string ~fmt:(`Indent 2) |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <foo xmlns:ns1="http://namespace1" xmlns:ns2="http://namespace2" ns1:a="with-namespace" b="no-namespace" c="any-namespace">
        <ns2:d>
          with-namespace
        </ns2:d>
        <e>
          no-namespace
        </e>
        <f>
          any-namespace
        </f>
        <ns1:sub_a>
          <a>
            with-namespace
          </a>
        </ns1:sub_a>
        <sub_b>
          <b>
            no-namespace
          </b>
        </sub_b>
        <sub_c>
          <c>
            any-namespace
          </c>
        </sub_c>
      </foo>
      |}]
  ;;
end

module%test Namespace_deserialization = struct
  let namespace_1 = "http://namespace1"
  let namespace_2 = "http://namespace2"

  module Variant = struct
    type t =
      | Namespace_1 [@xml.empty "variant" ~namespace:namespace_1]
      | Namespace_2 [@xml.empty "variant" ~namespace:namespace_2]
      | No_namespace [@xml.empty "variant" ~assert_no_namespace]
    [@@deriving sexp_of, xml]
  end

  module Sub_record_namespace1 = struct
    type t = { a : (string[@xml.leaf "a"]) }
    [@@deriving sexp_of, xml ~tag:"sub" ~namespace:namespace_1]
  end

  module Sub_record_namespace2 = struct
    type t = { a : (string[@xml.leaf "a"]) }
    [@@deriving sexp_of, xml ~tag:"sub" ~namespace:namespace_2]
  end

  module Sub_record_no_namespace = struct
    type t = { a : (string[@xml.leaf "a"]) }
    [@@deriving sexp_of, xml ~tag:"sub" ~assert_no_namespace]
  end

  module Sub_record_any_namespace = struct
    type t = { a : (string[@xml.leaf "a"]) } [@@deriving sexp_of, xml ~tag:"sub-any"]
  end

  type t =
    { attribute_with_namespace_1 : string [@xml.attribute "attr" ~namespace:namespace_1]
    ; attribute_with_namespace_2 : string [@xml.attribute "attr" ~namespace:namespace_2]
    ; attribute_without_namespace : string [@xml.attribute "attr" ~assert_no_namespace]
    ; attribute_accepts_all_namespaces : string [@xml.attribute "attr-any"]
    ; leaf_with_namespace : (string[@xml.leaf "leaf" ~namespace:namespace_2])
    ; leaf_without_namespace : (string[@xml.leaf "leaf" ~assert_no_namespace])
    ; leaf_accepts_all_namespaces : (string[@xml.leaf "leaf-any"])
    ; variant : Variant.t list [@xml.list]
    ; sub_record_with_namespace_1 : Sub_record_namespace1.t
    ; sub_record_with_namespace_2 : Sub_record_namespace2.t
    ; sub_record_without_namespace : Sub_record_no_namespace.t
    ; sub_record_with_any_namespace : Sub_record_any_namespace.t
    }
  [@@deriving sexp_of, xml ~tag:"foo"]

  let input_xml =
    `String
      {|
      <foo xmlns:ns1="http://namespace1" xmlns:ns2="http://namespace2" xmlns:ns3="http://namespace3"
           ns1:attr="attr-namespace1" ns2:attr="attr-namespace2" attr="attr-no-namespace" ns3:attr-any="attr-any-namespace">
        <ns2:leaf>leaf-namespace2</ns2:leaf>
        <leaf>leaf-no-namespace</leaf>
        <ns3:leaf-any>leaf-any-namespace</ns3:leaf-any>
        <ns1:variant/>
        <variant/>
        <ns2:variant/>
        <ns1:sub><a>sub-namespace1</a></ns1:sub>
        <ns2:sub><a>sub-namespace2</a></ns2:sub>
        <sub><a>sub-no-namespace</a></sub>
        <ns3:sub-any><a>sub-any-namespace</a></ns3:sub-any>
      </foo>
    |}
    |> Simple_xml.parse
  ;;

  let%expect_test "Deserialize" =
    input_xml |> t_of_xml |> sexp_of_t |> print_s;
    [%expect
      {|
      ((attribute_with_namespace_1 attr-namespace1)
       (attribute_with_namespace_2 attr-namespace2)
       (attribute_without_namespace attr-no-namespace)
       (attribute_accepts_all_namespaces attr-any-namespace)
       (leaf_with_namespace leaf-namespace2)
       (leaf_without_namespace leaf-no-namespace)
       (leaf_accepts_all_namespaces leaf-any-namespace)
       (variant (Namespace_1 No_namespace Namespace_2))
       (sub_record_with_namespace_1 ((a sub-namespace1)))
       (sub_record_with_namespace_2 ((a sub-namespace2)))
       (sub_record_without_namespace ((a sub-no-namespace)))
       (sub_record_with_any_namespace ((a sub-any-namespace))))
      |}]
  ;;
end

module%test Functor = struct
  module M (M : T) = M

  module type Xml_t = sig
    type t [@@deriving string, typerep]
  end

  let sexp_of_m__t (type a) (module M : Sexpable with type t = a) (m : a) = M.sexp_of_t m

  let xml_of_m__t (type a) (module M : Xml_t with type t = a) =
    let tag = Typerep_lib.Typename.name M.typename_of_t in
    [%xml_of: (M.t[@xml.leaf tag])]
  ;;

  let m__t_of_xml_description (type a) (module M : Xml_t with type t = a) =
    let tag = Typerep_lib.Typename.name M.typename_of_t in
    [%of_xml_description: (M.t[@xml.leaf tag])]
  ;;

  type t =
    { string : M(String).t
    ; int : M(Int).t
    ; float : M(Float).t
    }
  [@@deriving sexp_of, xml ~tag:"test"]

  let%expect_test "Serialization" =
    let t = { string = "string"; int = 1; float = 1.0 } in
    xml_of_t t |> Element |> Simple_xml.to_string ~fmt:(`Indent 2) |> print_endline;
    [%expect
      {|
      <?xml version="1.0" encoding="UTF-8"?>
      <test>
        <string.ml.t>
          string
        </string.ml.t>
        <int.ml.t>
          1
        </int.ml.t>
        <float.ml.Stable.V1.T1.t>
          1.
        </float.ml.Stable.V1.T1.t>
      </test>
      |}];
    xml_of_t t |> t_of_xml |> sexp_of_t |> print_s;
    [%expect {| ((string string) (int 1) (float 1)) |}]
  ;;
end

module%test Check_parsing_errors = struct
  open Expect_test_helpers_core

  module Simple_record = struct
    type t =
      { name : (string[@xml.leaf "name"])
      ; age : (int[@xml.leaf "age"])
      }
    [@@deriving sexp_of, of_xml ~tag:"person"]
  end

  let%expect_test "Missing required field" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<person><name>Alice</name></person>|})
      |> Simple_record.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at [person]: Expected 1 instance of age, got 0")
      |}]
  ;;

  let%expect_test "Wrong tag name" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<user><name>Alice</name><age>30</age></user>|})
      |> Simple_record.t_of_xml);
    [%expect
      {| (Failure "Ppx_simple_xml_conv, at []: Expected 1 instance of person, got 0") |}]
  ;;

  let%expect_test "Invalid integer parsing" =
    require_does_raise (fun () ->
      Simple_xml.parse
        (`String {|<person><name>Alice</name><age>not_a_number</age></person>|})
      |> Simple_record.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at [person > age]: [of_string] raised when parsing element content: (Failure int_of_string)")
      |}]
  ;;

  let%expect_test "Duplicate field" =
    require_does_raise (fun () ->
      Simple_xml.parse
        (`String {|<person><name>Alice</name><name>Bob</name><age>30</age></person>|})
      |> Simple_record.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at [person]: Expected 1 instance of name, got 2")
      |}]
  ;;

  module With_attributes = struct
    type t =
      { id : int [@xml.attribute "id"]
      ; name : (string[@xml.leaf "name"])
      }
    [@@deriving sexp_of, of_xml ~tag:"item"]
  end

  let%expect_test "Missing required attribute" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<item><name>Test</name></item>|})
      |> With_attributes.t_of_xml);
    [%expect {| (Failure "Ppx_simple_xml_conv, at [item]: Attribute id missing") |}]
  ;;

  let%expect_test "Invalid attribute type" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<item id="abc"><name>Test</name></item>|})
      |> With_attributes.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at [item]: [of_string] raised when parsing the value of attribute \"id\": (Failure int_of_string)")
      |}]
  ;;

  let%expect_test "Extra unexpected attributes" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<item id="123" extra="value"><name>Test</name></item>|})
      |> With_attributes.t_of_xml);
    [%expect {| (Failure "Ppx_simple_xml_conv, at [item]: extra attributes: extra") |}]
  ;;

  module Simple_variant = struct
    type t =
      | A of (int[@xml.leaf "value"])
      | B of (string[@xml.leaf "text"])
    [@@deriving sexp_of, xml]
  end

  let%expect_test "Unknown variant constructor" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<C><value>123</value></C>|}) |> Simple_variant.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at []: Expected 1 instance of value or text, got 0")
      |}]
  ;;

  let%expect_test "Variant with wrong inner structure" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<A><wrong_field>123</wrong_field></A>|})
      |> Simple_variant.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at []: Expected 1 instance of value or text, got 0")
      |}]
  ;;

  module With_text_content = struct
    type t =
      { id : int [@xml.attribute "id"]
      ; content : string [@xml.text]
      }
    [@@deriving sexp_of, of_xml ~tag:"message"]
  end

  let%expect_test "Mixed content with elements not allowed" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<message id="1">Hello <extra>world</extra></message>|})
      |> With_text_content.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at [message]: Expected a single text node, got child elements, tag: message, children: ((Text\"Hello \")(Element((tag((tag extra)))(children((Text world))))))")
      |}]
  ;;

  module Nested_structure = struct
    module Inner = struct
      type t = { value : (int[@xml.leaf "val"]) }
      [@@deriving sexp_of, of_xml ~tag:"inner"]
    end

    type t = { inner : Inner.t } [@@deriving sexp_of, of_xml ~tag:"outer"]
  end

  let%expect_test "Nested parsing error" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<outer><inner><val>not_int</val></inner></outer>|})
      |> Nested_structure.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at [outer > inner > val]: [of_string] raised when parsing element content: (Failure int_of_string)")
      |}]
  ;;

  let%expect_test "Missing nested element" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<outer></outer>|}) |> Nested_structure.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at [outer]: Expected 1 instance of inner, got 0")
      |}]
  ;;

  module With_namespace_constraints = struct
    type t = { field : (string[@xml.leaf "field" ~namespace:"http://example.com"]) }
    [@@deriving sexp_of, of_xml ~tag:"root"]
  end

  let%expect_test "Wrong namespace" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<root><field>value</field></root>|})
      |> With_namespace_constraints.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at [root]: Expected 1 instance of field[namespace=http://example.com], got 0")
      |}]
  ;;

  let%expect_test "Namespace required but not provided" =
    require_does_raise (fun () ->
      Simple_xml.parse
        (`String {|<root xmlns:ns="http://wrong.com"><ns:field>value</ns:field></root>|})
      |> With_namespace_constraints.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at [root]: Expected 1 instance of field[namespace=http://example.com], got 0")
      |}]
  ;;

  module With_custom_converters = struct
    let time_ns_of_timestamp timestamp =
      Int63.of_string timestamp
      |> Time_ns.Span.of_int63_seconds
      |> Time_ns.of_span_since_epoch
    ;;

    type t =
      { timestamp : Time_ns.Alternate_sexp.t
           [@xml.attribute "ts" ~of_string:time_ns_of_timestamp]
      }
    [@@deriving sexp_of, of_xml ~tag:"event"]
  end

  let%expect_test "Custom converter failure" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<event ts="invalid_timestamp"></event>|})
      |> With_custom_converters.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at [event]: [of_string] raised when parsing the value of attribute \"ts\": (Failure \"Int.of_string: \\\"invalid_timestamp\\\"\")")
      |}]
  ;;

  module Empty_element_errors = struct
    type t =
      | Empty [@xml.empty "empty"]
      | With_data of (int[@xml.leaf "value"])
    [@@deriving sexp_of, xml]
  end

  let%expect_test "Empty element with unexpected content" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<empty>unexpected content</empty>|})
      |> Empty_element_errors.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at [empty]: Expected empty tag, tag not empty: empty, contents: \"unexpected content\"")
      |}]
  ;;

  let%expect_test "Empty element with child elements" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<empty><child/></empty>|})
      |> Empty_element_errors.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv: Expected a single text node, got child elements, tag: empty, children: ((Element((tag((tag child))))))")
      |}]
  ;;

  module With_lifting_in_the_middle = struct
    module Foo = struct
      type t = { a : (int[@xml.leaf "a"]) } [@@deriving sexp_of, of_xml ~tag:"foo"]
    end

    module Bar = struct
      module Xmlable = struct
        type t = { foo : Foo.t } [@@deriving of_xml ~tag:"bar"]
      end

      type t = Foo.t [@@deriving sexp_of]

      include
        Ppx_simple_xml_conv_lib.Of_xml.Of_xmlable
          (Xmlable)
          (struct
            type nonrec t = t

            let of_xmlable { Xmlable.foo } = foo
          end)
    end

    type t = { bar : Bar.t } [@@deriving sexp_of, of_xml ~tag:"root"]
  end

  let%expect_test "Lifting in the middle" =
    require_does_raise (fun () ->
      Simple_xml.parse (`String {|<root><bar><foo><a>x</a></foo></bar></root>|})
      |> With_lifting_in_the_middle.t_of_xml);
    [%expect
      {|
      (Failure
       "Ppx_simple_xml_conv, at [root > bar > foo > a]: [of_string] raised when parsing element content: (Failure int_of_string)")
      |}]
  ;;
end
