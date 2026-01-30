open Core

module Offset_units = struct
  type t =
    { encoding : [ `UTF_8 | `UTF_16 | `UTF_32 ]
    ; units : [ `Bytes | `Uchars | `Code_units ]
    }
  [@@deriving equal, sexp_of, quickcheck]

  let length t uchar =
    match t with
    | { encoding = `UTF_8; units = `Bytes | `Code_units } -> Uchar.Utf8.byte_length uchar
    | { encoding = `UTF_16; units = `Bytes } -> Uchar.Utf16le.byte_length uchar
    | { encoding = `UTF_16; units = `Code_units } -> Uchar.Utf16le.byte_length uchar / 2
    | { encoding = `UTF_32; units = `Bytes } -> 4
    | { encoding = `UTF_32; units = `Code_units } -> 1
    | { encoding = `UTF_8 | `UTF_16 | `UTF_32; units = `Uchars } -> 1
  ;;
end

let[@cold] raise_invalid_from_offset
  (type utf)
  ~from_offset
  ~from_units
  ~text_encoding
  ~text
  =
  let (module Utf : String.Utf with type t = utf) = text_encoding in
  Error.raise_s
    [%message
      "The provided offset is invalid when [text] is encoded in [from]."
        ~offset:(from_offset : int)
        ~from:(from_units : Offset_units.t)
        ~text_encoding:(Utf.codec_name : string)
        (text : Utf.t)]
;;

let convert_offset
  (type utf)
  from_offset
  ~from:from_units
  ~to_:to_units
  ~text_encoding
  ~text
  =
  let (module Utf : String.Utf with type t = utf) = text_encoding in
  let local_ from_index = ref 0 in
  let local_ to_index = ref 0 in
  Utf.fold_until
    text
    ~init:()
    ~f:(fun () uchar ->
      match Ordering.of_int (compare !from_index from_offset) with
      | Greater -> raise_invalid_from_offset ~from_offset ~from_units ~text_encoding ~text
      | Equal -> Stop ()
      | Less ->
        from_index := !from_index + Offset_units.length from_units uchar;
        to_index := !to_index + Offset_units.length to_units uchar;
        Continue ())
    ~finish:(fun () ->
      match equal !from_index from_offset with
      | true -> ()
      | false -> raise_invalid_from_offset ~from_offset ~from_units ~text_encoding ~text);
  !to_index
;;

module Test_case = struct
  type 'utf text_encoding = (module String.Utf with type t = 'utf)

  let sexp_of_text_encoding
    (type utf)
    (_ : utf -> Sexp.t)
    (module Utf : String.Utf with type t = utf)
    =
    Sexp.Atom Utf.codec_name
  ;;

  type t =
    | T :
        { from_offset : int
        ; from : Offset_units.t
        ; to_ : Offset_units.t
        ; text_encoding : 'utf text_encoding
        ; text : 'utf
        ; uchars : Uchar.t list
        ; to_offset : int
        }
        -> t
  [@@deriving sexp_of]

  let (encodings : (module String.Utf) list) =
    [ (module String.Utf8); (module String.Utf16le); (module String.Utf16be) ]
  ;;

  let quickcheck_generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind (module Utf) = Quickcheck.Generator.of_list encodings in
    let%bind text = [%quickcheck.generator: Utf.t]
    and from = Offset_units.quickcheck_generator
    and to_ = Offset_units.quickcheck_generator in
    let uchars = Utf.to_list text in
    let%map character_offset = Int.gen_uniform_incl 0 (List.length uchars) in
    let units_offset_of_character_offset ~units =
      List.fold (List.take uchars character_offset) ~init:0 ~f:(fun offset uchar ->
        offset + Offset_units.length units uchar)
    in
    let from_offset = units_offset_of_character_offset ~units:from in
    let to_offset = units_offset_of_character_offset ~units:to_ in
    T { from_offset; from; to_; text_encoding = (module Utf); text; uchars; to_offset }
  ;;
end

(* Test the common case of converting between byte offset and Unicode charcter offset. *)
let%expect_test "Counting by uchars" =
  let zero_width_joiner = "\u{200D}" in
  let emoji_modifier_fitzpatrick_type_3 = "\u{1F3FC}" in
  let man = "\u{1F468}" in
  let woman = "\u{1F469}" in
  let girl = "\u{1F467}" in
  let boy = "\u{1F466}" in
  let fitzpatrick_type_3_family =
    [ man; woman; girl; boy ]
    |> List.map ~f:(fun person -> person ^ emoji_modifier_fitzpatrick_type_3)
    |> String.concat ~sep:zero_width_joiner
    |> String.Utf8.of_string
  in
  (* The unicode sequence for this emoji can be confirmed at
     https://emojipedia.org/family-man-medium-light-skin-tone-woman-medium-light-skin-tone-girl-medium-light-skin-tone-boy-medium-light-skin-tone#technical *)
  print_s [%sexp (String.Utf8.to_list fitzpatrick_type_3_family : Uchar.t list)];
  [%expect
    {|
    (U+1F468 U+1F3FC U+200D U+1F469 U+1F3FC U+200D U+1F467 U+1F3FC U+200D U+1F466
     U+1F3FC)
    |}];
  (* Some platforms support rendering this emoji as a single glyph; others render each
     family member separately (4 glyphs are used). In the terminal without any glyph
     support the sequence may render in a fragmented form. *)
  print_endline (fitzpatrick_type_3_family :> string);
  [%expect {| 👨🏼‍👩🏼‍👧🏼‍👦🏼 |}];
  let expected_offsets ~length_of_uchar =
    String.Utf8.fold
      fitzpatrick_type_3_family
      ~init:(0, [])
      ~f:(fun (offset, offsets) uchar ->
        length_of_uchar uchar + offset, offset :: offsets)
    |> Tuple2.uncurry List.cons
    |> List.rev
  in
  let actual_offsets ~from_units ~to_units =
    List.map ~f:(fun offset ->
      convert_offset
        offset
        ~from:{ encoding = `UTF_8; units = from_units }
        ~to_:{ encoding = `UTF_8; units = to_units }
        ~text_encoding:(module String.Utf8)
        ~text:fitzpatrick_type_3_family)
  in
  let expected_uchar_offsets = expected_offsets ~length_of_uchar:(const 1) in
  let expected_utf8_byte_offsets =
    expected_offsets ~length_of_uchar:Uchar.Utf8.byte_length
  in
  let actual_uchar_offsets =
    actual_offsets expected_utf8_byte_offsets ~from_units:`Bytes ~to_units:`Uchars
  in
  let actual_utf8_byte_offsets =
    actual_offsets expected_uchar_offsets ~from_units:`Uchars ~to_units:`Bytes
  in
  let test ~expected ~actual =
    match [%equal: int list] expected actual with
    | true -> print_s [%sexp (actual : int list)]
    | false ->
      print_s
        [%message "Offset conversion failed" (expected : int list) (actual : int list)]
  in
  test ~expected:expected_uchar_offsets ~actual:actual_uchar_offsets;
  [%expect {| (0 1 2 3 4 5 6 7 8 9 10 11) |}];
  test ~expected:expected_utf8_byte_offsets ~actual:actual_utf8_byte_offsets;
  [%expect {| (0 4 8 11 15 19 22 26 30 33 37 41) |}]
;;

let%expect_test "Quickcheck conversions" =
  Quickcheck.test
    [%quickcheck.generator: Test_case.t]
    ~sexp_of:[%sexp_of: Test_case.t]
    ~f:(fun (T { from_offset; from; to_; text_encoding; text; uchars = _; to_offset }) ->
      let actual_to_offset = convert_offset from_offset ~from ~to_ ~text_encoding ~text in
      if actual_to_offset <> to_offset
      then
        raise_s
          [%message
            "Actual and expected offsets are different"
              ~expected:(to_offset : int)
              ~actual:(actual_to_offset : int)])
;;

let%expect_test "Invalid [from_offset] is detected properly" =
  let facade_with_cedilla_in_utf8 = "fa\xC3\xA7ade" in
  let test ~from_offset =
    match
      convert_offset
        from_offset
        ~from:{ encoding = `UTF_8; units = `Bytes }
        ~to_:{ encoding = `UTF_8; units = `Bytes }
        ~text_encoding:(module String.Utf8)
        ~text:(String.Utf8.of_string facade_with_cedilla_in_utf8)
    with
    | (_ : int) -> "No failure"
    | exception exn -> Exn.to_string_mach exn
  in
  for i = -1 to String.length facade_with_cedilla_in_utf8 + 1 do
    let visual =
      let cursor = i + 1 in
      let text = sprintf " %s " facade_with_cedilla_in_utf8 (* avoid out-of-bounds *) in
      let prefix = String.escaped (String.subo text ~len:cursor) in
      let suffix = String.escaped (String.subo text ~pos:cursor) in
      sprintf "%s\u{2502}%s" prefix suffix
    in
    printf "%2d [ %s ]: %s\n" i visual (test ~from_offset:i)
  done;
  [%expect
    {|
    -1 [ │ fa\195\167ade  ]: ("The provided offset is invalid when [text] is encoded in [from]."(offset -1)(from((encoding UTF_8)(units Bytes)))(text_encoding UTF-8)(text"fa\195\167ade"))
     0 [  │fa\195\167ade  ]: No failure
     1 [  f│a\195\167ade  ]: No failure
     2 [  fa│\195\167ade  ]: No failure
     3 [  fa\195│\167ade  ]: ("The provided offset is invalid when [text] is encoded in [from]."(offset 3)(from((encoding UTF_8)(units Bytes)))(text_encoding UTF-8)(text"fa\195\167ade"))
     4 [  fa\195\167│ade  ]: No failure
     5 [  fa\195\167a│de  ]: No failure
     6 [  fa\195\167ad│e  ]: No failure
     7 [  fa\195\167ade│  ]: No failure
     8 [  fa\195\167ade │ ]: ("The provided offset is invalid when [text] is encoded in [from]."(offset 8)(from((encoding UTF_8)(units Bytes)))(text_encoding UTF-8)(text"fa\195\167ade"))
    |}]
;;
