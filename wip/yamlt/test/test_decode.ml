(* Decoding limits, scalar resolution, null handling and document streams. *)

let check message condition = if not condition then failwith message
let get = function Ok value -> value | Error message -> failwith message
let rejected = function Ok _ -> false | Error _ -> true
let any_list = Jsont.list Jsont.json

let () =
  let nested depth = String.make depth '[' ^ String.make depth ']' in
  check "nesting within max_depth accepted"
    (not (rejected (Yamlt.decode_string ~max_depth:8 Jsont.json (nested 7))));
  check "nesting beyond max_depth rejected"
    (rejected (Yamlt.decode_string ~max_depth:8 Jsont.json (nested 64)));
  check "default depth limit rejects deep nesting"
    (rejected (Yamlt.decode_string Jsont.json (nested 10_000)))

let () =
  let items n = "[" ^ String.concat ", " (List.init n string_of_int) ^ "]" in
  check "node count within max_nodes accepted"
    (not (rejected (Yamlt.decode_string ~max_nodes:100 any_list (items 10))));
  check "node count beyond max_nodes rejected"
    (rejected (Yamlt.decode_string ~max_nodes:100 any_list (items 200)));
  (* Each level multiplies the expanded size by eight through aliases. Aliases
     are currently refused as unknown anchors, so this only guards that
     expansion stays bounded once they are resolved. *)
  let laughs =
    String.concat "\n"
      ("a: &a [x, x, x, x, x, x, x, x]"
      :: List.init 8 (fun i ->
             let prev = Char.chr (Char.code 'a' + i)
             and next = Char.chr (Char.code 'b' + i) in
             Printf.sprintf "%c: &%c [*%c, *%c, *%c, *%c, *%c, *%c, *%c, *%c]"
               next next prev prev prev prev prev prev prev prev))
  in
  check "alias expansion bounded by max_nodes"
    (rejected (Yamlt.decode_string ~max_nodes:100_000 Jsont.json laughs))

let () =
  check "expected string type keeps yes"
    (get (Yamlt.decode_string Jsont.string "yes") = "yes");
  check "yes resolves to a boolean" (get (Yamlt.decode_string Jsont.bool "yes"));
  check "OFF resolves to a boolean"
    (not (get (Yamlt.decode_string Jsont.bool "OFF")));
  check "hex integer" (get (Yamlt.decode_string Jsont.int "0x1f") = 31);
  check "octal integer" (get (Yamlt.decode_string Jsont.int "0o17") = 15);
  check "negative infinity"
    (get (Yamlt.decode_string Jsont.number "-.inf") = Float.neg_infinity);
  check "NaN" (Float.is_nan (get (Yamlt.decode_string Jsont.number ".nan")));
  check "quoted number stays a string"
    (get (Yamlt.decode_string Jsont.string "'42'") = "42");
  check "mismatched scalar rejected"
    (rejected (Yamlt.decode_string Jsont.int "forty-two"))

let () =
  check "null array is empty" (get (Yamlt.decode_string any_list "~") = []);
  check "null number is NaN"
    (Float.is_nan (get (Yamlt.decode_string Jsont.number "null")));
  check "null int rejected" (rejected (Yamlt.decode_string Jsont.int "null"));
  check "null bool rejected" (rejected (Yamlt.decode_string Jsont.bool "~"));
  check "null string rejected"
    (rejected (Yamlt.decode_string Jsont.string "null"));
  check "option accepts null"
    (get (Yamlt.decode_string (Jsont.option Jsont.int) "null") = None);
  let required =
    Jsont.Object.map Fun.id
    |> Jsont.Object.mem "name" Jsont.string ~enc:Fun.id
    |> Jsont.Object.finish
  in
  check "null object needs required members"
    (rejected (Yamlt.decode_string required "~"))

let () =
  check "multi-document input rejected by decode"
    (rejected (Yamlt.decode_string Jsont.int "--- 1\n--- 2\n"));
  let docs =
    Yamlt.decode_all Jsont.int
      (Bytesrw.Bytes.Reader.of_string "--- 1\n--- two\n--- 3\n")
    |> List.of_seq
  in
  check "decode_all yields each document" (List.length docs = 3);
  (match docs with
  | [ Ok 1; Error _; Ok 3 ] -> ()
  | _ -> failwith "decode_all reports per-document results");
  let encode ?format value =
    let buffer = Buffer.create 64 in
    get
      (Yamlt.encode ?format any_list value ~eod:true
         (Bytesrw.Bytes.Writer.of_buffer buffer));
    Buffer.contents buffer
  in
  let strings = [ "yes"; "Off"; "1"; "1.5"; "0x1f"; ".inf"; ""; "~"; "null" ] in
  let value = List.map Jsont.Json.string strings @ [ Jsont.Json.int 1 ] in
  List.iter
    (fun format ->
      let decoded = get (Yamlt.decode_string any_list (encode ~format value)) in
      let as_string json = Jsont.Json.decode Jsont.string json in
      check "ambiguous strings stay strings"
        (List.map as_string (List.filteri (fun i _ -> i < 9) decoded)
        = List.map Result.ok strings);
      check "numbers stay numbers"
        (Jsont.Json.decode Jsont.int (List.nth decoded 9) = Ok 1))
    [ Yamlt.Block; Yamlt.Flow ]
