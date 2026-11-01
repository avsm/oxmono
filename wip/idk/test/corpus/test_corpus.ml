(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A corpus test against another implementation's fixtures.

    Stalwart's calcard is a Rust implementation of vCard, JSContact and their
    conversions. Its JSContact fixtures hold a few hundred fragments taken from
    RFC 9553, RFC 9555, RFC 6350, RFC 6715 and RFC 9554, written in a line
    format where a block introduced by [> test] or [> convert] is either a vCard
    or a run of JSContact object members. They are vendored under
    [test/corpus/data]; see the README there for their provenance and licence.

    Every JSContact block is spliced into a minimal Card and put through a
    decode, encode, decode cycle. The test checks that the Card decodes, that
    the cycle reaches a fixed point, that the value survives it unchanged, and
    that no property is lost: a member may only disappear if it is an implied
    [@type] or a value equal to a default the RFC documents. It does not check
    that the values agree with calcard's own reading of them; what it exercises
    is this library against property values it did not choose for itself.

    Set [JSCONTACT_CORPUS] to read a working tree of calcard instead of the
    vendored copy, which is how the vendored copy is checked before it is
    refreshed:

    {v
    JSCONTACT_CORPUS=../calcard/resources/jscontact dune runtest test/corpus
    v} *)

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* A block runs from a "> " line to the next one, and holds JSContact when its
   first non-blank line opens a JSON string. *)
let blocks_of_file s =
  let lines = String.split_on_char '\n' s in
  let flush acc buf =
    match List.rev buf with [] -> acc | b -> String.concat "\n" b :: acc
  in
  let rec go acc buf = function
    | [] -> List.rev (flush acc buf)
    | line :: rest ->
        if String.starts_with ~prefix:"> " line then go (flush acc buf) [] rest
        else go acc (line :: buf) rest
  in
  let is_jscontact b =
    match
      List.find_opt (fun l -> String.trim l <> "") (String.split_on_char '\n' b)
    with
    | Some l -> String.starts_with ~prefix:"\"" (String.trim l)
    | None -> false
  in
  List.filter is_jscontact (go [] [] lines)

let decode t s = Jsont_bytesrw.decode_string' t s
let encode t v = Jsont_bytesrw.encode_string' t v

(* Splice the members of a fragment into a Card, without naming a member the
   fragment already names: RFC 7493 forbids an object from doing that. *)
let card_of_fragment frag =
  match decode Jsont.json ("{" ^ frag ^ "}") with
  | Error _ -> None
  | Ok (Jsont.Object (mems, meta)) ->
      let has name =
        List.exists (fun ((n, _), _) -> String.equal n name) mems
      in
      let add name v mems =
        if has name then mems else ((name, Jsont.Meta.none), v) :: mems
      in
      let mems =
        mems
        |> add "uid" (Jsont.Json.string "corpus")
        |> add "version" (Jsont.Json.string "1.0")
        |> add "@type" (Jsont.Json.string "Card")
      in
      Result.to_option (encode Jsont.json (Jsont.Object (mems, meta)))
  | Ok _ -> None

(* Walk the decoded input and the re-encoded output together and report any
   member the cycle dropped. A member may legitimately disappear when it is
   the @type of a nested object, which Section 1.3.4 lets a reader imply, or
   when its value is the default the RFC documents for it, which this library
   does not write back. Everything else is a property this library lost. *)
let dropped_member name v =
  match (name, v) with
  | "@type", _ -> true
  | "kind", Jsont.String ("individual", _) -> true
  | "kind", Jsont.String ("title", _) -> true
  | "isOrdered", Jsont.Bool (false, _) -> true
  | "relation", Jsont.Object ([], _) -> true
  | _ -> false

let rec no_loss path a b =
  match (a, b) with
  | Jsont.Object (ma, _), Jsont.Object (mb, _) ->
      let find n =
        Option.map snd
          (List.find_opt (fun ((n', _), _) -> String.equal n n') mb)
      in
      let check acc ((n, _), va) =
        match acc with
        | Error _ -> acc
        | Ok () -> (
            match find n with
            | None ->
                if dropped_member n va then Ok ()
                else Error (Printf.sprintf "%s/%s was dropped" path n)
            | Some vb -> no_loss (path ^ "/" ^ n) va vb)
      in
      List.fold_left check (Ok ()) ma
  | Jsont.Array (ea, _), Jsont.Array (eb, _) ->
      if List.length ea <> List.length eb then
        Error (Printf.sprintf "%s changed length" path)
      else
        List.fold_left2
          (fun acc a b ->
            match acc with Error _ -> acc | Ok () -> no_loss path a b)
          (Ok ()) ea eb
  | _ -> Ok ()

type outcome = {
  decoded : int;
  skipped : int;
  failures : (string * string) list;
}

let check_card name json o =
  match decode Jscontact.Card.jsont json with
  | Error e ->
      { o with failures = (name, Jsont.Error.to_string e) :: o.failures }
  | Ok c -> (
      match encode Jscontact.Card.jsont c with
      | Error e ->
          { o with failures = (name, Jsont.Error.to_string e) :: o.failures }
      | Ok s1 -> (
          match decode Jscontact.Card.jsont s1 with
          | Error e ->
              {
                o with
                failures =
                  (name ^ " (re-decode)", Jsont.Error.to_string e) :: o.failures;
              }
          | Ok c2 -> (
              let stable =
                match encode Jscontact.Card.jsont c2 with
                | Ok s2 -> String.equal s1 s2
                | Error _ -> false
              in
              if not stable then
                { o with failures = (name ^ " (unstable)", s1) :: o.failures }
              else if not (Jscontact.Card.equal c c2) then
                { o with failures = (name ^ " (unequal)", s1) :: o.failures }
              else
                match (decode Jsont.json json, decode Jsont.json s1) with
                | Ok a, Ok b -> (
                    match no_loss "" a b with
                    | Ok () -> { o with decoded = o.decoded + 1 }
                    | Error msg ->
                        {
                          o with
                          failures = (name ^ " (lossy)", msg) :: o.failures;
                        })
                | _ -> { o with decoded = o.decoded + 1 })))

let run dir =
  let files = Array.to_list (Sys.readdir dir) in
  let files =
    List.sort String.compare
      (List.filter (fun f -> Filename.check_suffix f ".txt") files)
  in
  let o = { decoded = 0; skipped = 0; failures = [] } in
  let per_file o file =
    let blocks = blocks_of_file (read_file (Filename.concat dir file)) in
    List.fold_left
      (fun (o, i) b ->
        let name = Printf.sprintf "%s[%d]" file i in
        match card_of_fragment b with
        | None -> ({ o with skipped = o.skipped + 1 }, i + 1)
        | Some json -> (check_card name json o, i + 1))
      (o, 0) blocks
    |> fst
  in
  List.fold_left per_file o files

(* The loss detector is the whole point of the corpus test, so check that it
   reports a loss rather than passing vacuously. *)
let test_no_loss_detects () =
  let json s = Option.get (Result.to_option (decode Jsont.json s)) in
  let reports name a b =
    match no_loss "" (json a) (json b) with
    | Error _ -> ()
    | Ok () -> Alcotest.failf "%s: a dropped member went unreported" name
  in
  reports "a dropped member" {|{"a":1,"b":2}|} {|{"a":1}|};
  reports "a member dropped inside an array" {|{"a":[{"b":1}]}|} {|{"a":[{}]}|};
  reports "a shortened array" {|{"a":[1,2]}|} {|{"a":[1]}|};
  (match no_loss "" (json {|{"@type":"Name","a":1}|}) (json {|{"a":1}|}) with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "an implied @type is not a loss: %s" msg);
  match
    no_loss "" (json {|{"kind":"individual","a":1}|}) (json {|{"a":1}|})
  with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "an omitted default is not a loss: %s" msg

let test_corpus dir () =
  let o = run dir in
  List.iter
    (fun (name, msg) -> Printf.printf "FAIL %s: %s\n" name msg)
    (List.rev o.failures);
  Printf.printf "%d fragments round tripped, %d not JSContact, %d failed\n"
    o.decoded o.skipped (List.length o.failures);
  Alcotest.(check int) "failures" 0 (List.length o.failures);
  Alcotest.(check bool) "the corpus is not empty" true (o.decoded > 100)

let () =
  let dir =
    match Sys.getenv_opt "JSCONTACT_CORPUS" with
    | Some dir -> dir
    | None -> "data"
  in
  Alcotest.run "jscontact"
    [
      ( "corpus",
        [
          Alcotest.test_case "the loss detector" `Quick test_no_loss_detects;
          Alcotest.test_case "calcard fixtures" `Quick (test_corpus dir);
        ] );
    ]
