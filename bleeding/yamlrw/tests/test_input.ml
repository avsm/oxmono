(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Yamlrw
module B = Bytesrw.Bytes

(* Exercise borrowed slices with a nonzero offset and storage reused on every
   read. Looking ahead must not invalidate the input's current slice. *)
let chunked_reader chunk_size text =
  let buffer = Bytes.create (chunk_size + 2) in
  let offset = ref 0 in
  let reads = ref 0 in
  let reader =
    B.Reader.make (fun () ->
        incr reads;
        let length = min chunk_size (String.length text - !offset) in
        if length = 0 then B.Slice.eod
        else (
          Bytes.blit_string text !offset buffer 1 length;
          offset := !offset + length;
          B.Slice.make buffer ~first:1 ~length))
  in
  (reader, reads)

let check_position message expected actual =
  Alcotest.(check (triple int int int))
    message
    (expected.Position.index, expected.line, expected.column)
    (actual.Position.index, actual.line, actual.column)

let option_of_nullable = function This c -> Some c | Null -> None

let positions_and_marks () =
  let text = "a\r\nb\nc\r\195\169z" in
  let initial = { Position.index = 31; line = 5; column = 9 } in
  let input =
    Input.of_reader ~initial_position:initial (B.Reader.of_string text)
  in
  let start = Input.mark input in
  let expected = ref initial in
  String.iter
    (fun c ->
      Alcotest.(check char) "next" c (Input.next_exn input);
      expected := Position.advance_char c !expected;
      check_position "position after byte" !expected (Input.position input);
      Alcotest.(check int) "line" !expected.line (Input.line input);
      Alcotest.(check int) "column" !expected.column (Input.column input);
      check_position "saved mark stays immutable" initial start)
    text;
  let stop = Input.mark input in
  Alcotest.(check (option char)) "EOF" None (Input.next input);
  check_position "EOF does not advance" stop (Input.position input);
  match Input.next_exn input with
  | _ -> Alcotest.fail "Expected EOF exception"
  | exception Error.Yamlrw_error error -> (
      match (error.kind, error.span) with
      | Error.Unexpected_eof, Some span ->
          check_position "EOF span start" stop span.start;
          check_position "EOF span stop" stop span.stop
      | _ -> Alcotest.fail "Unexpected EOF error")

let lookahead_across_slices () =
  let text = "abcdefgh\r\n0123456789\195\169Z" in
  for chunk_size = 1 to 12 do
    let reader, _ = chunked_reader chunk_size text in
    let input = Input.of_reader reader in
    for offset = 0 to String.length text do
      let expected_at n =
        if offset + n < String.length text then Some text.[offset + n] else None
      in
      Alcotest.(check (option char)) "peek" (expected_at 0) (Input.peek input);
      for n = 0 to 7 do
        Alcotest.(check (option char))
          "lookahead" (expected_at n)
          (option_of_nullable (Input.peek_nth_or_null input n));
        Alcotest.(check (option char))
          "option lookahead" (expected_at n) (Input.peek_nth input n)
      done;
      let length = min 11 (String.length text - offset) in
      Alcotest.(check string)
        "string lookahead"
        (String.sub text offset length)
        (Input.peek_string input 11);
      Alcotest.(check (option char))
        "next after lookahead" (expected_at 0)
        (option_of_nullable (Input.next_or_null input));
      Alcotest.(check int)
        "lookahead does not consume"
        (min (offset + 1) (String.length text))
        (Input.position input).index
    done
  done

let buffered_lookahead () =
  let reader, reads = chunked_reader 8 "abcdefghijklmnop" in
  let input = Input.of_reader reader in
  ignore (Input.peek_exn input);
  let before = !reads in
  Alcotest.(check (option char))
    "within buffered slice" (Some 'h') (Input.peek_nth input 7);
  Alcotest.(check int) "no upstream read" before !reads;
  Input.skip input 3;
  Alcotest.(check (option char))
    "previous byte" (Some 'c')
    (option_of_nullable (Input.peek_back_or_null input));
  Alcotest.(check (option char))
    "crossing buffered slice" (Some 'k') (Input.peek_nth input 7);
  Alcotest.(check char)
    "current byte survives upstream read" 'd' (Input.next_exn input)

let invalid_lookahead () =
  let reader, reads = chunked_reader 8 "abcdefgh" in
  let input = Input.of_reader reader in
  let check () =
    let before = !reads in
    let position = Input.position input in
    List.iter
      (fun n ->
        let rejects f =
          match f () with
          | _ -> Alcotest.fail "invalid lookahead accepted"
          | exception Invalid_argument _ -> ()
        in
        rejects (fun () -> Input.peek_nth input n);
        rejects (fun () -> option_of_nullable (Input.peek_nth_or_null input n)))
      [ -1; -2; min_int; Sys.max_string_length; max_int ];
    Alcotest.(check int) "invalid lookahead does not read" before !reads;
    check_position "invalid lookahead does not advance" position
      (Input.position input)
  in
  check ();
  Input.skip input 2;
  check ();
  Input.skip input 6;
  check ()

let previous_byte_across_slices () =
  let text = "ab cd\n#ef" in
  for chunk_size = 1 to 5 do
    let reader, _ = chunked_reader chunk_size text in
    let input = Input.of_reader reader in
    Alcotest.(check (option char))
      "no previous byte" None (Input.peek_back input);
    String.iter
      (fun c ->
        ignore (Input.next_exn input);
        ignore (Input.peek_or_null input);
        Alcotest.(check (option char))
          "previous byte after refill" (Some c) (Input.peek_back input))
      text
  done

let remaining_drains_cursor () =
  let text = "a\r\nb\nc" in
  let initial = { Position.index = 10; line = 3; column = 4 } in
  let stop =
    String.fold_left (fun p c -> Position.advance_char c p) initial text
  in
  for chunk_size = 1 to 8 do
    for offset = 0 to String.length text do
      List.iter
        (fun buffered ->
          let reader, _ = chunked_reader chunk_size text in
          let input = Input.of_reader ~initial_position:initial reader in
          Input.skip input offset;
          if buffered then ignore (Input.peek input);
          let mark = Input.mark input in
          let before = Input.position input in
          Alcotest.(check string)
            "remaining bytes"
            (String.sub text offset (String.length text - offset))
            (Input.remaining input);
          check_position "drain advances position" stop (Input.position input);
          check_position "drain preserves saved mark" before mark;
          Alcotest.(check bool) "drain reaches EOF" true (Input.is_eof input);
          Alcotest.(check (option char))
            "no buffered byte remains" None (Input.next input);
          Alcotest.(check string)
            "second drain is empty" "" (Input.remaining input))
        [ false; true ]
    done
  done

let bom_and_breaks () =
  for chunk_size = 1 to 5 do
    let reader, _ = chunked_reader chunk_size "\239\187\191a\r\nb\rc\nd" in
    let input = Input.of_reader reader in
    check_position "BOM excluded from position" Position.initial
      (Input.mark input);
    Alcotest.(check char) "BOM skipped" 'a' (Input.next_exn input);
    Input.consume_break input;
    check_position "CRLF position"
      { Position.index = 3; line = 2; column = 1 }
      (Input.position input);
    Input.skip input 1;
    Input.consume_break input;
    check_position "CR byte position"
      { Position.index = 5; line = 2; column = 3 }
      (Input.position input);
    Input.skip input 1;
    Input.consume_break input;
    check_position "LF position"
      { Position.index = 7; line = 3; column = 1 }
      (Input.position input);
    Alcotest.(check char) "last byte" 'd' (Input.next_exn input)
  done

let scanner_chunking () =
  let documents =
    [
      "# comment\n---\nkey: [one, two, {three: 3}]\n...\n";
      "\239\187\191%YAML 1.2\n---\nkey: \"a\\nb\\u00e9\"\n";
      "key: |\n  first\n  second\nother: 'it''s fine'\n";
      "a: &anchor [1, 2]\nb: *anchor\nc: \195\169\n";
      "key: value # inline comment\nother: \"quoted\" # comment\n";
    ]
  in
  let tokens = Alcotest.testable Token.pp_spanned ( = ) in
  List.iter
    (fun document ->
      let expected = Scanner.to_list (Scanner.of_string document) in
      for chunk_size = 1 to 12 do
        let reader, _ = chunked_reader chunk_size document in
        let actual = Scanner.to_list (Scanner.of_reader reader) in
        Alcotest.(check (list tokens))
          "tokens and spans across slices" expected actual
      done)
    documents

let scanner_comment_separator () =
  let document = "key: \"quoted\"# missing separator\n" in
  let scan scanner =
    match Scanner.to_list scanner with
    | _ -> Alcotest.fail "missing comment separator accepted"
    | exception Error.Yamlrw_error error ->
        if error.kind <> Error.Invalid_comment then
          Alcotest.fail "unexpected scanner error";
        error.span
  in
  let expected = scan (Scanner.of_string document) in
  for chunk_size = 1 to 12 do
    let reader, _ = chunked_reader chunk_size document in
    Alcotest.(check bool)
      "same comment error span across slices" true
      (expected = scan (Scanner.of_reader reader))
  done

let minor_words f =
  let before = Gc.minor_words () in
  f ();
  Gc.minor_words () -. before

let allocation () =
  let length = 100_000 in
  let input = Input.of_string (String.make length 'x') in
  ignore (Input.peek_exn input);
  let peek_words =
    minor_words (fun () ->
        for _ = 1 to length do
          match Sys.opaque_identity (Input.peek_or_null input) with
          | This 'x' -> ()
          | _ -> failwith "Unexpected peek"
        done)
  in
  let nth_words =
    minor_words (fun () ->
        for _ = 1 to length do
          match Sys.opaque_identity (Input.peek_nth_or_null input 7) with
          | This 'x' -> ()
          | _ -> failwith "Unexpected lookahead"
        done)
  in
  let next_words =
    minor_words (fun () ->
        for _ = 1 to length do
          match Sys.opaque_identity (Input.next_or_null input) with
          | This 'x' -> ()
          | _ -> failwith "Unexpected next"
        done)
  in
  (* Allow constant measurement overhead, but catch a per-byte allocation. *)
  List.iter
    (fun (name, words) ->
      if words > 64. then
        Alcotest.failf "%s allocated %.0f words for %d buffered bytes" name
          words length)
    [
      ("peek_or_null", peek_words);
      ("peek_nth_or_null", nth_words);
      ("next_or_null", next_words);
    ]

let () =
  Alcotest.run "YAML input"
    [
      ( "cursor",
        [
          Alcotest.test_case "positions and immutable marks" `Quick
            positions_and_marks;
          Alcotest.test_case "lookahead with reusable slices" `Quick
            lookahead_across_slices;
          Alcotest.test_case "buffered lookahead" `Quick buffered_lookahead;
          Alcotest.test_case "invalid lookahead bounds" `Quick invalid_lookahead;
          Alcotest.test_case "previous byte across slices" `Quick
            previous_byte_across_slices;
          Alcotest.test_case "remaining drains cursor" `Quick
            remaining_drains_cursor;
          Alcotest.test_case "BOM and line breaks" `Quick bom_and_breaks;
          Alcotest.test_case "scanner chunk invariance" `Quick scanner_chunking;
          Alcotest.test_case "comment separator across slices" `Quick
            scanner_comment_separator;
          Alcotest.test_case "buffered allocation" `Quick allocation;
        ] );
    ]
