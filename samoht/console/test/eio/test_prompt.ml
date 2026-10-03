module Prompt = Console_eio.Prompt

let run f = Eio_main.run (fun _env -> f ())

(* A prompt over a string source and a buffer sink: the same code path a
   terminal takes, with the answers scripted and the questions captured. *)
let prompt ?interactive input =
  let out = Buffer.create 64 in
  let t =
    Prompt.v ?interactive
      ~stdin:(Eio.Flow.string_source input)
      ~stdout:(Eio.Flow.buffer_sink out) ()
  in
  (t, out)

(* The wording of a refusal is the implementation's to choose, so an answer is
   compared after collapsing every error to [Refused]. The one message whose
   content is itself a specification -- the gate must not echo the word it is
   asking for -- gets its own assertion. *)
type 'a answer = Got of 'a | Refused

let seen = function Ok v -> Got v | Error (`Msg _) -> Refused
let error_message = function Ok _ -> "<ok>" | Error (`Msg m) -> m

let answer t =
  let equal a b =
    match (a, b) with
    | Got x, Got y -> Alcotest.equal t x y
    | Refused, Refused -> true
    | Got _, Refused | Refused, Got _ -> false
  in
  let pp ppf = function
    | Got v -> Fmt.pf ppf "Got %a" (Alcotest.pp t) v
    | Refused -> Fmt.string ppf "Refused"
  in
  Alcotest.testable pp equal

let string_answer = answer Alcotest.string
let bool_answer = answer Alcotest.bool
let unit_answer = answer Alcotest.unit

let contains ~needle s =
  let n = String.length needle and len = String.length s in
  let rec at i = i + n <= len && (String.sub s i n = needle || at (i + 1)) in
  n = 0 || at 0

(* A string source is not a terminal, so the default is a prompt that refuses
   every question without writing a byte. *)
let test_a_string_source_is_not_a_terminal () =
  run @@ fun () ->
  let t, out = prompt "y\n" in
  Alcotest.(check bool) "not interactive" false (Prompt.interactive t);
  Alcotest.check string_answer "line" Refused (seen (Prompt.line t "name: "));
  Alcotest.check bool_answer "confirm" Refused
    (seen (Prompt.confirm t "delete?"));
  Alcotest.check unit_answer "expect" Refused
    (seen (Prompt.expect t "type it: " "vol"));
  Alcotest.(check string) "asked nothing" "" (Buffer.contents out)

(* One [t] owns the read buffer, so the second question gets the second line
   rather than whatever survived the first read. *)
let test_line_asks_verbatim_and_keeps_its_place () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "alpha\nbeta\n" in
  Alcotest.check string_answer "first answer" (Got "alpha")
    (seen (Prompt.line t "name: "));
  Alcotest.(check string) "first question" "name: " (Buffer.contents out);
  Alcotest.check string_answer "second answer" (Got "beta")
    (seen (Prompt.line t "again: "));
  Alcotest.(check string) "both questions" "name: again: " (Buffer.contents out)

(* The answer is what was typed, spaces and all, minus the terminator. *)
let test_line_keeps_the_spaces_it_was_given () =
  run @@ fun () ->
  let t, _ = prompt ~interactive:true "  spaced out  \n" in
  Alcotest.check string_answer "answer" (Got "  spaced out  ")
    (seen (Prompt.line t "name: "))

let test_line_drops_a_crlf_terminator () =
  run @@ fun () ->
  let t, _ = prompt ~interactive:true "alpha\r\nbeta\r\n" in
  Alcotest.check string_answer "first" (Got "alpha")
    (seen (Prompt.line t "name: "));
  Alcotest.check string_answer "second" (Got "beta")
    (seen (Prompt.line t "again: "))

(* The question goes out before the read, so it is on the terminal even when
   the answer never comes. *)
let test_line_at_end_of_input_is_refused () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "" in
  Alcotest.check string_answer "answer" Refused (seen (Prompt.line t "name: "));
  Alcotest.(check string) "question" "name: " (Buffer.contents out)

let test_confirm_asks_with_a_default_of_no () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "y\n" in
  Alcotest.check bool_answer "answer" (Got true)
    (seen (Prompt.confirm t "delete?"));
  Alcotest.(check string) "question" "delete? [y/N] " (Buffer.contents out)

let check_confirm input expected =
  let t, _ = prompt ~interactive:true (input ^ "\n") in
  Alcotest.check bool_answer
    (Fmt.str "confirm %S" input)
    (Got expected)
    (seen (Prompt.confirm t "delete?"))

let test_confirm_takes_y_and_yes_in_any_case () =
  run @@ fun () ->
  List.iter
    (fun input -> check_confirm input true)
    [ "y"; "Y"; "yes"; "  YES  " ]

let test_confirm_takes_everything_else_as_no () =
  run @@ fun () ->
  List.iter (fun input -> check_confirm input false) [ "n"; ""; "maybe" ]

let test_confirm_at_end_of_input_is_refused () =
  run @@ fun () ->
  let t, _ = prompt ~interactive:true "" in
  Alcotest.check bool_answer "answer" Refused
    (seen (Prompt.confirm t "delete?"))

let test_expect_opens_only_on_the_exact_word () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "backups\n" in
  Alcotest.check unit_answer "answer" (Got ())
    (seen (Prompt.expect t "type the volume name: " "backups"));
  Alcotest.(check string)
    "question" "type the volume name: " (Buffer.contents out)

(* An operator who cannot name the volume must not be told its name by the
   error that turns them away. *)
let test_expect_refuses_without_naming_the_word () =
  run @@ fun () ->
  let t, _ = prompt ~interactive:true "backup\n" in
  let result = Prompt.expect t "type the volume name: " "backups" in
  Alcotest.check unit_answer "answer" Refused (seen result);
  Alcotest.(check bool)
    "the message never echoes the word" false
    (contains ~needle:"backups" (error_message result))

let test_expect_at_end_of_input_is_refused () =
  run @@ fun () ->
  let t, _ = prompt ~interactive:true "" in
  Alcotest.check unit_answer "answer" Refused
    (seen (Prompt.expect t "type the volume name: " "backups"))

let suite =
  ( "prompt",
    [
      Alcotest.test_case "a string source is not a terminal" `Quick
        test_a_string_source_is_not_a_terminal;
      Alcotest.test_case "line asks verbatim and keeps its place" `Quick
        test_line_asks_verbatim_and_keeps_its_place;
      Alcotest.test_case "line keeps the spaces it was given" `Quick
        test_line_keeps_the_spaces_it_was_given;
      Alcotest.test_case "line drops a crlf terminator" `Quick
        test_line_drops_a_crlf_terminator;
      Alcotest.test_case "line at end of input is refused" `Quick
        test_line_at_end_of_input_is_refused;
      Alcotest.test_case "confirm asks with a default of no" `Quick
        test_confirm_asks_with_a_default_of_no;
      Alcotest.test_case "confirm takes y and yes in any case" `Quick
        test_confirm_takes_y_and_yes_in_any_case;
      Alcotest.test_case "confirm takes everything else as no" `Quick
        test_confirm_takes_everything_else_as_no;
      Alcotest.test_case "confirm at end of input is refused" `Quick
        test_confirm_at_end_of_input_is_refused;
      Alcotest.test_case "expect opens only on the exact word" `Quick
        test_expect_opens_only_on_the_exact_word;
      Alcotest.test_case "expect refuses without naming the word" `Quick
        test_expect_refuses_without_naming_the_word;
      Alcotest.test_case "expect at end of input is refused" `Quick
        test_expect_at_end_of_input_is_refused;
    ] )
