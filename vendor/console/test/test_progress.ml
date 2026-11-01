(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let contains affix s = Astring.String.is_infix ~affix s

let test_names () =
  Alcotest.(check (list string))
    "names"
    [ "auto"; "tty"; "plain"; "quiet"; "json" ]
    (List.map fst Progress.all);
  List.iter
    (fun (name, p) ->
      Alcotest.(check bool)
        ("of_string " ^ name) true
        (match Progress.of_string name with
        | Ok p' -> p' = p
        | Error _ -> false);
      Alcotest.(check string) ("to_string " ^ name) name (Progress.to_string p))
    Progress.all;
  match Progress.of_string "silent" with
  | Ok _ -> Alcotest.fail "unknown name accepted"
  | Error (`Msg m) ->
      Alcotest.(check bool) "names the choices" true (contains "quiet" m)

(* What a display given [progress] prints for a task that succeeds, with a
   line of output, and one that fails, with another, under a header. *)
let shown ?size progress =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let root = Task.root ~label:"Building" ~now:(fun () -> 0.) () in
  let d = Progress.show ?size ~progress ppf root in
  Task.run root "fine" (fun t -> Task.log t "quiet line");
  (try
     Task.run root "broken" (fun t ->
         Task.log t "loud line";
         failwith "broken")
   with Failure _ -> ());
  Task.succeed root;
  Display.close d;
  Format.pp_print_flush ppf ();
  Buffer.contents buffer

let lines s = List.filter (( <> ) "") (String.split_on_char '\n' s)

(* [`Quiet] prints the failure, its line and its output, and nothing else,
   also when [size] says there is a terminal. *)
let test_quiet () =
  List.iter
    (fun size ->
      let out = shown ?size `Quiet in
      Alcotest.(check bool) "the failure" true (contains "broken" out);
      Alcotest.(check bool) "its output" true (contains "loud line" out);
      Alcotest.(check bool) "no success" false (contains "fine" out);
      Alcotest.(check bool)
        "no output of a success" false
        (contains "quiet line" out);
      Alcotest.(check bool) "no header" false (contains "Building" out);
      Alcotest.(check bool) "no escape" false (String.contains out '\027'))
    [ None; Some (fun () -> (80, 24)) ]

let decode line = Jsont_bytesrw.decode_string Task.Event.jsont line

(* Each line decodes with [Task.Event.jsont], and its event is written
   as the line was. *)
let reads_back line =
  match decode line with
  | Ok e -> Alcotest.(check string) "written back" line (Task.Event.to_string e)
  | Error e -> Alcotest.failf "%S does not decode: %s" line e

(* [`Json] prints one event a line, and nothing else. *)
let test_json () =
  List.iter
    (fun size ->
      let out = shown ?size `Json in
      List.iter
        (fun line ->
          Alcotest.(check bool) line true (contains "{\"time\":" line);
          reads_back line)
        (lines out);
      Alcotest.(check bool)
        "an output event" true
        (contains "\"text\":\"quiet line\"" out);
      Alcotest.(check bool)
        "an end event" true
        (contains "\"status\":\"failed\"" out))
    [ None; Some (fun () -> (80, 24)) ]

(* As JSON, each change of a count is one event, unthrottled, and a call
   that changes nothing is none. *)
let test_json_counts () =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let root = Task.root ~now:(fun () -> 0.) () in
  let d = Progress.show ~progress:`Json ppf root in
  Task.run root "work" (fun t ->
      List.iter (fun count -> Task.set_items t ~count ~total:3) [ 1; 1; 2; 3 ]);
  Display.close d;
  Format.pp_print_flush ppf ();
  Alcotest.(check int)
    "three progress events" 3
    (List.length
       (List.filter
          (contains "\"event\":\"progress\"")
          (lines (Buffer.contents buf))))

(* What a display given [`Json] writes for every kind of event: tasks under
   tasks, counts and bytes, a new label, endings with and without a
   message, output outside any task, awkward text and a result with its
   detail, with the clock at [step] seconds a call. *)
let events ~step result =
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  let clock = ref 0. in
  let now () =
    let t = !clock in
    clock := t +. step;
    t
  in
  let root = Task.root ~now () in
  let d = Progress.show ~progress:`Json ppf root in
  Display.with_output d (fun ppf -> Fmt.pf ppf "above \027[1m\xc2\x85@.");
  (try
     Task.run root "q\"uote\\ \xff\xe2\x80\xa8" (fun t ->
         Task.set_items t ~count:1 ~total:3;
         Task.log t "tab\there del\127 nel\xc2\x85 \xf0\x9f\x98\x80";
         Task.run t "child" (fun c ->
             Task.set_bytes c ~count:1024L ~total:2048L;
             Task.set_label c "renamed";
             Task.succeed ~message:"done" c);
         Task.run t "cancelled" (fun c -> Task.cancel c);
         failwith "broken")
   with Failure _ -> ());
  Option.iter
    (fun (detail, r) ->
      Option.iter (Task.log root) detail;
      match r with
      | Ok m -> Task.succeed ~message:m root
      | Error m -> Task.fail ~message:m root)
    result;
  Display.close d;
  Format.pp_print_flush ppf ();
  lines (Buffer.contents buffer)

(* Every line the display writes decodes, and is written back as it was. *)
let test_json_reads_back () =
  List.iter
    (fun (step, result) ->
      let lines = events ~step result in
      Alcotest.(check bool) "events" true (List.length lines > 10);
      List.iter reads_back lines)
    [
      (0., None);
      (0.1234567, Some (Some "first\nsecond", Ok "built"));
      (1e-4, Some (None, Error "failed \027[31m"));
      (1e9 +. 0.5, Some (Some "", Ok ""));
    ]

module Event = Task.Event

let event time make = Event.to_string (make ~time)

(* The line of each event, its members in order. *)
let test_json_lines () =
  let check name expected kind =
    Alcotest.(check string) name expected (event 1.5 kind);
    reads_back expected
  in
  let task = (2, [ "a"; "b" ]) in
  check "start"
    {|{"time":1.5,"event":"start","id":2,"task":["a","b"],"parent":1}|}
    (Event.start ~parent:1 task);
  check "progress"
    {|{"time":1.5,"event":"progress","id":2,"task":["a","b"],"count":3,"total":999999999999999,"unit":"bytes"}|}
    (Event.progress ~count:3L ~total:999999999999999L ~unit:`Bytes task);
  check "from 10^15"
    {|{"time":1.5,"event":"progress","id":2,"task":["a","b"],"count":"1000000000000000","total":"9223372036854775807","unit":"items"}|}
    (Event.progress ~count:1_000_000_000_000_000L ~total:Int64.max_int
       ~unit:`Items task);
  check "label"
    {|{"time":1.5,"event":"label","id":2,"task":["a","b"],"text":"c"}|}
    (fun ~time -> Event.label ~time task "c");
  check "controls"
    ({|{"time":1.5,"event":"output","text":"\u001B\u007F|} ^ "\u{FFFD}"
   ^ {|\t\"\\|} ^ "\xc3\xa9" ^ {|","level":"warning"}|})
    (Event.output ~level:`Warning "\027\127\xc2\x85\t\"\\\xc3\xa9");
  check "invalid UTF-8"
    "{\"time\":1.5,\"event\":\"output\",\"id\":2,\"task\":[\"a\",\"b\"],\"text\":\"\u{FFFD}\"}"
    (Event.output ~task "\xff");
  check "end"
    {|{"time":1.5,"event":"end","id":2,"task":["a","b"],"status":"failed","elapsed":0.25,"message":"m"}|}
    (fun ~time -> Event.failed ~task ~message:"m" ~time ~elapsed:0.25 ());
  check "the end of the work"
    {|{"time":1.5,"event":"end","status":"succeeded","elapsed":1.5,"message":"t"}|}
    (fun ~time -> Event.succeeded ~message:"t" ~time ~elapsed:1.5 ());
  check "other" {|{"time":1.5,"event":"pause"}|} (Event.other "pause");
  reads_back
    {|{"time":1.5,"event":"end","id":2,"task":["a","b"],"status":"skipped","elapsed":0}|};
  Alcotest.(check string)
    "a time to the millisecond" {|{"time":861415.19,"event":"pause"}|}
    (event 861415.1899999 (Event.other "pause"));
  Alcotest.(check string)
    "a time that is not finite" {|{"time":0,"event":"pause"}|}
    (event Float.nan (Event.other "pause"))

(* A consumer skips the members, events and values it does not know, and an
   event that lacks a member of its kind does not decode. *)
let test_json_growth () =
  let kind line =
    match decode line with
    | Ok e -> Some (Event.kind e, Event.task e, Event.text e, Event.level e)
    | Error _ -> None
  in
  Alcotest.(check bool)
    "unknown members" true
    (kind
       {|{"time":0,"event":"label","new":{"a":[1]},"id":0,"task":["x"],"text":"y","more":null}|}
    = Some (`Label, Some (0, [ "x" ]), Some "y", None));
  Alcotest.(check bool)
    "unknown event" true
    (kind {|{"time":0,"event":"pause","id":0}|}
    = Some (`Other "pause", None, None, None));
  Alcotest.(check bool)
    "unknown value" true
    (kind {|{"time":0,"event":"output","task":[],"text":"","level":"trace"}|}
    = Some (`Output, None, Some "", Some (`Other "trace")));
  Alcotest.(check bool)
    "a missing member" true
    (kind {|{"time":0,"event":"start","task":["x"]}|} = None)

(* [`Plain] prints the lines of a pipe on a terminal too, and [`Auto] as
   [size] says. *)
let test_plain () =
  let plain = shown `Plain in
  Alcotest.(check string)
    "on a terminal" plain
    (shown ~size:(fun () -> (80, 24)) `Plain);
  Alcotest.(check string) "auto on a pipe" plain (shown `Auto);
  Alcotest.(check bool)
    "plain has no escape" false
    (String.contains plain '\027');
  Alcotest.(check bool) "a line for the success" true (contains "fine" plain)

(* [`Tty] redraws the rows, on 80 by 24 without [size]. *)
let test_tty () =
  let hide = "\027[?25l" in
  Alcotest.(check bool) "tty hides the cursor" true (contains hide (shown `Tty));
  Alcotest.(check bool)
    "auto on a terminal too" true
    (contains hide (shown ~size:(fun () -> (80, 24)) `Auto))

(* Rounding the largest finite float to 15 digits must still read back. *)
let test_json_max_float () =
  let e =
    Event.succeeded ~task:(0, [ "work" ]) ~time:Float.max_float
      ~elapsed:Float.max_float ()
  in
  reads_back (Event.to_string e)

(* [`Other s] with [s] a listed value is that value; [other] refuses a
   listed event name. *)
let test_other_listed () =
  (match Event.other ~time:0. "start" with
  | _ -> Alcotest.fail "other took a listed event name"
  | exception Invalid_argument _ -> ());
  let same what a b = Alcotest.(check bool) what true (a = b) in
  same "unit"
    (Event.progress ~time:0. ~count:1L ~total:1L ~unit:(`Other "items") (0, []))
    (Event.progress ~time:0. ~count:1L ~total:1L ~unit:`Items (0, []));
  same "level"
    (Event.output ~level:(`Other "error") ~time:0. "")
    (Event.output ~level:`Error ~time:0. "");
  same "status"
    (Event.of_string {|{"time":0,"event":"end","status":"failed","elapsed":0}|})
    (Ok (Event.failed ~time:0. ~elapsed:0. ()));
  let files =
    Event.progress ~time:0. ~count:1L ~total:1L ~unit:(`Other "files") (0, [])
  in
  Alcotest.(check bool)
    "another unit reads back" true
    (Event.of_string (Event.to_string files) = Ok files)

let test_negative () =
  let refused what f =
    match f () with
    | _ -> Alcotest.failf "a negative %s taken" what
    | exception Invalid_argument _ -> ()
  in
  refused "id" (fun () -> Event.start ~time:0. (-1, []));
  refused "parent" (fun () -> Event.start ~parent:(-1) ~time:0. (0, []));
  refused "count" (fun () ->
      Event.progress ~time:0. ~count:(-1L) ~total:1L ~unit:`Items (0, []));
  List.iter
    (fun line ->
      match Event.of_string line with
      | Ok _ -> Alcotest.failf "%s read" line
      | Error _ -> ())
    [
      {|{"time":0,"event":"start","id":-1,"task":[]}|};
      {|{"time":0,"event":"start","id":1.5,"task":[]}|};
      {|{"time":0,"event":"start","id":"-1","task":[]}|};
    ]

(* An event of another name keeps its id and task when they have their
   types, and skips any other member. *)
let test_unknown_event_members () =
  let read line =
    match Event.of_string line with
    | Ok e -> e
    | Error (`Msg m) -> Alcotest.failf "unknown event rejected: %s" m
  in
  Alcotest.(check bool)
    "only the name and time" true
    (read {|{"time":0,"event":"pause","id":"bad","text":5}|}
    = Event.other ~time:0. "pause");
  Alcotest.(check bool)
    "its task" true
    (read {|{"time":0,"event":"pause","id":4,"task":["a","b"],"text":5}|}
    = Event.other ~task:(4, [ "a"; "b" ]) ~time:0. "pause")

let test_error_plain () =
  match Event.of_string {|{"event":"x"}|} with
  | Ok _ -> Alcotest.fail "an event without time read"
  | Error (`Msg m) ->
      Alcotest.(check bool)
        ("no escape in " ^ String.escaped m)
        false (String.contains m '\027')

(* Each maximal subpart of an ill-formed sequence is one U+FFFD. *)
let test_invalid_bytes_individually () =
  List.iter
    (fun (raw, expected) ->
      let text = Event.text (Event.output ~time:0. raw) in
      let sanitized = Console_text.sanitize raw in
      let vte = Console_vte.v ~rows:3 ~cols:8 () in
      Console_vte.feed vte (sanitized ^ "\r\n" ^ Option.get text);
      Alcotest.(check (list string))
        "one visible cell per subpart" [ expected; expected ]
        (Console_vte.screen vte);
      Alcotest.(check (option string)) "event text" (Some expected) text;
      Alcotest.(check string) "sanitized text" expected sanitized)
    [
      ("\xf0\x9f", "\u{FFFD}");
      ("\xf0\x9f\x98", "\u{FFFD}");
      ("\xff\xfe", "\u{FFFD}\u{FFFD}");
      ("\xf0\x9fa", "\u{FFFD}a");
    ]

let test_event_text_limits () =
  let input = String.make 5000 'x' in
  let expected = String.make 4093 'x' ^ "..." in
  let check text =
    Alcotest.(check int) "retained bytes" 4096 (String.length text);
    Alcotest.(check string) "cut text" expected text
  in
  let events =
    [
      Event.start ~time:0. (0, [ input ]);
      Event.label ~time:0. (0, [ "work" ]) input;
      Event.failed ~time:0. ~elapsed:0. ~message:input ();
    ]
  in
  List.iter
    (fun event ->
      let encoded = Event.to_string event in
      let decoded =
        match Event.of_string encoded with
        | Ok event -> event
        | Error (`Msg message) -> Alcotest.fail message
      in
      List.iter
        (fun event ->
          Option.iter
            (fun (_, path) -> List.iter check path)
            (match Event.kind event with
            | `Start -> Event.task event
            | _ -> None);
          Option.iter check (Event.text event);
          Option.iter check (Event.message event))
        [ event; decoded ])
    events

let suite =
  ( "progress",
    [
      Alcotest.test_case "event constructors bound labels and messages to 4 KiB"
        `Quick test_event_text_limits;
      Alcotest.test_case "each maximal subpart is replaced" `Quick
        test_invalid_bytes_individually;
      Alcotest.test_case "negative integers" `Quick test_negative;
      Alcotest.test_case "errors are plain text" `Quick test_error_plain;
      Alcotest.test_case "unknown events own their member schemas" `Quick
        test_unknown_event_members;
      Alcotest.test_case "largest finite event times read back" `Quick
        test_json_max_float;
      Alcotest.test_case "other with a listed name" `Quick test_other_listed;
      Alcotest.test_case "names" `Quick test_names;
      Alcotest.test_case "quiet" `Quick test_quiet;
      Alcotest.test_case "json" `Quick test_json;
      Alcotest.test_case "json counts" `Quick test_json_counts;
      Alcotest.test_case "json events read back" `Quick test_json_reads_back;
      Alcotest.test_case "json lines" `Quick test_json_lines;
      Alcotest.test_case "json growth" `Quick test_json_growth;
      Alcotest.test_case "plain" `Quick test_plain;
      Alcotest.test_case "tty" `Quick test_tty;
    ] )
