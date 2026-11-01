(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
module Event = Console.Task.Event

let known_events = [ "start"; "progress"; "label"; "output"; "end" ]

(* Strings that are, often, a listed value of a member. *)
let value listed = choose (bytes :: List.map const listed)

let open_value listed =
  map
    [ value (List.map fst listed) ]
    (fun s ->
      match List.assoc_opt s listed with Some v -> v | None -> `Other s)

let count_unit = open_value [ ("items", `Items); ("bytes", `Bytes) ]

let level =
  open_value
    [
      ("app", `App);
      ("error", `Error);
      ("warning", `Warning);
      ("info", `Info);
      ("debug", `Debug);
    ]

let ending =
  choose [ const Event.succeeded; const Event.failed; const Event.cancelled ]

let nat = map [ int ] (fun n -> n land max_int)
let task = map [ nat; list bytes ] (fun id path -> (id, path))
let nat64 = map [ int64 ] (fun n -> Int64.logand n Int64.max_int)

(* Any event, but for its time: [other] names an event that is not
   listed. *)
let kind =
  choose
    [
      map [ task; option nat ] (fun t parent -> Event.start ?parent t);
      map [ task; nat64; nat64; count_unit ] (fun t count total unit ->
          Event.progress ~count ~total ~unit t);
      map [ task; bytes ] (fun t text ~time -> Event.label ~time t text);
      map
        [ option task; bytes; option level ]
        (fun task s level -> Event.output ?task ?level s);
      map
        [ ending; option task; float; option bytes ]
        (fun e task elapsed message ~time -> e ?task ?message ~time ~elapsed ());
      map [ bytes ] (fun s ->
          if List.mem s known_events then bad_test () else Event.other s);
    ]

(* No byte of a line is a control character, nor is a character of it
   C1. *)
let no_control s =
  String.for_all (fun c -> Char.code c >= 0x20 && c <> '\127') s
  &&
  let rec no_c1 i =
    i >= String.length s
    ||
    let d = String.get_utf_8_uchar s i in
    let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
    (c < 0x80 || c > 0x9f) && no_c1 (i + Uchar.utf_decode_length d)
  in
  no_c1 0

let decode line =
  match Event.of_string line with
  | Ok e -> e
  | Error (`Msg e) -> failf "%S does not decode: %s" line e

(* An event's line decodes, and its event is written as the line is: valid
   UTF-8 on one line, without a control character. *)
let test_round_trip time kind =
  let e = kind ~time in
  let line = Event.to_string e in
  check (String.is_valid_utf_8 line);
  check (no_control line);
  let back = decode line in
  check_eq ~pp:Fmt.Dump.string line (Event.to_string back);
  check (back = e)

(* [other] refuses a listed event name; [`Other s] with [s] a listed value
   is that value. Either reads back as it is. *)
let test_other s =
  (match Event.other ~time:0. s with
  | e ->
      if List.mem s known_events then failf "%S, a listed name, taken" s;
      check (decode (Event.to_string e) = e)
  | exception Invalid_argument _ ->
      if not (List.mem s known_events) then failf "%S refused as other" s);
  List.iter
    (fun e -> check (decode (Event.to_string e) = e))
    [
      Event.progress ~time:0. ~count:0L ~total:0L ~unit:(`Other s) (0, []);
      Event.output ~level:(`Other s) ~time:0. "";
    ];
  let status = Fmt.str {|{"time":0,"event":"end","status":%S,"elapsed":0}|} s in
  match Event.of_string status with
  | Ok e -> check (decode (Event.to_string e) = e)
  | Error _ -> ()

(* [s] as an event holds it: each maximal subpart of an ill-formed
   sequence and each C1 control is U+FFFD. *)
let cleaned s =
  let buf = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      let c = Uchar.to_int u in
      Buffer.add_utf_8_uchar buf
        (if Uchar.utf_decode_is_valid d && (c < 0x80 || c > 0x9f) then u
         else Uchar.rep);
      go (i + Uchar.utf_decode_length d)
    end
  in
  go 0;
  Buffer.contents buf

(* Any bytes read back as valid UTF-8 without a C1 control, the bytes of
   each other character kept. *)
let test_string s =
  let e = decode (Event.to_string (Event.label ~time:0. (0, [ s ]) s)) in
  match (Event.task e, Event.text e) with
  | Some (_, [ l ]), Some t ->
      check_eq ~pp:Fmt.Dump.string (cleaned s) l;
      check_eq ~pp:Fmt.Dump.string (cleaned s) t
  | _ -> fail "not the label event"

(* A time reads back rounded to the millisecond, held in [-10^12, 10^12]
   seconds, and is zero if it is not finite. *)
let test_time x =
  let back = Event.time (decode (Event.to_string (Event.other ~time:x "x"))) in
  if not (Float.is_finite x) then check_eq ~pp:Fmt.float 0. back
  else
    let x = Float.min 1e12 (Float.max (-1e12) x) in
    check_eq ~pp:Fmt.float ((Float.round (x *. 1000.) /. 1000.) +. 0.) back

let contains ~affix s =
  let n = String.length affix in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = affix || go (i + 1))
  in
  go 0

(* A count reads back as it was, a JSON number below 10^15 and a string of
   decimal digits from there. *)
let test_count c n =
  let line =
    Event.to_string
      (Event.progress ~time:0. ~count:c ~total:n ~unit:`Bytes (0, []))
  in
  let exact = Int64.compare c 1_000_000_000_000_000L < 0 in
  check
    (contains
       ~affix:
         (if exact then Fmt.str {|"count":%Ld,|} c
          else Fmt.str {|"count":"%Ld",|} c)
       line);
  let e = decode line in
  check (Event.count e = Some c);
  check (Event.total e = Some n)

(* Check every documented event field through the public Display API,
   including arbitrary bytes in labels, end messages and result text: each
   line decodes with Progress.Event.jsont, is written back as it was, and
   is the event expected. *)
let public_schema_events k raw =
  let open Console in
  let clean s = Console_text.sanitize ~keep_newlines:false s in
  let now = ref 42. and out = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer out in
  let root = Task.root ~now:(fun () -> !now) () in
  let d = Progress.show ~progress:`Json ppf root in
  let parent = Task.start root raw in
  now := 42.125;
  let task = Task.start ~transient:true parent (raw ^ "/child") in
  now := 42.375;
  Task.set_label parent (raw ^ "/new");
  let count =
    if k land 1 = 0 then begin
      Task.set_items task ~count:max_int ~total:max_int;
      (Int64.of_int max_int, `Items)
    end
    else begin
      Task.set_bytes task ~count:Int64.max_int ~total:Int64.max_int;
      (Int64.max_int, `Bytes)
    end
  in
  Task.log task "line";
  let ending, status =
    match k mod 3 with
    | 0 -> (Task.succeed, Event.succeeded)
    | 1 -> (Task.fail, Event.failed)
    | _ -> (Task.cancel, Event.cancelled)
  in
  ending ~message:(raw ^ "/end") task;
  Task.succeed parent;
  Task.log root "root";
  let detail = clean raw ^ "\ndetail" in
  Task.log root detail;
  if k land 2 = 0 then Task.succeed ~message:raw root
  else Task.fail ~message:raw root;
  Display.close d;
  Format.pp_print_flush ppf ();
  let events =
    String.split_on_char '\n' (Buffer.contents out)
    |> List.filter (( <> ) "")
    |> List.map (fun line ->
        check (String.is_valid_utf_8 line);
        check (no_control line);
        match Event.of_string line with
        | Ok e ->
            check_eq ~pp:Fmt.Dump.string line (Event.to_string e);
            e
        | Error (`Msg e) -> failf "%s does not decode: %s" line e)
  in
  (events, (count, status, detail))

(* A display keeps the first 4 KiB of a label or a message, cut between
   two characters and ended with three dots. *)
let kept s =
  if String.length s <= 4096 then s
  else
    let head = String.sub s 0 (4096 - 3) in
    String.sub head 0 (Console_text.boundary head) ^ "..."

let public_schema_expected k raw
    ((count, unit), (ending : ?task:_ -> ?message:_ -> _), detail) =
  let clean s = Console_text.sanitize ~keep_newlines:false s in
  let label = kept (clean raw) and child = kept (clean (raw ^ "/child")) in
  let renamed = kept (clean (raw ^ "/new"))
  and message = kept (clean (raw ^ "/end")) in
  let work = if k land 2 = 0 then Event.succeeded else Event.failed in
  let child_task = (1, [ renamed; child ]) in
  [
    Event.start ~time:0. (0, [ label ]);
    Event.start ~parent:0 ~time:0.125 (1, [ label; child ]);
    Event.label ~time:0.375 (0, [ renamed ]) renamed;
    Event.progress ~time:0.375 ~count ~total:count ~unit child_task;
    Event.output ~task:child_task ~time:0.375 "line";
    ending ~task:child_task ~message ~time:0.375 ~elapsed:0.25 ();
    Event.succeeded ~task:(0, [ renamed ]) ~time:0.375 ~elapsed:0.375 ();
    Event.output ~time:0.375 "root";
  ]
  @ List.map
      (fun l -> Event.output ~time:0.375 l)
      (String.split_on_char '\n' detail)
  @ [ work ~message:(kept (clean raw)) ~time:0.375 ~elapsed:0.375 () ]

let test_public_schema k text =
  let raw = if k mod 16 = 0 then String.make 65537 'x' ^ text else text in
  let events, values = public_schema_events k raw in
  let expected = public_schema_expected k raw values in
  check_eq ~pp:Fmt.int (List.length expected) (List.length events);
  List.iteri
    (fun i (expected, actual) ->
      if expected <> actual then
        failf "event %d is %a, not %a" i Event.pp actual Event.pp expected)
    (List.combine expected events)

(* Include the finite limits and all IEEE-754 bit patterns, rather than
   only the ordinary magnitudes produced by the float generator. *)
let extreme_seconds =
  choose
    [
      const Float.max_float;
      const (-.Float.max_float);
      map [ int64 ] Int64.float_of_bits;
    ]

let test_extreme_seconds time =
  test_round_trip time (fun ~time:t ->
      Event.succeeded ~task:(0, [ "work" ]) ~time:t ~elapsed:time ())

(* A future event may reuse a current member's name for another JSON type.
   None of those members belongs to the unknown event's schema here. *)
let test_unknown_schema k =
  let members =
    [|
      ("id", {|"bad"|});
      ("task", "false");
      ("parent", "[]");
      ("count", "{}");
      ("total", "true");
      ("unit", "42");
      ("status", "[]");
      ("elapsed", {|"later"|});
      ("text", "5");
      ("message", "[]");
      ("level", "{}");
    |]
  in
  let name, json = members.(k mod Array.length members) in
  let line = Fmt.str {|{"time":1.25,"event":"pause","%s":%s}|} name json in
  check (decode line = Event.other ~time:1.25 "pause")

(* Deliberately malformed inputs, not prefixes cut out of UTF-8 text. *)
let invalid_prefix =
  choose (List.map const [ "\xf0\x9f"; "\xf0\x9f\x98"; "\xe2\x82" ])

(* Each of these is one maximal subpart, so one U+FFFD. *)
let test_invalid_bytes raw =
  let expected = "\u{FFFD}" in
  check (Event.text (Event.output ~time:0. raw) = Some expected);
  check_eq ~pp:Fmt.Dump.string expected (Console_text.sanitize raw)

(* Accessors expose just the members of their event, including optional
   members; the constructor and the codec must agree on their absence. *)
let test_accessors raw id n =
  let text = cleaned raw and task = (id, [ cleaned raw ]) in
  let verify ?task ?parent ?count ?total ?unit ?status ?elapsed ?text ?message
      ?level kind e =
    List.iter
      (fun e ->
        check_eq ~pp:Fmt.float 1.25 (Event.time e);
        List.iter
          (fun (name, equal) ->
            if not equal then
              failf "%s accessor disagrees with %a" name Event.pp e)
          [
            ("kind", Event.kind e = kind);
            ("task", Event.task e = task);
            ("parent", Event.parent e = parent);
            ("count", Event.count e = count);
            ("total", Event.total e = total);
            ("unit", Event.unit e = unit);
            ("status", Event.status e = status);
            ("elapsed", Event.elapsed e = elapsed);
            ("text", Event.text e = text);
            ("message", Event.message e = message);
            ("level", Event.level e = level);
          ])
      [ e; decode (Event.to_string e) ]
  in
  let raw_task = (id, [ raw ]) in
  verify ~task ~parent:id `Start (Event.start ~parent:id ~time:1.25 raw_task);
  verify ~task ~count:n ~total:n ~unit:`Bytes `Progress
    (Event.progress ~time:1.25 ~count:n ~total:n ~unit:`Bytes raw_task);
  verify ~task ~text `Label (Event.label ~time:1.25 raw_task raw);
  verify ~task ~text ~level:`Warning `Output
    (Event.output ~task:raw_task ~level:`Warning ~time:1.25 raw);
  verify ~text `Output (Event.output ~time:1.25 raw);
  verify ~task ~message:text ~status:`Failed ~elapsed:0.5 `End
    (Event.failed ~task:raw_task ~message:raw ~time:1.25 ~elapsed:0.5 ());
  verify ~task ~status:`Cancelled ~elapsed:0.5 `End
    (Event.cancelled ~task:raw_task ~time:1.25 ~elapsed:0.5 ());
  verify ~message:text ~status:`Succeeded ~elapsed:0.5 `End
    (Event.succeeded ~message:raw ~time:1.25 ~elapsed:0.5 ());
  verify (`Other "pause") (Event.other ~time:1.25 "pause")

(* A retained text is cut between two grapheme clusters, then ends in
   ASCII dots. *)
let test_retained_cut k =
  let tails =
    [|
      "\u{754C}";
      "\u{1F600}";
      "e\u{0301}";
      "\u{1F468}\u{200D}\u{1F469}";
      "\u{1F1EB}\u{1F1F7}";
      "\u{1F3F4}\u{E0067}\u{E0062}\u{E0073}\u{E0063}\u{E0074}\u{E007F}";
      "\027[31mred";
    |]
  in
  let raw =
    String.make (4086 + (k mod 16)) 'a' ^ tails.(k mod Array.length tails)
  in
  let clean = Console_text.sanitize ~keep_newlines:false raw in
  let expected =
    if String.length clean <= 4096 then clean
    else
      let rec prefix acc n = function
        | c :: rest when n + String.length c <= 4093 ->
            prefix (c :: acc) (n + String.length c) rest
        | _ -> String.concat "" (List.rev acc)
      in
      prefix [] 0 (Width.clusters clean) ^ "..."
  in
  let out = Buffer.create 16384 in
  let ppf = Format.formatter_of_buffer out in
  let root = Console.Task.root ~now:(fun () -> 0.) () in
  let d = Console.Progress.show ~progress:`Json ppf root in
  let t = Console.Task.start root raw in
  Console.Task.set_label t raw;
  Console.Task.fail ~message:raw t;
  Console.Display.close d;
  Format.pp_print_flush ppf ();
  let events =
    String.split_on_char '\n' (Buffer.contents out)
    |> List.filter (( <> ) "")
    |> List.map decode
  in
  List.iter
    (fun e ->
      Option.iter
        (fun (_, path) ->
          List.iter (check_eq ~pp:Fmt.Dump.string expected) path)
        (Event.task e);
      Option.iter (check_eq ~pp:Fmt.Dump.string expected) (Event.text e))
    events

let test_task_endings n ending =
  let module Task = Console.Task in
  let root = Task.root ~now:(fun () -> 0.) () in
  let starts = ref [] and ends = ref [] and work_ends = ref 0 in
  Task.on_event root (fun event ->
      match (Event.kind event, Event.task event) with
      | `Start, Some (id, _) -> starts := id :: !starts
      | `End, Some (id, _) -> ends := id :: !ends
      | `End, None -> incr work_ends
      | _ -> ());
  let count = 1 + (n mod 30) in
  let children = List.init count (fun i -> Task.start root (string_of_int i)) in
  (match ending mod 3 with
  | 0 -> Task.succeed root
  | 1 -> Task.fail root
  | _ -> Task.cancel root);
  Task.succeed root;
  Task.fail root;
  let expected = if ending mod 3 = 0 then `Succeeded else `Cancelled in
  List.iter (fun child -> check (Task.state child = expected)) children;
  check_eq ~pp:Fmt.int 1 !work_ends;
  check_eq
    ~pp:Fmt.(Dump.list int)
    (List.init count Fun.id)
    (List.sort Int.compare !starts);
  check_eq
    ~pp:Fmt.(Dump.list int)
    (List.sort Int.compare !starts)
    (List.sort Int.compare !ends)

let test_output_suffix n k =
  let module Task = Console.Task in
  let root = Task.root ~now:(fun () -> 0.) () in
  let task = Task.start root "bounded" in
  let length = 4070 + (k mod 26) and count = 260 + (n mod 40) in
  let lines =
    List.init count (fun i ->
        Fmt.str "%04d|%s" i (String.make (length - 5) 'x'))
  in
  List.iter (Task.log task) lines;
  let kept = min count (1048576 / (length + 1)) in
  check_eq ~pp:Fmt.int (count - kept) (Task.dropped task);
  let expected = List.filteri (fun i _ -> i >= count - kept) lines in
  check_eq ~pp:Fmt.(Dump.list Dump.string) expected (Task.output task);
  check
    (List.fold_left
       (fun n line -> n + String.length line + 1)
       0 (Task.output task)
    <= 1048576);
  Task.fail task;
  Task.log task "after the end";
  check_eq ~pp:Fmt.(Dump.list Dump.string) expected (Task.output task)

let test_event_text_limits n unicode =
  let glyph = if unicode then "界" else "x" in
  let count = (4080 / String.length glyph) + n in
  let repeat n = String.concat "" (List.init n (Fun.const glyph)) in
  let raw = repeat count in
  let expected =
    if String.length raw <= 4096 then raw
    else repeat (4093 / String.length glyph) ^ "..."
  in
  let task = (0, [ raw ]) in
  let events =
    [
      Event.start ~time:0. task;
      Event.progress ~time:0. ~count:1L ~total:2L ~unit:`Items task;
      Event.label ~time:0. task raw;
      Event.output ~task ~time:0. raw;
      Event.succeeded ~task ~message:raw ~time:0. ~elapsed:0. ();
      Event.failed ~task ~message:raw ~time:0. ~elapsed:0. ();
      Event.cancelled ~task ~message:raw ~time:0. ~elapsed:0. ();
      Event.other ~task ~time:0. "pause";
    ]
  in
  let bounded text =
    check (String.is_valid_utf_8 text);
    check (String.length text <= 4096);
    check_eq ~pp:Fmt.Dump.string expected text
  in
  List.iter
    (fun event ->
      List.iter
        (fun event ->
          Option.iter
            (fun (_, path) -> List.iter bounded path)
            (Event.task event);
          Option.iter bounded (Event.message event);
          match Event.kind event with
          | `Label -> Option.iter bounded (Event.text event)
          | `Output ->
              check_eq
                ~pp:Fmt.(Dump.option Dump.string)
                (Some raw) (Event.text event)
          | _ -> ())
        [ event; decode (Event.to_string event) ])
    events

let byte_count =
  choose
    [
      int64;
      map [ uint8; uint8 ] (fun power offset ->
          Int64.sub
            (Int64.shift_left 1L (10 * (1 + (power mod 6))))
            (Int64.of_int offset));
    ]

let test_byte_units count =
  let shown = Fmt.str "%a" Console.Progress.pp_bytes count in
  match String.split_on_char ' ' shown with
  | [ number; "B" ] ->
      check (count < 1024L);
      check_eq ~pp:Fmt.Dump.string (Int64.to_string (Int64.max 0L count)) number
  | [ number; unit ] ->
      let units = [ "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB" ] in
      let index =
        match List.find_index (String.equal unit) units with
        | Some index -> index
        | None -> failf "unknown byte unit in %S" shown
      in
      let value = Float.of_string number in
      let scale = 1024. ** float_of_int (index + 1) in
      check (Float.is_finite value && value >= 1. && value < 1024.);
      let dot = String.index number '.' in
      check_eq ~pp:Fmt.int 2 (String.length number - dot - 1);
      check (Float.abs (value -. (Int64.to_float count /. scale)) <= 0.005001);
      if index > 0 then
        check (Int64.to_float count /. (scale /. 1024.) >= 1023.995)
  | _ -> failf "not a byte count: %S" shown

let suite =
  ( "progress",
    [
      test_case "rounded byte counts stay normalized across every unit boundary"
        [ byte_count ] test_byte_units;
      test_case "public event labels and messages respect the 4 KiB bound"
        [ uint8; bool ] test_event_text_limits;
      test_case "bounded task output is the ordered suffix counted by dropped"
        [ uint8; uint8 ] test_output_suffix;
      test_case "root endings give each task and the work exactly one end"
        [ uint8; uint8 ] test_task_endings;
      test_case "retained labels and messages cut between characters at 4 KiB"
        [ uint8 ] test_retained_cut;
      test_case "every event accessor exposes its schema" [ bytes; nat; nat64 ]
        test_accessors;
      test_case "each maximal subpart gets a replacement character"
        [ invalid_prefix ] test_invalid_bytes;
      test_case "unknown events ignore members of other schemas" [ uint8 ]
        test_unknown_schema;
      test_case "event times read back at every finite magnitude"
        [ extreme_seconds ] test_extreme_seconds;
      test_case "an event reads back" [ float; kind ] test_round_trip;
      test_case "other is refused for a listed name"
        [
          value
            (known_events
            @ [ "items"; "bytes"; "app"; "error"; "warning"; "info"; "debug" ]
            @ [ "succeeded"; "failed"; "cancelled"; "ok" ]);
        ]
        test_other;
      test_case "a string reads back" [ bytes ] test_string;
      test_case "a time reads back to the millisecond" [ float ] test_time;
      test_case "a count reads back" [ nat64; nat64 ] test_count;
      test_case "public JSON schema keeps every field and renamed ancestry"
        [ uint8; bytes ] test_public_schema;
    ] )
