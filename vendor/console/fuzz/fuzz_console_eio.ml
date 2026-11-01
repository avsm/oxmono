(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

(* A logging transport can refuse any record. The caller still completes,
   with the record handed once to the reporter installed before the run. *)
(* A formatter whose first write raises the exception [k] picks. *)
let refusing k =
  let refused = ref false in
  Format.make_formatter
    (fun _ _ _ ->
      if not !refused then begin
        refused := true;
        match k mod 3 with
        | 0 -> failwith "transport refused"
        | 1 -> invalid_arg "transport refused"
        | _ -> raise End_of_file
      end)
    (fun () -> ())

let test_refused_record s k =
  Eio_mock.Backend.run @@ fun () ->
  let text = String.sub s 0 (min 64 (String.length s)) in
  let kept = ref [] in
  let report _ _ ~over continue msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Fmt.kstr
          (fun message ->
            kept := message :: !kept;
            over ();
            continue ())
          fmt)
  in
  let before = Logs.reporter () in
  Logs.set_reporter { Logs.report };
  Fun.protect ~finally:(fun () -> Logs.set_reporter before) @@ fun () ->
  let previous = Logs.reporter () in
  let ppf = refusing k in
  let over_count = ref 0 and continued = ref 0 in
  let clock = Eio_mock.Clock.Mono.make () in
  let failure =
    try
      Console_eio.run ~clock ~ppf (fun _ ->
          let reporter = Logs.reporter () in
          reporter.Logs.report Logs.default Logs.Info
            ~over:(fun () -> incr over_count)
            (fun () -> incr continued)
            (fun m -> m "%s" text));
      None
    with (Failure _ | Invalid_argument _ | End_of_file) as ex -> Some ex
  in
  check (Logs.reporter () == previous);
  (match failure with
  | None -> ()
  | Some ex -> failf "a Logs call raised %s" (Printexc.to_string ex));
  (* An empty message writes nothing, so nothing refuses it. *)
  let expected = if text = "" then [] else [ text ] in
  check_eq ~pp:Fmt.(Dump.list Dump.string) expected (List.rev !kept);
  check_eq ~pp:Fmt.int 1 !over_count;
  check_eq ~pp:Fmt.int 1 !continued

(* A joined sibling retains the display while its first callback unwinds,
   so its later task output still has a live destination and attribution. *)
let test_sibling_lifetime k failing =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer out in
  Fmt.set_utf_8 ppf true;
  let first = ref None and body_ended = ref false and returned = ref false in
  let entered, enter = Eio.Promise.create () in
  let count = 1 + (k mod 8) in
  Eio.Fiber.both
    (fun () ->
      let raised =
        try
          Console_eio.run ~clock ~ppf (fun d ->
              first := Some d;
              Eio.Promise.await entered;
              body_ended := true;
              if failing then failwith "first callback failed");
          false
        with Failure message when message = "first callback failed" -> true
      in
      returned := true;
      check_eq ~pp:Fmt.bool failing raised)
    (fun () ->
      Console_eio.run ~clock ~ppf (fun d ->
          check (Option.fold ~none:false ~some:(( == ) d) !first);
          Eio.Promise.resolve enter ();
          Eio.Fiber.yield ();
          check !body_ended;
          check (not !returned);
          Console.Task.run d "sibling" (fun task ->
              for i = 0 to count - 1 do
                if !returned then
                  fail "the first run returned while its sibling still logs";
                Console.Task.log task (Fmt.str "record-%d" i);
                Eio.Fiber.yield ()
              done)));
  Format.pp_print_flush ppf ();
  let raw = Buffer.contents out in
  check (not (String.contains raw '\027'));
  let records =
    String.split_on_char '\n' raw
    |> List.filter (String.starts_with ~prefix:"  ")
  in
  check_eq
    ~pp:Fmt.(Dump.list Dump.string)
    (List.init count (fun i -> Fmt.str "  \u{203a} sibling: record-%d" i))
    records;
  check !returned

(* A context inherited by a fiber is only a hint to a live display. Once
   that run ended, a later call opens a fresh display or joins the current one. *)
let test_surviving_fiber k =
  let failing = k land 8 <> 0 and active = k land 16 <> 0 in
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer out in
  Fmt.set_utf_8 ppf true;
  let old = ref None and current = ref None and actual = ref None in
  let count = 1 + (k mod 8) in
  let before = Logs.reporter () in
  Eio.Switch.run (fun sw ->
      let ended, finish = Eio.Promise.create () in
      let completed, complete = Eio.Promise.create () in
      (try
         Console_eio.run ~clock ~ppf (fun d ->
             old := Some d;
             Eio.Fiber.fork ~sw (fun () ->
                 Eio.Promise.await ended;
                 Console_eio.run ~clock ~ppf (fun d ->
                     actual := Some d;
                     Console.Task.run d "later" (fun task ->
                         for i = 0 to count - 1 do
                           Console.Task.log task (Fmt.str "record-%d" i)
                         done));
                 Eio.Promise.resolve complete ());
             if failing then failwith "first callback failed")
       with Failure message when message = "first callback failed" -> ());
      if active then
        Console_eio.run ~clock ~ppf (fun d ->
            current := Some d;
            Eio.Promise.resolve finish ();
            Eio.Promise.await completed)
      else Eio.Promise.resolve finish ());
  check (Logs.reporter () == before);
  let same a b = match (a, b) with Some a, Some b -> a == b | _ -> false in
  if same !actual !old then fail "a surviving fiber reused its closed display";
  if active then check (same !actual !current);
  Format.pp_print_flush ppf ();
  let raw = Buffer.contents out in
  check (not (String.contains raw '\027'));
  let records =
    String.split_on_char '\n' raw
    |> List.filter (String.starts_with ~prefix:"  ")
  in
  check_eq
    ~pp:Fmt.(Dump.list Dump.string)
    (List.init count (fun i -> Fmt.str "  \u{203a} later: record-%d" i))
    records

(* With [`Json], each line the display writes is an event, written back
   as it is, and the labels and lines of output read back as the display
   shows them, whatever bytes they were given. *)
module Event = Console.Task.Event

(* [e] as a variant of its members, to match on. *)
let members e =
  let task () = Option.get (Event.task e) in
  let get = Option.get in
  match Event.kind e with
  | `Start -> `Start (task (), Event.parent e)
  | `Progress ->
      `Progress
        (task (), get (Event.count e), get (Event.total e), get (Event.unit e))
  | `Label -> `Label (task (), get (Event.text e))
  | `Output -> `Output (Event.task e, get (Event.text e), Event.level e)
  | `End ->
      `End
        ( Event.task e,
          get (Event.status e),
          get (Event.elapsed e),
          Event.message e )
  | `Other name -> `Other name

(* The events of the lines of [out], each checked to be valid UTF-8 and to
   be written back as it is. *)
let events out =
  String.split_on_char '\n' out
  |> List.filter (fun l -> l <> "")
  |> List.map (fun line ->
      check (String.is_valid_utf_8 line);
      match Jsont_bytesrw.decode_string Event.jsont line with
      | Ok e ->
          check_eq ~pp:Fmt.Dump.string line (Event.to_string e);
          members e
      | Error e -> failf "%s does not decode: %s" line e)

let test_json_events label text =
  (* A tab goes to a tab stop, which a space does not need. *)
  let text = String.map (fun c -> if c = '\t' then ' ' else c) text in
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer out in
  Console_eio.run ~clock ~ppf ~progress:`Json (fun display ->
      Console.Task.run display label (fun task ->
          Console.Task.set_items task ~count:1 ~total:2;
          Console.Task.log task text;
          Console.Task.set_label task label;
          Console.Task.fail task));
  Format.pp_print_flush ppf ();
  let events = events (Buffer.contents out) in
  let shown = Console_text.sanitize ~keep_newlines:false label in
  let labels =
    List.filter_map
      (function
        | `Start ((_, labels), _) | `End (Some (_, labels), _, _, _) ->
            Some labels
        | _ -> None)
      events
  in
  check_eq ~pp:Fmt.int 2 (List.length labels);
  List.iter (check_eq ~pp:Fmt.(Dump.list Dump.string) [ shown ]) labels;
  let expected =
    match
      String.split_on_char '\n' (Console_text.sanitize text) |> List.rev
    with
    | "" :: rest -> List.rev rest
    | lines -> List.rev lines
  in
  let expected = if text = "" then [] else expected in
  check_eq
    ~pp:Fmt.(Dump.list Dump.string)
    expected
    (List.filter_map
       (function `Output (_, text, _) -> Some text | _ -> None)
       events);
  check
    (List.exists
       (function `End (_, `Failed, _, _) -> true | _ -> false)
       events)

(* The id and the name of an event of a task. *)
let of_task = function
  | `Start ((id, _), _) -> Some (id, "start")
  | `Progress ((id, _), _, _, _) -> Some (id, "progress")
  | `Label ((id, _), _) -> Some (id, "label")
  | `Output (Some (id, _), _, _) -> Some (id, "output")
  | `End (Some (id, _), _, _, _) -> Some (id, "end")
  | `Output (None, _, _) | `End (None, _, _, _) | `Other _ -> None

(* A formatter backed by an Eio flow may yield after any chunk. Records
   still form a complete event stream, regardless of the task interleaving. *)
let test_json_concurrent_records chunk tasks =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 256 in
  let chunk = 1 + (chunk mod 32) and tasks = 2 + (tasks mod 4) in
  let ppf =
    Format.make_formatter
      (fun text offset length ->
        let rec write offset length =
          if length > 0 then begin
            let n = min chunk length in
            Buffer.add_substring out text offset n;
            Eio.Fiber.yield ();
            write (offset + n) (length - n)
          end
        in
        write offset length)
      (fun () -> ())
  in
  Console_eio.run ~clock ~ppf ~progress:`Json (fun d ->
      Eio.Fiber.List.iter
        (fun i ->
          Console.Task.run d (string_of_int i) (fun t ->
              Console.Task.log t "line";
              Console.Task.set_items t ~count:1 ~total:1))
        (List.init tasks Fun.id));
  Format.pp_print_flush ppf ();
  let records = events (Buffer.contents out) in
  check_eq ~pp:Fmt.int ((tasks * 4) + 1) (List.length records);
  let records = List.filter_map of_task records in
  List.iter
    (fun i ->
      check_eq
        ~pp:Fmt.(Dump.list Dump.string)
        [ "start"; "output"; "progress"; "end" ]
        (List.filter_map
           (fun (id, name) -> if id = i then Some name else None)
           records))
    (List.init tasks Fun.id)

(* tty.exe replays the real terminal session through matrix.vte. The same
   capture rule applies to Task.log, Fmt on either output and Logs. *)
let test_quiet_terminal_records ending =
  let mode =
    match ending mod 3 with
    | 0 -> "quiet-ok"
    | 1 -> "quiet-fail"
    | _ -> "quiet-cancel"
  in
  let helper name =
    Filename.concat
      (Filename.dirname Sys.executable_name)
      ("../test/eio/" ^ name ^ ".exe")
  in
  let input =
    Unix.open_process_args_in (helper "tty")
      [| "tty.exe"; helper "teardown"; mode |]
  in
  let screen = In_channel.input_all input in
  match Unix.close_process_in input with
  | Unix.WEXITED 77 -> ()
  | status ->
      check (status = Unix.WEXITED 0);
      let lines =
        String.split_on_char '\n' screen
        |> List.filter (String.ends_with ~suffix:" line")
      in
      let expected =
        if ending mod 3 = 0 then [ "root line" ]
        else
          [
            "  \u{203a} display line";
            "  \u{203a} stdout line";
            "  \u{203a} stderr line";
            "  logs line";
            "root line";
          ]
      in
      check_eq ~pp:Fmt.(Dump.list Dump.string) expected lines

(* Consumers can pair every start with one end, follow a parent, and recover
   counts and log levels under interleaved success, failure and cancellation. *)
let lifecycle_levels =
  [|
    (Logs.App, "app");
    (Logs.Error, "error");
    (Logs.Warning, "warning");
    (Logs.Info, "info");
    (Logs.Debug, "debug");
  |]

let lifecycle_worker k d i =
  let label = Fmt.str "worker-%d 👩‍💻" i in
  let ending = (k + i) mod 4 in
  let status =
    match ending with 0 -> "succeeded" | 1 | 3 -> "failed" | _ -> "cancelled"
  in
  let level, level_name =
    lifecycle_levels.((k + i) mod Array.length lifecycle_levels)
  in
  let ready, announce = Eio.Promise.create () in
  let work () =
    Console.Task.run d label (fun task ->
        Console.Task.set_bytes task ~count:Int64.max_int ~total:Int64.max_int;
        Eio.Fiber.yield ();
        let reporter = Logs.reporter () in
        reporter.Logs.report Logs.default level ~over:ignore ignore (fun m ->
            m "line-%d" i);
        match ending with
        | 1 -> Console.Task.fail task
        | 2 ->
            Eio.Promise.resolve announce ();
            Eio.Fiber.await_cancel ()
        | 3 -> failwith "worker failed"
        | _ -> ())
  in
  (try
     if ending = 2 then Eio.Fiber.first work (fun () -> Eio.Promise.await ready)
     else work ()
   with Failure message when message = "worker failed" -> ());
  (label, status, level_name)

let capture_lifecycles k count =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer out in
  let expected = ref [] in
  Console_eio.run ~clock ~ppf ~progress:`Json (fun d ->
      Console.Task.run d "parent 界" (fun parent ->
          Eio.Fiber.List.iter
            (fun i ->
              let ending = lifecycle_worker k parent i in
              expected := ending :: !expected)
            (List.init count Fun.id));
      ignore (Console.Task.start d "left running" : Console.Task.t);
      Console.Task.log d "root line";
      Console.Task.log d "detail";
      Console.Task.fail ~message:"result" d);
  Format.pp_print_flush ppf ();
  (events (Buffer.contents out), !expected)

(* The labels of an event of a task. *)
let labels_of = function
  | `Start ((_, l), _)
  | `Progress ((_, l), _, _, _)
  | `Label ((_, l), _)
  | `Output (Some (_, l), _, _)
  | `End (Some (_, l), _, _, _) ->
      Some l
  | `Output (None, _, _) | `End (None, _, _, _) | `Other _ -> None

let check_lifecycle_pairs count records =
  let tasks = List.filter_map of_task records in
  let starts =
    List.filter_map
      (function `Start (task, _) -> Some task | _ -> None)
      records
  in
  let ends = List.filter (fun (_, name) -> name = "end") tasks in
  check_eq ~pp:Fmt.int (count + 2) (List.length starts);
  check_eq ~pp:Fmt.int (List.length starts) (List.length ends);
  List.iter
    (fun (id, _) ->
      let own =
        List.filter_map
          (fun (id', name) -> if id = id' then Some name else None)
          tasks
      in
      check_eq ~pp:Fmt.int 1 (List.length (List.filter (( = ) "end") own));
      check (List.hd own = "start");
      check (List.hd (List.rev own) = "end"))
    starts;
  fst (List.find (fun (_, labels) -> labels = [ "parent 界" ]) starts)

let status_name = function
  | `Succeeded -> "succeeded"
  | `Failed -> "failed"
  | `Cancelled -> "cancelled"
  | `Other s -> s

let level_name = function
  | `App -> "app"
  | `Error -> "error"
  | `Warning -> "warning"
  | `Info -> "info"
  | `Debug -> "debug"
  | `Other s -> s

let check_worker_events records parent (label, status, level) =
  let path = Some [ "parent 界"; label ] in
  let own = List.filter (fun e -> labels_of e = path) records in
  let find f =
    match List.find_map f own with Some v -> v | None -> fail "missing event"
  in
  check (find (function `Start (_, p) -> Some p | _ -> None) = Some parent);
  check_eq ~pp:Fmt.string status
    (find (function `End (_, s, _, _) -> Some (status_name s) | _ -> None));
  let count, total, unit =
    find (function `Progress (_, c, n, u) -> Some (c, n, u) | _ -> None)
  in
  check (Int64.equal count Int64.max_int);
  check (Int64.equal total Int64.max_int);
  check (unit = `Bytes);
  let levels =
    List.filter_map (function `Output (_, _, l) -> Some l | _ -> None) own
  in
  check_eq
    ~pp:Fmt.(Dump.list (Dump.option string))
    [ Some level ]
    (List.map (Option.map level_name) levels)

let test_json_lifecycle k =
  let count = 1 + (k mod 8) in
  let records, expected = capture_lifecycles k count in
  check_eq ~pp:Fmt.int count (List.length expected);
  let parent = check_lifecycle_pairs count records in
  List.iter (check_worker_events records parent) expected;
  check
    (List.find_map
       (function `Output (task, "root line", _) -> Some task | _ -> None)
       records
    = Some None);
  check
    (List.find_map
       (function
         | `End (Some (_, [ "left running" ]), s, _, _) -> Some s | _ -> None)
       records
    = Some `Cancelled);
  check
    (List.exists
       (function `Output (None, "detail", _) -> true | _ -> false)
       records);
  match List.rev records with
  | `End (None, `Failed, _, Some "result") :: _ -> ()
  | _ -> fail "the work does not end failed, last"

(* Forced tty mode must keep the display's screen guarantees even when the
   destination has no terminal, using COLUMNS and LINES for its geometry. *)
let with_dimensions width height f =
  let bindings =
    [ ("COLUMNS", string_of_int width); ("LINES", string_of_int height) ]
  in
  let saved =
    List.map (fun (name, _) -> (name, Sys.getenv_opt name)) bindings
  in
  List.iter (fun (name, value) -> Unix.putenv name value) bindings;
  Fun.protect f ~finally:(fun () ->
      List.iter
        (fun (name, value) -> Unix.putenv name (Option.value value ~default:""))
        saved)

let tty_destination width height k =
  let screen = Console_vte.v ~raw:true ~rows:height ~cols:width () in
  let written = Buffer.create 256 in
  let ppf =
    Format.make_formatter
      (fun text offset length ->
        let chunk = String.sub text offset length in
        check (String.is_valid_utf_8 chunk);
        String.split_on_char '\n' (Console_text.strip chunk)
        |> List.iter (fun line ->
            check (Console.View.width (Console.View.text line) <= width));
        Buffer.add_string written chunk;
        Console_vte.feed screen chunk)
      (fun () -> ())
  in
  Fmt.set_utf_8 ppf true;
  Fmt.set_style_renderer ppf `Ansi_tty;
  let _, theme =
    List.nth Console.Theme.all (k mod List.length Console.Theme.all)
  in
  Console.Theme.set ppf (Console.Theme.still theme);
  (screen, written, ppf)

let test_forced_tty_screen k label =
  let width, height = [| (1, 1); (80, 2); (300, 10); (20, 8) |].(k mod 4) in
  with_dimensions width height (fun () ->
      Eio_mock.Backend.run @@ fun () ->
      let clock = Eio_mock.Clock.Mono.make () in
      let screen, written, ppf = tty_destination width height k in
      let raises = k land 4 <> 0 in
      let raised =
        try
          Console_eio.run ~clock ~ppf ~progress:`Tty (fun d ->
              check (not (Console_vte.cursor_visible screen));
              for i = 0 to k mod 32 do
                let task = Console.Task.start d (string_of_int i ^ label) in
                Console.Task.set_items task ~count:1 ~total:2
              done;
              Console_eio.with_output ignore;
              let before = Buffer.length written in
              Eio.Fiber.yield ();
              check_eq ~pp:Fmt.int before (Buffer.length written);
              if raises then failwith "body failed");
          false
        with Failure message when message = "body failed" -> true
      in
      check_eq ~pp:Fmt.bool raises raised;
      check (Console_vte.cursor_visible screen);
      check_eq ~pp:Fmt.int 0 (snd (Console_vte.cursor_pos screen));
      (* A subsequent ordinary write remains on the main screen in order. *)
      Console_vte.feed screen "Z\r\n";
      check
        (List.exists
           (fun line -> String.contains line 'Z')
           (Console_vte.screen ~scrollback:true screen)))

(* What [run] writes on {!Fmt.stdout} as [ppf], which it also takes the
   program's lines from under [progress]. *)
let capture_stdout progress f =
  Eio_mock.Backend.run @@ fun () ->
  Format.pp_print_flush Fmt.stdout ();
  let saved = Format.pp_get_formatter_out_functions Fmt.stdout () in
  let out = Buffer.create 256 in
  Format.pp_set_formatter_out_functions Fmt.stdout
    {
      saved with
      out_string = Buffer.add_substring out;
      out_flush = (fun () -> ());
    };
  Fun.protect
    ~finally:(fun () -> Format.pp_set_formatter_out_functions Fmt.stdout saved)
    (fun () ->
      Console_eio.run
        ~clock:(Eio_mock.Clock.Mono.make ())
        ~ppf:Fmt.stdout ~progress f;
      Format.pp_print_flush Fmt.stdout ());
  Buffer.contents out

(* Quiet stays silent when all tasks succeed, even if they flush or yield
   in the middle of a line. Replay the bytes on a terminal as well. *)
let test_quiet_unended_success size flush =
  let text = String.make (1 + (size mod 64)) 'x' in
  let out =
    capture_stdout `Quiet (fun d ->
        Console.Task.run d "success" (fun _ ->
            Fmt.pr "%s" text;
            if flush then Format.pp_print_flush Fmt.stdout ();
            Eio.Fiber.yield ()))
  in
  let screen = Console_vte.v ~raw:true ~rows:8 ~cols:80 () in
  Console_vte.feed screen out;
  check_eq ~pp:Fmt.(Dump.list Dump.string) [] (Console_vte.screen screen);
  check_eq ~pp:Fmt.Dump.string "" out;
  check (Console_vte.cursor_visible screen)

(* Suspending partway through a line must not transfer that text to a
   sibling task or emit it outside the task after its end event. *)
let test_json_partial_attribution size newline =
  let prefix i = string_of_int i ^ String.make (1 + (size mod 32)) 'x' in
  let out =
    capture_stdout `Json (fun d ->
        Eio.Fiber.List.iter
          (fun i ->
            Console.Task.run d (string_of_int i) (fun _ ->
                Fmt.pr "%s%!" (prefix i);
                Eio.Fiber.yield ();
                Fmt.pr "done";
                if newline then Fmt.pr "@."
                else Format.pp_print_flush Fmt.stdout ()))
          [ 0; 1 ])
  in
  let records = events out in
  let active = Hashtbl.create 2 and text = Array.make 2 "" in
  List.iter
    (fun kind ->
      let id =
        match kind with
        | `Start ((i, _), _)
        | `Progress ((i, _), _, _, _)
        | `Label ((i, _), _)
        | `Output (Some (i, _), _, _)
        | `End (Some (i, _), _, _, _)
          when i >= 0 && i < 2 ->
            i
        | _ -> fail "a task's partial line became root output"
      in
      match kind with
      | `Start _ -> Hashtbl.add active id ()
      | _ -> (
          check (Hashtbl.mem active id);
          match kind with
          | `End _ -> Hashtbl.remove active id
          | `Output (_, s, _) -> text.(id) <- text.(id) ^ s
          | _ -> ()))
    (List.filter (function `End (None, _, _, _) -> false | _ -> true) records);
  List.iter
    (fun i -> check_eq ~pp:Fmt.Dump.string (prefix i ^ "done") text.(i))
    [ 0; 1 ];
  check_eq ~pp:Fmt.int 0 (Hashtbl.length active)

(* The JSON representation of a printed line is independent of how the
   formatter chunks or flushes it, including at the 64 KiB capture bound. *)
let test_json_long_line size split =
  let length = if size land 1 = 0 then 65536 + size else 1 + size in
  let text = String.make length 'x' in
  let cut = min length (split * 257) in
  let out =
    capture_stdout `Json (fun d ->
        Console.Task.run d "large" (fun _ ->
            Fmt.pr "%s%!" (String.sub text 0 cut);
            Fmt.pr "%s@." (String.sub text cut (length - cut))))
  in
  let output =
    List.filter_map
      (function `Output (task, s, _) -> Some (task, s) | _ -> None)
      (events out)
  in
  check_eq ~pp:Fmt.int 1 (List.length output);
  check (fst (List.hd output) = Some (0, [ "large" ]));
  check_eq ~pp:Fmt.Dump.string text (snd (List.hd output))

(* What [run ppf] writes, with [ppf] a buffer's formatter or
   {!Format.str_formatter}, in [theme]. *)
let capture_progress_formatter ~theme ~use_str run =
  let buf = Buffer.create 256 in
  let ppf =
    if use_str then begin
      ignore (Format.flush_str_formatter ());
      Format.str_formatter
    end
    else Format.formatter_of_buffer buf
  in
  let old_theme = Console.Theme.get ppf in
  Console.Theme.set ppf theme;
  Fun.protect
    ~finally:(fun () -> Console.Theme.set ppf old_theme)
    (fun () ->
      run ppf;
      Format.pp_print_flush ppf ();
      if use_str then Format.flush_str_formatter () else Buffer.contents buf)

(* With output the program does not print itself, Progress.show and run show
   each progress mode alike, on Format.str_formatter too and in every
   theme. *)
let test_direct_progress_parity k label use_str =
  let label = String.sub label 0 (min 128 (String.length label)) in
  let _, theme =
    List.nth Console.Theme.all (k mod List.length Console.Theme.all)
  in
  let program d =
    Console.Task.run d label (fun parent ->
        let child =
          Console.Task.start ~transient:(k land 1 = 0) parent "child"
        in
        Console.Task.set_bytes child ~count:3L ~total:5L;
        Console.Task.log child "child output";
        Console.Task.cancel child;
        Console.Task.set_items parent ~count:1 ~total:1);
    Console.Task.log d "root output";
    Console.Task.log d "detail";
    Console.Task.succeed ~message:"done" d
  in
  with_dimensions 80 24 (fun () ->
      List.iter
        (fun (_, progress) ->
          let direct =
            capture_progress_formatter ~theme ~use_str (fun ppf ->
                let size =
                  if progress = `Auto || k land 2 = 0 then None
                  else Some (fun () -> (80, 24))
                in
                let root =
                  Console.Task.root ~label:"header" ~now:(fun () -> 0.) ()
                in
                let d = Console.Progress.show ?size ~progress ppf root in
                program root;
                Console.Display.close d)
          in
          let driven =
            capture_progress_formatter ~theme ~use_str (fun ppf ->
                Eio_mock.Backend.run (fun () ->
                    Console_eio.run
                      ~clock:(Eio_mock.Clock.Mono.make ())
                      ~ppf ~progress ~label:"header" program))
          in
          check_eq ~pp:Fmt.Dump.string direct driven)
        Console.Progress.all)

(* Progress.all is directly usable as a command-line enum, and diagnostics
   for any unknown spelling enumerate every available choice. *)
let test_progress_enum text =
  let parse =
    Cmdliner.Arg.conv_parser (Cmdliner.Arg.enum Console.Progress.all)
  in
  List.iter
    (fun (name, expected) ->
      (match parse name with
      | Ok got -> check (got = expected)
      | Error _ -> failf "Cmdliner rejects progress %s" name);
      match Console.Progress.of_string name with
      | Ok got -> check (got = expected)
      | Error _ -> failf "of_string rejects progress %s" name)
    Console.Progress.all;
  let unknown = "invalid:" ^ text in
  check (Result.is_error (parse unknown));
  match Console.Progress.of_string unknown with
  | Ok _ -> fail "an unknown progress spelling was accepted"
  | Error (`Msg message) ->
      List.iter
        (fun (name, _) ->
          let rec contains i =
            i + String.length name <= String.length message
            && (String.sub message i (String.length name) = name
               || contains (i + 1))
          in
          if not (contains 0) then failf "diagnostic omitted progress %s" name)
        Console.Progress.all

(* How late the backing clock wakes two concurrent sleeps does not change
   the bytes of the run, and each task runs for its own sleep. *)
let test_deterministic_concurrency a b =
  let key = "CONSOLE_DETERMINISTIC_CLOCK" in
  let saved = Sys.getenv_opt key in
  Unix.putenv key "0";
  Fun.protect ~finally:(fun () ->
      Unix.putenv key (Option.value saved ~default:""))
  @@ fun () ->
  let duration n = float_of_int (1 + (n mod 20)) /. 10. in
  let first = duration a and second = duration b in
  let capture overshoot =
    Eio_mock.Backend.run @@ fun () ->
    let backing = Eio_mock.Clock.Mono.make () in
    let clock = Console_eio.clock backing in
    let out = Buffer.create 512 in
    let ppf = Format.formatter_of_buffer out in
    let ready, wake = Eio.Promise.create () in
    let entered = ref 0 in
    Console_eio.run ~clock ~ppf ~progress:`Json (fun d ->
        let work label delay =
          Console.Task.run d label (fun _ ->
              incr entered;
              if !entered = 2 then Eio.Promise.resolve wake ();
              Eio.Time.Mono.sleep clock delay)
        in
        Eio.Fiber.both
          (fun () ->
            Eio.Fiber.both
              (fun () -> work "first" first)
              (fun () -> work "second" second))
          (fun () ->
            Eio.Promise.await ready;
            Eio_mock.Clock.Mono.set_time backing (Mtime.of_uint64_ns overshoot)));
    Format.pp_print_flush ppf ();
    let output = Buffer.contents out in
    let ends =
      String.split_on_char '\n' output
      |> List.filter (( <> ) "")
      |> List.filter_map (fun line ->
          match Console.Task.Event.of_string line with
          | Ok e when Console.Task.Event.kind e = `End -> (
              match Console.Task.Event.task e with
              | Some (_, path) -> Some (path, Console.Task.Event.elapsed e)
              | None -> None)
          | Ok _ -> None
          | Error (`Msg m) -> fail m)
    in
    check
      (List.sort compare ends
      = List.sort compare
          [ ([ "first" ], Some first); ([ "second" ], Some second) ]);

    output
  in
  check_eq ~pp:Fmt.Dump.string (capture 10_000_000_000L)
    (capture 20_000_000_000L)

let with_recording deterministic f =
  let file = Filename.temp_file "console-fuzz" ".cast" in
  let bindings =
    [
      ("CONSOLE_CAST", file);
      ("CONSOLE_DETERMINISTIC_CLOCK", if deterministic then "1" else "");
      ("COLUMNS", "40");
      ("LINES", "8");
    ]
  in
  let saved = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) bindings in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (k, v) -> Unix.putenv k (Option.value v ~default:"")) saved;
      Sys.remove file)
    (fun () ->
      List.iter (fun (k, v) -> Unix.putenv k v) bindings;
      f file)

(* The mock clock behind never moves, so a sleep that waited on it would
   leave the mock backend with nothing to run, which it raises. *)
let test_cast_sleep n =
  with_recording false @@ fun _ ->
  Eio_mock.Backend.run @@ fun () ->
  let clock = Console_eio.clock (Eio_mock.Clock.Mono.make ()) in
  let ppf = Format.formatter_of_buffer (Buffer.create 64) in
  let slept = ref false in
  Console_eio.run ~clock ~ppf (fun _ ->
      Eio.Time.Mono.sleep clock (float_of_int (1 + n));
      slept := true);
  check !slept

let test_cast_chunking n =
  with_recording true @@ fun file ->
  let glyphs = [| "界"; "👩‍💻"; "é"; "🇫🇷" |] in
  let text = "\027[31m" ^ glyphs.(n mod Array.length glyphs) ^ "text\027[0m" in
  Eio_mock.Backend.run (fun () ->
      let clock = Console_eio.clock (Eio_mock.Clock.Mono.make ()) in
      let ppf = Format.formatter_of_buffer (Buffer.create 64) in
      Console_eio.run ~clock ~ppf (fun _ ->
          let output = Format.pp_get_formatter_out_functions ppf () in
          String.iter
            (fun byte ->
              output.out_string (String.make 1 byte) 0 1;
              output.out_flush ())
            text));
  let lines = In_channel.with_open_bin file In_channel.input_lines in
  let events =
    match lines with _ :: events -> events | [] -> fail "no header"
  in
  let replay = Console_vte.v ~rows:8 ~cols:40 () in
  let output = Buffer.create 256 in
  let last = ref 0. in
  List.iter
    (fun line ->
      check (String.is_valid_utf_8 line);
      match Jsont_bytesrw.decode_string Jsont.json line with
      | Ok
          (Jsont.Array
             ( [
                 Jsont.Number (time, _);
                 Jsont.String ("o", _);
                 Jsont.String (data, _);
               ],
               _ )) ->
          check (time >= !last);
          last := time;
          Buffer.add_string output data
      | _ -> fail "invalid asciicast output event")
    events;
  Console_vte.feed replay (Buffer.contents output);
  let expected = Console_vte.v ~rows:8 ~cols:40 () in
  Console_vte.feed expected text;
  check_eq
    ~pp:Fmt.(Dump.list string)
    (Console_vte.screen expected)
    (Console_vte.screen replay)

let test_repeated_signals first second cut closing =
  let helper =
    Filename.concat
      (Filename.dirname Sys.executable_name)
      "../test/eio/teardown.exe"
  in
  let signal n = if n land 1 = 0 then "int" else "term" in
  let input =
    Unix.open_process_args_in helper
      [|
        "teardown.exe";
        "repeated-signal";
        signal first;
        signal second;
        string_of_int cut;
        string_of_bool closing;
      |]
  in
  let raw = In_channel.input_all input in
  let status = Unix.close_process_in input in
  check
    (match status with
    | Unix.WSIGNALED s -> s = Sys.sigint || s = Sys.sigterm
    | _ -> false);
  let terminal = Console_vte.v ~rows:24 ~cols:80 () in
  Console_vte.feed terminal raw;
  check (Console_vte.cursor_visible terminal);
  check (String.is_valid_utf_8 raw);
  check (String.ends_with ~suffix:"\027[?2026l" raw)

let test_prompt_line_limit n unicode crlf =
  Eio_mock.Backend.run @@ fun () ->
  let length = 4090 + (n mod 9) in
  let line =
    if unicode then (
      let out = Buffer.create length in
      for _ = 1 to length / 3 do
        Buffer.add_utf_8_uchar out (Uchar.of_int 0x754c)
      done;
      Buffer.add_string out (String.make (length mod 3) 'x');
      Buffer.contents out)
    else String.make length 'x'
  in
  let ending = if crlf then "\r\n" else "\n" in
  let ppf = Format.formatter_of_buffer (Buffer.create 16) in
  let prompt =
    Console_eio.Prompt.v ~interactive:true ~ppf
      (Eio.Flow.string_source (line ^ ending ^ "next\n"))
  in
  let first = Console_eio.Prompt.line prompt "" in
  check (Console_eio.Prompt.line prompt "" = Ok "next");
  if length <= 4096 then
    match first with
    | Ok answer -> check_eq ~pp:Fmt.Dump.string line answer
    | Error _ -> failf "a %d-byte answer was refused" length
  else check (Result.is_error first)

let suite =
  ( "console_eio",
    [
      test_case "prompt answers fit up to 4096 bytes without their terminator"
        [ uint8; bool; bool ] test_prompt_line_limit;
      test_case "repeated signals restore the terminal at every write cut"
        [ uint8; uint8; uint8; bool ]
        test_repeated_signals;
      test_case "recording alone makes sleeps virtual" [ uint8 ] test_cast_sleep;
      test_case "recording byte chunks preserves Unicode escapes and the screen"
        [ uint8 ] test_cast_chunking;
      test_case "concurrent deterministic sleeps give reproducible JSON"
        [ uint8; uint8 ] test_deterministic_concurrency;
      test_case
        "progress names are a Cmdliner enum and errors list every choice"
        [ bytes ] test_progress_enum;
      test_case
        "direct progress modes agree with run on buffer and string formatters"
        [ uint8; bytes; bool ] test_direct_progress_parity;
      test_case "JSON preserves long lines across formatter chunks"
        [ uint8; uint8 ] test_json_long_line;
      test_case "JSON partial lines stay inside their originating task"
        [ uint8; bool ] test_json_partial_attribution;
      test_case "quiet successful partial lines leave no output or screen text"
        [ uint8; bool ] test_quiet_unended_success;
      test_case "concurrent JSON records stay whole and complete"
        [ uint8; uint8 ] test_json_concurrent_records;
      test_case "quiet captures every terminal output source" [ uint8 ]
        test_quiet_terminal_records;
      test_case "JSON lifecycles keep counts parents levels and endings"
        [ uint8 ] test_json_lifecycle;
      test_case "forced tty keeps screen guarantees without a terminal"
        [ uint8; bytes ] test_forced_tty_screen;
      test_case "JSON events read back" [ bytes; bytes ] test_json_events;
      test_case "a surviving fiber uses a live display" [ uint8 ]
        test_surviving_fiber;
      test_case "a refused record falls back and completes once"
        [ bytes; uint8 ] test_refused_record;
      test_case "a sibling keeps the display after the first callback ends"
        [ uint8; bool ] test_sibling_lifetime;
    ] )
