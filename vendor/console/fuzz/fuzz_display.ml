(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A display drives an emulated terminal. Each write it makes is fed to the
   emulator as one chunk, and the screen is read after every chunk: the
   guarantees of [display.mli] are checked on what a reader would see. *)

open Alcobar
open Console
module Driver = Display_harness.Driver

let labels =
  [|
    "fetch";
    "compile a rather long label that will not fit a narrow terminal";
    "\xe7\x95\x8c\xe7\x95\x8c wide";
    "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9 family";
    "e\xcc\x81 accent";
    "\027[2J escape";
    "tab\there";
  |]

type op =
  | Task of int * int option
  | Count of int * int
  | Bytes of int * int
  | Log of int * int
  | End of int * int
  | Report of int
  | Advance of int
  | Resize of int * int
  | Redraw
  | Output of int
  | Program of int * int

let op =
  let open Alcobar in
  choose
    [
      map [ uint8; option uint8 ] (fun l p -> Task (l, p));
      map [ uint8; uint8 ] (fun i n -> Count (i, n));
      map [ uint8; uint8 ] (fun i n -> Bytes (i, n));
      map [ uint8; uint8 ] (fun i l -> Log (i, l));
      map [ uint8; uint8 ] (fun i o -> End (i, o));
      map [ uint8 ] (fun l -> Report l);
      map [ uint8 ] (fun d -> Advance d);
      map [ uint8; uint8 ] (fun w h -> Resize (w, h));
      const Redraw;
      map [ uint8 ] (fun l -> Output l);
      map [ uint8; uint8 ] (fun c n -> Program (c, n));
    ]

let pp_op ppf = function
  | Task (l, p) -> Fmt.pf ppf "task %d %a" l Fmt.(option int) p
  | Count (i, n) -> Fmt.pf ppf "count %d %d" i n
  | Bytes (i, n) -> Fmt.pf ppf "bytes %d %d" i n
  | Log (i, l) -> Fmt.pf ppf "log %d %d" i l
  | End (i, o) -> Fmt.pf ppf "end %d %d" i o
  | Report l -> Fmt.pf ppf "report %d" l
  | Advance d -> Fmt.pf ppf "advance %d" d
  | Resize (w, h) -> Fmt.pf ppf "resize %d %d" w h
  | Redraw -> Fmt.string ppf "redraw"
  | Output l -> Fmt.pf ppf "output %d" l
  | Program (c, n) -> Fmt.pf ppf "program %d %d" c n

let op = with_printer pp_op op
let header = "HEADER"

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

let screen vte =
  List.map rstrip (String.split_on_char '\n' (Console_vte.to_string vte))

let nonblank vte = List.filter (( <> ) "") (screen vte)
let count p l = List.length (List.filter p l)
let has affix s = Re.execp (Re.compile (Re.str affix)) s

(* Each line a chunk writes, its escapes removed, fits the terminal. *)
let check_chunk_lines ~width chunk =
  List.iter
    (fun l ->
      (* A carriage return moves to the first column; it draws nothing. *)
      let l = String.concat "" (String.split_on_char '\r' l) in
      let l = Console_text.strip l in
      if Width.string_width l > width then
        failf "a line of %d cells on %d columns: %S" (Width.string_width l)
          width l)
    (String.split_on_char '\n' chunk)

(* Every line feed the display writes on a terminal follows a carriage
   return, so a tty in raw mode starts each line at the first column too. *)
let check_line_ends chunk =
  String.iteri
    (fun i c ->
      if c = '\n' && (i = 0 || chunk.[i - 1] <> '\r') then
        failf "a bare line feed at byte %d of %S" i chunk)
    chunk

(* Every style turned on in a chunk is off again at the end of each line. *)
let check_styles_closed chunk =
  List.iter
    (fun l ->
      let rec last_sgr i acc =
        match String.index_from_opt l i '\027' with
        | None -> acc
        | Some j when j + 1 < String.length l && l.[j + 1] = '[' -> (
            match String.index_from_opt l j 'm' with
            | Some k
              when String.for_all
                     (fun c -> (c >= '0' && c <= '9') || c = ';')
                     (String.sub l (j + 2) (k - j - 2)) ->
                last_sgr (k + 1) (Some (String.sub l j (k - j + 1)))
            | _ -> last_sgr (j + 1) acc)
        | Some j -> last_sgr (j + 1) acc
      in
      match last_sgr 0 None with
      | None | Some "\027[0m" -> ()
      | Some s -> failf "%S left on in %S" s l)
    (String.split_on_char '\n' chunk)

(* A write of the display is one synchronized update: it opens and closes
   the mode once, at its ends. *)
let check_synchronized chunk =
  let begin_ = "\027[?2026h" and end_ = "\027[?2026l" in
  let occurrences sub =
    List.length (Re.matches (Re.compile (Re.str sub)) chunk)
  in
  if
    not
      (String.starts_with ~prefix:begin_ chunk
      && String.ends_with ~suffix:end_ chunk
      && occurrences begin_ = 1
      && occurrences end_ = 1)
  then failf "not one synchronized update: %S" chunk

(* A display on an emulated terminal, and what the test knows of it. *)
type term = {
  vte : Console_vte.t;
  size : (int * int) ref;
  time : float ref;
  chunks : int ref;
  in_output : bool ref;
  mutable display : Display.t option;
  mutable root : Task.t option;
  mutable tasks : Task.t array;
}

(* The header is on the screen exactly once: the region is never blank, half
   drawn or drawn twice. *)
let check_screen t =
  let rows = screen t.vte in
  let n = count (has header) rows in
  if n <> 1 then
    failf "the header is on the screen %d times:@.%a" n
      Fmt.(list ~sep:cut string)
      rows;
  match List.find_opt (has header) rows with
  | Some row when String.contains row 'Z' ->
      failf "the program's bytes are in the header row: %S" row
  | _ -> ()

(* A frame drawn in place shows what the same frame drawn afresh shows, down
   to its last row: nothing of the frame before it is left, and nothing of
   its own erased. Below the region, rows a program's cursor moves left
   behind are erased by the fresh draw only. *)
let check_in_place t =
  let before = screen t.vte in
  Option.iter (fun d -> Display.with_output d ignore) t.display;
  let after = screen t.vte in
  let rec upto i last = function
    | [] -> last
    | r :: rs -> upto (i + 1) (if r = "" then last else i + 1) rs
  in
  let n = upto 0 0 after in
  let before = List.filteri (fun i _ -> i < n) before
  and after = List.filteri (fun i _ -> i < n) after in
  if before <> after then
    failf "drawn in place:@.%a@.drawn afresh:@.%a"
      Fmt.(list ~sep:cut string)
      before
      Fmt.(list ~sep:cut string)
      after

let terminal ?(prefix = "") ?theme ?scope ~width ~height ~styled () =
  let t =
    {
      vte = Console_vte.v ~rows:height ~cols:width ();
      size = ref (width, height);
      time = ref 0.;
      chunks = ref 0;
      in_output = ref false;
      display = None;
      root = None;
      tasks = [||];
    }
  in
  let chunk = Buffer.create 256 in
  let out_flush () =
    let s = Buffer.contents chunk in
    Buffer.clear chunk;
    if s <> "" then begin
      incr t.chunks;
      (* What the program writes under [with_output] is its own. *)
      if not !(t.in_output) then begin
        check_chunk_lines ~width:(fst !(t.size)) s;
        check_line_ends s;
        check_synchronized s
      end;
      if styled then check_styles_closed s;
      Console_vte.feed t.vte s
    end
  in
  Console_vte.feed t.vte prefix;
  let ppf = Format.make_formatter (Buffer.add_substring chunk) out_flush in
  if styled then Fmt.set_style_renderer ppf `Ansi_tty;
  Option.iter (Theme.set ppf) theme;
  let size () = !(t.size) and now () = !(t.time) in
  let root =
    match scope with
    | Some scope -> Driver.root ~label:header ~scope ~now ()
    | None -> Task.root ~label:header ~now ()
  in
  t.root <- Some root;
  t.display <- Some (Progress.show ~size ppf root);
  t

let display t = Option.get t.display
let root t = Option.get t.root
let label i = labels.(i mod Array.length labels)
let levels = [| `App; `Error; `Warning; `Info; `Debug |]
let level i = levels.(i mod Array.length levels)

let task t i =
  let n = Array.length t.tasks in
  if n = 0 then None else Some t.tasks.(i mod n)

let progress t i f =
  Option.iter
    (fun task -> try f task with Invalid_argument _ -> ())
    (task t i)

let finish task o =
  match o mod 3 with
  | 0 -> Task.succeed task
  | 1 -> Task.fail task
  | _ -> Task.cancel task

(* A redraw changes only the region: the rows from the cursor's up to the
   height of the screen less one. A second redraw at the same time changes
   nothing and writes nothing. *)
let redraw_twice t =
  let before = screen t.vte in
  Display.redraw (display t);
  let after = screen t.vte in
  let _, height = !(t.size) in
  let top = fst (Console_vte.cursor_pos t.vte) - (height - 2) in
  if
    Console_vte.scrollback_size t.vte = 0
    || List.length before = List.length after
  then
    List.iteri
      (fun i (b, a) ->
        if i < top && not (String.equal a b) then
          failf "row %d above the region changed: %S -> %S" i b a)
      (List.combine before after);
  let before = !(t.chunks) in
  Display.redraw (display t);
  if !(t.chunks) <> before then fail "an idle redraw wrote"

let output t l =
  Display.with_output (display t) (fun ppf ->
      t.in_output := true;
      Fmt.pf ppf "%s@." (label l);
      t.in_output := false)

(* What a program writes on the display's terminal goes out as it is: here
   a line of [Z] with bytes that move the cursor or erase in it. *)
let moves =
  [|
    "\r";
    "\027[A";
    "\027[3A";
    "\027[K";
    "\027[1K";
    "\027[J";
    "\027[H";
    "\027[9C";
    "\b";
    "\t";
  |]

let program t c n =
  let move = moves.(c mod Array.length moves) in
  let z = String.make (1 + (n mod 30)) 'Z' in
  Display.with_output (display t) (fun ppf ->
      t.in_output := true;
      Fmt.pf ppf "%s%s%s@." z move z;
      t.in_output := false)

let apply t = function
  | Task (l, p) ->
      let parent = Option.value (Option.bind p (task t)) ~default:(root t) in
      let task = Task.start ~log_lines:(l mod 4) parent (label l) in
      t.tasks <- Array.append t.tasks [| task |]
  | Count (i, n) ->
      progress t i (fun task -> Task.set_items task ~count:(n / 2) ~total:n)
  | Bytes (i, n) ->
      progress t i (fun task ->
          Task.set_bytes task ~count:(Int64.of_int (n * 1000)) ~total:256000L)
  | Log (i, l) -> Option.iter (fun task -> Task.log task (label l)) (task t i)
  | End (i, o) -> Option.iter (fun task -> finish task o) (task t i)
  | Report l -> Driver.report (root t) (level l) (label l)
  | Advance n -> t.time := !(t.time) +. (Float.of_int n /. 100.)
  | Resize (w, h) ->
      let w = 8 + (w mod 72) and h = 3 + (h mod 20) in
      t.size := (w, h);
      Console_vte.resize t.vte ~rows:h ~cols:w
  | Redraw -> redraw_twice t
  | Output l -> output t l
  | Program (c, n) -> program t c n

(* The host left [p] cells of a line unended: the region starts on a row of
   its own, below it. *)
let test ops w h styled p =
  let width = 8 + (w mod 72) in
  let prefix = String.make (p mod width) 'p' in
  let t = terminal ~prefix ~width ~height:(3 + (h mod 20)) ~styled () in
  (match Console_vte.scrollback_lines t.vte @ screen t.vte with
  | _ when prefix = "" -> ()
  | first :: _ when rstrip first = prefix -> ()
  | first :: _ -> failf "the host's line %S became %S" prefix first
  | [] -> fail "no screen");
  List.iter
    (fun o ->
      apply t o;
      Display.redraw (display t);
      check_screen t;
      check_in_place t)
    ops;
  Display.close (display t);
  if not (Console_vte.cursor_visible t.vte) then
    fail "the cursor is hidden after close";
  if snd (Console_vte.cursor_pos t.vte) <> 0 then
    fail "the cursor is not at the start of a line"

(* Without a terminal, nothing moves the cursor: every line is printed once,
   and the header comes last. *)
let test_plain ops =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let root = Task.root ~label:header ~now:(fun () -> 0.) () in
  let d = Progress.show ppf root in
  let tasks = ref [] in
  List.iter
    (function
      | Task (l, _) ->
          tasks := Task.start root labels.(l mod Array.length labels) :: !tasks
      | Log (_, l) -> List.iter (fun t -> Task.log t labels.(l mod 7)) !tasks
      | End _ -> List.iter Task.succeed !tasks
      | _ -> ())
    ops;
  Display.close d;
  Format.pp_print_flush ppf ();
  let out = Buffer.contents buf in
  if has "\027" out then failf "an escape without a terminal: %S" out;
  let lines = List.filter (( <> ) "") (String.split_on_char '\n' out) in
  match List.rev lines with
  | last :: _ ->
      if not (has header last) then failf "the header is not last: %S" last
  | [] -> fail "no header"

(* The sink and the caller retain no completed task, and a running task
   keeps at most 1 MiB of its output, for the case where it fails. After
   warmup past that cap, live heap measures retention rather than
   allocation volume. The task succeeds at the end, so that the 1 MiB it
   holds is not printed. *)
let test_retained_heap n =
  let ppf = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()) in
  let root = Task.root ~now:(fun () -> 0.) () in
  let d = Progress.show ~size:(fun () -> (80, 10)) ppf root in
  let task = Task.start ~log_lines:2 root "long running" in
  let batch () =
    for i = 1 to 1000 + n do
      Task.succeed (Task.start ~transient:true root "short lived");
      Task.log task (string_of_int i ^ String.make 512 'x')
    done;
    Display.redraw d;
    Gc.full_major ();
    (Gc.stat ()).live_words
  in
  Fun.protect
    (fun () ->
      ignore (batch ());
      ignore (batch ());
      let warm = batch () in
      for _ = 1 to 3 do
        let retained = batch () in
        if retained > warm + 4096 then
          failf "retained heap grew from %d to %d words" warm retained
      done)
    ~finally:(fun () ->
      Task.succeed task;
      Display.close d)

(* Each call has a unique token, so duplicates, reorderings and misattribution
   are detectable even when several tasks have the same display label. *)
let test_pipe_attribution n =
  let b = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer b in
  let root = Task.root ~label:"SUMMARY" ~now:(fun () -> 0.) () in
  let d = Progress.show ppf root in
  let tasks =
    Array.init 4 (fun i -> Task.start root ("task" ^ string_of_int i))
  in
  let expected =
    List.init (1 + n) (fun i ->
        let task = i mod 4 in
        let token = "record_" ^ string_of_int i ^ "_end" in
        Task.log tasks.(task) (token ^ "\n" ^ token ^ "_second");
        [
          "task" ^ string_of_int task ^ ": " ^ token;
          "task" ^ string_of_int task ^ ": " ^ token ^ "_second";
        ])
    |> List.concat
  in
  Array.iter Task.succeed tasks;
  Display.close d;
  Format.pp_print_flush ppf ();
  let out = Buffer.contents b in
  check (String.is_valid_utf_8 out);
  check (not (String.contains out '\027'));
  let records =
    String.split_on_char '\n' out
    |> List.filter (has ": record_")
    |> List.map (fun line ->
        match String.index_opt line 't' with
        | Some i -> String.sub line i (String.length line - i)
        | None -> fail "missing task label")
  in
  check_eq ~pp:Fmt.(Dump.list string) expected records;
  let nonempty = List.filter (( <> ) "") (String.split_on_char '\n' out) in
  check (has "SUMMARY" (List.hd (List.rev nonempty)))

let test_output_exception s cancel =
  let t = terminal ~width:40 ~height:8 ~styled:true () in
  let d = display t in
  ignore (Task.start (root t) "interrupted");
  Display.redraw d;
  let partial = Console_text.sanitize ~keep_newlines:false s in
  let partial = View.to_string (View.truncate 30 (View.text partial)) in
  (match
     Display.with_output d (fun ppf ->
         t.in_output := true;
         Fun.protect
           (fun () ->
             Fmt.pf ppf "%s" partial;
             raise Exit)
           ~finally:(fun () ->
             Format.pp_print_flush ppf ();
             t.in_output := false))
   with
  | _ -> fail "callback exception swallowed"
  | exception Exit -> ());
  Display.redraw d;
  check_screen t;
  if not cancel then Task.succeed ~message:"done" (root t);
  Display.close d;
  check (Console_vte.cursor_visible t.vte);
  check_eq ~pp:Fmt.int 0 (snd (Console_vte.cursor_pos t.vte));
  let y, _ = Console_vte.cursor_pos t.vte in
  Console_vte.feed t.vte "AFTER";
  check (has "AFTER" (List.nth (screen t.vte) y));
  let grid = Console_vte.grid t.vte in
  let index = Grid.idx grid ~x:0 ~y in
  let reference = Console_vte.v ~rows:8 ~cols:40 () in
  Console_vte.feed reference "AFTER";
  let plain = Console_vte.grid reference in
  let first = Grid.idx plain ~x:0 ~y:0 in
  let colours g i =
    Ansi.Color.(to_rgba (Console_vte.fg g i), to_rgba (Console_vte.bg g i))
  in
  check (colours grid index = colours plain first)

let test_resize_erases_old_frame n =
  let t = terminal ~width:80 ~height:12 ~styled:true () in
  let d = display t in
  let a = Task.start (root t) ("OLD_SENTINEL_" ^ String.make 45 'x') in
  let b = Task.start (root t) ("OLD_SENTINEL_" ^ String.make 45 'y') in
  Display.redraw d;
  let w = 12 + (n mod 40) in
  t.size := (w, 8);
  Console_vte.resize t.vte ~rows:8 ~cols:w;
  Task.set_label a "new-a";
  Task.set_label b "new-b";
  Display.redraw d;
  check_screen t;
  let rows =
    String.concat "\n"
      (Console_vte.scrollback_lines t.vte @ [ Console_vte.to_string t.vte ])
  in
  if has "OLD_SENTINEL" rows then failf "old frame survived shrink: %S" rows;
  t.size := (80, 12);
  Console_vte.resize t.vte ~rows:12 ~cols:80;
  Display.redraw d;
  let rows =
    String.concat "\n"
      (Console_vte.scrollback_lines t.vte @ [ Console_vte.to_string t.vte ])
  in
  if has "OLD_SENTINEL" rows then
    failf "old frame returned after growth: %S" rows;
  check (has "new-a" rows && has "new-b" rows);
  redraw_twice t;
  Display.close d

let test_static_rows_and_idle n =
  let t = terminal ~width:60 ~height:10 ~styled:true () in
  let d = display t in
  let task = Task.start (root t) "moving" in
  Display.with_output d (fun ppf ->
      t.in_output := true;
      Fmt.pf ppf "STATIC_SENTINEL@.";
      t.in_output := false);
  let before = screen t.vte in
  let rec prefix = function
    | [] -> fail "permanent output missing"
    | l :: ls -> if has "STATIC_SENTINEL" l then [ l ] else l :: prefix ls
  in
  let permanent = prefix before in
  for i = 0 to n do
    t.time := Float.of_int i /. 5.;
    Task.set_items task ~count:i ~total:256;
    redraw_twice t;
    check_eq ~pp:Fmt.(Dump.list string) permanent (prefix (screen t.vte))
  done;
  Display.close d

(* Compare every visible frame: CRLF handling may remove trailing padding
   without changing what the terminal shows. Keep cursor state in the oracle. *)
let capture_frames () =
  let out = Buffer.create 512 and chunk = Buffer.create 512 in
  let vte = Console_vte.v ~rows:8 ~cols:40 () in
  Console_vte.feed vte "OUTSIDE_SENTINEL\r\n";
  let frames = ref [] in
  let write bytes start len =
    Buffer.add_substring out bytes start len;
    Buffer.add_substring chunk bytes start len
  in
  let flush () =
    let bytes = Buffer.contents chunk in
    Buffer.clear chunk;
    if bytes <> "" then begin
      Console_vte.feed vte bytes;
      let rows =
        List.map rstrip (Console_vte.scrollback_lines vte @ screen vte)
      in
      frames :=
        (rows, Console_vte.cursor_pos vte, Console_vte.cursor_visible vte)
        :: !frames
    end
  in
  (Format.make_formatter write flush, out, frames)

(* A line sanitized, each tab gone to the next stop of eight columns. *)
let sanitized_line line =
  String.split_on_char '\t' line
  |> List.map Console_text.sanitize
  |> List.fold_left
       (fun acc piece ->
         match acc with
         | None -> Some piece
         | Some text ->
             let last =
               match String.rindex_opt text '\n' with
               | Some i -> String.sub text (i + 1) (String.length text - i - 1)
               | None -> text
             in
             let stop = 8 - (Width.string_width last mod 8) in
             Some (text ^ String.make stop ' ' ^ piece))
       None
  |> Option.value ~default:""

(* A CR before a line feed is part of that line end, and a CR that ends the
   text is a line end of its own. *)
let sanitized_log_lines input =
  let text =
    String.split_on_char '\n' input
    |> List.map (fun line ->
        let line =
          if String.ends_with ~suffix:"\r" line then
            String.sub line 0 (String.length line - 1)
          else line
        in
        sanitized_line line)
    |> String.concat "\n"
  in
  if String.ends_with ~suffix:"\r" input then text ^ "\n" else text

let test_untrusted_arguments s =
  let render input lines =
    let ppf, out, frames = capture_frames () in
    Fmt.set_style_renderer ppf `Ansi_tty;
    let theme = Theme.v ~marks:(fun _ -> input) ~spinner:[ input ] () in
    let layout =
      Progress.Layout.(
        list
          [
            const (View.text input);
            custom (fun _ -> View.styled Style.bold (View.text input));
            label ();
            spacer;
            elapsed;
          ])
    in
    Theme.set ppf theme;
    let root = Task.root ~label:input ~now:(fun () -> 0.) () in
    let d = Progress.show ~layout ~size:(fun () -> (40, 8)) ppf root in
    let task = Task.start root input in
    Task.set_label task input;
    Task.log task lines;
    Driver.report root `Warning lines;
    Display.redraw d;
    Task.succeed ~message:input task;
    Task.log root lines;
    Task.succeed ~message:input root;
    Display.close d;
    Format.pp_print_flush ppf ();
    (Buffer.contents out, List.rev !frames)
  in
  List.iter
    (fun input ->
      let raw, actual = render input input in
      let _, expected =
        render (Console_text.sanitize input) (sanitized_log_lines input)
      in
      check (String.is_valid_utf_8 raw && not (has "\027]" raw));
      check_styles_closed raw;
      if actual <> expected then failf "unsafe visible frame from %S" input)
    [
      s;
      String.make 18 'x' ^ "\r";
      "\r";
      "x\n\r";
      "s\002\246\147/\194\r";
      "\027]52;c;Y2xpcGJvYXJk\007";
      "\027]8;;https://evil.invalid\027\\click";
      "\027[2J\027[?25l";
      "\u{009b}2J\255\u{202e}spoof";
    ]

let test_extreme_terminals n styled =
  List.iter
    (fun (width, height) ->
      let t = terminal ~width ~height ~styled () in
      let d = display t in
      for i = 0 to 20 + (n mod 20) do
        let task =
          Task.start ~log_lines:1 (root t)
            (string_of_int i ^ " \u{754c}\u{1f469}\u{200d}\u{1f4bb}")
        in
        Task.log task "\u{754c}\u{1f1eb}\u{1f1f7} tail";
        Task.set_items task ~count:i ~total:100
      done;
      redraw_twice t;
      (* Nothing has completed or logged a permanent line. A region that fits
       leaves its bottom row free and cannot have produced scrollback. *)
      check_eq ~pp:Fmt.int 0 (Console_vte.scrollback_size t.vte);
      Display.close d;
      check (Console_vte.cursor_visible t.vte);
      check_eq ~pp:Fmt.int 0 (snd (Console_vte.cursor_pos t.vte)))
    [ (1, 1); (2, 80); (300, 10) ]

(* On a terminal of any width above 100 columns, no row the display draws
   nor line it prints for a task is wider than 100. *)
let test_wide n label =
  let width = 101 + (n land 0xff) in
  let t = terminal ~width ~height:10 ~styled:true () in
  let d = display t in
  let task = Task.start ~log_lines:2 (root t) label in
  Task.set_bytes task ~count:(Int64.of_int (n land 0xffff)) ~total:1_000_000L;
  Task.log task label;
  let child = Task.start task label in
  Task.set_items child ~count:1 ~total:(1 + (n land 0xfff));
  redraw_twice t;
  Task.succeed child;
  Task.fail task;
  Display.close d;
  List.iter
    (fun l ->
      if Width.string_width l > 100 then
        failf "a line of %d cells on %d columns: %S" (Width.string_width l)
          width l)
    (List.map rstrip (Console_vte.scrollback_lines t.vte) @ screen t.vte)

(* [n] running tasks with [l] log lines each, on a screen [h] rows high: the
   region is at most [h - 1] lines, the summary last; every task shows, or
   the latest ones after a line that counts exactly the others. *)
let test_marker_counts n l h =
  let n = 1 + (n mod 12) and l = l mod 3 and h = 3 + (h mod 12) in
  let t = terminal ~width:30 ~height:h ~styled:false () in
  let d = display t in
  for i = 0 to n - 1 do
    let task = Task.start ~log_lines:l (root t) ("t" ^ string_of_int i) in
    for j = 1 to l do
      Task.log task ("l" ^ string_of_int j)
    done
  done;
  Display.redraw d;
  let rows = List.filter (( <> ) "") (screen t.vte) in
  let shown = List.length rows in
  if shown > h - 1 then failf "%d lines on %d rows" shown h;
  (match List.rev rows with
  | last :: _ when has header last -> ()
  | _ -> fail "the summary is not last");
  let tasks =
    count (fun r -> Re.execp (Re.Perl.compile_pat "t[0-9]+") r) rows
  in
  (match List.find_opt (has "more running") rows with
  | Some m ->
      let hidden = Scanf.sscanf (String.trim m) "%_s %d" Fun.id in
      check_eq ~pp:Fmt.int n (tasks + hidden)
  | None -> check_eq ~pp:Fmt.int n tasks);
  Display.close d

(* A model of where each line goes. Every line is a unique token; the model
   follows which task is current, what each window holds and how each task
   ends, and the properties count where each token lands. *)

type step =
  | New of int option * int
  | Out of int
  | Diag of int option
  | Finish of int * int

let step =
  choose
    [
      map [ option uint8; uint8 ] (fun p l -> New (p, l));
      map [ uint8 ] (fun i -> Out i);
      map [ option uint8 ] (fun i -> Diag i);
      map [ uint8; uint8 ] (fun i o -> Finish (i, o));
    ]

let pp_step ppf = function
  | New (p, l) -> Fmt.pf ppf "new %a %d" Fmt.(option int) p l
  | Out i -> Fmt.pf ppf "out %d" i
  | Diag i -> Fmt.pf ppf "diag %a" Fmt.(option int) i
  | Finish (i, o) -> Fmt.pf ppf "finish %d %d" i o

let step = with_printer pp_step step

type mtask = {
  task : Task.t;
  name : string;
  window_size : int;
  mutable window : string list;  (** Newest first. *)
  mutable outcome : [ `Succeeded | `Failed | `Cancelled ] option;
  mutable kids : mtask list;
}

(* A token, the task it was given to if any, whether it is output and
   whether that task still ran. *)
type line = { token : string; owner : mtask option; output : bool; ran : bool }

let rec take n = function x :: l when n > 0 -> x :: take (n - 1) l | _ -> []

(* The model of [finish]: the children that run end first. *)
let rec model_end o t =
  if Option.is_none t.outcome then begin
    let child = match o with `Succeeded -> `Succeeded | _ -> `Cancelled in
    List.iter (model_end child) (List.rev t.kids);
    t.outcome <- Some o
  end

let outcome o =
  match o mod 3 with 0 -> `Succeeded | 1 -> `Failed | _ -> `Cancelled

let scope current =
  Driver.scope
    ~current:(fun () -> !current)
    ~within:(fun task f ->
      let saved = !current in
      current := Some task;
      Fun.protect f ~finally:(fun () -> current := saved))
    ~cancelled:(fun _ -> false)

(* A task of the model, started under [parent], or [root]. *)
let new_task root parent name window_size =
  let task =
    Task.start ~log_lines:window_size
      (match parent with Some p -> p.task | None -> root)
      name
  in
  let t = { task; name; window_size; window = []; outcome = None; kids = [] } in
  (match parent with
  | Some ({ outcome = None; _ } as p) -> p.kids <- t :: p.kids
  | Some { outcome = Some `Succeeded; _ } -> model_end `Succeeded t
  | Some _ -> model_end `Cancelled t
  | None -> ());
  t

let end_task t o =
  let o = outcome o in
  (match o with
  | `Succeeded -> Task.succeed t.task
  | `Failed -> Task.fail t.task
  | `Cancelled -> Task.cancel t.task);
  model_end o t

(* Runs [steps] on the tasks of [root], whose scope reads its current task
   from [current], and closes [d], which shows them. *)
let run_steps root d current steps =
  let tasks = ref [||] and lines = ref [] in
  let pick i =
    let n = Array.length !tasks in
    if n = 0 then None else Some !tasks.(i mod n)
  in
  let add owner output =
    let token = Fmt.str "k%04d" (List.length !lines) in
    let ran =
      match owner with Some t -> Option.is_none t.outcome | None -> false
    in
    (match owner with
    | Some t when ran -> t.window <- take t.window_size (token :: t.window)
    | _ -> ());
    lines := { token; owner; output; ran } :: !lines;
    token
  in
  List.iter
    (function
      | New (p, l) when Array.length !tasks < 16 ->
          let name = "t" ^ string_of_int (Array.length !tasks) in
          let t = new_task root (Option.bind p pick) name (l mod 4) in
          tasks := Array.append !tasks [| t |]
      | New _ -> ()
      | Out i ->
          Option.iter (fun t -> Task.log t.task (add (Some t) true)) (pick i)
      | Diag i ->
          let t = Option.bind i pick in
          let token = add t false in
          current := Option.map (fun t -> t.task) t;
          Driver.report root `Info token;
          current := None
      | Finish (i, o) -> Option.iter (fun t -> end_task t o) (pick i))
    steps;
  Display.close d;
  (* Closing cancels what runs, the latest first. *)
  List.iter (fun t -> model_end `Cancelled t) (List.rev (Array.to_list !tasks));
  List.rev !lines

(* [count token ~label lines] is how many of [lines] hold [token] after
   [label], and how many hold it otherwise. *)
let count token ~label lines =
  List.fold_left
    (fun (l, o) line ->
      if not (has token line) then (l, o)
      else
        match label with
        | Some name when has (name ^ ": " ^ token) line -> (l + 1, o)
        | _ -> (l, o + 1))
    (0, 0) lines

let check_lines ~where expected out =
  List.iter
    (fun (l, (want_labelled, want_other)) ->
      let label = Option.map (fun t -> t.name) l.owner in
      let labelled, other = count l.token ~label out in
      if labelled <> want_labelled || other <> want_other then
        failf "%s: %s %d labelled (want %d), %d other (want %d) in@.%a" where
          l.token labelled want_labelled other want_other
          Fmt.(list ~sep:cut string)
          out)
    expected

let ended l = match l.owner with Some t -> t.outcome | None -> None

(* On a pipe every line comes at once and once only, after its task's
   label or alone without a task. *)
let test_pipe_lines steps =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let current = ref None in
  let root = Driver.root ~scope:(scope current) ~now:(fun () -> 0.) () in
  let d = Progress.show ppf root in
  let lines = run_steps root d current steps in
  Format.pp_print_flush ppf ();
  let expected l =
    match l.owner with
    | None -> (0, 1)
    | Some _ when l.output && not l.ran -> (0, 0)
    | Some _ -> (1, 0)
  in
  check_lines ~where:"pipe"
    (List.map (fun l -> (l, expected l)) lines)
    (String.split_on_char '\n' (Buffer.contents buf))

(* On a terminal a line of a running task, output or Logs message alike,
   never reaches the scrollback if the task succeeds, and goes once under
   its line, without its label, if it fails or is cancelled. A Logs message
   of a task that ended goes above at once, after its label. *)
let test_terminal_lines steps =
  let current = ref None in
  let t =
    terminal ~scope:(scope current) ~width:80 ~height:12 ~styled:false ()
  in
  let lines = run_steps (root t) (display t) current steps in
  let expected l =
    match l.owner with
    | None -> (0, 1)
    | Some _ when not l.ran -> if l.output then (0, 0) else (1, 0)
    | Some _ -> (
        match ended l with
        | Some (`Failed | `Cancelled) -> (0, 1)
        | Some `Succeeded | None -> (0, 0))
  in
  check_lines ~where:"terminal"
    (List.map (fun l -> (l, expected l)) lines)
    (List.map rstrip (Console_vte.scrollback_lines t.vte) @ screen t.vte)

let themes = Array.of_list (List.map snd Theme.all)
let pick_theme n = themes.(n mod Array.length themes)

(* The glyphs of straight lines: the only ones a frame that moves swaps, so
   that its corners and tees stay where lines meet. *)
let line_glyphs =
  List.concat_map
    (fun b ->
      let c = Border.chars b in
      [ c.top; c.left; c.right; c.bottom ])
    Border.[ ascii; single; rounded; double; heavy ]

let clusters = Width.clusters

(* A table of [cells] from [labels], or a panel with them as its title and
   its lines, as a printer and as a span at a width. *)
let widget kind cells =
  let cells = List.filteri (fun i _ -> i < 4) cells in
  let cells = List.map (fun c -> View.text (label c)) cells in
  let cells = if cells = [] then [ View.text "x" ] else cells in
  if kind then
    let table =
      Table.view
        ~rules:(List.length cells mod 2 = 0)
        [
          Table.column (View.text "a");
          Table.column ~align:`Right (View.text "b");
        ]
        (List.map (fun c -> [ c; c ]) cells)
    in
    ((fun ppf -> View.pp ppf table), fun width -> View.sized ~width table)
  else
    let panel =
      Panel.view ~title:(List.hd cells)
        (View.concat ~sep:(View.text "\n") cells)
    in
    ((fun ppf -> View.pp ppf panel), fun width -> View.sized ~width panel)

(* [top f] above the default view of [root], [f] given the room's width. *)
let above top root =
  View.fit (fun r ->
      View.vcat
        [ top (Option.value (Room.width r) ~default:80); Progress.view root ])

(* The rows above the summary, the last line of the view, at each of
   [times] in tenths of a second. *)
let view_frames theme ~width span times =
  let t = terminal ~theme ~width ~height:600 ~styled:true () in
  let d = display t in
  Display.set_view d (above (fun width -> span (min width 100)) (root t));
  let frames =
    List.map
      (fun dt ->
        t.time := !(t.time) +. (Float.of_int dt /. 10.);
        Display.redraw d;
        let rows = screen t.vte in
        let last =
          List.fold_left max (-1)
            (List.mapi (fun i r -> if r = "" then -1 else i) rows)
        in
        List.filteri (fun i _ -> i < last) rows)
      times
  in
  Display.close d;
  frames

(* In every frame of a view in a theme with motion, only the glyphs of
   straight lines differ from the theme without motion, each from a glyph as wide: every
   line keeps its width and every cell its place. The theme without motion
   draws the same frame at every time: the table as Fmt prints it. *)
let test_view_frames kind cells n w times =
  let theme = pick_theme n and width = 8 + (w mod 113) in
  let pp, span = widget kind cells in
  let times = 0 :: times in
  let moving = view_frames theme ~width span times in
  let still = view_frames (Theme.still theme) ~width span times in
  (* As the same emulator shows it. *)
  let printed =
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    Theme.set ppf theme;
    Format.pp_set_margin ppf (min width 100);
    pp ppf;
    Format.pp_print_flush ppf ();
    let vte = Console_vte.v ~rows:600 ~cols:width () in
    Console_vte.feed vte
      (String.concat "\r\n" (String.split_on_char '\n' (Buffer.contents buf)));
    nonblank vte
  in
  List.iter (fun rows -> check_eq ~pp:Fmt.(Dump.list string) printed rows) still;
  List.iter2
    (fun moving still ->
      if List.length moving <> List.length still then
        failf "%d rows moving, %d still" (List.length moving)
          (List.length still);
      List.iter2
        (fun m s ->
          let a = clusters m and b = clusters s in
          if List.length a <> List.length b then failf "%S moved to %S" s m;
          List.iter2
            (fun a b ->
              if
                a <> b
                && not
                     (List.mem b line_glyphs
                     && Width.string_width a = Width.string_width b)
              then failf "%S moved to %S" s m)
            a b)
        moving still)
    moving still

(* On a pipe a view is drawn once, at the end, before nothing but the
   result, and a widget printed on a formatter is motionless: printed twice
   it is the same bytes, without colour those of its theme without motion,
   and in colour too but for a sweep, which colours its frame. *)
let test_view_pipe kind cells n m =
  let theme = pick_theme n in
  let pp, _ = widget kind cells in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  let root = Task.root ~label:header ~now:(fun () -> 1.5) () in
  let view = above (fun _ -> View.text "VIEW_SENTINEL") root in
  let d = Progress.show ~view ppf root in
  Task.succeed (Task.start root "t");
  Display.close d;
  Format.pp_print_flush ppf ();
  (match List.rev (String.split_on_char '\n' (Buffer.contents buf)) with
  | "" :: last :: sentinel :: _ when has header last ->
      check_eq ~pp:Fmt.Dump.string "VIEW_SENTINEL" sentinel
  | _ -> failf "the view is not last on a pipe: %S" (Buffer.contents buf));
  let printed ?(styled = true) theme =
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    Theme.set ppf theme;
    if styled then begin
      Fmt.set_style_renderer ppf `Ansi_tty;
      Color.set_depth ppf `True_color
    end;
    Format.pp_set_margin ppf (4 + (m mod 100));
    pp ppf;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  check_eq ~pp:Fmt.Dump.string (printed theme) (printed theme);
  check_eq ~pp:Fmt.Dump.string
    (printed ~styled:false (Theme.still theme))
    (printed ~styled:false theme);
  if Theme.motion theme <> Some `Sweep then
    check_eq ~pp:Fmt.Dump.string (printed (Theme.still theme)) (printed theme)

(* Views are user code too: once one raises, closing must still restore the
   cursor and default attributes for the next terminal writer. *)
let test_view_exception_close n k styled =
  let width = 20 + (n mod 61) and height = 8 + (k mod 8) in
  let term = Console_vte.v ~raw:true ~rows:height ~cols:width () in
  let ppf =
    Format.make_formatter
      (fun s off len -> Console_vte.feed term (String.sub s off len))
      (fun () -> ())
  in
  Theme.set ppf (snd (List.nth Theme.all (k mod List.length Theme.all)));
  if styled then Fmt.set_style_renderer ppf `Ansi_tty;
  let root = Task.root ~now:(fun () -> 0.) () in
  let d = Progress.show ~size:(fun () -> (width, height)) ppf root in
  ignore (Task.start root "running");
  (match
     Display.set_view d
       (View.fit (fun _ ->
            ignore (View.styled Style.bold (View.text "partial view"));
            raise Exit))
   with
  | () -> fail "view exception swallowed"
  | exception Exit -> ());
  (try Display.close d with Exit -> fail "close raised the view's exception");
  check (Console_vte.cursor_visible term);
  let y, x = Console_vte.cursor_pos term in
  check_eq ~pp:Fmt.int 0 x;
  Console_vte.feed term "AFTER";
  let reference = Console_vte.v ~raw:true ~rows:height ~cols:width () in
  Console_vte.feed reference "AFTER";
  for x = 0 to 4 do
    let grid = Console_vte.grid term and plain = Console_vte.grid reference in
    check (Grid.cells_equal grid (Grid.idx grid ~x ~y) plain x)
  done

(* A long-lived parent retains a bounded part of the tasks and output of
   completed children, keyed failures and cancellations included: none
   without a terminal, and on one at most 1 MiB, which they wait in for their
   parent. The output sink deliberately retains no transcript, so this
   measures the display alone. *)
let test_finished_children_heap n terminal =
  let ppf = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()) in
  let size = if terminal then Some (fun () -> (80, 10)) else None in
  let root = Task.root ~now:(fun () -> 0.) () in
  let d = Progress.show ?size ppf root in
  let parent = Task.start root "long lived parent" in
  let serial = ref 0 in
  let batch () =
    for i = 0 to 99 + (n mod 100) do
      incr serial;
      let child = Task.start ~key:(string_of_int !serial) parent "child" in
      Task.log child (String.make 8192 'x');
      match i mod 3 with
      | 0 -> Task.succeed child
      | 1 -> Task.fail child
      | _ -> Task.cancel child
    done;
    Display.redraw d;
    Gc.full_major ();
    (Gc.stat ()).live_words
  in
  Fun.protect
    (fun () ->
      (* On a terminal the children wait for their parent, up to 1 MiB, the
         oldest dropped past it down to half of it, so the heap goes up and
         down by as much: it may exceed what it held warm by 1 MiB, never by
         the three batches, of a megabyte or more each, that follow. *)
      ignore (batch ());
      let warm = ref (batch ()) in
      if terminal then warm := max !warm (batch ());
      let warm = !warm in
      let slack = if terminal then 1_048_576 / (Sys.word_size / 8) else 4096 in
      for _ = 1 to 3 do
        let retained = batch () in
        if retained > warm + slack then
          failf "completed children retained: %d to %d words" warm retained
      done)
    ~finally:(fun () -> Display.close d)

(* Changing the formatter's look redraws the whole region like a fresh
   display, including the view and the task's log window. *)
let theme_switch_display width theme =
  let term = Console_vte.v ~raw:true ~rows:12 ~cols:width () in
  Console_vte.feed term "before\r\n";
  let writes = ref [] in
  let ppf =
    Format.make_formatter
      (fun s off len ->
        let chunk = String.sub s off len in
        writes := chunk :: !writes;
        Console_vte.feed term chunk)
      ignore
  in
  Fmt.set_utf_8 ppf true;
  Fmt.set_style_renderer ppf `Ansi_tty;
  Theme.set ppf theme;
  let root = Task.root ~label:"build" ~now:(fun () -> 0.) () in
  let display = Progress.show ~size:(fun () -> (width, 12)) ppf root in
  let task = Task.start root "\u{754c} compile" in
  Task.set_items task ~count:1 ~total:2;
  Task.log task "a log";
  Display.set_view display
    (above
       (fun width ->
         View.sized ~width:(min width 100)
           (Panel.view ~title:(View.text "result") (View.text "hello")))
       root);
  Display.redraw display;
  (ppf, display, term, writes)

let test_theme_switch old_index next_index n =
  let width = 8 + (n mod 73) in
  let theme i =
    Theme.still (snd (List.nth Theme.all (i mod List.length Theme.all)))
  in
  let ppf, display, actual, writes =
    theme_switch_display width (theme old_index)
  in
  let _, reference, expected, _ =
    theme_switch_display width (theme next_index)
  in
  Fun.protect
    ~finally:(fun () ->
      Display.close display;
      Display.close reference)
    (fun () ->
      Theme.set ppf (theme next_index);
      Display.redraw display;
      check_eq
        ~pp:Fmt.(Dump.list Dump.string)
        (Console_vte.screen expected)
        (Console_vte.screen actual);
      let count = List.length !writes in
      Display.redraw display;
      check_eq ~pp:Fmt.int count (List.length !writes);
      List.iter
        (fun chunk ->
          check_chunk_lines ~width chunk;
          check_line_ends chunk;
          check_styles_closed chunk)
        !writes);
  check (Console_vte.cursor_visible actual);
  check_eq ~pp:Fmt.int 0 (snd (Console_vte.cursor_pos actual))

(* {1 The scrollback and the region}

   Random trees of tasks, ended in a random order: a model says, for each
   task, the tasks it moves to the scrollback with. *)

type ttask = {
  tk : Task.t;
  tname : string;
  tparent : ttask option;
  mutable tkids : ttask list;  (** Newest first. *)
  mutable ended : bool;
  mutable moved : bool;
}

let rec top x = match x.tparent with None -> x | Some p -> top p
let rec preorder x = x.tname :: List.concat_map preorder (List.rev x.tkids)

let rec end_model x =
  if not x.ended then begin
    List.iter end_model x.tkids;
    x.ended <- true
  end

type tree_step = Start of int option | Stop of int * int | Note of int | Wait

let tree_step =
  choose
    [
      map [ option uint8 ] (fun p -> Start p);
      map [ uint8; uint8 ] (fun i o -> Stop (i, o));
      map [ uint8 ] (fun i -> Note i);
      const Wait;
    ]

let pp_tree_step ppf = function
  | Start p -> Fmt.pf ppf "start %a" Fmt.(option int) p
  | Stop (i, o) -> Fmt.pf ppf "stop %d %d" i o
  | Note i -> Fmt.pf ppf "note %d" i
  | Wait -> Fmt.string ppf "wait"

let tree_step = with_printer pp_tree_step tree_step

(* Runs [steps] on [d]: the tasks, and the groups of names, in the order
   they move to the scrollback, unless [keep_ended]. [frame] is called after each
   step. *)
let run_tree ?(keep_ended = false) root ~frame steps =
  let tasks = ref [||] and moves = ref [] in
  let pick i =
    let n = Array.length !tasks in
    if n = 0 then None else Some !tasks.(i mod n)
  in
  let move x =
    if not x.moved then begin
      x.moved <- true;
      moves := preorder x :: !moves
    end
  in
  List.iter
    (fun step ->
      (match step with
      | Start p when Array.length !tasks < 16 ->
          let tparent = Option.bind p pick in
          let tname = Fmt.str "n%02d" (Array.length !tasks) in
          let tk =
            Task.start
              (match tparent with Some p -> p.tk | None -> root)
              tname
          in
          let x =
            { tk; tname; tparent; tkids = []; ended = false; moved = false }
          in
          (match tparent with
          | Some p when keep_ended || not (top p).moved ->
              p.tkids <- x :: p.tkids
          | Some _ ->
              (* Under a tree already in the scrollback: alone. *)
              x.ended <- true;
              moves := [ tname ] :: !moves;
              x.moved <- true
          | None -> ());
          (match tparent with
          | Some p when p.ended && not x.moved -> x.ended <- true
          | _ -> ());
          tasks := Array.append !tasks [| x |]
      | Start _ | Wait -> ()
      | Stop (i, o) ->
          Option.iter
            (fun x ->
              finish x.tk o;
              end_model x;
              if x.tparent = None && not keep_ended then move x)
            (pick i)
      | Note i -> Option.iter (fun x -> Task.log x.tk "note") (pick i));
      frame ())
    steps;
  (!tasks, moves)

let names_in line tasks =
  List.filter (fun x -> has x.tname line) (Array.to_list tasks)

(* Every frame has the summary last, no redraw scrolls the terminal, and at
   close the scrollback lists every task once, each
   top task's tree whole, in the order of the tree: its tasks under it, in
   the order they started. *)
let test_scrollback_tree steps h =
  let height = 6 + (h mod 15) and width = 80 in
  let t = terminal ~width ~height ~styled:false () in
  let d = display t in
  let frame () =
    let before = Console_vte.scrollback_size t.vte in
    Display.redraw d;
    if Console_vte.scrollback_size t.vte <> before then
      fail "a redraw scrolled the terminal";
    let rows = List.filter (( <> ) "") (screen t.vte) in
    match List.rev rows with
    | last :: _ when has header last -> ()
    | _ -> failf "the summary is not last:@.%a" Fmt.(list ~sep:cut string) rows
  in
  let tasks, moves = run_tree (root t) ~frame steps in
  Display.close d;
  (* Closing moves the trees still running, in the order they started. *)
  Array.iter
    (fun x ->
      if x.tparent = None && not x.moved then begin
        x.moved <- true;
        moves := preorder x :: !moves
      end)
    tasks;
  let lines = Console_vte.scrollback_lines t.vte @ screen t.vte in
  let seen =
    List.concat_map
      (fun l -> List.map (fun x -> x.tname) (names_in l tasks))
      lines
  in
  check_eq ~pp:Fmt.(Dump.list string) (List.concat (List.rev !moves)) seen

(* With [keep_ended] and a view of the top tasks' trees side by side, every frame
   fits the screen, the summary last, and the last frame holds every task
   once. *)
let test_columns steps h =
  let height = 6 + (h mod 15) and width = 120 in
  let vte = Console_vte.v ~rows:height ~cols:width () in
  let buf = Buffer.create 256 in
  let ppf =
    Format.make_formatter (Buffer.add_substring buf) (fun () ->
        let s = Buffer.contents buf in
        Buffer.clear buf;
        check_chunk_lines ~width s;
        Console_vte.feed vte s)
  in
  let root = Task.root ~label:header ~now:(fun () -> 0.) () in
  let view =
    View.fit (fun r ->
        let width = Option.value (Room.width r) ~default:width in
        let height = Option.map (fun h -> h - 1) (Room.height r) in
        let tops = Task.children root in
        let n = max 1 (List.length tops) in
        let column = (min width 100 - (2 * (n - 1))) / n in
        View.vcat
          [
            View.hcat
              (List.map
                 (fun t -> View.sized ~width:column ?height (Progress.tree t))
                 tops);
            Progress.summary root;
          ])
  in
  let d =
    Progress.show ~view ~keep_ended:true
      ~size:(fun () -> (width, height))
      ppf root
  in
  let frame () =
    Display.redraw d;
    let rows = nonblank vte in
    if List.length rows > height - 1 then failf "%d rows" (List.length rows);
    match List.rev rows with
    | last :: _ when has header last -> ()
    | _ -> fail "the summary is not last"
  in
  (* At most three top tasks, so that each column holds its names. *)
  let steps =
    let tops = ref 0 in
    List.filter
      (function
        | Start None ->
            incr tops;
            !tops <= 3
        | _ -> true)
      steps
  in
  let tasks, _ = run_tree ~keep_ended:true root ~frame steps in
  Display.close d;
  let lines = Console_vte.screen ~scrollback:true vte in
  Array.iter
    (fun x ->
      let n = List.length (List.filter (has x.tname) lines) in
      if n <> 1 then
        failf "%s is %d times in:@.%a" x.tname n
          Fmt.(list ~sep:cut string)
          lines)
    tasks

(* The default view is the tree of the root, fit to the room the summary
   leaves, then the summary: the two draw the same bytes. *)
let test_default_view steps h =
  let height = 4 + (h mod 15) in
  let run view =
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    let root = Task.root ~label:header ~now:(fun () -> 0.) () in
    let view = Option.map (fun f -> f root) view in
    let d = Progress.show ?view ~size:(fun () -> (60, height)) ppf root in
    ignore (run_tree root ~frame:(fun () -> Display.redraw d) steps);
    Display.close d;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  let explicit root =
    View.fit (fun r ->
        let height = Option.map (fun h -> h - 1) (Room.height r) in
        View.vcat
          [ View.sized ?height (Progress.tree root); Progress.summary root ])
  in
  check_eq ~pp:Fmt.Dump.string (run None) (run (Some explicit))

type pack =
  | Begin of int option * int
  | Items of int * int * int
  | Bytes_of of int * int
  | Finish of int * int
  | Tick

let pack =
  choose
    [
      map [ option uint8; uint8 ] (fun p n -> Begin (p, n));
      map [ uint8; uint8; uint8 ] (fun i c n -> Items (i, c, n));
      map [ uint8; uint8 ] (fun i c -> Bytes_of (i, c));
      map [ uint8; uint8 ] (fun i o -> Finish (i, o));
      const Tick;
    ]

let pp_pack ppf = function
  | Begin (p, n) -> Fmt.pf ppf "begin %a %d" Fmt.(option int) p n
  | Items (i, c, n) -> Fmt.pf ppf "items %d %d %d" i c n
  | Bytes_of (i, c) -> Fmt.pf ppf "bytes %d %d" i c
  | Finish (i, o) -> Fmt.pf ppf "finish %d %d" i o
  | Tick -> Fmt.string ppf "tick"

let pack = with_printer pp_pack pack

(* The cell [s] starts at in [row], each glyph one cell. *)
let find_cell s row =
  let s = Display_harness.glyphs s in
  let n = Array.length s in
  let rec go i =
    if i + n > Array.length row then None
    else if Array.for_all2 Uchar.equal s (Array.sub row i n) then Some i
    else go (i + 1)
  in
  go 0

type laid = {
  ends : int;  (** The cell after the label. *)
  bar : (int * int) option;  (** The first and last cells of the bar. *)
  last : int;  (** The cell after the last glyph. *)
}

let laid_out label row =
  let cells = Display_harness.glyphs row in
  Option.map
    (fun at ->
      {
        ends = at + String.length label;
        bar = Display_harness.bar_span row;
        last = Array.length cells;
      })
    (find_cell label cells)

(* The columns a frame's rows share widen only with a wider label, a first
   task of bytes or a count of items with more digits. *)
let widening tasks =
  List.fold_left
    (fun (bytes, items) (task, _) ->
      match Task.progress task with
      | `Bytes _ -> (true, items)
      | `Items (_, n) -> (bytes, max items (String.length (string_of_int n)))
      | `Spin -> (bytes, items))
    (false, 0) tasks

(* The rows of a region pack from the left: the bars of all rows start in
   one column, one cell after the widest label, and are as wide; every row
   with columns ends in one column; and over a run at one width, the bars
   never move left, and the columns move only when they widen. *)
let test_rows_pack steps w =
  let width = 40 + (w mod 81) and height = 24 in
  let vte = Console_vte.v ~rows:height ~cols:width () in
  let buf = Buffer.create 256 in
  let ppf =
    Format.make_formatter (Buffer.add_substring buf) (fun () ->
        let s = Buffer.contents buf in
        Buffer.clear buf;
        Console_vte.feed vte s)
  in
  Theme.set ppf Theme.dos;
  let now = ref 0. in
  let root = Task.root ~label:header ~now:(fun () -> !now) () in
  let d =
    Progress.show ~keep_ended:true ~size:(fun () -> (width, height)) ppf root
  in
  let tasks = ref [] in
  let pick i =
    match !tasks with
    | [] -> None
    | l -> Some (fst (List.nth l (i mod List.length l)))
  in
  let previous = ref None in
  let frame () =
    Display.redraw d;
    let rows = nonblank vte in
    let shown = String.concat "\n" rows in
    let laid =
      List.filter_map
        (fun (_, label) ->
          Option.bind
            (List.find_opt (fun r -> has (label ^ " ") r) rows)
            (laid_out label))
        !tasks
    in
    match List.filter_map (fun l -> l.bar) laid with
    | [] -> ()
    | (first, last) :: _ as bars ->
        if List.exists (( <> ) (first, last)) bars then
          failf "the bars are not in one column:@.%s" shown;
        let column = List.fold_left (fun n l -> max n l.ends) 0 laid in
        if first <> column + 1 then
          failf "the bars start at %d, after the labels at %d:@.%s" first column
            shown;
        let ends =
          List.sort_uniq Int.compare (List.map (fun l -> l.last) laid)
        in
        (match ends with
        | [ _ ] -> ()
        | _ -> failf "the rows end in several columns:@.%s" shown);
        let key = (column, widening !tasks) in
        (match !previous with
        | Some (_, (first', _), _, shown') when first < first' ->
            failf "the bars moved left:@.%s@.then@.%s" shown' shown
        | Some (key', bar', ends', shown')
          when key = key' && (bar' <> (first, last) || ends' <> ends) ->
            failf "the columns moved:@.%s@.then@.%s" shown' shown
        | _ -> ());
        previous := Some (key, (first, last), ends, shown)
  in
  List.iter
    (fun step ->
      (match step with
      | Begin (p, n) when List.length !tasks < 12 ->
          let label =
            Fmt.str "t%02d%s" (List.length !tasks) (String.make (n mod 24) '-')
          in
          let parent = Option.value (Option.bind p pick) ~default:root in
          tasks := !tasks @ [ (Task.start parent label, label) ]
      | Begin _ -> ()
      | Items (i, c, n) ->
          let total = [| 5; 12; 120; 1200; 99_999 |].(n mod 5) in
          Option.iter
            (fun task ->
              try Task.set_items task ~count:(c mod (total + 1)) ~total
              with Invalid_argument _ -> ())
            (pick i)
      | Bytes_of (i, c) ->
          let total = 2147483648L in
          let count = Int64.(div (mul total (of_int c)) 255L) in
          Option.iter
            (fun task ->
              try Task.set_bytes task ~count ~total
              with Invalid_argument _ -> ())
            (pick i)
      | Finish (i, o) -> Option.iter (fun task -> finish task o) (pick i)
      | Tick -> now := !now +. 0.25);
      frame ())
    steps;
  Display.close d

(* The row of a task that ended, from its label on, is the same in every
   frame while the columns of the rows stay as they are, as the time moves
   and the other tasks move, start and end. *)
let test_ended_rows_stay steps w =
  let width = 40 + (w mod 81) and height = 24 in
  let vte = Console_vte.v ~rows:height ~cols:width () in
  let buf = Buffer.create 256 in
  let ppf =
    Format.make_formatter (Buffer.add_substring buf) (fun () ->
        let s = Buffer.contents buf in
        Buffer.clear buf;
        Console_vte.feed vte s)
  in
  Theme.set ppf Theme.dos;
  let now = ref 0. in
  let root = Task.root ~label:header ~now:(fun () -> !now) () in
  let d =
    Progress.show ~keep_ended:true ~size:(fun () -> (width, height)) ppf root
  in
  let tasks = ref [] in
  let pick i =
    match !tasks with
    | [] -> None
    | l -> Some (fst (List.nth l (i mod List.length l)))
  in
  let seen = Hashtbl.create 16 in
  let frame () =
    Display.redraw d;
    let rows = nonblank vte in
    let own label =
      Option.bind
        (List.find_opt (fun r -> has (label ^ " ") r) rows)
        (fun r ->
          let cells = Display_harness.glyphs r in
          Option.map
            (fun at ->
              let b = Buffer.create 80 in
              for i = at to Array.length cells - 1 do
                Buffer.add_utf_8_uchar b cells.(i)
              done;
              Buffer.contents b)
            (find_cell label cells))
    in
    let column =
      List.fold_left
        (fun n (_, label) ->
          match List.find_opt (fun r -> has (label ^ " ") r) rows with
          | None -> n
          | Some r ->
              Option.fold ~none:n
                ~some:(fun l -> max n l.ends)
                (laid_out label r))
        0 !tasks
    in
    let key = (column, widening !tasks) in
    List.iter
      (fun (task, label) ->
        if Task.state task <> `Running then
          match own label with
          | None -> ()
          | Some row -> (
              match Hashtbl.find_opt seen label with
              | Some (key', row') when key = key' && row <> row' ->
                  failf "the row of %s moved:@.%s@.then@.%s@.in@.%s" label row'
                    row (String.concat "\n" rows)
              | _ -> Hashtbl.replace seen label (key, row)))
      !tasks
  in
  List.iter
    (fun step ->
      (match step with
      | Begin (p, n) when List.length !tasks < 12 ->
          let label =
            Fmt.str "t%02d%s" (List.length !tasks) (String.make (n mod 24) '-')
          in
          let parent = Option.value (Option.bind p pick) ~default:root in
          tasks := !tasks @ [ (Task.start parent label, label) ]
      | Begin _ -> ()
      | Items (i, c, n) ->
          let total = [| 5; 12; 120; 1200; 99_999 |].(n mod 5) in
          Option.iter
            (fun task ->
              try Task.set_items task ~count:(c mod (total + 1)) ~total
              with Invalid_argument _ -> ())
            (pick i)
      | Bytes_of (i, c) ->
          let total = 2147483648L in
          let count = Int64.(div (mul total (of_int c)) 255L) in
          Option.iter
            (fun task ->
              try Task.set_bytes task ~count ~total
              with Invalid_argument _ -> ())
            (pick i)
      | Finish (i, o) -> Option.iter (fun task -> finish task o) (pick i)
      | Tick -> now := !now +. 0.25);
      frame ())
    steps;
  Display.close d

(* Shrinking windows keeps the latest output, regardless of tree order. *)
let test_window_age n newest_child =
  let t = Display_harness.terminal ~width:60 ~height:4 () in
  let d = t.display in
  let parent = Task.start t.root "parent" in
  let child = Task.start parent "child" in
  let older, newer =
    if newest_child then (parent, child) else (child, parent)
  in
  for i = 0 to n mod 6 do
    Task.log older (Fmt.str "old-%d" i);
    t.advance 1.
  done;
  Task.log newer "newest-output";
  Display.redraw d;
  let rows = Display_harness.screen t in
  let scrolled = Console_vte.scrollback_size t.vte in
  Display.close d;
  check_eq ~pp:Fmt.int 0 scrolled;
  check_eq ~pp:Fmt.int 3 (List.length rows);
  if not (List.exists (Display_harness.contains "newest-output") rows) then
    failf "the newest output was dropped: %a" Fmt.(Dump.list string) rows

(* The newest completed child cannot exempt an arbitrarily large label
   or ending message from the bound on tasks waiting for their parent. *)
let test_oversized_waiting_task n message =
  let ppf = Format.make_formatter (fun _ _ _ -> ()) ignore in
  let root = Task.root ~now:(fun () -> 0.) () in
  let d = Progress.show ~size:(fun () -> (40, 8)) ppf root in
  let parent = Task.start root "parent" in
  let live_bytes () =
    Gc.full_major ();
    (Gc.stat ()).live_words * (Sys.word_size / 8)
  in
  let before = live_bytes () in
  let add () =
    let text = String.make ((2 * 1024 * 1024) + (n * 1024)) 'x' in
    if message then Task.succeed ~message:text (Task.start parent "child")
    else Task.succeed (Task.start parent text)
  in
  add ();
  let retained = live_bytes () - before in
  Task.succeed (Task.start parent "replacement");
  Display.close d;
  if retained > 1_048_576 + 65_536 then
    failf "one completed child retains %d bytes (limit 1 MiB)" retained

(* Measuring a tree for one place in a view must not affect its layout
   in another place, nor invalidate an unchanged frame on the terminal. *)
let test_tree_local_dimensions w h =
  let t = Display_harness.terminal ~width:80 ~height:12 ~label:"summary" () in
  let d = t.display in
  let parent = Task.start t.root "parent" in
  let child = Task.start parent "child" in
  Task.set_items parent ~count:1 ~total:10;
  Task.log parent "parent output";
  Task.log child "child output";
  Display.redraw d;
  let frame () =
    View.to_string (View.sized ~width:80 ~height:11 (Progress.view t.root))
  in
  let before = frame () in
  let writes = List.length !(t.chunks) in
  let width = 1 + (w mod 200) and height = h mod 11 in
  List.iter
    (fun task ->
      let tree () = View.sized ~width ~height (Progress.tree task) in
      let fitted = tree () in
      check (View.width fitted <= width);
      check (View.height fitted <= height);
      check_eq ~pp:Fmt.Dump.string (View.to_string fitted)
        (View.to_string (tree ())))
    [ t.root; parent; child ];
  let after = frame () in
  Display.redraw d;
  let unchanged = List.length !(t.chunks) = writes in
  Display.close d;
  check_eq ~pp:Fmt.Dump.string before after;
  check unchanged

(* A view that reads neither the time nor the program's state is drawn
   once: redraws at later times write nothing, whatever the theme, as long
   as nothing moves it. *)
let test_static_view w h k times =
  let width = 8 + (w mod 100) and height = 3 + (h mod 20) in
  let writes = ref 0 in
  let ppf =
    Format.make_formatter (fun _ _ len -> if len > 0 then incr writes) ignore
  in
  Theme.set ppf (pick_theme k);
  let time = ref 0. in
  let table =
    Table.view
      [ Table.column (View.text "name"); Table.column (View.text "size") ]
      [ [ View.text "a"; View.text "1" ]; [ View.text "b"; View.text "22" ] ]
  in
  let d =
    Display.v ~size:(fun () -> (width, height)) ~now:(fun () -> !time) ppf table
  in
  Display.redraw d;
  let drawn = !writes in
  List.iter
    (fun dt ->
      time := !time +. (Float.of_int dt /. 10.);
      Display.redraw d)
    times;
  check_eq ~pp:Fmt.int drawn !writes;
  Display.close d

(* What a task leaves in the scrollback, when it moves there, is what
   [commit] makes of it: one line for each task that moves, and none of the
   tasks that move with it; or nothing at all. *)
let test_commit steps policy =
  let width = 80 and height = 8 in
  let vte = Console_vte.v ~rows:height ~cols:width () in
  let buf = Buffer.create 256 in
  let ppf =
    Format.make_formatter (Buffer.add_substring buf) (fun () ->
        Console_vte.feed vte (Buffer.contents buf);
        Buffer.clear buf)
  in
  let root = Task.root ~label:header ~now:(fun () -> 0.) () in
  let commit t =
    if policy then Some (View.text ("DONE " ^ Task.label t)) else None
  in
  let d = Progress.show ~commit ~size:(fun () -> (width, height)) ppf root in
  let tasks, moves = run_tree root ~frame:(fun () -> Display.redraw d) steps in
  Task.succeed root;
  Display.close d;
  (* The end of the work moves the trees still running. *)
  Array.iter
    (fun x ->
      if x.tparent = None && not x.moved then moves := preorder x :: !moves)
    tasks;
  let heads =
    List.filter_map (function h :: _ -> Some h | [] -> None) !moves
  in
  let lines = Console_vte.screen ~scrollback:true vte in
  Array.iter
    (fun x ->
      let n = List.length (List.filter (has x.tname) lines) in
      let want = if policy && List.mem x.tname heads then 1 else 0 in
      if n <> want then
        failf "%s is %d times, not %d, in:@.%a" x.tname n want
          Fmt.(list ~sep:cut string)
          lines;
      if want = 1 && not (List.exists (has ("DONE " ^ x.tname)) lines) then
        failf "%s is not committed as DONE" x.tname)
    tasks

let test_plain_callback_keeps_tree n =
  let out = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer out in
  let root = Task.root ~now:(fun () -> 0.) () in
  let observed = ref [] in
  let plain event =
    let line = Task.Event.to_string event in
    observed := line :: !observed;
    Some (View.text line)
  in
  let names () =
    List.concat_map
      (fun parent ->
        Task.path parent :: List.map Task.path (Task.children parent))
      (Task.children root)
    |> List.map (String.concat "/")
  in
  let view =
    View.fit (fun _ -> View.text ("kept " ^ String.concat "," (names ())))
  in
  let display =
    Progress.show ~progress:`Plain ~plain ~keep_ended:true ~view ppf root
  in
  let count = 1 + (n mod 5) in
  let expected =
    List.concat
      (List.init count (fun i ->
           let label = "parent" ^ string_of_int i in
           let parent = Task.start root label in
           let child = Task.start parent "child" in
           Task.log child "one\ntwo";
           Task.succeed child;
           Task.succeed parent;
           [ label; label ^ "/child" ]))
  in
  Task.log root "outside";
  Task.succeed root;
  Display.close display;
  Format.pp_print_flush ppf ();
  check_eq ~pp:Fmt.(Dump.list string) expected (names ());
  check_eq ~pp:Fmt.Dump.string
    (String.concat "\n"
       (List.rev !observed
       @ [ "outside"; "kept " ^ String.concat "," expected; "" ]))
    (Buffer.contents out)

let test_delay_deadline n quick =
  let open Display_harness in
  let t =
    terminal
      ~width:(10 + (n mod 70))
      ~height:8 ~delay:2.
      ~view:(fun _ -> View.text "live")
      ()
  in
  Display.print t.display (View.text "notice");
  t.advance (Float.of_int (1 + (n mod 100)) /. 100.);
  Display.redraw t.display;
  check_eq ~pp:Fmt.(Dump.list string) [ "notice" ] (screen t);
  check (not (contains "\027[?25l" (written t)));
  if not quick then begin
    t.advance 2.;
    Display.redraw t.display;
    check (List.mem "live" (screen t));
    check (contains "\027[?25l" (written t))
  end;
  close t;
  check_eq ~pp:Fmt.(Dump.list string) [ "notice"; "live" ] (lines t);
  check (Console_vte.cursor_visible t.vte)

let suite =
  ( "display",
    [
      test_case "delayed displays wait for the controlled deadline"
        [ uint8; bool ] test_delay_deadline;
      test_case "custom pipe lines keep ended trees for the final view"
        [ uint8 ] test_plain_callback_keeps_tree;
      test_case "a still view is drawn once"
        [ uint8; uint8; uint8; list uint8 ]
        test_static_view;
      test_case "a top task leaves what its commit makes of it"
        [ list tree_step; bool ]
        test_commit;
      test_case "tree dimensions belong to each composition" [ uint8; uint8 ]
        test_tree_local_dimensions;
      test_case "one oversized waiting task stays bounded" [ uint8; bool ]
        test_oversized_waiting_task;
      test_case "windows give way oldest first across tasks" [ uint8; bool ]
        test_window_age;
      test_case "switching themes redraws like a fresh display"
        [ uint8; uint8; uint8 ] test_theme_switch;
      test_case "a running parent keeps a bounded part of completed children"
        [ uint8; bool ] test_finished_children_heap;
      test_case "the scrollback keeps every tree whole, in its order"
        [ list tree_step; uint8 ]
        test_scrollback_tree;
      test_case "rows pack from the left, their bars in one column"
        [ list pack; uint8 ]
        test_rows_pack;
      test_case "the row of a task that ended stays as it is"
        [ list pack; uint8 ]
        test_ended_rows_stay;
      test_case "trees side by side hold every task once"
        [ list tree_step; uint8 ]
        test_columns;
      test_case "the default view is the tree and the summary"
        [ list tree_step; uint8 ]
        test_default_view;
      test_case "view exceptions leave the terminal usable"
        [ uint8; uint8; bool ] test_view_exception_close;
      test_case "rows keep to 100 columns on a wide terminal" [ int; bytes ]
        test_wide;
      test_case "extreme terminal sizes keep region width and height bounded"
        [ uint8; bool ] test_extreme_terminals;
      test_case "untrusted display arguments behave like sanitized text"
        [ bytes ] test_untrusted_arguments;
      test_case "only live rows change and idle frames write nothing" [ uint8 ]
        test_static_rows_and_idle;
      test_case "resize erases every old live row" [ uint8 ]
        test_resize_erases_old_frame;
      test_case "callback exceptions restore a usable terminal" [ bytes; bool ]
        test_output_exception;
      test_case "pipe logs retain order and task attribution" [ uint8 ]
        test_pipe_attribution;
      test_case "long runs retain bounded heap" [ uint8 ] test_retained_heap;
      test_case "screen guarantees" [ list op; uint8; uint8; bool; uint8 ] test;
      test_case "plain output" [ list op ] test_plain;
      test_case "on a pipe each line comes once after its task's label"
        [ list step ]
        test_pipe_lines;
      test_case "on a terminal a line stays as its task ended"
        [ list step ]
        test_terminal_lines;
      test_case "the region fits and counts what it hides"
        [ uint8; uint8; uint8 ] test_marker_counts;
      test_case "a view's frames move only its border"
        [ bool; list uint8; uint8; uint8; list uint8 ]
        test_view_frames;
      test_case "a view is still off a live display"
        [ bool; list uint8; uint8; uint8 ]
        test_view_pipe;
    ] )
