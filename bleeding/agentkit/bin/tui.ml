(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A terminal interface for the agent, on Mosaic.

   The transcript is the screen. Everything the agent does that is not the
   reply itself is folded into one line per turn, because tool output is bulky,
   repetitive and rarely what a reader wants after the fact. Press tab to
   unfold the last turn's output.

   Mosaic owns the terminal and Eio owns the process, which
   [Matrix_eio] reconciles: the render loop is a fiber, so the agent can block
   in another without the interface stopping. *)

module Agent = Agentkit.Agent
module Driver = Agentkit.Driver
module Camel = Ds4.Camel
module Color = Mosaic.Ansi.Color
module Style = Mosaic.Ansi.Style
module Journal = Agentkit.Journal
module Trace = Agentkit.Trace

(* OCaml's orange, and the greys the interface is otherwise built from. *)
let orange = Color.Extended 208
let hump_style = Style.fg orange Style.default
let dim = Style.with_dim true Style.default
let faint = Style.fg Color.Bright_black Style.default
let plain = Style.default
let key_chip = Style.fg Color.Black (Style.bg orange Style.default)

(* A hump for a camel to reply behind. The interface is mostly grey, so the
   humps and the orange are what carry the OCaml in it. *)
let hump_marks = [| "ʌ"; "ʌʌ"; "ʌ__"; "ʌ_ʌ" |]

(* What the camel is doing while you wait. OCaml has always been named after an
   animal that chews slowly, so the wait may as well say so. *)
let mutterings =
  [|
    "ruminating";
    "let rec think = think";
    "chewing the cud";
    "unifying";
    "folding left";
    "∀ humps";
    "pattern matching";
    "in tail position";
  |]

(* A tool call and, once it returns, what it said. *)
type call = { name : string; args : string; result : string option }

type entry =
  | Asked of string
  | Replied of string
  | Note of string
  | Failed of string

(* Fixed when the interface starts. Apart from the model so that what is left
   is what actually changes. *)
type config = {
  title : string;
  about : string;
  max_ctx : int; (* how far the context may grow *)
}

type model = {
  entries : entry list; (* oldest first *)
  input : string;
  busy : bool;
  stats : Agent.stats option;
  (* The window the gauge draws. It comes from the stats after a turn, but a
     context that has just grown must be shown at its new size straight away,
     and the stats do not arrive until the turn that grew it ends. *)
  ctx_size : int;
  (* Tokens prefilled and tokens expected, sampled on every tick. *)
  prefill : int * int;
  rates : float list; (* newest first, for the sparkline *)
  unfolded : bool;
  (* The field owns its buffer, so it is remounted under a new key to clear it
     or to fill it from the history. *)
  revision : int;
  queued : string list; (* asked while busy, oldest first *)
  history : string list; (* everything asked, newest first *)
  browsing : int option; (* position in the history, while browsing it *)
  draft : string; (* what was typed before browsing began *)
  activity : call list; (* newest first, for the tool column *)
  (* The steps a tool is taking, newest first and bounded, shown above the
     calls. It is what a call that has stopped answering says about itself, so
     it is kept out of the transcript and forgotten with the session. *)
  trace : string list;
  reply : Buffer.t;
  mutter : int; (* which muttering to show, advanced once a turn *)
  (* The terminal's width, so the view can drop what will not fit. Mosaic
     reports a change but not the width at startup, which [run] seeds. *)
  cols : int;
}

type msg =
  | Typed of string
  | Submitted of string
  | Agent_event of Agent.event
  | Agent_done
  | Agent_failed of string
  | Resized of int
  | Toggle
  | Older
  | Newer
  | Tick
  | Cancel
  | Quit

(* Blocks, low to high. A rate history reads better as a shape than as a
   number that changes every turn. *)
let blocks = [| "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |]
let sparkline_len = 24

(* How much of a tool's progress is kept, and how much of it is shown. A stuck
   call is read from the newest line, and the few before it say how it got
   there. *)
let trace_kept = 50
let trace_shown = 4

let sparkline values =
  let top = List.fold_left max 1. values in
  values
  |> List.rev_map (fun v ->
      let i = int_of_float (v /. top *. 7.) in
      blocks.(max 0 (min 7 i)))
  |> String.concat ""

(* Tool arguments are a flat JSON object. Shown as fields they fit a narrow
   column and read at a glance, where the raw object does not. Empty values are
   dropped, since an unset capability says nothing. *)
let json_fields args =
  let text_of = function
    | Jsont.String (x, _) -> x
    | Jsont.Number (n, _) ->
        if Float.is_integer n then Printf.sprintf "%.0f" n
        else Printf.sprintf "%g" n
    | Jsont.Bool (b, _) -> string_of_bool b
    | Jsont.Null _ -> "null"
    | other -> Dsml.Json.Value.to_string other
  in
  match Dsml.Json.Value.of_string args with
  | Ok (Jsont.Object (mems, _)) ->
      List.filter_map
        (fun ((k, _), v) ->
          match text_of v with "" -> None | value -> Some (k, value))
        mems
  | Ok other -> [ ("", Dsml.Json.Value.to_string other) ]
  | Error _ -> [ ("", args) ]

(* The three shapes every row and column in here takes. *)
let full_w = Mosaic.size_wh (Mosaic.pct 100) Mosaic.auto
let line = Mosaic.size_wh (Mosaic.pct 100) (Mosaic.px 1)
let fill = Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100)
let sep = Mosaic.text ~style:faint "·"
let spacer = Mosaic.box ~flex_grow:1. []

(* [thousands n] is [n] in units a glance can compare. *)
let thousands n =
  if n >= 1000 then Printf.sprintf "%dk" (n / 1000) else string_of_int n

(* What a prefill still running says about itself, and nothing while none is.
   A turn that follows a grown context prefills the whole conversation again,
   which is minutes in which the interface would otherwise say only that it was
   busy, and a busy interface that never finishes reads as a wedged one. *)
let prefilling (filled, expected) =
  if expected <= 0 || filled >= expected then None
  else
    Some
      (Printf.sprintf "prefilling %s of %s" (thousands filled)
         (thousands expected))

(* A flex item's minimum is its content unless told otherwise, so anything
   that must give way says so. [give_x] lets a line of text narrow; [give]
   also lets a box shrink below its content and scroll instead of growing
   past its parent. *)
let give_x = Mosaic.size_wh (Mosaic.px 0) Mosaic.auto
let give = Mosaic.size_wh (Mosaic.px 0) (Mosaic.px 0)
let plural n noun = Printf.sprintf "%d %s%s" n noun (if n = 1 then "" else "s")

let short n s =
  if String.length s <= n then s else String.sub s 0 (max 0 (n - 1)) ^ "…"

let row marker style body =
  let open Mosaic in
  box ~size:full_w ~gap:(gap_xy 1 0)
    [
      text ~style:faint marker;
      text ~style ~wrap:`Word ~flex_grow:1. ~min_size:give_x body;
    ]

(* The conversation, and nothing else. The model writes markdown, so it is
   rendered rather than shown raw. *)
let view_entry i e =
  let open Mosaic in
  match e with
  | Asked s -> row "›" plain s
  | Replied s ->
      box ~size:full_w ~gap:(gap_xy 1 0)
        [
          text ~style:hump_style hump_marks.(i mod Array.length hump_marks);
          box ~flex_grow:1. ~min_size:give_x [ markdown ~conceal:true s ];
        ]
  | Note s -> row "⚿" (Style.fg Color.Yellow Style.default) s
  | Failed s -> row "✗" (Style.fg Color.Red Style.default) s

(* A key and what it does. The key is set on a block of colour so the eye
   separates it from the words describing it. *)
let key_hint k what =
  let open Mosaic in
  box ~flex_shrink:0. ~gap:(gap_xy 1 0)
    [ text ~style:key_chip (Printf.sprintf " %s " k); text ~style:faint what ]

(* A prompt that arrived while the model was working, shown where it was typed
   so that it is plainly not lost. *)
let view_queued s = row "·" faint s

(* One call in the narrow column: the tool that ran, then its arguments as
   fields. The result is bulky enough to be worth a key press. *)
let view_call unfolded (c : call) =
  let open Mosaic in
  let field (k, v) =
    box ~gap:(gap_xy 1 0) ~size:full_w
      [
        text ~style:faint (if k = "" then " " else k);
        text ~style:dim ~wrap:`Char ~flex_grow:1. ~min_size:give_x v;
      ]
  in
  box ~flex_direction:Flex_direction.Column ~size:full_w
    ([
       box ~gap:(gap_xy 1 0)
         [ text ~style:hump_style "⚙"; text ~style:plain c.name ];
     ]
    @ List.map field (json_fields c.args)
    @
    match (unfolded, c.result) with
    | true, Some r ->
        [ text ~style:faint ~wrap:`Char ~min_size:give_x (short 240 r) ]
    | _ -> [])

let view cfg m =
  let open Mosaic in
  let used, turns, tools =
    match m.stats with
    | Some s -> (s.ctx_used, s.turns, s.tool_calls)
    | None -> (0, 0, 0)
  in
  let size = m.ctx_size in
  let full = 100. *. float_of_int used /. float_of_int (max 1 size) in
  let rate = Option.value (List.nth_opt m.rates 0) ~default:0. in
  (* What the width affords. The conversation is the point of the screen, so
     the tool column takes a share of the width rather than a fixed slice, and
     goes entirely when what is left would no longer hold a sentence. The
     vitals and the key hints shed their least important items the same way,
     since a row that overruns its width is truncated wherever it happens to
     reach. *)
  let show_tools = m.cols >= 76 in
  let tool_w = max 30 (min 46 (m.cols * 2 / 5)) in
  (* [from n el] is [el] on a terminal at least [n] columns wide. *)
  let from n el = if m.cols >= n then el else empty in
  (* A header row rather than a border title, so the parts can space themselves
     across the width instead of being crammed into a line of box drawing. *)
  let header =
    box ~size:line ~padding:(padding_xy 1 0) ~gap:(gap_xy 1 0)
      [
        text ~style:hump_style "ʌ__";
        text ~style:plain cfg.title;
        spacer;
        text ~style:dim cfg.about;
      ]
  in
  (* The reply as it is being written. It becomes an entry once the turn
     ends. *)
  let live =
    let b = Buffer.contents m.reply in
    if b = "" then []
    else
      [
        box ~size:full_w ~gap:(gap_xy 1 0)
          [
            box ~flex_direction:Flex_direction.Column ~flex_shrink:0.
              (List.map (fun l -> text ~style:hump_style l) Camel.art);
            text ~style:plain ~wrap:`Word ~flex_grow:1. ~min_size:give_x b;
          ];
      ]
  in
  (* An empty transcript is where the camel introduces itself. A note is not a
     conversation, so one queued before anything was asked, such as what became
     of the dune tools, appears under the greeting rather than in place of
     it. *)
  let said = List.exists (function Note _ -> false | _ -> true) m.entries in
  let greeting =
    if said || live <> [] then []
    else
      [
        box ~flex_direction:Flex_direction.Column ~padding:(padding_xy 0 1)
          (List.map (fun l -> text ~style:hump_style l) Camel.art
          @ [
              text ~style:faint ~wrap:`Word ~min_size:give_x
                "  a camel, some humps, and a large language model";
              text ~style:dim ~wrap:`Word ~min_size:give_x ("  " ^ cfg.about);
            ]);
      ]
  in
  (* Each column is a scrolling stack under a border. Both must be able to
     shrink below their content, or they grow past the frame instead of
     scrolling inside it. [head] sits above the stack rather than in it, since
     the stack is stuck to its bottom and would carry anything put there out of
     sight. *)
  let column ?title ?(head = []) ~border_color children =
    box ?title ~size:fill ~min_size:give ~border:true ~border_color
      ~padding:(padding_xy 1 0) ~flex_direction:Flex_direction.Column
      (head
      @ [
          (* The stack takes what the head leaves rather than asking for the
             whole column and then shrinking back, which would take a row off
             the head at every size. *)
          scroll_box ~flex_grow:1. ~flex_basis:(px 0) ~scrollbar_width:1.
            ~size:fill ~min_size:give ~sticky_scroll:true ~sticky_start:`Bottom
            [
              box ~flex_direction:Flex_direction.Column ~size:full_w
                ~gap:(gap_xy 0 1) children;
            ];
        ])
  in
  (* What okit is doing now, at the top of the column and above the calls it
     belongs to. Each line is cut to the column rather than wrapped, so a slow
     step costs one row. [min_size] is pinned, since a box left with its
     content-based minimum grows past its parent rather than giving way, and on
     a short screen this one would carry the column past the frame. A session
     with no okit has never traced anything, and the column is then what it
     always was. *)
  let steps =
    match m.trace with
    | [] -> []
    | lines ->
        [
          box ~flex_direction:Flex_direction.Column ~size:full_w ~min_size:give
            (List.filteri (fun i _ -> i < trace_shown) lines
            |> List.map (fun l ->
                text ~style:faint ~min_size:give_x (short (tool_w - 4) l)));
        ]
  in
  (* The conversation gets a readable measure on the left, and the machinery a
     narrow ticker on the right, so tool calls no longer interrupt the prose
     they were made in aid of. This row shrinks too, or the columns push the
     vitals, the keys and the prompt off the bottom of a short screen. *)
  let columns =
    box ~flex_grow:1. ~flex_shrink:1. ~min_size:give ~gap:(gap_xy 1 0)
      [
        box ~flex_grow:1. ~min_size:give
          [
            column ~border_color:orange
              (greeting
              @ List.mapi view_entry m.entries
              @ live
              @ List.map view_queued m.queued);
          ];
        (if not show_tools then empty
         else
           box
             ~size:(size_wh (px tool_w) (pct 100))
             ~flex_shrink:0.
             [
               column
                 ~title:(Printf.sprintf "tools %d" (List.length m.activity))
                 ~head:steps ~border_color:Color.Bright_black
                 (List.rev_map (view_call m.unfolded) m.activity);
             ]);
      ]
  in
  (* Vitals, named rather than abbreviated. The context is the budget that ends
     a session, so it says both what is used and how far it can grow. The bar
     warms as it fills, so how close the session is to its limit is legible
     without reading the number. *)
  let vitals =
    let ctx_color =
      if full > 85. then Color.Red
      else if full > 60. then Color.Yellow
      else orange
    in
    box ~size:line ~gap:(gap_xy 1 0) ~padding:(padding_xy 1 0)
      ~align_items:Align.Center
      [
        from 60 (text ~style:faint "context");
        progress_bar ~value:full ~min:0. ~max:100. ~filled_color:ctx_color
          ~empty_color:Color.Bright_black
          ~size:(size_wh (px (if m.cols >= 80 then 16 else 8)) (px 1))
          ();
        text
          ~style:(Style.fg ctx_color Style.default)
          (Printf.sprintf "%.0f%% of %s" full (thousands size));
        from 66
          (text ~style:faint (Printf.sprintf "→ %s" (thousands cfg.max_ctx)));
        from 90 sep;
        from 90 (text ~style:hump_style (sparkline m.rates));
        from 78 (text ~style:dim (Printf.sprintf "%.0f tok/s" rate));
        from 100 sep;
        from 100 (text ~style:dim (plural turns "turn"));
        from 100 (text ~style:dim (plural tools "call"));
        spacer;
        (if not m.busy then empty
         else
           box ~gap:(gap_xy 1 0)
             [
               spinner ~color:orange ~frame_set:Spinner.dots ();
               (* A prefill in flight displaces the muttering, which says
                  nothing, with the one number that shows the wait ending. *)
               (match prefilling m.prefill with
               | Some note -> text ~style:(Style.fg orange Style.default) note
               | None ->
                   text ~style:hump_style
                     mutterings.(m.mutter mod Array.length mutterings));
             ]);
      ]
  in
  (* The keys, since none of them can be guessed. *)
  let keys =
    box ~size:line ~padding:(padding_xy 1 0) ~gap:(gap_xy 2 0)
      [
        key_hint "enter" "send";
        from 72 (key_hint "↑↓" "history");
        from 52
          (key_hint "tab"
             (if m.cols < 88 then "tools"
              else if m.unfolded then "hide tool results"
              else "show tool results"));
        key_hint "ctrl-c" "quit";
      ]
  in
  (* The field owns its buffer, so [revision] remounts it when the model sets
     its contents rather than the typist. *)
  let prompt =
    box ~size:full_w ~border:true ~border_color:Color.Bright_black
      ~padding:(padding_xy 1 0)
      [
        input ~key:(string_of_int m.revision) ~autofocus:true ~value:m.input
          ~flex_grow:1. ~size:line
          ~placeholder:
            (if m.busy then "ask again, and it waits its turn"
             else "ask the camel")
          ~cursor_color:orange ~placeholder_color:Color.Bright_black
          ~on_change:(fun s -> Some (Typed s))
          ~on_submit:(fun s -> Some (Submitted s))
          ();
      ]
  in
  box ~flex_direction:Flex_direction.Column ~size:fill
    [ header; columns; vitals; keys; prompt ]

(* The transcript is oldest first, which is the order it is read in. *)
let add es m = { m with entries = m.entries @ es }
let say e m = add [ e ] m

(* Fold the agent's events into the transcript. Content accumulates into a
   buffer so the reply appears as it is written, and becomes one entry when the
   turn ends. *)
let apply_event m (e : Agent.event) =
  match e with
  | Agent.Content c ->
      Buffer.add_string m.reply c;
      m
  | Agent.Reasoning _ -> m
  | Agent.Tool_call tc ->
      {
        m with
        activity =
          { name = tc.name; args = tc.arguments; result = None } :: m.activity;
      }
  | Agent.Tool_result (n, r) ->
      (* Results arrive in call order, so attach to the most recent call of
         that name that is still waiting. *)
      let rec attach = function
        | [] -> []
        | c :: rest when c.name = n && c.result = None ->
            { c with result = Some r } :: rest
        | c :: rest -> c :: attach rest
      in
      { m with activity = attach m.activity }
  | Agent.Expanded n ->
      say (Note (Printf.sprintf "context grew to %d" n)) { m with ctx_size = n }
  | Agent.Squeezed n ->
      say (Note (Printf.sprintf "context full, only %d tokens to reply in" n)) m
  | Agent.Compacted c ->
      (* Unlike [Expanded], the window's capacity is unchanged: only how much
         of it is used shrank, which the next [Stats] event already reports. *)
      say
        (Note
           (Printf.sprintf "conversation compacted from %d to %d tokens"
              c.Agent.before c.Agent.after))
        m
  (* Said whatever else is shown. A discarded call is work the model believes
     it did, and a reply that stops at the ceiling is not the whole answer. *)
  | Agent.Cut_off c ->
      say
        (Note
           (if c.Agent.tool_call then
              Printf.sprintf
                "the reply hit its %d token ceiling while writing a tool call, \
                 which was discarded"
                c.Agent.tokens
            else
              Printf.sprintf "the reply hit its %d token ceiling and stopped"
                c.Agent.tokens))
        m
  | Agent.Stats s ->
      let rate =
        if s.generate_seconds <= 0. then 0.
        else float_of_int s.generated /. s.generate_seconds
      in
      let reply = Buffer.contents m.reply in
      Buffer.clear m.reply;
      let m = if reply = "" then m else say (Replied reply) m in
      {
        m with
        stats = Some s;
        ctx_size = s.ctx_size;
        (* Only as many rates as the sparkline can draw. *)
        rates = List.filteri (fun i _ -> i < sparkline_len) (rate :: m.rates);
        mutter = m.mutter + 1;
      }
  | Agent.Done -> m

(* [send] blocks until the agent finishes its exchange, so it runs in its own
   fiber and reports progress by dispatching. Mosaic states that a perform
   callback may dispatch from anywhere, which is what makes this safe. The
   journal trace sees every event as well, so the exchange is on the record. *)
let start_exchange trace agent prompt =
  Mosaic.Cmd.perform (fun dispatch ->
      let on_event e =
        Trace.event trace e;
        dispatch (Agent_event e)
      in
      match Driver.send agent prompt ~on_event with
      | () -> dispatch Agent_done
      | exception Failure m -> dispatch (Agent_failed m)
      | exception e -> dispatch (Agent_failed (Printexc.to_string e)))

(* Lines queued by whatever cannot print, because the interface owns the
   terminal. Each is written where it is queued. Drained into the transcript at
   the first frame and whenever anything else happens. *)
let drain_notes notes m =
  if Queue.is_empty notes then m
  else begin
    let entries = Queue.fold (fun acc line -> Note line :: acc) [] notes in
    Queue.clear notes;
    add (List.rev entries) m
  end

(* The steps okit queued since the last message. Drained on every message, the
   tick included, which is what keeps the column moving while a tool call
   blocks and nothing else happens. *)
let drain_trace trace m =
  if Queue.is_empty trace then m
  else begin
    let lines = Queue.fold (fun acc line -> line :: acc) m.trace trace in
    Queue.clear trace;
    { m with trace = List.filteri (fun i _ -> i < trace_kept) lines }
  end

(* Walk the history, filling the field from it. Stepping back past the newest
   entry restores whatever was being typed when browsing began. *)
let browse m step =
  if m.history = [] then m
  else
    let last = List.length m.history - 1 in
    let pos =
      match m.browsing with
      | None -> if step > 0 then Some 0 else None
      | Some i -> if i + step < 0 then None else Some (min last (i + step))
    in
    (* What was being typed is kept the first time browsing moves off it. *)
    let draft = if m.browsing = None then m.input else m.draft in
    let input =
      match pos with None -> draft | Some i -> List.nth m.history i
    in
    { m with browsing = pos; draft; input; revision = m.revision + 1 }

(* An exchange has ended. Start whatever was typed while it ran, or fall
   idle. *)
let next_prompt trace agent m =
  match m.queued with
  | [] -> ({ m with busy = false }, Mosaic.Cmd.none)
  | next :: rest ->
      ( say (Asked next) { m with queued = rest },
        start_exchange trace agent next )

let update notes qtrace append trace agent msg m =
  let m = drain_trace qtrace (drain_notes notes m) in
  match msg with
  | Typed s -> ({ m with input = s; browsing = None }, Mosaic.Cmd.none)
  | Submitted s when String.trim s = "" -> (m, Mosaic.Cmd.none)
  | Submitted s ->
      (* Clearing the field means remounting it, and a prompt sent while the
         model is busy waits its turn rather than being refused. *)
      let m =
        {
          m with
          input = "";
          revision = m.revision + 1;
          browsing = None;
          draft = "";
          history = s :: m.history;
        }
      in
      append (Journal.Prompt s);
      if m.busy then ({ m with queued = m.queued @ [ s ] }, Mosaic.Cmd.none)
      else (say (Asked s) { m with busy = true }, start_exchange trace agent s)
  | Agent_event e -> (apply_event m e, Mosaic.Cmd.none)
  | Agent_done -> next_prompt trace agent m
  | Agent_failed why ->
      append (Journal.Error { Journal.where = "agent"; what = why });
      next_prompt trace agent (say (Failed why) m)
  | Resized cols -> ({ m with cols }, Mosaic.Cmd.none)
  | Toggle -> ({ m with unfolded = not m.unfolded }, Mosaic.Cmd.none)
  | Older -> (browse m 1, Mosaic.Cmd.none)
  | Newer -> (browse m (-1), Mosaic.Cmd.none)
  (* A tick asks Mosaic to redraw, which is what animates the spinner while a
     turn runs. It is also where the prefill counters are read, since the agent
     is blocked in another fiber and reports nothing until its turn is done. *)
  | Tick -> ({ m with prefill = Driver.prefill_progress agent }, Mosaic.Cmd.none)
  | Cancel ->
      Driver.cancel agent;
      (say (Note "stopping current exchange") m, Mosaic.Cmd.none)
  | Quit -> (m, Mosaic.Cmd.quit)

let subscriptions m =
  Mosaic.Sub.batch
    [
      (if m.busy then Mosaic.Sub.Every (0.2, fun () -> Tick)
       else Mosaic.Sub.None);
      (* Without this the view never learns the new size, and a narrow window
         keeps a layout built for a wide one. *)
      Mosaic.Sub.On_resize (fun ~width ~height:_ -> Resized width);
      (* The field holds focus and takes every printable key, so quitting and
         browsing have to be caught before it. Ctrl-D only quits on an empty
         field, which is the shell's rule and the one a hand already knows. *)
      Mosaic.Sub.On_key_all
        (fun k ->
          let ev = Mosaic.Event.Key.data k in
          (* Matrix maps a C0 byte to 0x40 plus its offset, so ctrl-D arrives
             as an upper case D. Compare on either case. *)
          let ctrl_char c =
            match ev.key with
            | Input.Key.Char u ->
                ev.modifier.ctrl
                && Char.lowercase_ascii (Char.chr (Uchar.to_int u land 0x7f))
                   = c
            | _ -> false
          in
          if ctrl_char 'c' then Some (if m.busy then Cancel else Quit)
          else if ctrl_char 'd' && m.input = "" then Some Quit
          else
            match ev.key with
            | Input.Key.Tab -> Some Toggle
            (* Escape is left alone. It quit here once, which lost a loaded
               model to a key people press to take something back. *)
            | Input.Key.Up -> Some Older
            | Input.Key.Down -> Some Newer
            | _ -> None);
    ]

let run ~env ~sw ~title ~about ~max_ctx ~ctx_size ~notes ~trace ~journal agent =
  let matrix =
    Matrix_eio.create ~sw ~clock:(Eio.Stdenv.clock env)
      ~stdin:(Eio.Stdenv.stdin env) ~stdout:(Eio.Stdenv.stdout env) ()
  in
  (* Mosaic reports a change of size but not the size it started at, so the
     first layout has to ask. Without this a window that is never resized is
     laid out for a guess. *)
  let cols0, _rows = Matrix.size matrix in
  let cfg = { title; about; max_ctx } in
  (* What was queued before the interface started belongs in the first frame,
     since nothing else may have happened for a long while. *)
  let fresh () =
    {
      entries = [];
      activity = [];
      trace = [];
      input = "";
      busy = false;
      stats = None;
      ctx_size;
      prefill = (0, 0);
      rates = [];
      unfolded = false;
      revision = 0;
      queued = [];
      history = [];
      browsing = None;
      draft = "";
      reply = Buffer.create 512;
      mutter = 0;
      cols = cols0;
    }
  in
  let init () =
    (drain_trace trace (drain_notes notes (fresh ())), Mosaic.Cmd.none)
  in
  (* A perform callback becomes a fiber rather than a thread, so the agent
     keeps the Eio capabilities its tools need. *)
  let process_perform thunk =
    Eio.Fiber.fork_daemon ~sw (fun () ->
        thunk ();
        `Stop_daemon)
  in
  (* The journal the conversation is recorded into. Every prompt, reasoning,
     reply, tool call and result is appended as it happens, so a session can
     be read back from the journal after it has ended. *)
  let append k = ignore (Journal.append journal k) in
  let jtrace =
    Trace.create
      ~now:(fun () -> Eio.Time.now (Eio.Stdenv.clock env))
      ~emit:append ()
  in
  Eio.Switch.on_release sw (fun () ->
      append (Journal.Run_stop "the session ended");
      Journal.close journal);
  Mosaic.run ~matrix ~process_perform
    {
      init;
      update = update notes trace append jtrace agent;
      view = view cfg;
      subscriptions;
    }
