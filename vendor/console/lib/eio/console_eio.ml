(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Prompt = Prompt

(* The descriptor a formatter writes to, when it is one of the two the
   standard library opens on them. *)
let descriptor ppf =
  if ppf == Fmt.stdout then Some (Eio_unix.Fd.stdout, Unix.stdout)
  else if ppf == Fmt.stderr then Some (Eio_unix.Fd.stderr, Unix.stderr)
  else None

let terminal ppf =
  match descriptor ppf with
  | Some (fd, unix) when Console_tty.is_terminal unix -> Some (fd, unix)
  | _ -> None

(* Two descriptors on one terminal are on one device. *)
let device unix =
  match Unix.fstat unix with
  | st -> Some st.Unix.st_rdev
  | exception Unix.Unix_error _ -> None

(* The standard formatters on the display's terminal, whose lines go above
   its region; one on another terminal goes out as it is written. *)
let routed unix =
  let on_display std =
    match terminal std with
    | Some (_, u) -> Option.equal Int.equal (device u) (device unix)
    | None -> false
  in
  List.filter on_display [ Fmt.stdout; Fmt.stderr ]

let file unix =
  match Unix.fstat unix with
  | st -> Some (st.Unix.st_dev, st.Unix.st_ino)
  | exception Unix.Unix_error _ -> None

(* The standard formatters that write where [unix] does. *)
let sharing unix =
  let shares std =
    match (descriptor std, file unix) with
    | Some (_, u), Some f -> file u = Some f
    | _ -> false
  in
  List.filter shares [ Fmt.stdout; Fmt.stderr ]

(* The display writes on a formatter of its own, to the same output as
   [ppf] and with the same tag functions, which hold its colours, its theme
   and whether it is UTF-8, as [Fmt.with_buffer ~like] does, so that [ppf]
   itself can be routed above the region. *)
let own ppf =
  let own =
    Format.formatter_of_out_functions
      (Format.pp_get_formatter_out_functions ppf ())
  in
  Format.pp_set_formatter_stag_functions own
    (Format.pp_get_formatter_stag_functions ppf ());
  own

let cursor unix visible =
  let s = if visible then "\027[?25h" else "\027[?25l" in
  try ignore (Unix.write_substring unix s 0 (String.length s))
  with Unix.Unix_error _ -> ()

(* The seconds since the display started, on [clock]. *)
let seconds clock =
  let start = Eio.Time.Mono.now clock in
  fun () ->
    Mtime.Span.to_float_ns (Mtime.span start (Eio.Time.Mono.now clock)) *. 1e-9

let deterministic = Cast.deterministic

(* A virtual clock. A sleep of [d] started at [t] wakes at [t + d], once it
   has also lasted [d] on [real]. Sleeps wake one at a time, in the order of
   their wake-up times then of their start, and each sets the time to its
   own wake-up time as its fiber resumes. The next one wakes only once the
   fibers the last one made runnable have run, until the clock sees no use
   for two yields in a row, so the time never depends on the order in which
   the real durations ended. A passive sleep, a redraw's, wakes in that order
   too, but only before a sleep of the program, so that it never moves the
   time further than the program would, and as soon as a sleep of the program
   after it has lasted its real duration, so that it never holds one up.
   For a recording, which has its own times, no sleep waits on [real]. *)
module Sleeps = struct
  type sleep = {
    until : Mtime.t;
    seq : int;
    active : bool;
    mutable elapsed : bool;
    mutable pending : bool;
    mutable waiting : unit Eio.Promise.u option;
    overtaken : unit Eio.Promise.t * unit Eio.Promise.u;
  }

  type t = {
    real : Eio.Time.Mono.ty Eio.Resource.t;
    waits : bool;
    mutable now : Mtime.t;
    mutable sleeps : sleep list;
    mutable next : int;
    mutable used : int;
  }

  type time = Mtime.t

  let now t =
    t.used <- t.used + 1;
    t.now

  let before a b =
    match Mtime.compare a.until b.until with 0 -> a.seq < b.seq | c -> c < 0

  let rec insert s = function
    | x :: rest when before x s -> x :: insert s rest
    | l -> s :: l

  let due t =
    match t.sleeps with
    | s :: rest
      when s.elapsed && (s.active || List.exists (fun x -> x.active) rest) ->
        Some s
    | _ -> None

  let is_due t s = match due t with Some x -> x == s | None -> false

  let wake t =
    match due t with
    | Some ({ waiting = Some u; _ } as s) ->
        s.waiting <- None;
        Eio.Promise.resolve u ()
    | _ -> ()

  let remove t s =
    if s.pending then begin
      s.pending <- false;
      t.sleeps <- List.filter (fun x -> x != s) t.sleeps;
      wake t
    end

  (* The rounds are bounded, so that a fiber that polls the clock in a loop
     of yields cannot stop it. *)
  let settle t =
    let rec go quiet rounds =
      if quiet < 2 && rounds < 64 then begin
        let used = t.used in
        Eio.Fiber.yield ();
        go (if t.used = used then quiet + 1 else 0) (rounds + 1)
      end
    in
    go 0 0

  let rec turn t s =
    if is_due t s then begin
      settle t;
      if is_due t s then begin
        t.now <- s.until;
        t.used <- t.used + 1;
        remove t s
      end
      else turn t s
    end
    else begin
      let p, u = Eio.Promise.create () in
      s.waiting <- Some u;
      Eio.Promise.await p;
      turn t s
    end

  let overtake t s =
    let rec go = function
      | x :: rest when x != s ->
          if not x.active then
            ignore (Eio.Promise.try_resolve (snd x.overtaken) ());
          go rest
      | _ -> ()
    in
    go t.sleeps

  let overtaken t s =
    List.exists (fun x -> x.active && x.elapsed && before s x) t.sleeps

  let elapse t s span =
    if not t.waits then ()
    else if s.active then begin
      Eio.Time.Mono.sleep_span t.real span;
      overtake t s
    end
    else if not (overtaken t s) then
      Eio.Fiber.first
        (fun () -> Eio.Time.Mono.sleep_span t.real span)
        (fun () -> Eio.Promise.await (fst s.overtaken))

  let sleep t ~active time =
    if Mtime.is_later time ~than:t.now then begin
      let s =
        {
          until = time;
          seq = t.next;
          active;
          elapsed = false;
          pending = true;
          waiting = None;
          overtaken = Eio.Promise.create ();
        }
      in
      t.next <- t.next + 1;
      t.used <- t.used + 1;
      t.sleeps <- insert s t.sleeps;
      wake t;
      Fun.protect
        ~finally:(fun () -> remove t s)
        (fun () ->
          elapse t s (Mtime.span t.now time);
          s.elapsed <- true;
          turn t s)
    end

  let sleep_until t time = sleep t ~active:true time

  let redraw t d =
    let span = Mtime.Span.of_uint64_ns (Int64.of_float (d *. 1e9)) in
    Option.iter (sleep t ~active:false) (Mtime.add_span t.now span)

  (* The time, read without counting as a use. *)
  let seconds t () = Int64.to_float (Mtime.to_uint64_ns t.now) *. 1e-9
end

type (_, _, _) Eio.Resource.pi +=
  | Redraw : ('t, 't -> float -> unit, [> `Console_sleeps ]) Eio.Resource.pi
  | Seconds : ('t, 't -> unit -> float, [> `Console_sleeps ]) Eio.Resource.pi

let sleeps =
  Eio.Resource.handler
    (H (Redraw, Sleeps.redraw)
    :: H (Seconds, Sleeps.seconds)
    :: Eio.Resource.bindings (Eio.Time.Pi.clock (module Sleeps)))

let clock c =
  let real = (c :> Eio.Time.Mono.ty Eio.Resource.t) in
  if deterministic () then
    Eio.Resource.T
      ( {
          Sleeps.real;
          waits = Option.is_none (Cast.file ());
          now = Mtime.of_uint64_ns 0L;
          sleeps = [];
          next = 0;
          used = 0;
        },
        sleeps )
  else real

(* The redraws of a clock of {!clock}, which wait on its time. *)
let redraw (Eio.Resource.T (t, ops)) =
  Option.map (fun redraw -> redraw t) (Eio.Resource.get_opt ops Redraw)

(* The time of a clock of {!clock}, or zero. *)
let virtual_time clock =
  let (Eio.Resource.T (t, ops)) = (clock :> Eio.Time.Mono.ty Eio.Resource.t) in
  match Eio.Resource.get_opt ops Seconds with
  | Some seconds -> seconds t
  | None -> fun () -> 0.

(* What the display reads and what its redraws wait for. Under
   CONSOLE_DETERMINISTIC_CLOCK the display reads a clock of {!clock} and
   redraws at its instants, without moving its time, or reads a clock
   stopped at zero. *)
let reading clock =
  let clock = (clock :> Eio.Time.Mono.ty Eio.Resource.t) in
  if not (deterministic ()) then (seconds clock, Eio.Time.Mono.sleep clock)
  else
    match redraw clock with
    | Some redraw -> (seconds clock, redraw)
    | None -> ((fun () -> 0.), Eio.Time.Mono.sleep clock)

(* The display of [root], the formatters whose lines it takes and what
   shows or hides the cursor. On a terminal it takes the standard formatters
   on that terminal; quiet or as events, those that write where [ppf] does,
   so that a task's output is printed only if it fails, or is an event;
   drawn on another output, [ppf] alone; [recorded], the terminal of the
   recording, which the standard formatters share if [ppf] is one. *)
let display ~recorded ~progress ?layout ?view ?keep_ended ?commit ?plain ?delay
    ppf root =
  let show ?size progress ppf =
    let view = Option.map (fun f -> f root) view in
    Progress_driver.show ~write_error:Session.write_error ~progress ?layout
      ?view ?keep_ended ?commit ?plain ?delay ?size ppf root
  in
  let live ~size ~intercept ~cursor =
    (show ~size `Tty (own ppf), intercept, cursor)
  in
  let on_terminal () =
    Option.map
      (fun (fd, unix) ->
        live
          ~size:(fun () -> Terminal.dimensions (Some fd))
          ~intercept:(routed unix) ~cursor:(cursor unix))
      (terminal ppf)
  in
  (* Plain lines end with the last frame as wide as the terminal, or as
     the recording's, and whole on a pipe. *)
  let lines form =
    match (form, descriptor ppf) with
    | (`Json | `Quiet), Some (_, unix) ->
        (show (form :> Console.Progress.t) (own ppf), sharing unix, ignore)
    | `Plain, _ when recorded ->
        (show ~size:Recording.size `Plain ppf, [], ignore)
    | `Plain, _ -> (
        match terminal ppf with
        | Some (fd, _) ->
            let size () = Terminal.dimensions (Some fd) in
            (show ~size `Plain ppf, [], ignore)
        | None -> (show `Plain ppf, [], ignore))
    | _ -> (show (form :> Console.Progress.t) ppf, [], ignore)
  in
  let std = Option.is_some (descriptor ppf) in
  match progress with
  | (`Auto | `Tty) when recorded ->
      live ~size:Recording.size
        ~intercept:(if std then [ Fmt.stdout; Fmt.stderr ] else [])
        ~cursor:ignore
  | `Auto -> ( match on_terminal () with Some d -> d | None -> lines `Plain)
  | `Tty -> (
      match on_terminal () with
      | Some d -> d
      | None ->
          let cursor =
            match descriptor ppf with
            | Some (_, unix) -> cursor unix
            | None -> ignore
          in
          let intercept =
            match descriptor ppf with Some _ -> [ ppf ] | None -> []
          in
          live ~size:(fun () -> Terminal.dimensions None) ~intercept ~cursor)
  | (`Plain | `Quiet | `Json) as form -> lines form

(* A display of its own, and the root of the work, which [f] is given. *)
let open_display ~clock ~ppf ?layout ?label ?view ?keep_ended ?commit ?plain
    ?delay ~progress f =
  Console_tty.ensure ();
  Session.await_release ();
  Session.claim ();
  let now, sleep = reading clock in
  match
    let recorded = Recording.start ~time:(virtual_time clock) ppf in
    let root = Task_driver.root ?label ~scope:Session.scope ~now () in
    let display, intercept, cursor =
      display ~recorded ~progress ?layout ?view ?keep_ended ?commit ?plain
        ?delay ppf root
    in
    ({ Session.display; root; json = progress = `Json }, intercept, cursor)
  with
  | run, intercept, cursor -> Session.run ~cursor ~sleep ~intercept run f
  | exception e ->
      Session.release ();
      raise e

let run ~clock ?(ppf = Fmt.stderr) ?progress ?layout ?label ?view ?keep_ended
    ?commit ?plain ?delay f =
  let progress = Option.value progress ~default:`Auto in
  match Session.find () with
  | Some root -> f (Session.given root)
  | None -> (
      match Session.join f with
      | Some v -> v
      | None ->
          open_display ~clock ~ppf ?layout ?label ?view ?keep_ended ?commit
            ?plain ?delay ~progress f)

let with_output f =
  match Session.display () with
  | Some d -> Console.Display.with_output d f
  | None -> f Fmt.stderr

let size ppf =
  Option.map (fun (fd, _) () -> Terminal.dimensions (Some fd)) (terminal ppf)

let print v =
  match Session.display () with
  | Some d -> Console.Display.print d v
  | None -> Fmt.epr "%a@." Console.View.pp v
