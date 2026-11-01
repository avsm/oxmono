(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The game a browser offers when the network is down, with OCaml's camel
   jumping over lambdas: Space or the up arrow jumps, q quits. The game
   moves in ticks of 100 ms, and the obstacles come from a seed, the first
   argument: the same keys at the same ticks give the same game. It draws
   in a live region on standard error, which keeps the last frame in the
   scrollback; without a terminal it prints the opening scene and exits.
   Given ticks after the seed, the game plays itself, pressing Space at
   each of them, in a display of [Console_eio.run]: under CONSOLE_CAST,
   that is its recording, on the deterministic clock. *)

open Console

let tick = Mtime.Span.(100 * ms)
let half_tick = Mtime.Span.(50 * ms)
let speed = 3
let field = 80
let sky = 12
let ground = sky
let camel_x = 3

(* Pixels, two to a cell: '#' is drawn, '.' is not. *)
let body =
  [|
    "..............##..";
    "......##.....#####";
    ".....####....###..";
    "....######...##...";
    "...########.##....";
    "..###########.....";
    ".############.....";
    "#.##########......";
  |]

let stride =
  [|
    [|
      "..##......##......";
      "..#.#.....#.#.....";
      "..#.#.....#.#.....";
      "..#.#.....#.#.....";
    |];
    [|
      "..##......##......";
      "..#.#....#..#.....";
      ".#...#..#....#....";
      ".#...#..#....#....";
    |];
  |]

let cloud =
  [|
    ".....##.....";
    "...##..#....";
    ".##.....##..";
    "#.........#.";
    "###########.";
  |]

(* The camel's lift above the ground, in pixels, at each tick of a jump. *)
let jump = [| 5; 9; 11; 12; 11; 9; 5 |]

type obstacle = { at : int; wide : int; tall : int }

(* A jump asked between two ticks starts at the second. *)
type feet = Ground | Asked | Air of int

type run = {
  ticks : int;
  feet : feet;
  ahead : obstacle list;  (** In the world, nearest first. *)
  spawned : int;
}

type phase = Ready | Running of run | Over of run
type game = { seed : int; best : int; phase : phase }

(* The [n]th draw of a [stream] of the game, in \[0;bound\[. *)
let roll seed stream n bound =
  Random.State.int (Random.State.make [| seed; stream; n |]) bound

let obstacle seed n ~after =
  let wide, tall =
    match roll seed 0 n 7 with
    | 0 | 1 -> (1, 1)
    | 2 -> (2, 1)
    | 3 -> (3, 1)
    | 4 | 5 -> (1, 2)
    | _ -> (2, 2)
  in
  { at = after + 42 + roll seed 1 n 36; wide; tall }

(* Enough obstacles to fill [width] columns past [distance]. *)
let rec fill seed run ~distance ~width =
  let last =
    match List.rev run.ahead with o :: _ -> o.at | [] -> distance + (width / 2)
  in
  if last > distance + width then run
  else
    let o = obstacle seed run.spawned ~after:last in
    fill seed
      { run with ahead = run.ahead @ [ o ]; spawned = run.spawned + 1 }
      ~distance ~width

let distance run = run.ticks * speed
let lift run = match run.feet with Air i -> jump.(i) | Ground | Asked -> 0

let sprite run =
  match run.feet with
  | Air _ -> Array.append body stride.(0)
  | Ground | Asked -> Array.append body stride.(run.ticks mod 2)

let pixels sprite f =
  Array.iteri
    (fun y row -> String.iteri (fun x c -> if c = '#' then f x y) row)
    sprite

(* The camel's pixels are on the sky's grid, two to a row of cells. *)
let crashed run =
  let top = (2 * sky) - Array.length (sprite run) - lift run
  and d = distance run in
  let under x y o =
    x >= o.at - d && x < o.at - d + o.wide && y >= (2 * sky) - (2 * o.tall)
  in
  let hit = ref false in
  pixels (sprite run) (fun x y ->
      if List.exists (under (camel_x + x) (top + y)) run.ahead then hit := true);
  !hit

let start seed =
  fill seed
    { ticks = 0; feet = Ground; ahead = []; spawned = 0 }
    ~distance:0 ~width:field

let step seed run =
  let feet =
    match run.feet with
    | Asked -> Air 0
    | Air i when i + 1 < Array.length jump -> Air (i + 1)
    | Air _ | Ground -> Ground
  in
  let run = { run with ticks = run.ticks + 1; feet } in
  let d = distance run in
  let ahead = List.filter (fun o -> o.at + o.wide > d) run.ahead in
  fill seed { run with ahead } ~distance:d ~width:field

let update game =
  match game.phase with
  | Running run ->
      let run = step game.seed run in
      if crashed run then
        { game with best = max game.best run.ticks; phase = Over run }
      else { game with phase = Running run }
  | Ready | Over _ -> game

let press game =
  match game.phase with
  | Ready | Over _ -> { game with phase = Running (start game.seed) }
  | Running ({ feet = Ground; _ } as run) ->
      { game with phase = Running { run with feet = Asked } }
  | Running _ -> game

let moving game = match game.phase with Running _ -> true | _ -> false

(* {1 Drawing} *)

type ink = Plain | Camel | Crash | Lambda | Muted | Bold

let ink = function
  | Plain -> Fun.id
  | Camel -> View.styled (Style.fg (Color.rgb 0xee 0x6a 0x1a))
  | Crash -> View.role `Failed
  | Lambda -> fun v -> View.styled Style.bold (View.role `Accent v)
  | Muted -> View.role `Muted
  | Bold -> View.styled Style.bold

type canvas = (string * ink) array array

let canvas ~width ~height : canvas =
  Array.init height (fun _ -> Array.make width (" ", Plain))

let set (c : canvas) ~x ~y cell =
  if y >= 0 && y < Array.length c && x >= 0 && x < Array.length c.(y) then
    c.(y).(x) <- cell

let write c ink ~x ~y s =
  let rec go i x =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      let n = Uchar.utf_decode_length d in
      set c ~x ~y (String.sub s i n, ink);
      go (i + n) (x + 1))
  in
  go 0 x

let centre c ink ~y s =
  let width = Array.length c.(0) in
  write c ink ~x:((width - View.width (View.text s)) / 2) ~y s

(* A sprite at pixel row [py], in half blocks. *)
let paint c ink ~x ~py sprite =
  let on y x =
    y >= 0
    && y < Array.length sprite
    && x < String.length sprite.(y)
    && sprite.(y).[x] = '#'
  in
  let first = Int.div (py - 1) 2 and last = (py + Array.length sprite) / 2 in
  for row = first to last do
    for dx = 0 to String.length sprite.(0) - 1 do
      let top = on ((2 * row) - py) dx
      and bottom = on ((2 * row) + 1 - py) dx in
      let glyph =
        match (top, bottom) with
        | true, true -> Some "█"
        | true, false -> Some "▀"
        | false, true -> Some "▄"
        | false, false -> None
      in
      Option.iter (fun g -> set c ~x:(x + dx) ~y:row (g, ink)) glyph
    done
  done

let score n = Fmt.str "%05d" n

(* The score blinks for a moment at each hundred. *)
let points run =
  let n = run.ticks in
  if n >= 100 && n mod 100 < 8 then
    if n mod 2 = 0 then score (n / 100 * 100) else "     "
  else score n

let draw_score c ~best points =
  let x = Array.length c.(0) - 6 in
  write c Bold ~x ~y:0 points;
  if best > 0 then write c Muted ~x:(x - 10) ~y:0 ("HI " ^ score best)

let pebbles = [| "·"; "."; "˙"; "-" |]

let draw_ground c seed d =
  for x = 0 to Array.length c.(0) - 1 do
    let w = d + x in
    set c ~x ~y:ground ((if roll seed 2 w 9 = 0 then "▔" else "─"), Plain);
    if roll seed 3 w 6 = 0 then
      set c ~x ~y:(ground + 1) (pebbles.(roll seed 4 w 4), Muted)
  done

(* The clouds drift at a sixth of the ground's speed. *)
let draw_clouds c seed ticks =
  let d = ticks / 2 in
  for n = d / 30 to ((d + Array.length c.(0)) / 30) + 1 do
    let x = (n * 30) + roll seed 5 n 20 - d
    and py = 2 * (1 + roll seed 6 n 2) in
    paint c Muted ~x ~py cloud
  done

let draw_obstacles c run =
  let d = distance run in
  List.iter
    (fun o ->
      for dy = 1 to o.tall do
        for dx = 0 to o.wide - 1 do
          set c ~x:(o.at - d + dx) ~y:(ground - dy) ("λ", Lambda)
        done
      done)
    run.ahead

let draw_camel c ink run =
  let sprite = sprite run in
  paint c ink ~x:camel_x ~py:((2 * sky) - Array.length sprite - lift run) sprite

(* A line of cells, in runs of one ink. *)
let line cells =
  Array.fold_left
    (fun runs (glyph, k) ->
      match runs with
      | (k', s) :: rest when k' = k -> (k, s ^ glyph) :: rest
      | _ -> (k, glyph) :: runs)
    [] cells
  |> ( function
  | (Plain, s) :: runs when String.trim s = "" -> runs
  | runs -> runs )
  |> List.rev_map (fun (k, s) -> ink k (View.text s))
  |> View.concat

let scene ~width game =
  let c = canvas ~width ~height:(ground + 2) in
  let seed = game.seed and best = game.best in
  (match game.phase with
  | Ready ->
      draw_clouds c seed 0;
      draw_ground c seed 0;
      draw_score c ~best (score 0);
      centre c Bold ~y:5 "Press Space to start";
      centre c Muted ~y:7 "Space or Up jumps, q quits";
      draw_camel c Camel (start seed)
  | Running run ->
      draw_clouds c seed run.ticks;
      draw_ground c seed (distance run);
      draw_score c ~best (points run);
      draw_obstacles c run;
      draw_camel c Camel run
  | Over run ->
      draw_clouds c seed run.ticks;
      draw_ground c seed (distance run);
      draw_score c ~best (score run.ticks);
      centre c Bold ~y:5 "G A M E   O V E R";
      centre c Muted ~y:7 "Space to play again, q to quit";
      draw_obstacles c run;
      draw_camel c Crash run);
  View.concat ~sep:(View.text "\n") (Array.to_list (Array.map line c))

let view game =
  View.fit (fun room ->
      let width = min field (Option.value (Room.width room) ~default:field) in
      scene ~width game)

(* {1 Playing} *)

type key = Jump | Quit

(* A key, or [None] for one the game ignores. Escape alone, as nothing
   follows it in the same read, quits. *)
let key input =
  let open Eio.Buf_read in
  match any_char input with
  | ' ' -> Some Jump
  | 'q' | 'Q' | '\004' -> Some Quit
  | '\027' when buffered_bytes input = 0 -> Some Quit
  | '\027' -> (
      match peek_char input with
      | Some ('[' | 'O') ->
          skip 1 input;
          if any_char input = 'A' then Some Jump else None
      | _ -> None)
  | _ -> None

(* The keys typed, then q at the end of the input. *)
let typed input push =
  let rec read () =
    match key input with
    | k ->
        Option.iter push k;
        read ()
    | exception End_of_file -> push Quit
  in
  read ()

(* Space at each of [ticks], counted from the start, then q five seconds
   after the last. *)
let replay ~clock ticks push =
  let start = Eio.Time.Mono.now clock in
  List.iter
    (fun n ->
      Eio.Time.Mono.sleep_until clock
        (Option.get (Mtime.add_span start Mtime.Span.(n * tick)));
      push Jump)
    ticks;
  Eio.Time.Mono.sleep clock 5.;
  push Quit

(* The game drawn by [show], with the keys that [keys push] gives [push].
   A key restarts the clock of the ticks, the next one half a tick after it.
   A recorder that presses keys a whole number of ticks apart thus lands each
   between the same two ticks, however late each key arrives. *)
let play ~clock ~show seed keys =
  let pending = Queue.create () and arrived = Eio.Condition.create () in
  let push k =
    Queue.push (Eio.Time.Mono.now clock, k) pending;
    Eio.Condition.broadcast arrived
  in
  let after t span = Option.get (Mtime.add_span t span) in
  let before t t' = Mtime.is_earlier t ~than:t' in
  let show game =
    show game;
    game
  in
  let ticking game t = if moving game then Some t else None in
  (* [next] is the time of the next tick, while the camel runs. *)
  let rec loop game next =
    let now = Eio.Time.Mono.now clock in
    match (next, Queue.peek_opt pending) with
    | Some t, None when not (before now t) -> advance game t
    | Some t, Some (at, _) when before t at -> advance game t
    | _, Some (at, key) -> (
        ignore (Queue.pop pending);
        match key with
        | Quit -> ()
        | Jump ->
            let game = show (press game) in
            loop game (ticking game (after at half_tick)))
    | _, None ->
        Eio.Fiber.first
          (fun () -> Eio.Condition.await_no_mutex arrived)
          (fun () ->
            match next with
            | Some t -> Eio.Time.Mono.sleep_until clock t
            | None -> Eio.Fiber.await_cancel ());
        loop game next
  and advance game t =
    let game = show (update game) in
    loop game (ticking game (after t tick))
  in
  Eio.Fiber.first
    (fun () ->
      keys push;
      Eio.Fiber.await_cancel ())
    (fun () -> loop { seed; best = 0; phase = Ready } None)

let () =
  let seed, ticks =
    match List.map int_of_string_opt (List.tl (Array.to_list Sys.argv)) with
    | [] -> (2026, [])
    | Some seed :: ticks when List.for_all Option.is_some ticks ->
        (seed, List.map Option.get ticks)
    | _ ->
        Fmt.epr "usage: %s [SEED [TICK...]]@." Sys.argv.(0);
        exit 2
  in
  Console_tty.setup ();
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let ready = { seed; best = 0; phase = Ready } in
  match ticks with
  | _ :: _ ->
      let clock = Console_eio.clock clock and game = ref ready in
      Console_eio.run ~clock
        ~view:(fun _ -> View.fit (fun _ -> view !game))
        (fun _ -> play ~clock ~show:(( := ) game) seed (replay ~clock ticks))
  | [] ->
      let prompt = Console_eio.Prompt.v (Eio.Stdenv.stdin env) in
      let played =
        match Console_eio.size Fmt.stderr with
        | None -> Error (`Msg "no terminal")
        | Some size ->
            Console_eio.Prompt.keys prompt @@ fun input ->
            (* The scene moves by ticks, not by the display's time. *)
            let d =
              Display.v ~size ~now:(fun () -> 0.) Fmt.stderr (view ready)
            in
            Fun.protect ~finally:(fun () -> Display.close d) @@ fun () ->
            play ~clock
              ~show:(fun game -> Display.set_view d (view game))
              seed (typed input)
      in
      if Result.is_error played then (
        Fmt.epr "%a@.camel: play it in a terminal@." View.pp
          (scene ~width:field ready);
        exit 1)
