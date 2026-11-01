(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A trading day on an invented exchange, in five seconds: a dozen invented
   tickers whose prices follow a seeded random walk, each with its session
   drawn as a sparkline, and the day's returns as a histogram. A price that
   just traded shows in the colour of its tick. At the close the table stays
   in the scrollback. [--theme] draws it in another theme. *)

open Console

type stock = {
  symbol : string;
  name : string;
  close : float; (* The previous close, from which the day's change counts. *)
  drift : float;
  volatility : float;
  lot : float;
  mutable price : float;
  mutable tick : float; (* The last trade's move, [0.] once it is old. *)
  mutable volume : float;
  mutable turnover : float;
  mutable session : float list; (* The prices sampled so far, latest first. *)
}

let stock symbol name close ~drift ~volatility ~lot =
  {
    symbol;
    name;
    close;
    drift;
    volatility;
    lot;
    price = close;
    tick = 0.;
    volume = 0.;
    turnover = 0.;
    session = [ close ];
  }

let stocks =
  [
    stock "LMDY" "Lambda Dynamics" 182.40 ~drift:0.0012 ~volatility:0.004
      ~lot:90e3;
    stock "MNDM" "Monad Mining" 47.15 ~drift:(-0.0006) ~volatility:0.006
      ~lot:240e3;
    stock "FNFR" "Functor Freight" 93.72 ~drift:0.0003 ~volatility:0.003
      ~lot:60e3;
    stock "CRYK" "Curry Kitchens" 28.90 ~drift:(-0.0015) ~volatility:0.005
      ~lot:310e3;
    stock "ZPLG" "Zipper Logistics" 64.05 ~drift:0.0005 ~volatility:0.004
      ~lot:120e3;
    stock "PTPH" "Pattern Pharma" 211.33 ~drift:0.0018 ~volatility:0.007
      ~lot:45e3;
    stock "PLMB" "Polymorph Bio" 12.84 ~drift:(-0.0010) ~volatility:0.009
      ~lot:800e3;
    stock "TCTL" "Tail Call Telecom" 38.61 ~drift:0. ~volatility:0.002
      ~lot:150e3;
    stock "HPRB" "Heap Robotics" 156.02 ~drift:0.0008 ~volatility:0.005
      ~lot:70e3;
    stock "CLCP" "Closure Capital" 74.48 ~drift:(-0.0003) ~volatility:0.003
      ~lot:95e3;
    stock "FXPE" "Fixpoint Energy" 51.27 ~drift:(-0.0008) ~volatility:0.004
      ~lot:180e3;
    stock "UNKA" "Unikernel Air" 23.66 ~drift:0.0022 ~volatility:0.006
      ~lot:420e3;
  ]

(* The day is [trades] rounds of 0.1s, from 09:30 to 16:00, and the
   sparklines take a sample every [every] rounds. *)
let trades = 50
let every = 2
let samples = trades / every
let round = ref 0
let closed = ref false
let rng = Random.State.make [| 2026 |]

(* A standard normal draw, by Box-Muller. *)
let gauss () =
  let u = 1. -. Random.State.float rng 1. and v = Random.State.float rng 1. in
  sqrt (-2. *. log u) *. cos (2. *. Float.pi *. v)

let trade s =
  if Random.State.float rng 1. < 0.45 then begin
    let next = s.price *. exp (s.drift +. (s.volatility *. gauss ())) in
    s.tick <- next -. s.price;
    s.price <- next;
    let shares = s.lot *. (0.5 +. Random.State.float rng 1.) in
    s.volume <- s.volume +. shares;
    s.turnover <- s.turnover +. (shares *. next)
  end
  else s.tick <- 0.

let next_round () =
  incr round;
  List.iter trade stocks;
  if !round mod every = 0 then
    List.iter (fun s -> s.session <- s.price :: s.session) stocks

(* The day's change in whole cents, so that a change too small to show is
   none, never [-0.00]. *)
let change s =
  let cents = Float.round ((s.price -. s.close) *. 100.) in
  if cents = 0. then 0. else cents /. 100.

let percent s = 100. *. change s /. s.close
let clock () = 570 + (!round * 390 / trades)

(* Up in the theme's colour of a success, down in that of a failure. *)
let signed x v = if x < 0. then View.role `Failed v else View.role `Succeeded v

let arrow x =
  View.fit (fun room ->
      match (Room.utf_8 room, x < 0.) with
      | true, true -> View.text "▼"
      | true, false -> View.text "▲"
      | false, true -> View.text "v"
      | false, false -> View.text "^")

let figure v =
  if v >= 1e9 then Fmt.str "%.2fB" (v /. 1e9)
  else if v >= 1e6 then Fmt.str "%.1fM" (v /. 1e6)
  else Fmt.str "%.0fk" (v /. 1e3)

(* The session so far, in the colour of the day's change. *)
let sparkline s =
  View.fit (fun room ->
      let theme = Room.theme room in
      let colour =
        if change s < 0. then Theme.failed theme else Theme.succeeded theme
      in
      Bar.sparkline ~style:(Style.fg colour) (List.rev s.session))

let row s =
  let price = View.styled Style.bold (View.text (Fmt.str "%.2f" s.price)) in
  [
    View.styled Style.bold (View.text s.symbol);
    View.text s.name;
    (if s.tick = 0. then price else signed s.tick price);
    signed (change s) (View.text (Fmt.str "%+.2f" (change s)));
    signed (change s)
      (View.hcat ~gap:1
         [ arrow (change s); View.text (Fmt.str "%+.2f%%" (percent s)) ]);
    sparkline s;
    View.text (figure s.volume);
  ]

(* Every column is as wide as it will be at the close, so that the table
   keeps still while the session draws itself. In a narrow terminal the
   session gives way first. *)
let table () =
  Table.view
    [
      Table.column ~shrinkable:false (View.text "Symbol");
      Table.column ~shrinkable:false ~max_width:17 ~overflow:`Truncate
        (View.text "Company");
      Table.column ~align:`Right ~width:7 (View.text "Last");
      Table.column ~align:`Right ~width:6 (View.text "Chg");
      Table.column ~align:`Right ~width:8 (View.text "Chg %");
      Table.column ~width:(samples + 1) (View.text "Session");
      Table.column ~align:`Right ~width:6 (View.text "Volume");
    ]
    (List.map row stocks)

(* The composite index, its stocks equally weighted, from its previous
   close. *)
let previous = 3141.59

let composite () =
  let mean =
    List.fold_left (fun a s -> a +. (s.price /. s.close)) 0. stocks
    /. float_of_int (List.length stocks)
  in
  previous *. mean

let header () =
  let index = composite () in
  let move = index -. previous in
  let status =
    let time = Fmt.str "%02d:%02d" (clock () / 60) (clock () mod 60) in
    if !closed then View.role `Muted (View.text ("closed " ^ time))
    else
      View.role `Accent
        (View.hcat ~gap:1 [ Spinner.view (); View.text ("live " ^ time) ])
  in
  View.hcat ~gap:3
    [
      View.styled Style.bold (View.gradient (View.text "LAMBDA EXCHANGE"));
      View.hcat ~gap:1
        [
          View.role `Muted (View.text "LBX Composite");
          View.styled Style.bold (View.text (Fmt.str "%.2f" index));
          signed move
            (View.hcat ~gap:1
               [
                 arrow move;
                 View.text (Fmt.str "%+.2f%%" (100. *. move /. previous));
               ]);
        ];
      status;
    ]

(* The day's returns in buckets of one percent, the outer ones holding
   everything beyond them, losses red and gains green. *)
let histogram () =
  let bucket s =
    Float.to_int (Float.round (Float.min 4. (Float.max (-4.) (percent s))))
  in
  let count b = List.length (List.filter (fun s -> bucket s = b) stocks) in
  let buckets = List.init 9 (fun i -> (Fmt.str "%+d%%" (i - 4), i - 4)) in
  let bars = List.map (fun (l, b) -> (l, float_of_int (count b))) buckets in
  Panel.view
    ~title:(View.text "Returns today")
    (View.fit (fun room ->
         let theme = Room.theme room in
         let style label _ =
           let b = List.assoc label buckets in
           Style.fg
             (if b < 0 then Theme.failed theme
              else if b > 0 then Theme.succeeded theme
              else Theme.accent theme)
         in
         Bar.chart ~height:3 ~style bars))

(* A panel of lines of a muted label and its figures, the figures against
   the right edge. *)
let facts title rows =
  let n = List.fold_left (fun n (_, c) -> max n (List.length c)) 0 rows in
  let line (label, cells) =
    let pad = List.init (n - List.length cells) (fun _ -> View.empty) in
    (View.role `Muted (View.text label) :: pad) @ cells
  in
  Panel.view ~title:(View.text title)
    (Table.view ~border:Border.none
       (Table.column View.empty
       :: List.init n (fun _ -> Table.column ~align:`Right View.empty))
       (List.map line rows))

let quote s =
  [
    View.styled Style.bold (View.text s.symbol);
    signed (percent s) (View.text (Fmt.str "%+.2f%%" (percent s)));
  ]

let breadth () =
  let up = List.length (List.filter (fun s -> change s >= 0.) stocks) in
  let sorted = List.sort (fun a b -> compare (percent b) (percent a)) stocks in
  let sum f = List.fold_left (fun a s -> a +. f s) 0. stocks in
  let count role n = [ View.role role (View.text (string_of_int n)) ] in
  facts "Breadth"
    [
      ("advancing", count `Succeeded up);
      ("declining", count `Failed (List.length stocks - up));
      ("best", quote (List.hd sorted));
      ("worst", quote (List.hd (List.rev sorted)));
      ("volume", [ View.text (figure (sum (fun s -> s.volume))) ]);
      ("turnover", [ View.text (figure (sum (fun s -> s.turnover))) ]);
    ]

let most_active () =
  let sorted = List.sort (fun a b -> compare b.volume a.volume) stocks in
  facts "Most active"
    (List.mapi
       (fun i s ->
         (string_of_int (i + 1), quote s @ [ View.text (figure s.volume) ]))
       (List.take 6 sorted))

(* The panels under the table, as many as fit from the left. *)
let rec panels room = function
  | [] -> View.empty
  | views -> (
      let v = View.hcat ~gap:2 views in
      match Room.width room with
      | Some w when View.width v > w ->
          panels room (List.take (List.length views - 1) views)
      | _ -> v)

let view =
  View.fit (fun room ->
      View.vcat ~gap:1
        [
          header ();
          table ();
          panels room [ histogram (); breadth (); most_active () ];
        ])

let main theme =
  Console_tty.setup ~theme ();
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~ppf:Fmt.stdout
    ~view:(fun _ -> view)
    (fun _ ->
      while !round < trades do
        Eio.Time.Mono.sleep clock 0.1;
        next_round ()
      done;
      List.iter (fun s -> s.tick <- 0.) stocks;
      closed := true)

let theme =
  Cmdliner.Arg.(value & opt (enum Theme.all) Theme.default & info [ "theme" ])

let cmd = Cmdliner.Cmd.(v (info "stocks") Cmdliner.Term.(const main $ theme))
let () = exit (Cmdliner.Cmd.eval cmd)
