(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [apt install curl] on Debian bookworm, with seven libraries it pulls in.
   What apt and dpkg print stays in the scrollback; the bottom line is the
   live region: the percentage of each step of reading the package lists, the
   downloads, then dpkg's [Progress:] bar. apt keeps that bar off the rest of
   the output with a scroll region; here the region is console's, and dpkg's
   lines are given to the root, so they go above it. Pipe it through [cat] to
   see what a log gets: the scrollback alone. *)

open Console

type pkg = {
  name : string;
  arch : string;
  multi_arch : bool;
  version : string;
  size : int;
  installed : int;
}

let pkg ?(arch = "amd64") ?(multi_arch = true) name version size installed =
  { name; arch; multi_arch; version; size; installed }

(* In the order apt fetches and dpkg unpacks them. *)
let pkgs =
  [
    pkg "libbrotli1" "1.0.9-2+b6" 275_416 784_000;
    pkg "libnghttp2-14" "1.52.0-1+deb12u2" 72_744 193_000;
    pkg "libpsl5" "0.21.2-1" 58_708 92_000;
    pkg "librtmp1" "2.4+20151223.gitfa8646d.1-2+b2" 60_800 145_000;
    pkg "libssh2-1" "1.10.0-3+b1" 179_496 471_000;
    pkg "libcurl4" "7.88.1-10+deb12u8" 390_884 822_000;
    pkg ~multi_arch:false "curl" "7.88.1-10+deb12u8" 315_324 497_000;
    pkg ~arch:"all" ~multi_arch:false "publicsuffix" "20230209.2326-1" 126_492
      351_000;
  ]

let qualified p = if p.multi_arch then p.name ^ ":amd64" else p.name
let sum f = List.fold_left (fun n p -> n + f p) 0 pkgs

(* apt's SizeToStr: four significant digits at most, in powers of 1000. *)
let size_to_str n =
  let digits x =
    let n = Float.to_int (Float.round x) in
    if n >= 1000 then Fmt.str "%d,%03d" (n / 1000) (n mod 1000)
    else Int.to_string n
  in
  let rec go x u = function
    | u' :: us when x >= 10000. -> go (x /. 1000.) u' us
    | _ when x < 100. && u <> "" -> Fmt.str "%.1f %s" x u
    | _ -> Fmt.str "%s %s" (digits x) u
  in
  go (Int.to_float n) "" [ "k"; "M"; "G" ] ^ "B"

let time_to_str s = Fmt.str "%ds" (Float.to_int s)

(* apt's lines and dpkg's are the root's output, which goes above the region. *)
let say root fmt = Fmt.kstr (Task.log root) fmt

(* The bottom line, which each step sets while it runs. *)
let line = ref View.empty

let with_line view f =
  line := view;
  Fun.protect ~finally:(fun () -> line := View.empty) f

let percent t =
  match Task.progress t with
  | `Items (n, total) when total > 0 ->
      100. *. Int.to_float n /. Int.to_float total
  | `Bytes (n, total) when total > 0L ->
      100. *. Int64.to_float n /. Int64.to_float total
  | _ -> 0.

(* [Reading package lists... 45%], then [... Done] in the scrollback. *)
let step clock root what =
  Task.run ~transient:true root what (fun t ->
      let view =
        View.fit (fun _ -> Fmt.kstr View.text "%s... %.0f%%" what (percent t))
      in
      with_line view (fun () ->
          for i = 0 to 4 do
            Task.set_items t ~count:i ~total:4;
            Eio.Time.Mono.sleep clock 0.1
          done));
  say root "%s... Done" what

let plan root =
  let names l = "  " ^ String.concat " " (List.sort String.compare l) in
  let all = List.map (fun p -> p.name) pkgs in
  say root "The following additional packages will be installed:";
  say root "%s" (names (List.filter (fun n -> n <> "curl") all));
  say root "The following NEW packages will be installed:";
  say root "%s" (names all);
  say root "0 upgraded, %d newly installed, 0 to remove and 0 not upgraded."
    (List.length pkgs);
  say root "Need to get %s of archives." (size_to_str (sum (fun p -> p.size)));
  say root "After this operation, %s of additional disk space will be used."
    (size_to_str (sum (fun p -> p.installed)));
  say root "Do you want to continue? [Y/n] y"

(* [45% [6 libcurl4 154 kB/391 kB 39%]], the rate and the time left on the
   right. *)
let acquire fetch =
  View.fit @@ fun room ->
  let item =
    match
      List.find_opt (fun t -> Task.state t = `Running) (Task.children fetch)
    with
    | Some t -> (
        match Task.progress t with
        | `Bytes (n, total) ->
            Fmt.str " [%s %s/%s %.0f%%]" (Task.label t)
              (size_to_str (Int64.to_int n))
              (size_to_str (Int64.to_int total))
              (percent t)
        | _ -> " [Working]")
    | None -> " [Working]"
  in
  let left = View.text (Fmt.str "%.0f%%%s" (percent fetch) item) in
  let right =
    match (Task.progress fetch, Task.rate fetch) with
    | `Bytes (n, total), Some cps ->
        let eta = Int64.to_float (Int64.sub total n) /. cps in
        Some
          (Fmt.str " %s/s %s"
             (size_to_str (Float.to_int cps))
             (time_to_str eta))
    | _ -> None
  in
  match (right, Room.width room) with
  | Some r, Some w when View.width left + String.length r < w ->
      View.hcat ~gap:0
        [
          left;
          View.place ~align:`Right ~width:(w - View.width left) (View.text r);
        ]
  | _ -> left

let rate = 600_000

(* One download, [Get:] as it starts: [fetch] counts the bytes of all of them,
   [before] of which came before this one. *)
let download clock root fetch ~before id p =
  Task.run ~transient:true fetch (Fmt.str "%d %s" id p.name) @@ fun t ->
  let size = Int64.of_int p.size in
  let total = Int64.of_int (sum (fun p -> p.size)) in
  Task.set_bytes t ~count:0L ~total:size;
  say root
    "Get:%d http://deb.debian.org/debian bookworm/main amd64 %s %s %s [%s]" id
    p.name p.arch p.version (size_to_str p.size);
  let rec go n =
    if n < p.size then begin
      let chunk = min (rate / 10) (p.size - n) in
      Eio.Time.Mono.sleep clock (Int.to_float chunk /. Int.to_float rate);
      let n = n + chunk in
      Task.set_bytes t ~count:(Int64.of_int n) ~total:size;
      Task.set_bytes fetch ~count:(Int64.of_int (before + n)) ~total;
      go n
    end
  in
  go 0

let fetch clock root =
  let total = sum (fun p -> p.size) in
  let elapsed =
    Task.run ~transient:true root "Fetching" @@ fun fetch ->
    with_line (acquire fetch) (fun () ->
        ignore
          (List.fold_left
             (fun (id, before) p ->
               download clock root fetch ~before id p;
               (id + 1, before + p.size))
             (1, 0) pkgs));
    Task.elapsed fetch
  in
  say root "Fetched %s in %s (%s/s)" (size_to_str total) (time_to_str elapsed)
    (size_to_str (Float.to_int (Int.to_float total /. elapsed)))

(* [Progress: [ 45%]] in black on green, then a bar of [#] on [.] as wide as
   the row leaves it. *)
let dpkg_progress t =
  View.fit @@ fun room ->
  let pct = percent t in
  let label = Fmt.str "Progress: [%3.0f%%]" pct in
  let label =
    View.styled Style.(fg Color.black + bg Color.green) (View.text label)
  in
  let width =
    Option.value ~default:80 (Room.width room) - 4 - View.width label
  in
  if width < 3 then label
  else
    let size = width - 2 in
    let fill = Float.to_int (Float.of_int size *. pct /. 100.) in
    let bar =
      "[" ^ String.make fill '#' ^ String.make (size - fill) '.' ^ "]"
    in
    View.hcat ~gap:1 [ label; View.text bar ]

let install clock root =
  Task.run ~transient:true root "Installing" @@ fun t ->
  let total = (2 * List.length pkgs) + 1 in
  let n = ref 0 in
  let tick d =
    incr n;
    Task.set_items t ~count:!n ~total;
    Eio.Time.Mono.sleep clock d
  in
  Task.set_items t ~count:0 ~total;
  with_line (dpkg_progress t) @@ fun () ->
  List.iteri
    (fun i p ->
      say root "Selecting previously unselected package %s." (qualified p);
      if i = 0 then
        say root
          "(Reading database ... 6680 files and directories currently \
           installed.)";
      say root "Preparing to unpack .../%d-%s_%s_%s.deb ..." i p.name p.version
        p.arch;
      say root "Unpacking %s (%s) ..." (qualified p) p.version;
      tick 0.15)
    pkgs;
  List.iter
    (fun p ->
      say root "Setting up %s (%s) ..." (qualified p) p.version;
      tick 0.1)
    pkgs;
  say root "Processing triggers for libc-bin (2.36-9+deb12u9) ...";
  tick 0.1

let () =
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~ppf:Fmt.stdout
    ~view:(fun _ -> View.fit (fun _ -> !line))
    (fun root ->
      step clock root "Reading package lists";
      step clock root "Building dependency tree";
      step clock root "Reading state information";
      plan root;
      fetch clock root;
      install clock root)
