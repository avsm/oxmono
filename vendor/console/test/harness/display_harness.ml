(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
module Driver = Task_driver

let contains affix s = Astring.String.is_infix ~affix s

let glyphs s =
  let rec go i acc =
    if i >= String.length s then Array.of_list (List.rev acc)
    else
      let d = String.get_utf_8_uchar s i in
      go (i + Uchar.utf_decode_length d) (Uchar.utf_decode_uchar d :: acc)
  in
  go 0 []

let bar_span row =
  let bar u =
    Uchar.equal u (Uchar.of_int 0x2588) || Uchar.equal u (Uchar.of_int 0x2591)
  in
  let cells = glyphs row in
  let rec first i =
    if i >= Array.length cells || bar cells.(i) then i else first (i + 1)
  in
  let rec last i = if i < 0 || bar cells.(i) then i else last (i - 1) in
  let i = first 0 in
  if i >= Array.length cells then None
  else Some (i, last (Array.length cells - 1))

let clock () =
  let time = ref 0. in
  ((fun () -> !time), fun dt -> time := !time +. dt)

(* The work ends as under [Console_eio.run]: it succeeds once [f] returns,
   unless [f] ended it, and the display then closes. *)
let finish root d =
  Task.succeed root;
  Display.close d

let plain ?label ?layout ?theme ?(progress = `Plain) ?view ?keep_ended f =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  Option.iter (Theme.set ppf) theme;
  let now, advance = clock () in
  let root = Task.root ?label ~now () in
  let view = Option.map (fun f -> f root) view in
  let d = Progress.show ~progress ?layout ?view ?keep_ended ppf root in
  let v = f root advance in
  finish root d;
  Format.pp_print_flush ppf ();
  (v, Buffer.contents buffer)

type term = {
  vte : Console_vte.t;
  root : Task.t;
  display : Display.t;
  advance : float -> unit;
  size : (int * int) ref;
  chunks : string list ref;
}

let close t = finish t.root t.display

let terminal ?(width = 40) ?(height = 8) ?label ?layout ?theme ?(styled = false)
    ?(raw = false) ?(before = "") ?view ?keep_ended ?commit ?plain ?delay () =
  let buffer = Buffer.create 512 in
  let vte = Console_vte.v ~raw ~rows:height ~cols:width () in
  let chunks = ref [] and size = ref (width, height) in
  let out_string s o l = Buffer.add_substring buffer s o l in
  let out_flush () =
    let s = Buffer.contents buffer in
    Buffer.clear buffer;
    if s <> "" then begin
      chunks := s :: !chunks;
      Console_vte.feed vte s
    end
  in
  Console_vte.feed vte before;
  let ppf = Format.make_formatter out_string out_flush in
  if styled then begin
    Fmt.set_style_renderer ppf `Ansi_tty;
    Color.set_depth ppf `True_color
  end;
  Option.iter (Theme.set ppf) theme;
  let now, advance = clock () in
  let root = Task.root ?label ~now () in
  let view = Option.map (fun f -> f root) view in
  let display =
    Progress.show ?layout ?view ?keep_ended ?commit ?plain ?delay
      ~size:(fun () -> !size)
      ppf root
  in
  { vte; root; display; advance; size; chunks }

let written t = String.concat "" (List.rev !(t.chunks))

let resize t ~width ~height =
  t.size := (width, height);
  Console_vte.resize t.vte ~rows:height ~cols:width

let screen t = Console_vte.screen t.vte
let lines t = Console_vte.screen ~scrollback:true t.vte

let check_invalid message f =
  Alcotest.check_raises message (Invalid_argument message) f

let skip_on_matrix_0_1 fix _ _ =
  Fmt.pr "Skipped on matrix 0.1.0, which lacks %s.@." fix;
  Alcotest.skip ()
