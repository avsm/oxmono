(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  vte : Vte.t;
  raw : bool;
  scan : Ansi.Parser.t;  (** Reads the bytes [vte] reads, for [split]. *)
  mutable split : int option;
      (** The length of a spacing mark just read, which xterm and VTE give a
          cell of its own where matrix draws it in the cell of its letter, but
          not at the start of a write. *)
  pending : Buffer.t;  (** The first bytes of a character the next feed ends. *)
}

let v ?(raw = false) ~rows ~cols () =
  {
    vte = Vte.create ~rows ~cols ();
    raw;
    scan = Ansi.Parser.create ();
    split = None;
    pending = Buffer.create 4;
  }

let vte t = t.vte
let rows t = Vte.rows t.vte
let cols t = Vte.cols t.vte
let resize t ~rows ~cols = Vte.resize t.vte ~rows ~cols
let cursor_pos t = Vte.cursor_pos t.vte
let cursor_visible t = Vte.cursor_visible t.vte
let to_string t = Vte.to_string t.vte
let scrollback_lines t = Vte.scrollback_lines t.vte
let scrollback_size t = Vte.scrollback_size t.vte
let grid t = Vte.grid t.vte

let fg g i =
  Ansi.Color.of_rgba_f (Grid.get_fg_r g i) (Grid.get_fg_g g i)
    (Grid.get_fg_b g i) (Grid.get_fg_a g i)

let bg g i =
  Ansi.Color.of_rgba_f (Grid.get_bg_r g i) (Grid.get_bg_g g i)
    (Grid.get_bg_b g i) (Grid.get_bg_a g i)

let scan t = function
  | Ansi.Parser.Text s ->
      let c =
        Uchar.to_int (Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0))
      in
      if Width.is_spacing_mark c then t.split <- Some (String.length s)
  | _ -> ()

(* [s], a part of a character, is the start of one that more bytes can
   complete: some continuation byte makes the decoder read further. *)
let continues s =
  List.exists
    (fun next ->
      Uchar.utf_decode_length (String.get_utf_8_uchar (s ^ next) 0)
      > String.length s)
    [ "\x80"; "\x90"; "\xa0" ]

(* The length of [s] but for the first bytes of a character it leaves
   unended. *)
let complete s =
  let n = String.length s in
  let rec last i =
    if i >= n then n
    else
      let d = String.get_utf_8_uchar s i in
      let next = i + Uchar.utf_decode_length d in
      if next < n then last next
      else if
        (not (Uchar.utf_decode_is_valid d))
        && continues (String.sub s i (n - i))
      then i
      else n
  in
  last 0

(* A tty with output processing on writes each line feed as CR LF. *)
let feed t s =
  let s = Matrix_fix.backspace s in
  Buffer.add_string t.pending
    (if t.raw then s else String.concat "\r\n" (String.split_on_char '\n' s));
  let s = Buffer.contents t.pending in
  let n = complete s in
  Buffer.clear t.pending;
  Buffer.add_substring t.pending s n (String.length s - n);
  let out = Buffer.create n in
  let write () =
    Vte.feed_string t.vte (Buffer.contents out);
    Buffer.clear out
  in
  let rec characters at =
    if at < n then begin
      let d = String.get_utf_8_uchar s at in
      let next = at + Uchar.utf_decode_length d in
      for i = at to next - 1 do
        Buffer.add_char out s.[i];
        Matrix_fix.scan t.scan s ~at d i (scan t);
        match t.split with
        | None -> ()
        | Some k ->
            t.split <- None;
            let mark = Buffer.sub out (Buffer.length out - k) k in
            Buffer.truncate out (Buffer.length out - k);
            write ();
            Buffer.add_string out mark
      done;
      characters next
    end
  in
  characters 0;
  write ()

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && (s.[!n - 1] = ' ' || s.[!n - 1] = '\t') do
    decr n
  done;
  String.sub s 0 !n

let drop_trailing_blanks rows =
  let rec go = function "" :: tl -> go tl | rows -> rows in
  List.rev (go (List.rev rows))

let screen ?(scrollback = false) t =
  let visible = String.split_on_char '\n' (Vte.to_string t.vte) in
  let rows =
    if scrollback then Vte.scrollback_lines t.vte @ visible else visible
  in
  drop_trailing_blanks (List.map rstrip rows)

(* One record's header is 24 bytes: the payload length, the wall clock the
   capture stamped it with, and which of the three kinds it is. *)
let header_size = 24
let u64 s off = Int64.to_int (String.get_int64_le s off)
let i32 s off = Int32.to_int (String.get_int32_le s off)

let records recording =
  let n = String.length recording in
  let rec loop off acc =
    (* A capture of a program that was still running ends mid-record, so a
       header that does not fit, or one promising more payload than is left,
       ends the stream rather than raising. *)
    if off + header_size > n then List.rev acc
    else
      let len = u64 recording off in
      if len < 0 || len > n - off - header_size then List.rev acc
      else
        let seconds =
          float_of_int (u64 recording (off + 8))
          +. (float_of_int (i32 recording (off + 16)) /. 1e6)
        in
        let kind = Char.chr (i32 recording (off + 20) land 0xff) in
        let next = off + header_size + len in
        if kind <> 'o' || len = 0 then loop next acc
        else
          loop next
            ((seconds, String.sub recording (off + header_size) len) :: acc)
  in
  loop 0 []

let frames ~rows ~cols recording =
  match records recording with
  | [] -> []
  | (start, _) :: _ as records ->
      let t = v ~rows ~cols () in
      List.map
        (fun (seconds, written) ->
          feed t written;
          (seconds -. start, screen t))
        records
