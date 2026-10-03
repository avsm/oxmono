(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type esc = Normal | Esc | Csi

(* The line is a zipper of whole UTF-8 characters split at the cursor: [left]
   holds the characters before the cursor nearest-first (reversed), [right]
   those at and after it in order. Editing a whole character at a time -- rather
   than a byte -- is what keeps multi-byte input intact. *)
type t = {
  mutable left : string list;
  mutable right : string list;
  prompt : string;
  complete : string -> string list;
  char_width : Uchar.t -> int; (* display cells per character, for redraws *)
  mask : string option; (* replacement echoed for each secret character *)
  mutable hist : string list; (* submitted lines, newest first, seed included *)
  mutable pos : int option; (* history index being viewed, newest is 0 *)
  mutable draft : string; (* line in progress, saved while navigating *)
  mutable esc : esc; (* escape-sequence parser state *)
  mutable after_cr : bool; (* last byte was '\r', so swallow a following '\n' *)
  mutable partial : string; (* bytes of an incomplete multi-byte character *)
  mutable need : int; (* continuation bytes still expected for [partial] *)
}

let v ?(prompt = "") ?(history = []) ?(complete = fun _ -> [])
    ?(char_width = Width.default_char_width) ?mask () =
  {
    left = [];
    right = [];
    prompt;
    complete;
    char_width;
    mask;
    hist = List.rev history;
    pos = None;
    draft = "";
    esc = Normal;
    after_cr = false;
    partial = "";
    need = 0;
  }

let line t = String.concat "" (List.rev_append t.left t.right)
let pending t = line t
let history t = List.rev t.hist
let crlf s = String.concat "\r\n" (String.split_on_char '\n' s)
let add = Buffer.add_string

let utf8_len c =
  let b = Char.code c in
  if b < 0xc0 then 1 else if b < 0xe0 then 2 else if b < 0xf0 then 3 else 4

(* Split a complete UTF-8 string into one entry per character. *)
let chars s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let len = min (utf8_len s.[i]) (n - i) in
      go (i + len) (String.sub s i len :: acc)
  in
  go 0 []

let display_char t c = match t.mask with Some mask -> mask | None -> c
let display t s = String.concat "" (List.map (display_char t) (chars s))

(* Display width in terminal cells, via the caller's [char_width] hint -- the
   same mechanism the rest of console uses, so a wide CJK glyph counts as two by
   default. A masked editor measures the replacement glyphs, never the secret
   characters they conceal. *)
let width t s = Width.string_width ~char_width:t.char_width (display t s)
let display_chars t cs = String.concat "" (List.map (display_char t) cs)
let cols t cs = Width.string_width ~char_width:t.char_width (display_chars t cs)

(* Move the cursor back over [t.right] after rewriting it, one backspace per
   cell. *)
let reposition t echo =
  let tail = cols t t.right in
  if tail > 0 then add echo (String.make tail '\b')

(* Insert [s] (one or more whole characters) at the cursor: write it, redraw the
   tail it pushed right, then step the cursor back to just after [s]. *)
let insert t echo s =
  t.pos <- None;
  t.left <- List.rev_append (chars s) t.left;
  add echo (display t s);
  add echo (display_chars t t.right);
  reposition t echo

(* Erase the character before the cursor: step left, redraw the tail with a
   trailing space to clear the vacated cell, then step back. At end of line this
   is the familiar "\b \b". *)
let backspace t echo =
  t.pos <- None;
  match t.left with
  | [] -> ()
  | c :: tl ->
      t.left <- tl;
      let w = width t c in
      add echo (String.make w '\b');
      add echo (display_chars t t.right);
      add echo (String.make w ' ');
      add echo (String.make (cols t t.right + w) '\b')

let cursor_left t echo =
  match t.left with
  | [] -> ()
  | c :: tl ->
      t.left <- tl;
      t.right <- c :: t.right;
      add echo (String.make (width t c) '\b')

let cursor_right t echo =
  match t.right with
  | [] -> ()
  | c :: tl ->
      t.right <- tl;
      t.left <- c :: t.left;
      add echo (display_char t c)

let submit t echo =
  let line = line t in
  t.left <- [];
  t.right <- [];
  add echo "\r\n";
  (if String.length line > 0 && t.mask = None then
     match t.hist with
     | last :: _ when String.equal last line -> ()
     | _ -> t.hist <- line :: t.hist);
  t.pos <- None;
  t.draft <- "";
  line

(* Replace the whole line with [s] (history recall): move to the end, erase
   every character, then write [s] with the cursor left at its end. *)
let recall t echo s =
  add echo (display_chars t t.right);
  for _ = 1 to width t (line t) do
    add echo "\b \b"
  done;
  t.left <- List.rev (chars s);
  t.right <- [];
  add echo (display t s)

let hist_prev t echo =
  match (t.mask, t.hist) with
  | Some _, _ -> ()
  | None, [] -> ()
  | None, _ ->
      let last = List.length t.hist - 1 in
      let i = match t.pos with None -> 0 | Some i -> min (i + 1) last in
      if t.pos = None then t.draft <- line t;
      t.pos <- Some i;
      recall t echo (List.nth t.hist i)

let hist_next t echo =
  match (t.mask, t.pos) with
  | Some _, _ -> ()
  | None, None -> ()
  | None, Some 0 ->
      t.pos <- None;
      recall t echo t.draft
  | None, Some i ->
      t.pos <- Some (i - 1);
      recall t echo (List.nth t.hist (i - 1))

let common_prefix = function
  | [] -> ""
  | first :: rest ->
      List.fold_left
        (fun p s ->
          let n = min (String.length p) (String.length s) in
          let i = ref 0 in
          while !i < n && Char.equal p.[!i] s.[!i] do
            incr i
          done;
          String.sub p 0 !i)
        first rest

(* The word completion acts on is the run after the last space before the
   cursor. *)
let last_word s =
  match String.rindex_opt s ' ' with
  | None -> s
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)

let complete t echo =
  if t.mask <> None then ()
  else
    let before = String.concat "" (List.rev t.left) in
    let word = last_word before in
    match List.filter (String.starts_with ~prefix:word) (t.complete word) with
    | [] -> ()
    | candidates ->
        let lcp = common_prefix candidates in
        if String.length lcp > String.length word then
          insert t echo
            (String.sub lcp (String.length word)
               (String.length lcp - String.length word));
        if List.length candidates > 1 then begin
          add echo "\r\n";
          add echo (String.concat "  " candidates);
          add echo "\r\n";
          add echo t.prompt;
          add echo (display t (line t));
          reposition t echo
        end

let rec normal t echo lines ~after_cr c =
  let code = Char.code c in
  if t.need > 0 then
    if code >= 0x80 && code <= 0xbf then begin
      t.partial <- t.partial ^ String.make 1 c;
      t.need <- t.need - 1;
      if t.need = 0 then begin
        insert t echo t.partial;
        t.partial <- ""
      end
    end
    else begin
      (* not a continuation byte: drop the truncated character, retry [c] *)
      t.partial <- "";
      t.need <- 0;
      normal t echo lines ~after_cr c
    end
  else
    match c with
    | '\027' -> t.esc <- Esc
    | '\r' ->
        t.after_cr <- true;
        lines := submit t echo :: !lines
    | '\n' -> if not after_cr then lines := submit t echo :: !lines
    | '\t' -> complete t echo
    | '\127' | '\008' -> backspace t echo
    | c when code >= 0xc0 && code <= 0xf7 ->
        t.partial <- String.make 1 c;
        t.need <- utf8_len c - 1
    | c when code >= 0x20 && code < 0x7f -> insert t echo (String.make 1 c)
    | _ -> ()

let csi t echo c =
  let code = Char.code c in
  if code >= 0x40 && code <= 0x7e then begin
    (match c with
    | 'A' -> hist_prev t echo
    | 'B' -> hist_next t echo
    | 'C' -> cursor_right t echo
    | 'D' -> cursor_left t echo
    | _ -> ());
    t.esc <- Normal
  end
  else if not (code >= 0x30 && code <= 0x3f) then t.esc <- Normal

let feed t input =
  let echo = Buffer.create (String.length input) in
  let lines = ref [] in
  String.iter
    (fun c ->
      let after_cr = t.after_cr in
      t.after_cr <- false;
      match t.esc with
      | Esc -> t.esc <- (if Char.equal c '[' then Csi else Normal)
      | Csi -> csi t echo c
      | Normal -> normal t echo lines ~after_cr c)
    input;
  (Buffer.contents echo, List.rev !lines)
