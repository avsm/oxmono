(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Character input source with lookahead, based on Bytes.Reader.t

    This module wraps a bytesrw [Bytes.Reader.t] to provide
    character-by-character access with lookahead for the YAML scanner. Uses
    bytesrw's sniff and push_back for efficient lookahead without excessive
    copying.

    The same input type works with any reader source: strings, files, channels,
    or streaming sources like Eio. *)

open Bytesrw

include Char_class
(** Re-export character classification *)

type t = {
  reader : Bytes.Reader.t;
  mutable current_slice : Bytes.Slice.t option;
      (** Current slice being consumed *)
  mutable slice_pos : int;  (** Position within current slice *)
  mutable slice_owned : bool;
  mutable index : int;
  mutable line : int;
  mutable column : int;
  mutable previous : char or_null;
}

(** Ensure we have a current slice. Returns true if data available. *)
let ensure_slice t =
  match t.current_slice with
  | Some slice when t.slice_pos < Bytes.Slice.length slice -> true
  | _ ->
      let slice = Bytes.Reader.read t.reader in
      if Bytes.Slice.is_eod slice then (
        t.current_slice <- None;
        false)
      else (
        t.current_slice <- Some slice;
        t.slice_pos <- 0;
        t.slice_owned <- false;
        true)

let[@zero_alloc] peek_current t =
  match t.current_slice with
  | Some slice when t.slice_pos < Bytes.Slice.length slice ->
      let bytes = Bytes.Slice.bytes slice in
      let first = Bytes.Slice.first slice in
      This (Stdlib.Bytes.get bytes (first + t.slice_pos))
  | _ -> Null

(** Create input from a Bytes.Reader.t *)
let of_reader ?(initial_position = Position.initial) reader =
  let t =
    {
      reader;
      current_slice = None;
      slice_pos = 0;
      slice_owned = false;
      index = initial_position.Position.index;
      line = initial_position.line;
      column = initial_position.column;
      previous = Null;
    }
  in
  (* Use sniff for BOM detection - this is exactly what sniff is for *)
  let sample = Bytes.Reader.sniff 4 t.reader in
  let bom_len =
    if
      String.length sample >= 3
      && sample.[0] = '\xEF'
      && sample.[1] = '\xBB'
      && sample.[2] = '\xBF'
    then 3 (* UTF-8 BOM *)
    else 0
  in
  (* Skip BOM if present *)
  if bom_len > 0 then Bytes.Reader.skip bom_len t.reader;
  t

(** Create input from a string *)
let of_string s =
  let reader = Bytes.Reader.of_string s in
  of_reader reader

let position t = { Position.index = t.index; line = t.line; column = t.column }
let[@zero_alloc] line t = t.line
let[@zero_alloc] column t = t.column

let[@zero_alloc] advance t c =
  t.previous <- This c;
  t.index <- t.index + 1;
  if c = '\n' then (
    t.line <- t.line + 1;
    t.column <- 1)
  else t.column <- t.column + 1

let is_eof t = not (ensure_slice t)
let peek_or_null t = if ensure_slice t then peek_current t else Null
let peek t = match peek_or_null t with This c -> Some c | Null -> None

let peek_exn t =
  match peek_or_null t with
  | This c -> c
  | Null -> Error.raise_at (position t) Unexpected_eof

(* A reader may reuse its buffer on the next read. Lookahead must preserve
   the current slice before sniffing beyond it. *)
let sniff t n =
  (match t.current_slice with
  | Some slice when not t.slice_owned ->
      t.current_slice <- Some (Bytes.Slice.copy ~tight:true slice);
      t.slice_owned <- true
  | Some _ | None -> ());
  Bytes.Reader.sniff n t.reader

let peek_nth_or_null t n =
  if n < 0 then invalid_arg "Yamlrw.Input.peek_nth: negative lookahead";
  if n >= Sys.max_string_length then
    invalid_arg "Yamlrw.Input.peek_nth: lookahead is too large";
  if n = 0 then peek_or_null t
  else
    match t.current_slice with
    | Some slice ->
        let remaining = Bytes.Slice.length slice - t.slice_pos in
        if n < remaining then
          This
            (Stdlib.Bytes.get (Bytes.Slice.bytes slice)
               (Bytes.Slice.first slice + t.slice_pos + n))
        else
          let offset = n - remaining in
          let sample = sniff t (offset + 1) in
          if offset < String.length sample then This sample.[offset] else Null
    | None ->
        let sample = Bytes.Reader.sniff (n + 1) t.reader in
        if n < String.length sample then This sample.[n] else Null

let peek_nth t n =
  match peek_nth_or_null t n with This c -> Some c | Null -> None

(** Peek at up to n characters as a string *)
let rec peek_string t n =
  if n <= 0 then ""
  else
    match t.current_slice with
    | Some slice ->
        let slice_bytes = Bytes.Slice.bytes slice in
        let slice_first = Bytes.Slice.first slice in
        let slice_remaining = Bytes.Slice.length slice - t.slice_pos in
        if n <= slice_remaining then
          (* All within current slice *)
          Stdlib.Bytes.sub_string slice_bytes (slice_first + t.slice_pos) n
        else
          (* Need data from beyond current slice - use sniff *)
          let needed_from_reader = n - slice_remaining in
          let buf = Buffer.create n in
          Buffer.add_subbytes buf slice_bytes
            (slice_first + t.slice_pos)
            slice_remaining;
          let sample = sniff t needed_from_reader in
          Buffer.add_string buf sample;
          Buffer.contents buf
    | None -> if ensure_slice t then peek_string t n else ""

let next_or_null t =
  if ensure_slice t then
    match t.current_slice with
    | Some slice ->
        let bytes = Bytes.Slice.bytes slice in
        let first = Bytes.Slice.first slice in
        let c = Stdlib.Bytes.get bytes (first + t.slice_pos) in
        t.slice_pos <- t.slice_pos + 1;
        advance t c;
        if t.slice_pos >= Bytes.Slice.length slice then t.current_slice <- None;
        This c
    | None -> Null
  else Null

let next t = match next_or_null t with This c -> Some c | Null -> None

let next_exn t =
  match next_or_null t with
  | This c -> c
  | Null -> Error.raise_at (position t) Unexpected_eof

let skip t n =
  for _ = 1 to n do
    match next_or_null t with This _ | Null -> ()
  done

let skip_while t pred =
  let rec loop () =
    match peek_or_null t with
    | This c when pred c ->
        (match next_or_null t with This _ | Null -> ());
        loop ()
    | _ -> ()
  in
  loop ()

let next_is pred t =
  match peek_or_null t with Null -> false | This c -> pred c

let next_is_break t = next_is is_break t
let next_is_blank t = next_is is_blank t
let next_is_whitespace t = next_is is_whitespace t
let next_is_digit t = next_is is_digit t
let next_is_hex t = next_is is_hex t
let next_is_alpha t = next_is is_alpha t
let next_is_indicator t = next_is is_indicator t

(** Check if at document boundary (--- or ...) *)
let at_document_boundary t =
  if t.column <> 1 then false
  else
    let s = peek_string t 4 in
    let len = String.length s in
    if len < 3 then false
    else
      let prefix = String.sub s 0 3 in
      (prefix = "---" || prefix = "...") && (len = 3 || is_whitespace s.[3])

(** Consume line break, handling \r\n as single break *)
let consume_break t =
  match peek_or_null t with
  | This '\r' -> (
      skip t 1;
      match peek_or_null t with This '\n' -> skip t 1 | _ -> ())
  | This '\n' -> skip t 1
  | _ -> ()

let remaining t =
  let buf = Buffer.create 256 in
  while ensure_slice t do
    match t.current_slice with
    | Some slice ->
        let bytes = Bytes.Slice.bytes slice in
        let first = Bytes.Slice.first slice in
        let length = Bytes.Slice.length slice in
        Buffer.add_subbytes buf bytes (first + t.slice_pos)
          (length - t.slice_pos);
        for i = t.slice_pos to length - 1 do
          advance t (Stdlib.Bytes.get bytes (first + i))
        done;
        t.slice_pos <- length;
        t.current_slice <- None
    | None -> ()
  done;
  Buffer.contents buf

(** Mark current position for span creation *)
let mark = position

let[@zero_alloc] peek_back_or_null t = t.previous

let peek_back t =
  match peek_back_or_null t with This c -> Some c | Null -> None

(** Get a sample of the source for encoding detection. Uses sniff to peek
    without consuming. *)
let source t =
  (* First check current slice *)
  match t.current_slice with
  | Some slice ->
      let bytes = Bytes.Slice.bytes slice in
      let first = Bytes.Slice.first slice in
      let available = min 4 (Bytes.Slice.length slice - t.slice_pos) in
      Stdlib.Bytes.sub_string bytes (first + t.slice_pos) available
  | None ->
      (* Use sniff to peek at reader *)
      Bytes.Reader.sniff 4 t.reader

(** Get the byte position in the underlying stream *)
let byte_pos t = Bytes.Reader.pos t.reader
