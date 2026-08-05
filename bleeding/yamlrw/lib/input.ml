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
  mutable position : Position.t;  (** Line/column tracking *)
}

(** Ensure we have a current slice. Returns true if data available. *)
let ensure_slice t =
  match t.current_slice with
  | Some slice when t.slice_pos < Bytes.Slice.length slice -> true
  | _ ->
      let slice = Bytes.Reader.read t.reader in
      if Bytes.Slice.is_eod slice then begin
        t.current_slice <- None;
        false
      end
      else begin
        t.current_slice <- Some slice;
        t.slice_pos <- 0;
        true
      end

(** Get current character without advancing *)
let peek_current t =
  match t.current_slice with
  | Some slice when t.slice_pos < Bytes.Slice.length slice ->
      let bytes = Bytes.Slice.bytes slice in
      let first = Bytes.Slice.first slice in
      Some (Stdlib.Bytes.get bytes (first + t.slice_pos))
  | _ -> None

(** Create input from a Bytes.Reader.t *)
let of_reader ?(initial_position = Position.initial) reader =
  let t =
    { reader; current_slice = None; slice_pos = 0; position = initial_position }
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

let position t = t.position
let is_eof t = not (ensure_slice t)
let peek t = if ensure_slice t then peek_current t else None

let peek_exn t =
  match peek t with
  | Some c -> c
  | None -> Error.raise_at t.position Unexpected_eof

(** Peek at nth character (0-indexed from current position) *)
let peek_nth t n =
  if n = 0 then peek t
  else begin
    (* Use sniff for lookahead - it pushes back automatically *)
    let sample = Bytes.Reader.sniff (n + 1) t.reader in
    (* But sniff reads from reader, and we may have a current slice.
       We need to account for what's already in current_slice *)
    match t.current_slice with
    | Some slice ->
        let slice_bytes = Bytes.Slice.bytes slice in
        let slice_first = Bytes.Slice.first slice in
        let slice_remaining = Bytes.Slice.length slice - t.slice_pos in
        if n < slice_remaining then
          Some (Stdlib.Bytes.get slice_bytes (slice_first + t.slice_pos + n))
        else begin
          (* Need to look beyond current slice *)
          let sample_offset = n - slice_remaining in
          if sample_offset < String.length sample then
            Some sample.[sample_offset]
          else None
        end
    | None -> if n < String.length sample then Some sample.[n] else None
  end

(** Peek at up to n characters as a string *)
let rec peek_string t n =
  if n <= 0 then ""
  else begin
    match t.current_slice with
    | Some slice ->
        let slice_bytes = Bytes.Slice.bytes slice in
        let slice_first = Bytes.Slice.first slice in
        let slice_remaining = Bytes.Slice.length slice - t.slice_pos in
        if n <= slice_remaining then
          (* All within current slice *)
          Stdlib.Bytes.sub_string slice_bytes (slice_first + t.slice_pos) n
        else begin
          (* Need data from beyond current slice - use sniff *)
          let needed_from_reader = n - slice_remaining in
          let sample = Bytes.Reader.sniff needed_from_reader t.reader in
          let buf = Buffer.create n in
          Buffer.add_subbytes buf slice_bytes
            (slice_first + t.slice_pos)
            slice_remaining;
          Buffer.add_string buf sample;
          Buffer.contents buf
        end
    | None -> if ensure_slice t then peek_string t n else ""
  end

(** Consume next character *)
let next t =
  if ensure_slice t then begin
    match t.current_slice with
    | Some slice ->
        let bytes = Bytes.Slice.bytes slice in
        let first = Bytes.Slice.first slice in
        let c = Stdlib.Bytes.get bytes (first + t.slice_pos) in
        t.slice_pos <- t.slice_pos + 1;
        t.position <- Position.advance_char c t.position;
        (* Check if we've exhausted this slice *)
        if t.slice_pos >= Bytes.Slice.length slice then t.current_slice <- None;
        Some c
    | None -> None
  end
  else None

let next_exn t =
  match next t with
  | Some c -> c
  | None -> Error.raise_at t.position Unexpected_eof

let skip t n =
  for _ = 1 to n do
    ignore (next t)
  done

let skip_while t pred =
  let rec loop () =
    match peek t with
    | Some c when pred c ->
        ignore (next t);
        loop ()
    | _ -> ()
  in
  loop ()

(** Check if next char satisfies predicate *)
let next_is pred t = match peek t with None -> false | Some c -> pred c

let next_is_break t = next_is is_break t
let next_is_blank t = next_is is_blank t
let next_is_whitespace t = next_is is_whitespace t
let next_is_digit t = next_is is_digit t
let next_is_hex t = next_is is_hex t
let next_is_alpha t = next_is is_alpha t
let next_is_indicator t = next_is is_indicator t

(** Check if at document boundary (--- or ...) *)
let at_document_boundary t =
  if t.position.column <> 1 then false
  else begin
    let s = peek_string t 4 in
    let len = String.length s in
    if len < 3 then false
    else
      let prefix = String.sub s 0 3 in
      (prefix = "---" || prefix = "...") && (len = 3 || is_whitespace s.[3])
  end

(** Consume line break, handling \r\n as single break *)
let consume_break t =
  match peek t with
  | Some '\r' -> (
      ignore (next t);
      match peek t with Some '\n' -> ignore (next t) | _ -> ())
  | Some '\n' -> ignore (next t)
  | _ -> ()

(** Get remaining content from current position *)
let remaining t =
  let buf = Buffer.create 256 in
  (* Add current slice remainder *)
  (match t.current_slice with
  | Some slice ->
      let bytes = Bytes.Slice.bytes slice in
      let first = Bytes.Slice.first slice in
      let remaining = Bytes.Slice.length slice - t.slice_pos in
      if remaining > 0 then
        Buffer.add_subbytes buf bytes (first + t.slice_pos) remaining
  | None -> ());
  (* Add remaining from reader *)
  Bytes.Reader.add_to_buffer buf t.reader;
  Buffer.contents buf

(** Mark current position for span creation *)
let mark t = t.position

(** Get the character before the current position (limited lookahead) *)
let peek_back t =
  match t.current_slice with
  | Some slice when t.slice_pos > 0 ->
      let bytes = Bytes.Slice.bytes slice in
      let first = Bytes.Slice.first slice in
      Some (Stdlib.Bytes.get bytes (first + t.slice_pos - 1))
  | _ -> None

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
