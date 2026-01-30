(*---------------------------------------------------------------------------
  Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
  Copyright (c) 2012-2014 David Sheets <sheets@alum.mit.edu>
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>

  Permission to use, copy, modify, and distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 ---------------------------------------------------------------------------*)

(** URI Buf_write serialization for the requests library.

    This module provides efficient [Eio.Buf_write] serialization for [Uri.t]
    values. For all other URI operations, use the [uri] opam library directly.

    {[
      (* Use Uri for parsing and manipulation *)
      let uri = Uri.of_string "https://example.com/path" in
      let host = Uri.host uri in

      (* Use Huri.write for efficient serialization to Buf_write *)
      Eio.Buf_write.with_flow flow (fun w ->
        Huri.write w uri
      )
    ]} *)

(** {1 Type Alias} *)

type t = Uri.t
(** [t] is an alias for [Uri.t]. Use the [uri] library for all operations
    except [Buf_write] serialization. *)

(** {1 Buf_write Serialization} *)

(** Hex character lookup table for efficient percent-encoding *)
let hex_chars = "0123456789ABCDEF"

(** Safe characters for different URI components per RFC 3986 *)
module Safe_chars = struct
  type safe_chars = bool array

  let sub_delims a =
    let subd = "!$&'()*+,;=" in
    for i = 0 to String.length subd - 1 do
      a.(Char.code subd.[i]) <- true
    done;
    a

  let unreserved : safe_chars =
    let a = Array.make 256 false in
    let always_safe =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-~" in
    for i = 0 to String.length always_safe - 1 do
      a.(Char.code always_safe.[i]) <- true
    done;
    a

  let pchar : safe_chars =
    let a = sub_delims (Array.copy unreserved) in
    a.(Char.code ':') <- true;
    a.(Char.code '@') <- true;
    a

  let path : safe_chars =
    let a = sub_delims (Array.copy pchar) in
    a.(Char.code '/') <- false;
    a

  let query : safe_chars =
    let a = Array.copy pchar in
    a.(Char.code '/') <- true;
    a.(Char.code '?') <- true;
    a.(Char.code '&') <- false;
    a.(Char.code ';') <- false;
    a.(Char.code '+') <- false;
    a

  let query_key : safe_chars =
    let a = Array.copy query in
    a.(Char.code '=') <- false;
    a

  let query_value : safe_chars =
    let a = Array.copy query in
    a.(Char.code ',') <- false;
    a

  let fragment : safe_chars = query

  let userinfo : safe_chars =
    let a = Array.copy unreserved in
    a.(Char.code ':') <- false;
    a
end

module Writer = struct
  module Write = Eio.Buf_write

  let write_pct_char w c =
    Write.char w '%';
    Write.char w hex_chars.[Char.code c lsr 4];
    Write.char w hex_chars.[Char.code c land 0xf]

  let write_pct_encoded ~safe_chars w s =
    for i = 0 to String.length s - 1 do
      let c = s.[i] in
      if safe_chars.(Char.code c) then
        Write.char w c
      else
        write_pct_char w c
    done

  let write_path w path =
    let len = String.length path in
    let rec loop i =
      if i >= len then ()
      else if path.[i] = '/' then begin
        Write.char w '/';
        loop (i + 1)
      end else begin
        let rec find_end j = if j >= len || path.[j] = '/' then j else find_end (j + 1) in
        let j = find_end i in
        write_pct_encoded ~safe_chars:Safe_chars.path w (String.sub path i (j - i));
        loop j
      end
    in
    loop 0

  let write_query w query =
    List.iteri (fun i (k, vs) ->
      if i > 0 then Write.char w '&';
      write_pct_encoded ~safe_chars:Safe_chars.query_key w k;
      if vs <> [] then begin
        Write.char w '=';
        List.iteri (fun j v ->
          if j > 0 then Write.char w ',';
          write_pct_encoded ~safe_chars:Safe_chars.query_value w v
        ) vs
      end
    ) query

  let write w uri =
    (* Scheme *)
    Option.iter (fun s -> Write.string w s; Write.char w ':') (Uri.scheme uri);
    (* Authority *)
    (match Uri.userinfo uri, Uri.host uri, Uri.port uri with
     | Some _, _, _ | _, Some _, _ | _, _, Some _ -> Write.string w "//"
     | _ -> ());
    (* Userinfo *)
    Option.iter (fun ui ->
      write_pct_encoded ~safe_chars:Safe_chars.userinfo w ui;
      Write.char w '@'
    ) (Uri.userinfo uri);
    (* Host *)
    Option.iter (fun h ->
      if String.length h > 0 && h.[0] = '[' then Write.string w h
      else write_pct_encoded ~safe_chars:Safe_chars.unreserved w h
    ) (Uri.host uri);
    (* Port *)
    Option.iter (fun p -> Write.char w ':'; Write.string w (string_of_int p)) (Uri.port uri);
    (* Path *)
    let path = Uri.path uri in
    if path <> "" then write_path w path;
    (* Query *)
    let query = Uri.query uri in
    if query <> [] then begin Write.char w '?'; write_query w query end;
    (* Fragment *)
    Option.iter (fun f ->
      Write.char w '#';
      write_pct_encoded ~safe_chars:Safe_chars.fragment w f
    ) (Uri.fragment uri)
end

let write = Writer.write
(** [write w uri] writes [uri] directly to the buffer [w]. This is more
    efficient than [Uri.to_string] when writing to an I/O sink as it avoids
    intermediate string allocation. *)

(** {1 JSON Codec} *)

let jsont = Jsont.string |> Jsont.map ~dec:Uri.of_string ~enc:Uri.to_string
(** JSON codec for URIs. Encodes as a JSON string. *)
