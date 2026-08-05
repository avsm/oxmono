(*---------------------------------------------------------------------------
  Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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

    {2 Usage}

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

val write : Eio.Buf_write.t -> t -> unit
(** [write w uri] writes [uri] directly to the buffer [w]. This is more
    efficient than [Uri.to_string] when writing to an I/O sink as it avoids
    intermediate string allocation. *)

(** {1 JSON Codec} *)

val jsont : t Jsont.t
(** JSON codec for URIs. Encodes as a JSON string. *)
