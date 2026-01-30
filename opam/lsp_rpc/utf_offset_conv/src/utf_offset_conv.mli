(** This library facilitates converting offsets in a Unicode string when they are
    expressed in the wrong units / encoding. An example use case is that you have some
    offset that was taken w.r.t. text when it was in UTF-16, but the text has since been
    converted to UTF-8, and you want to find the corresponding offset. Or, conversely, you
    have text that is currently in UTF-8 and have taken an offset, but know it will be
    converted into UTF-16 and want the offset to be valid after that happens. This sort of
    thing is common when interfacing with JavaScript environments, since js_of_ocaml
    performs string conversion automatically:
    https://ocaml.org/p/js_of_ocaml/3.3.0/doc/Js/index.html#val-string. *)
open Core

module Offset_units : sig
  (** [encoding] is the encoding of the text at the time the offset is taken/applied: when
      providing an offset, it is the encoding of the text at the time the offset was
      taken; when using a returned offset, the text must be encoded in [encoding]. This is
      tracked separately from [text_encoding] as the text may have been converted to a
      different encoding since the time the provided offset was taken (or may yet be
      converted to a different encoding at the time the returned offset will be used). The
      endianness of the encoding does not matter for interpreting the offset units.

      [units] refers to the measurement of the offset - it is either a regular byte offset
      or expressed in terms of "code units," which are 1 for UTF-8, 2 for UTF-16, and 4
      for UTF-32. When [units] is [`Uchars], the value of [encoding] does not matter. *)
  type t =
    { encoding : [ `UTF_8 | `UTF_16 | `UTF_32 ]
    ; units : [ `Bytes | `Uchars | `Code_units ]
    }
  [@@deriving sexp_of]
end

(** Convert an offset in [text] from one type of units to another. [from] should be an
    offset in [text] at a valid Unicode boundary. It is allowed to point to the position
    after the final character. *)
val convert_offset
  :  int (** Offset into [text] in [from] units. *)
  -> from:Offset_units.t (** Units in which the offset is expressed. *)
  -> to_:Offset_units.t (** Units to which you want to convert the offset. *)
  -> text_encoding:(module String.Utf with type t = 'utf)
  -> text:'utf
  -> int
