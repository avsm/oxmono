(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  size : unit -> int;
  read : Byte_range.t -> Base_bigstring.t;
  read_many : Byte_range.t list -> Base_bigstring.t list;
}

(* [Byte_range.resolve] clips a range to the size it is given, so the
   offset and length it returns always name real bytes and a read here
   cannot fail. A caller that needs the whole of a range learns it was
   clipped from the length of the result. *)
let of_bigstring b =
  let len = Base_bigstring.length b in
  let read r =
    let pos, n = Byte_range.resolve ~size:len r in
    Base_bigstring.sub b ~pos ~len:n
  in
  { size = (fun () -> len); read; read_many = List.map read }
