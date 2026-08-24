(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  size : unit -> int;
  read : Byte_range.t -> Base_bigstring.t;
  read_many : Byte_range.t list -> Base_bigstring.t list;
}

let of_bigstring b =
  let len = Base_bigstring.length b in
  let read r =
    let pos, n = Byte_range.resolve ~size:len r in
    if pos < 0 || n < 0 || pos + n > len then
      Error.raise_ (Error.Store "byte range beyond end of chunk")
    else Base_bigstring.sub b ~pos ~len:n
  in
  { size = (fun () -> len); read; read_many = List.map read }
