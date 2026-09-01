(*
 * Copyright (c) 2012-2019 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2019 Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

include (Cstruct : module type of Cstruct with type t := Cstruct.t)

type 'a rd = < rd: unit; .. > as 'a
type 'a wr = < wr: unit; .. > as 'a

type 'a t = Cstruct.t

type rdwr =  < rd: unit; wr: unit; >
type ro = < rd: unit; >
type wo = < wr: unit; >

external ro : 'a rd t @ local -> ro t @ local @@ portable = "%identity"
external wo : 'a wr t @ local -> wo t @ local @@ portable = "%identity"

let of_string = Cstruct.of_string ?allocator:None
let of_bytes = Cstruct.of_bytes ?allocator:None

let (of_string_local @ portable) ?off ?len (s @ local) = exclave_
  Cstruct.of_string_local ?off ?len s

let (of_bytes_local @ portable) ?off ?len (b @ local) = exclave_
  Cstruct.of_bytes_local ?off ?len b

let pp ppf t = Cstruct.hexdump_pp ppf t

let[@zero_alloc] (length @ portable) (t @ local) = Cstruct.length t

let blit src ~src_off dst ~dst_off ~len =
  Cstruct.blit src src_off dst dst_off len
[@@inline]

let blit_from_string src ~src_off dst ~dst_off ~len =
  Cstruct.blit_from_string src src_off dst dst_off len
[@@inline]

let blit_from_bytes src ~src_off dst ~dst_off ~len =
  Cstruct.blit_from_bytes src src_off dst dst_off len
[@@inline]

let blit_to_bytes src ~src_off dst ~dst_off ~len =
  Cstruct.blit_to_bytes src src_off dst dst_off len
[@@inline]

let sub t ~off ~len =
  Cstruct.sub t off len
[@@inline]

let[@zero_alloc] (sub_local @ portable) (t @ local) ~off ~len = exclave_
  Cstruct.sub_local t off len

let sub_copy t ~off ~len =
  Cstruct.sub_copy t off len
[@@inline]

let[@zero_alloc] (shift_local @ portable) (t @ local) len = exclave_
  Cstruct.shift_local t len

let[@zero_alloc] (split_local @ portable) ?start (t @ local) len = exclave_
  Cstruct.split_local ?start t len

let (globalize @ portable) (t @ local) = Cstruct.globalize t
let (globalize_list @ portable) (ts @ local) = Cstruct.globalize_list ts

let[@zero_alloc] (shiftv_local @ portable) (ts @ local) len = exclave_
  Cstruct.shiftv_local ts len

let[@zero_alloc] (of_bigarray_local @ portable) ?off ?len buffer = exclave_
  Cstruct.of_bigarray_local ?off ?len buffer

let unsafe_to_bigarray = Cstruct.to_bigarray

let concat vss =
  let res = create_unsafe (Cstruct.sum_lengths ~caller:"Cstruct.Cap.concat" vss) in
  let go off v =
    let len = Cstruct.length v in
    Cstruct.blit v 0 res off len ;
    off + len in
  let len = List.fold_left go 0 vss in
  assert (len = Cstruct.length res) ;
  res
