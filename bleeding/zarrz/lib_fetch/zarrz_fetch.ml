(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Byte_range = Zarrz.Byte_range
module Error = Zarrz.Error
module Store = Zarrz.Store

(* At most this many range requests of one [get_ranges] call are in
   flight at once. The bound is politeness towards one origin, not a
   connection pool size, which belongs to the backend the caller built
   the client from. *)
let max_fibers = 6

(* The buffer a body with no declared length starts at, doubling from
   there. Large enough that a typical chunk lands in one or two
   allocations, small enough that a metadata document does not pay for
   a megabyte. *)
let initial_body_buffer = 65536

let store_err fmt = Printf.ksprintf (fun s -> Error.raise_ (Error.Store s)) fmt

let unexpected ~meth ~url status =
  store_err "%s %s: unexpected HTTP status %d" meth url status

(* {1 Body buffering} *)

(* [declared_length resp] is [resp]'s [Content-Length] as an [int], if
   it is present and fits. A length that overflows an [int] cannot name
   a buffer this process could hold, so it is treated as unknown and the
   growing path discovers the real size, or fails allocating. *)
let declared_length resp =
  match Fetch.header Fetch.Header.content_length resp with
  | Some n when n >= 0L && n <= Int64.of_int max_int -> Some (Int64.to_int n)
  | Some _ | None -> None

(* [read_exact src buf n] fills the first [n] bytes of [buf] from [src].
   Reading stops at [n] rather than at end of file, so a body longer
   than its declared length is truncated by the length the sender
   promised, as a conforming recipient must. *)
let read_exact src buf n =
  let filled = ref 0 in
  (try
     while !filled < n do
       let view = Cstruct.of_bigarray ~off:!filled ~len:(n - !filled) buf in
       filled := !filled + Eio.Flow.single_read src view
     done
   with End_of_file -> ());
  !filled

(* [read_growing src] buffers [src] to its end, doubling a bigstring as
   it goes. The final buffer is exact, so the caller never sees slack. *)
let read_growing src =
  let buf = ref (Base_bigstring.create initial_body_buffer) in
  let filled = ref 0 in
  (try
     while true do
       let cap = Base_bigstring.length !buf in
       if !filled = cap then begin
         let bigger = Base_bigstring.create (cap * 2) in
         Base_bigstring.blit ~src:!buf ~src_pos:0 ~dst:bigger ~dst_pos:0
           ~len:cap;
         buf := bigger
       end;
       let cap = Base_bigstring.length !buf in
       let view = Cstruct.of_bigarray ~off:!filled ~len:(cap - !filled) !buf in
       filled := !filled + Eio.Flow.single_read src view
     done
   with End_of_file -> ());
  if !filled = Base_bigstring.length !buf then !buf
  else Base_bigstring.sub !buf ~pos:0 ~len:!filled

(* [read_body ~meth ~url resp] is [resp]'s body in a fresh bigstring.
   Bytes land in the result buffer through [Cstruct] views over it, so
   no intermediate string exists at any point. *)
let read_body ~meth ~url resp =
  let src = Fetch.body resp in
  match declared_length resp with
  | None -> read_growing src
  | Some n ->
      let buf = Base_bigstring.create n in
      let got = read_exact src buf n in
      if got <> n then
        store_err "%s %s: body ended after %d of the %d declared bytes" meth url
          got n;
      buf

(* {1 Ranges} *)

(* [range_spec r] is [r] as the [Range] header spells it. RFC 9110
   §14.1.2 counts the last byte inclusively, so a length of [n] from
   [off] ends at [off + n - 1]. *)
let range_spec = function
  | Byte_range.From_start { off; len = Some n } ->
      `Range (Int64.of_int off, Some (Int64.of_int (off + n - 1)))
  | Byte_range.From_start { off; len = None } -> `Range (Int64.of_int off, None)
  | Byte_range.Suffix n -> `Suffix (Int64.of_int n)

(* A range of no bytes has no [Range] header that means it: [bytes=5-4]
   is malformed and [bytes=-0] is unsatisfiable. Such a read is answered
   from nothing instead, which is also what it costs. *)
let is_empty_range = function
  | Byte_range.From_start { len = Some 0; _ } | Byte_range.Suffix 0 -> true
  | Byte_range.From_start _ | Byte_range.Suffix _ -> false

let check_range = function
  | Byte_range.From_start { off; len } ->
      if off < 0 then invalid_arg "Zarrz_fetch: negative range offset";
      Option.iter
        (fun n ->
          if n < 0 then invalid_arg "Zarrz_fetch: negative range length")
        len
  | Byte_range.Suffix n ->
      if n < 0 then invalid_arg "Zarrz_fetch: negative range suffix"

(* {1 The store} *)

let key_url ~base_url key = base_url ^ "/" ^ key

let store ?(ranged = true) ~base_url client =
  if base_url = "" then invalid_arg "Zarrz_fetch: empty base_url";
  if String.ends_with ~suffix:"/" base_url then
    invalid_arg "Zarrz_fetch: base_url ends in a slash";
  let get ~key =
    let url = key_url ~base_url key in
    Fetch.with_response client `GET url (fun resp ->
        match Fetch.status resp with
        | 200 -> Some (read_body ~meth:"GET" ~url resp)
        | 404 | 410 -> None
        | s -> unexpected ~meth:"GET" ~url s)
  in
  let get_range ~key r =
    check_range r;
    if is_empty_range r then Some (Base_bigstring.create 0)
    else
      let url = key_url ~base_url key in
      let headers = Fetch.Header.[ (range, bytes [ range_spec r ]) ] in
      Fetch.with_response ~headers client `GET url (fun resp ->
          match Fetch.status resp with
          | 206 -> Some (read_body ~meth:"GET" ~url resp)
          | 200 ->
              (* The origin ignored the range and sent the whole object,
                 which it is entitled to do. Slice locally, truncating
                 exactly as a resolved range truncates. *)
              let whole = read_body ~meth:"GET" ~url resp in
              let pos, len =
                Byte_range.resolve ~size:(Base_bigstring.length whole) r
              in
              Some (Base_bigstring.sub whole ~pos ~len)
          | 404 | 410 -> None
          | 416 ->
              store_err "GET %s: range %s is unsatisfiable (HTTP 416)" url
                (Format.asprintf "%a" Byte_range.pp r)
          | s -> unexpected ~meth:"GET" ~url s)
  in
  let get_ranges ~key rs =
    match rs with
    | [] -> Some []
    | _ ->
        (* One fiber per range. Each [with_response] scopes its own
           switch, so no switch is needed here, and a failing fiber
           cancels its siblings through [Fiber.List.map].

           Coalescing adjacent ranges whose gap is under a megabyte into
           one request is a follow-up: it would turn the inner chunks of
           a densely read shard into a single GET. *)
        let bufs = Eio.Fiber.List.map ~max_fibers (get_range ~key) rs in
        if List.exists Option.is_none bufs then None
        else Some (List.map Option.get bufs)
  in
  let size ~key =
    let url = key_url ~base_url key in
    Fetch.with_response client `HEAD url (fun resp ->
        match Fetch.status resp with
        | 200 -> declared_length resp
        | 404 | 410 -> None
        | s -> unexpected ~meth:"HEAD" ~url s)
  in
  {
    Store.get;
    get_range;
    get_ranges;
    size;
    ranged;
    set = None;
    erase = None;
    list = None;
  }
