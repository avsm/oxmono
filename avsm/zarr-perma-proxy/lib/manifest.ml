(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module R = Coverage

type t = {
  url : string;
  absent : bool;
  generation : string;
  size : int;
  content_type : string;
  etag : string option;
  modified : string option;
  ranges : R.t list;
}

let version_jsont =
  Jsont.map ~kind:"manifest version"
    ~dec:(fun n -> if n <> 1. then
      Jsont.Error.msg Jsont.Meta.none "Unsupported cache manifest version")
    ~enc:(fun () -> 1.) Jsont.number

let validate m =
  let invalid message = Jsont.Error.msg Jsont.Meta.none message in
  if m.url = "" then invalid "Manifest URL is empty";
  if String.length m.generation <> 32 ||
      not (String.for_all
        (function '0'..'9' | 'a'..'f' -> true | _ -> false) m.generation)
  then invalid "Generation must contain 32 lowercase hexadecimal digits";
  if m.size < 0 then invalid "Object size is negative";
  if m.absent && (m.size <> 0 || m.ranges <> []) then
    invalid "Absent objects must have zero size and no coverage";
  let rec ordered previous = function
    | [] -> ()
    | r :: rs ->
        if r.R.start >= r.stop || r.stop > m.size then
          invalid "Coverage must be nonempty and inside the object";
        if r.start <= previous then
          invalid "Coverage must be sorted, disjoint and coalesced";
        ordered r.stop rs in
  ordered (-1) m.ranges

let jsont =
  let open Jsont.Object in
  map ~kind:"cache manifest" (fun () url absent generation size content_type etag modified ranges ->
      { url; absent; generation; size; content_type; etag; modified; ranges })
  |> mem "version" version_jsont ~enc:(fun _ -> ())
       ~dec_absent:(fun () -> ())
  |> mem "url" Jsont.string ~enc:(fun m -> m.url)
  |> mem "absent" Jsont.bool ~enc:(fun m -> m.absent)
  |> mem "generation" Jsont.string ~enc:(fun m -> m.generation)
  |> mem "size" R.offset_jsont ~enc:(fun m -> m.size)
  |> mem "content_type" Jsont.string ~enc:(fun m -> m.content_type)
  |> mem "etag" (Jsont.option Jsont.string) ~enc:(fun m -> m.etag)
  |> mem "modified" (Jsont.option Jsont.string) ~enc:(fun m -> m.modified)
  |> mem "ranges" (Jsont.list R.jsont) ~enc:(fun m -> m.ranges)
  |> error_unknown
  |> finish
  |> Jsont.iter ~dec:validate ~enc:validate

