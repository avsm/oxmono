(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module StringMap = Map.Make(String)

type entry_annotation = { slugs : string list }

type t = (string, entry_annotation) Hashtbl.t

let empty () = Hashtbl.create 16

let add_slug t ~url ~slug =
  let ann = match Hashtbl.find_opt t url with
    | Some a -> a
    | None -> { slugs = [] }
  in
  if not (List.mem slug ann.slugs) then
    Hashtbl.replace t url { slugs = ann.slugs @ [slug] }

let slugs_for_url t url =
  match Hashtbl.find_opt t url with
  | Some a -> a.slugs
  | None -> []

let entry_annotation_jsont =
  let open Jsont in
  let open Jsont.Object in
  map ~kind:"EntryAnnotation" (fun slugs -> { slugs })
  |> mem "slugs" (list string) ~enc:(fun a -> a.slugs)
  |> finish

let json_t =
  let map_jsont = Jsont.Object.as_string_map entry_annotation_jsont in
  Jsont.map ~kind:"Annotations"
    ~dec:(fun m ->
      let tbl = Hashtbl.create (StringMap.cardinal m) in
      StringMap.iter (fun k v -> Hashtbl.replace tbl k v) m;
      tbl)
    ~enc:(fun tbl ->
      Hashtbl.fold (fun k v acc -> StringMap.add k v acc) tbl StringMap.empty)
    map_jsont

let load path =
  try
    let data = Eio.Path.load path in
    let reader = Bytesrw.Bytes.Reader.of_string data in
    match Jsont_bytesrw.decode json_t reader with
    | Ok tbl -> tbl
    | Error msg ->
      Logs.warn (fun m -> m "Failed to decode annotations: %s" msg);
      empty ()
  with _ -> empty ()

let save path t =
  let buf = Buffer.create 1024 in
  let writer = Bytesrw.Bytes.Writer.of_buffer buf in
  match Jsont_bytesrw.encode json_t t ~eod:true writer with
  | Ok () -> Eio.Path.save ~create:(`Or_truncate 0o644) path (Buffer.contents buf)
  | Error err -> failwith ("Failed to encode annotations: " ^ err)
