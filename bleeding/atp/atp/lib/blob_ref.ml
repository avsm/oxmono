(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { cid : Cid.t; mime_type : string; size : int64 }

type error =
  [ `Blob_ref_missing_field of string
  | `Blob_ref_invalid_type of string
  | `Blob_ref_invalid_format ]

let pp_error ppf = function
  | `Blob_ref_missing_field f -> Fmt.pf ppf "blob ref missing field: %s" f
  | `Blob_ref_invalid_type t -> Fmt.pf ppf "blob ref invalid $type: %s" t
  | `Blob_ref_invalid_format -> Fmt.string ppf "blob ref invalid format"

type Eio.Exn.err += E of error

let () = Eio_error.register_pp pp_error (function E e -> Some e | _ -> None)
let raise_error e = Eio_error.raise_ (E e)

let to_dagcbor t : Dagcbor.value =
  `Map
    [
      ("$type", `String "blob");
      ("mimeType", `String t.mime_type);
      ("ref", `Link t.cid);
      ("size", `Int t.size);
    ]

let find_field name entries = List.assoc_opt name entries

let require_field name entries extract =
  match Option.bind (find_field name entries) extract with
  | Some v -> v
  | None -> raise_error (`Blob_ref_missing_field name)

let of_dagcbor (v : Dagcbor.value) : t =
  try
    match v with
    | `Map entries -> (
        match find_field "$type" entries with
        | Some (`String "blob") ->
            let cid =
              require_field "ref" entries (function
                | `Link cid -> Some cid
                | _ -> None)
            in
            let mime_type =
              require_field "mimeType" entries (function
                | `String s -> Some s
                | _ -> None)
            in
            let size =
              require_field "size" entries (function
                | `Int i -> Some i
                | _ -> None)
            in
            { cid; mime_type; size }
        | Some (`String other) -> raise_error (`Blob_ref_invalid_type other)
        | Some _ -> raise_error (`Blob_ref_invalid_type "<non-string>")
        | None ->
            (* Legacy format with cid string *)
            let cid =
              require_field "cid" entries (function
                | `String s -> Some (Cid.of_string s)
                | _ -> None)
            in
            let mime_type =
              require_field "mimeType" entries (function
                | `String s -> Some s
                | _ -> None)
            in
            { cid; mime_type; size = 0L })
    | _ -> raise_error `Blob_ref_invalid_format
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "parsing blob ref from DAG-CBOR"

let equal a b =
  Cid.equal a.cid b.cid
  && String.equal a.mime_type b.mime_type
  && Int64.equal a.size b.size

let compare a b =
  let c = Cid.compare a.cid b.cid in
  if c <> 0 then c
  else
    let c = String.compare a.mime_type b.mime_type in
    if c <> 0 then c else Int64.compare a.size b.size

let pp ppf t =
  Fmt.pf ppf "{cid=%a, mime=%s, size=%Ld}" Cid.pp t.cid t.mime_type t.size

(* JSON encoding: {"$type": "blob", "ref": {"$link": "..."}, "mimeType": "...", "size": ...} *)
let jsont =
  Jsont.Object.map ~kind:"Blob" (fun _type ref_ mime_type size ->
      { cid = ref_; mime_type; size })
  |> Jsont.Object.mem "$type" Jsont.string ~enc:(fun _ -> "blob")
  |> Jsont.Object.mem "ref" Cid.jsont ~enc:(fun b -> b.cid)
  |> Jsont.Object.mem "mimeType" Jsont.string ~enc:(fun b -> b.mime_type)
  |> Jsont.Object.mem "size" Jsont.int64 ~enc:(fun b -> b.size)
  |> Jsont.Object.finish
