(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type value =
  [ `Null
  | `Bool of bool
  | `Int of int64
  | `Float of float
  | `String of string
  | `Bytes of string
  | `Link of Cid.t
  | `Blob of Blob_ref.t
  | `List of value list
  | `Map of (string * value) list ]

(* Blob detection for IPLD values *)

let is_blob_map entries =
  let find k = List.assoc_opt k entries in
  match find "$type" with
  | Some (`String "blob") -> true
  | Some _ -> false
  | None -> (
      (* Legacy format: cid + mimeType *)
      match (find "cid", find "mimeType") with
      | Some (`String _), Some (`String _) -> true
      | _ -> false)

(* IPLD conversion *)

let rec of_dagcbor (v : Dagcbor.value) : value =
  match v with
  | `Map entries when is_blob_map entries ->
      `Blob (Blob_ref.of_dagcbor (`Map entries))
  | `Map entries -> `Map (List.map (fun (k, v) -> (k, of_dagcbor v)) entries)
  | `List items -> `List (List.map of_dagcbor items)
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Int i -> `Int i
  | `Float f -> `Float f
  | `String s -> `String s
  | `Bytes b -> `Bytes b
  | `Link cid -> `Link cid

let rec to_dagcbor (v : value) : Dagcbor.value =
  match v with
  | `Blob blob -> Blob_ref.to_dagcbor blob
  | `Map entries -> `Map (List.map (fun (k, v) -> (k, to_dagcbor v)) entries)
  | `List items -> `List (List.map to_dagcbor items)
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Int i -> `Int i
  | `Float f -> `Float f
  | `String s -> `String s
  | `Bytes b -> `Bytes b
  | `Link cid -> `Link cid

(* CBOR Encoding *)

let encode_cbor v = Dagcbor.encode_string (to_dagcbor v)
let decode_cbor s = of_dagcbor (Dagcbor.decode_string s)

let to_block v =
  let data = encode_cbor v in
  let cid = Cid.create `Dag_cbor data in
  (cid, data)

(* Jsont codec for lexicon values *)

module String_map = Map.Make (String)

(* Post-process decoded map to handle special encodings *)
let classify_map (m : value String_map.t) : value =
  let entries = String_map.bindings m in
  match entries with
  | [ ("$bytes", `String b64) ] -> (
      (* Bytes encoding *)
      match Base64.decode b64 with
      | Ok bytes -> `Bytes bytes
      | Error _ -> `Map entries)
  | [ ("$link", `String cid_str) ] ->
      (* Link encoding *)
      `Link (Cid.of_string cid_str)
  | _ -> (
      (* Check for blob ref *)
      match String_map.find_opt "$type" m with
      | Some (`String "blob") -> (
          match
            ( String_map.find_opt "ref" m,
              String_map.find_opt "mimeType" m,
              String_map.find_opt "size" m )
          with
          | Some (`Map ref_entries), Some (`String mime), Some (`Int size) -> (
              (* ref should be {"$link": "..."} *)
              match ref_entries with
              | [ ("$link", `String cid_str) ] ->
                  let cid = Cid.of_string cid_str in
                  `Blob { Blob_ref.cid; mime_type = mime; size }
              | _ -> `Map entries)
          | Some (`Map ref_entries), Some (`String mime), Some (`Float size)
            -> (
              match ref_entries with
              | [ ("$link", `String cid_str) ] ->
                  let cid = Cid.of_string cid_str in
                  `Blob
                    {
                      Blob_ref.cid;
                      mime_type = mime;
                      size = Int64.of_float size;
                    }
              | _ -> `Map entries)
          | _ -> `Map entries)
      | _ -> `Map entries)

(* Encoder for special values that encode as objects *)
let encode_object_value (v : value) : value String_map.t =
  match v with
  | `Bytes b -> String_map.singleton "$bytes" (`String (Base64.encode_string b))
  | `Link cid -> String_map.singleton "$link" (`String (Cid.to_string cid))
  | `Blob blob ->
      String_map.of_list
        [
          ("$type", `String "blob");
          ("ref", `Map [ ("$link", `String (Cid.to_string blob.cid)) ]);
          ("mimeType", `String blob.mime_type);
          ("size", `Int blob.size);
        ]
  | `Map entries -> String_map.of_list entries
  | _ -> assert false

let jsont : value Jsont.t =
  let rec value_t =
    lazy
      (let null_t =
         Jsont.map ~dec:(fun () -> `Null) ~enc:(fun _ -> ()) (Jsont.null ())
       in
       let bool_t =
         Jsont.map
           ~dec:(fun b -> `Bool b)
           ~enc:(function `Bool b -> b | _ -> assert false)
           Jsont.bool
       in
       let number_t =
         Jsont.map
           ~dec:(fun f ->
             if
               Float.is_integer f
               && (f >= Int64.(to_float min_int))
               && f <= Int64.(to_float max_int)
             then `Int (Int64.of_float f)
             else `Float f)
           ~enc:(function
             | `Int i -> Int64.to_float i | `Float f -> f | _ -> assert false)
           Jsont.number
       in
       let string_t =
         Jsont.map
           ~dec:(fun s -> `String s)
           ~enc:(function `String s -> s | _ -> assert false)
           Jsont.string
       in
       let array_t =
         Jsont.map
           ~dec:(fun items -> `List items)
           ~enc:(function `List items -> items | _ -> assert false)
           (Jsont.list (Jsont.rec' value_t))
       in
       let object_t =
         Jsont.map ~dec:classify_map ~enc:encode_object_value
           (Jsont.Object.as_string_map (Jsont.rec' value_t))
       in
       Jsont.any ~dec_null:null_t ~dec_bool:bool_t ~dec_number:number_t
         ~dec_string:string_t ~dec_array:array_t ~dec_object:object_t
         ~enc:(fun v ->
           match v with
           | `Null -> null_t
           | `Bool _ -> bool_t
           | `Int _ | `Float _ -> number_t
           | `String _ -> string_t
           | `List _ -> array_t
           | `Bytes _ | `Link _ | `Blob _ | `Map _ -> object_t)
         ())
  in
  Lazy.force value_t

(* Records *)

type record = (string * value) list

let record_type r =
  match List.assoc_opt "$type" r with Some (`String t) -> Some t | _ -> None

let encode_record_cbor r = encode_cbor (`Map r)

let decode_record_cbor s =
  match decode_cbor s with
  | `Map entries -> entries
  | _ -> failwith "record must be a map"

let record_to_block r = to_block (`Map r)

(* Blob finding *)

let rec find_blobs v =
  match v with
  | `Blob b -> [ b ]
  | `Map entries -> List.concat_map (fun (_, v) -> find_blobs v) entries
  | `List items -> List.concat_map find_blobs items
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Bytes _ | `Link _ -> []

(* Pretty printing *)

let rec pp ppf v =
  match v with
  | `Null -> Fmt.string ppf "null"
  | `Bool b -> Fmt.bool ppf b
  | `Int i -> Fmt.int64 ppf i
  | `Float f -> Fmt.float ppf f
  | `String s -> Fmt.pf ppf "%S" s
  | `Bytes b -> Fmt.pf ppf "<bytes:%d>" (String.length b)
  | `Link cid -> Fmt.pf ppf "<link:%a>" Cid.pp cid
  | `Blob b -> Fmt.pf ppf "<blob:%a>" Blob_ref.pp b
  | `List items -> Fmt.pf ppf "[%a]" (Fmt.list ~sep:Fmt.comma pp) items
  | `Map entries ->
      let pp_entry ppf (k, v) = Fmt.pf ppf "%S: %a" k pp v in
      Fmt.pf ppf "{%a}" (Fmt.list ~sep:Fmt.comma pp_entry) entries
