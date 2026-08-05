(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type codec = [ `Raw | `Dag_cbor ]

(* Multicodec constants *)
let version_1 = 0x01
let codec_raw = 0x55
let codec_dag_cbor = 0x71
let hash_sha256 = 0x12
let _hash_length_sha256 = 0x20 (* 32 bytes *)

type t = {
  codec : codec;
  digest : string; (* Raw SHA-256 bytes, 32 or 0 bytes *)
  raw_bytes : string; (* Cached raw CID bytes without multibase prefix *)
}

type error =
  [ `Invalid_cid_version of int
  | `Invalid_cid_codec of int
  | `Invalid_cid_hash_codec of int
  | `Invalid_cid_length of int * int
  | `Invalid_cid_multibase of string
  | `Cid_trailing_bytes of int
  | `Cid_encode_error of string ]

let pp_error ppf = function
  | `Invalid_cid_version v ->
      Fmt.pf ppf "invalid CID version: %d (expected 1)" v
  | `Invalid_cid_codec c -> Fmt.pf ppf "invalid CID codec: 0x%02x" c
  | `Invalid_cid_hash_codec c ->
      Fmt.pf ppf "invalid CID hash codec: 0x%02x (expected 0x12 sha256)" c
  | `Invalid_cid_length (expected, got) ->
      Fmt.pf ppf "invalid CID length: expected %d, got %d" expected got
  | `Invalid_cid_multibase s -> Fmt.pf ppf "invalid multibase encoding: %s" s
  | `Cid_trailing_bytes n -> Fmt.pf ppf "CID has %d trailing bytes" n
  | `Cid_encode_error msg -> Fmt.pf ppf "CID encoding error: %s" msg

type Eio.Exn.err += E of error

let () = Eio_error.register_pp pp_error (function E e -> Some e | _ -> None)
let raise_error e = Eio_error.raise_ (E e)
let codec_to_byte = function `Raw -> codec_raw | `Dag_cbor -> codec_dag_cbor

let codec_of_byte = function
  | b when b = codec_raw -> `Raw
  | b when b = codec_dag_cbor -> `Dag_cbor
  | b -> raise_error (`Invalid_cid_codec b)

let make_raw_bytes codec digest =
  let digest_len = String.length digest in
  let buf = Buffer.create (4 + digest_len) in
  Buffer.add_uint8 buf version_1;
  Buffer.add_uint8 buf (codec_to_byte codec);
  Buffer.add_uint8 buf hash_sha256;
  Buffer.add_uint8 buf digest_len;
  Buffer.add_string buf digest;
  Buffer.contents buf

let create (codec : [< codec ]) data =
  let codec = (codec :> codec) in
  let digest = Digestif.SHA256.(digest_string data |> to_raw_string) in
  let raw_bytes = make_raw_bytes codec digest in
  { codec; digest; raw_bytes }

let empty (codec : [< codec ]) =
  let codec = (codec :> codec) in
  let raw_bytes = make_raw_bytes codec "" in
  { codec; digest = ""; raw_bytes }

let of_digest (codec : [< codec ]) digest =
  let codec = (codec :> codec) in
  if String.length digest <> 32 then
    invalid_arg "Cid.of_digest: digest must be 32 bytes";
  let raw_bytes = make_raw_bytes codec digest in
  { codec; digest; raw_bytes }

let of_raw_bytes_prefix_internal s =
  let len = String.length s in
  if len < 4 then raise_error (`Invalid_cid_length (4, len));
  let version = Char.code s.[0] in
  if version <> version_1 then raise_error (`Invalid_cid_version version);
  let codec_byte = Char.code s.[1] in
  let codec = codec_of_byte codec_byte in
  let hash_codec = Char.code s.[2] in
  if hash_codec <> hash_sha256 then
    raise_error (`Invalid_cid_hash_codec hash_codec);
  let digest_len = Char.code s.[3] in
  if digest_len <> 32 && digest_len <> 0 then
    raise_error (`Invalid_cid_length (32, digest_len));
  let expected_len = 4 + digest_len in
  if len < expected_len then
    raise_error (`Invalid_cid_length (expected_len, len));
  let digest = String.sub s 4 digest_len in
  let raw_bytes = String.sub s 0 expected_len in
  ({ codec; digest; raw_bytes }, expected_len)

let of_raw_bytes_prefix s =
  try of_raw_bytes_prefix_internal s
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "parsing CID from raw bytes (%d bytes)"
      (String.length s)

let of_raw_bytes s =
  try
    let cid, expected_len = of_raw_bytes_prefix_internal s in
    let len = String.length s in
    if len > expected_len then
      raise_error (`Cid_trailing_bytes (len - expected_len));
    cid
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "parsing CID from raw bytes (%d bytes)"
      (String.length s)

let of_bytes s =
  try
    let len = String.length s in
    (* Expected: 37 bytes (full) or 5 bytes (empty) *)
    if len <> 37 && len <> 5 then raise_error (`Invalid_cid_length (37, len));
    if len > 0 && Char.code s.[0] <> 0 then
      raise_error
        (`Invalid_cid_multibase
           (Printf.sprintf "expected 0x00 prefix, got 0x%02x" (Char.code s.[0])));
    of_raw_bytes (String.sub s 1 (len - 1))
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "parsing CID from bytes (%d bytes)"
      (String.length s)

let of_string s =
  try
    let len = String.length s in
    (* Expected: 59 chars (full) or 8 chars (empty) *)
    if len <> 59 && len <> 8 then raise_error (`Invalid_cid_length (59, len));
    match Multibase.decode s with
    | Ok (_, decoded) -> of_raw_bytes decoded
    | Error (`Msg msg) -> raise_error (`Invalid_cid_multibase msg)
    | Error (`Unsupported enc) ->
        raise_error
          (`Invalid_cid_multibase
             (Printf.sprintf "unsupported encoding: %s"
                (Multibase.Encoding.to_string enc)))
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "parsing CID from string %S" s

let to_raw_bytes t = t.raw_bytes

let to_bytes t =
  let buf = Buffer.create (1 + String.length t.raw_bytes) in
  Buffer.add_char buf '\x00';
  Buffer.add_string buf t.raw_bytes;
  Buffer.contents buf

let to_string t =
  match Multibase.encode `Base32 t.raw_bytes with
  | Ok s -> s
  | Error (`Msg msg) -> raise_error (`Cid_encode_error msg)
  | Error (`Unsupported enc) ->
      raise_error
        (`Cid_encode_error
           (Printf.sprintf "unsupported encoding: %s"
              (Multibase.Encoding.to_string enc)))

let codec t = t.codec
let digest t = t.digest
let version _ = 1
let is_empty t = String.length t.digest = 0
let equal a b = String.equal a.raw_bytes b.raw_bytes
let compare a b = String.compare a.raw_bytes b.raw_bytes
let hash t = Hashtbl.hash t.raw_bytes
let pp ppf t = Fmt.string ppf (to_string t)

(* JSON encoding: {"$link": "<base32-cid>"} *)
let jsont =
  Jsont.Object.map ~kind:"CID Link" (fun link -> of_string link)
  |> Jsont.Object.mem "$link" Jsont.string ~enc:(fun cid -> to_string cid)
  |> Jsont.Object.finish

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

(* AT Protocol format: simplified CID without multibase prefix or length byte.
   Per draft-holmgren-at-repository.md Section 6.5 (line 368). *)

let to_atproto_bytes t =
  let buf = Buffer.create 35 in
  Buffer.add_uint8 buf version_1;
  Buffer.add_uint8 buf (codec_to_byte t.codec);
  Buffer.add_uint8 buf hash_sha256;
  (* No length byte - AT Protocol omits it *)
  Buffer.add_string buf t.digest;
  Buffer.contents buf

let of_atproto_bytes s =
  try
    let len = String.length s in
    if len <> 35 then raise_error (`Invalid_cid_length (35, len));
    let version = Char.code s.[0] in
    if version <> version_1 then raise_error (`Invalid_cid_version version);
    let codec_byte = Char.code s.[1] in
    let codec = codec_of_byte codec_byte in
    let hash_codec = Char.code s.[2] in
    if hash_codec <> hash_sha256 then
      raise_error (`Invalid_cid_hash_codec hash_codec);
    (* No length byte in AT Protocol format - hash is remaining 32 bytes *)
    let digest = String.sub s 3 32 in
    (* Store standard raw_bytes for internal consistency *)
    let raw_bytes = make_raw_bytes codec digest in
    { codec; digest; raw_bytes }
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt
      "parsing CID from AT Protocol bytes (%d bytes)" (String.length s)
