(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

(* An identity document is small, so a long answer is not one. *)
let limit = 1024 * 1024

let get http url =
  Fetch.with_response (Fetch.restrict http) `GET url (fun response ->
      let status = Fetch.status response in
      if status <> 200 then Error (Printf.sprintf "%s: HTTP %d" url status)
      else Ok (Fetch.decode ~limit Fetch.Media.octets response))

let member name = function
  | Jsont.Object (ms, _) -> (
      match Jsont.Json.find_mem name ms with
      | Some (_, v) -> Some v
      | None -> None)
  | _ -> None

let str = function Some (Jsont.String (s, _)) -> Some s | _ -> None

let pds_of_document json =
  match Jsont_bytesrw.decode_string Jsont.json json with
  | Error _ -> None
  | Ok doc -> (
      match member "service" doc with
      | Some (Jsont.Array (services, _)) ->
          List.find_map
            (fun s ->
              if str (member "type" s) = Some "AtprotoPersonalDataServer" then
                str (member "serviceEndpoint" s)
              else None)
            services
      | _ -> None)

let service_of_document ~id json =
  match Jsont_bytesrw.decode_string Jsont.json json with
  | Error _ -> None
  | Ok doc -> (
      let full = Option.map (fun did -> did ^ id) (str (member "id" doc)) in
      match member "service" doc with
      | Some (Jsont.Array (services, _)) ->
          List.find_map
            (fun s ->
              let sid = str (member "id" s) in
              if sid = Some id || (sid <> None && sid = full) then
                str (member "serviceEndpoint" s)
              else None)
            services
      | _ -> None)

(* The host of a [did:web] is a domain, with its port written as [%3A]. Nothing
   else may be encoded, so the host cannot reach another host or a path. *)
let web_host encoded =
  let is_host_char c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '.' || c = '-'
  in
  let is_port p = p <> "" && String.for_all (fun c -> c >= '0' && c <= '9') p in
  let is_host h = h <> "" && String.for_all is_host_char h in
  let b = Buffer.create (String.length encoded) in
  let n = String.length encoded in
  let rec decode i =
    if i < n then
      if
        i + 2 < n
        && encoded.[i] = '%'
        && encoded.[i + 1] = '3'
        && (encoded.[i + 2] = 'A' || encoded.[i + 2] = 'a')
      then (
        Buffer.add_char b ':';
        decode (i + 3))
      else (
        Buffer.add_char b encoded.[i];
        decode (i + 1))
  in
  decode 0;
  match String.split_on_char ':' (Buffer.contents b) with
  | [ host ] when is_host host -> Some host
  | [ host; port ] when is_host host && is_port port -> Some (host ^ ":" ^ port)
  | _ -> None

let document_url ?(plc = "https://plc.directory") did =
  match Atp.Did.of_string did with
  | Error _ -> Error (Printf.sprintf "%S is not a DID" did)
  | Ok _ -> (
      match String.split_on_char ':' did with
      | [ "did"; "plc"; _ ] -> Ok (plc ^ "/" ^ did)
      | [ "did"; "web"; host ] -> (
          match web_host host with
          | Some host ->
              Ok (Printf.sprintf "https://%s/.well-known/did.json" host)
          | None -> Error (Printf.sprintf "%s: invalid did:web host" did))
      | _ -> Error (Printf.sprintf "%s: unsupported DID method" did))

(* TODO: look TXT records up with a full DNS resolver. Asking Cloudflare
   over HTTPS is a stopgap: it depends on one public resolver, which sees
   every handle resolved. *)
let dns_over_https = "https://cloudflare-dns.com/dns-query"

(* A TXT record's data, as the JSON API gives it, is one or more quoted
   strings, which together are its value. *)
let txt_value data =
  let b = Buffer.create (String.length data) in
  let rec go i quoted =
    if i < String.length data then
      match data.[i] with
      | '"' -> go (i + 1) (not quoted)
      | '\\' when quoted && i + 1 < String.length data ->
          Buffer.add_char b data.[i + 1];
          go (i + 2) quoted
      | c ->
          if quoted then Buffer.add_char b c;
          go (i + 1) quoted
  in
  go 0 false;
  Buffer.contents b

(* The DID that the TXT record [_atproto.HANDLE] names with [did=DID]. More
   than one such record names none. *)
let did_of_dns http handle =
  let url =
    dns_over_https ^ "?name="
    ^ Uriz.pct_encode ~component:`Query_value ("_atproto." ^ handle)
    ^ "&type=TXT"
  in
  let* body =
    Fetch.with_response
      ~headers:[ Fetch.Header.raw "Accept" "application/dns-json" ]
      (Fetch.restrict http) `GET url
      (fun response ->
        if Fetch.status response <> 200 then
          Error (Printf.sprintf "%s: HTTP %d" url (Fetch.status response))
        else Ok (Fetch.decode ~limit Fetch.Media.octets response))
  in
  match Jsont_bytesrw.decode_string Jsont.json body with
  | Error _ -> Error "the DNS answer is not JSON"
  | Ok answer -> (
      let dids =
        match member "Answer" answer with
        | Some (Jsont.Array (records, _)) ->
            List.filter_map
              (fun r ->
                match (member "type" r, str (member "data" r)) with
                | Some (Jsont.Number (16., _)), Some data ->
                    let v = txt_value data in
                    if String.starts_with ~prefix:"did=" v then
                      Some (String.sub v 4 (String.length v - 4))
                    else None
                | _ -> None)
              records
        | _ -> []
      in
      match dids with
      | [ did ] when Atp.Did.is_valid did -> Ok did
      | [] -> Error (Printf.sprintf "%s has no _atproto TXT record" handle)
      | _ ->
          Error (Printf.sprintf "%s has an unusable _atproto TXT record" handle)
      )

let did_of_https http handle =
  let* body =
    get http (Printf.sprintf "https://%s/.well-known/atproto-did" handle)
  in
  let did = String.trim body in
  match Atp.Did.of_string did with
  | Ok _ -> Ok did
  | Error _ -> Error (Printf.sprintf "%s does not publish a DID" handle)

let did_of_handle http handle =
  match Atp.Handle.of_string handle with
  | Error _ -> Error (Printf.sprintf "%S is not a handle" handle)
  | Ok _ -> (
      (* DNS first, then HTTPS, as the handle specification allows either. A
         resolver that cannot be reached is no answer. *)
      match did_of_dns http handle with
      | Ok did -> Ok did
      | Error _ | (exception Eio.Io _) -> did_of_https http handle)

let document ?plc http did =
  let* url = document_url ?plc did in
  get http url

let pds_of_did ?plc http did =
  let* body = document ?plc http did in
  match pds_of_document body with
  | Some pds -> Ok pds
  | None -> Error (Printf.sprintf "%s lists no data server" did)

let service_of_did ?plc http did ~id =
  let* body = document ?plc http did in
  match service_of_document ~id body with
  | Some endpoint -> Ok endpoint
  | None -> Error (Printf.sprintf "%s lists no %s service" did id)

let pds_of_handle ?plc http handle =
  let* did = did_of_handle http handle in
  pds_of_did ?plc http did
