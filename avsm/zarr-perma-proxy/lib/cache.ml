(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module R = Coverage

type selection = Whole | From of int * int option | Suffix of int
exception Unsatisfiable of int
exception Upstream of string
exception Changed

let error fmt = Printf.ksprintf (fun s -> raise (Upstream s)) fmt
let chunk_bytes = 8 * 1024 * 1024

open Manifest

type meta = Manifest.t

type t = {
  dir : Eio.Fs.dir_ty Eio.Path.t;
  client : Fetch.plain;
  locks : Eio.Mutex.t array;
  random : Eio.Flow.source_ty Eio.Resource.t;
  manifests : (string, meta option) Hashtbl.t;
}

type entry = { meta : meta; path : Eio.Fs.dir_ty Eio.Path.t }

let create ~dir ~client ~random =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  { dir; client; locks = Array.init 64 (fun _ -> Eio.Mutex.create ());
    random; manifests = Hashtbl.create 64 }

let key url = Digest.to_hex (Digest.string url)
let manifest t url = Eio.Path.(t.dir / (key url ^ ".json"))
let data_path t m = Eio.Path.(t.dir / (key m.url ^ "." ^ m.generation ^ ".data"))
let locked t url f =
  (* Failed transfers leave the published manifest valid. Unlock on exceptions
     rather than poisoning a stripe shared with other objects. *)
  Eio.Mutex.use_ro t.locks.((Hashtbl.hash url) mod 64) f

let exists path = Eio.Path.kind ~follow:false path <> `Not_found

let load_disk t url =
  let path = manifest t url in
  if not (exists path) then None else
  let m = match Jsont_bytesrw.decode_string Manifest.jsont (Eio.Path.load path) with
    | Ok m -> m
    | Error e -> error "Corrupt cache manifest for %s: %s" url e in
  if m.url <> url then error "Cache manifest URL differs from its key: %s" url;
  if m.ranges <> [] then begin
    let data = data_path t m in
    if not (exists data) then error "Missing cache data for %s" url;
    let size = Optint.Int63.to_int64 (Eio.Path.stat ~follow:false data).size in
    if List.exists (fun r -> Int64.of_int r.R.stop > size) m.ranges then
      error "Truncated cache data for %s" url
  end;
  Some m

let remember t url m =
  if Hashtbl.length t.manifests >= 1024 then Hashtbl.clear t.manifests;
  Hashtbl.replace t.manifests url m

let load t url =
  match Hashtbl.find_opt t.manifests url with
  | Some m -> m
  | None ->
      let m = load_disk t url in
      remember t url m;
      m

let save t m =
  let json = match Jsont_bytesrw.encode_string Manifest.jsont m with
    | Ok s -> s | Error e -> error "Cannot encode cache metadata: %s" e in
  let path = manifest t m.url in
  let tmp = Eio.Path.(t.dir / (key m.url ^ ".pending")) in
  Eio.Path.with_open_out ~create:(`Or_truncate 0o600) tmp (fun file ->
      Eio.Flow.copy_string json file;
      Eio.File.sync file);
  Eio.Path.rename tmp path;
  remember t m.url (Some m)

let new_meta t url size resp =
  let bytes = Cstruct.create 16 in
  Eio.Flow.read_exact t.random bytes;
  let generation = Digest.to_hex (Cstruct.to_string bytes) in
  {
    url; absent = false; generation; size; ranges = [];
    content_type = Option.value ~default:"application/octet-stream"
        (Http.Header.get (Fetch.headers resp) "content-type");
    etag = Http.Header.get (Fetch.headers resp) "etag";
    modified = Http.Header.get (Fetch.headers resp) "last-modified";
  }

let size64 n =
  if n < 0L || n > Int64.of_int max_int then error "Object size out of range";
  Int64.to_int n

let declared resp = Option.map size64
    (Fetch.header Fetch.Header.content_length resp)

let check_coding resp =
  match Http.Header.get (Fetch.headers resp) "content-encoding" with
  | None | Some "identity" -> ()
  | Some s -> error "Upstream content coding %s changes byte offsets" s

let same_version m resp size =
  let get name = Http.Header.get (Fetch.headers resp) name in
  if m.size <> size
      || (m.etag <> None && m.etag <> get "etag")
      || (m.etag = None && m.modified <> None && m.modified <> get "last-modified")
  then raise Changed

let resolve size = function
  | Whole -> R.v 0 size
  | From (off, len) ->
      if off < 0 || Option.fold ~none:false ~some:(fun n -> n <= 0) len then
        invalid_arg "Cache: invalid byte range";
      if off >= size then raise (Unsatisfiable size);
      let n = Option.fold ~none:(size - off) ~some:(min (size - off)) len in
      R.v off (off + n)
  | Suffix n ->
      if n <= 0 then invalid_arg "Cache: suffix must be positive";
      if size = 0 then raise (Unsatisfiable size);
      R.v (max 0 (size - n)) size

let range_header : selection -> Fetch.Header.headers = function
  | Whole -> Fetch.Header.[]
  | From (off, len) ->
      let last = Option.map (fun n ->
          if n <= 0 || off < 0 || n - 1 > max_int - off then
            invalid_arg "Cache: invalid byte range";
          Int64.of_int (off + n - 1)) len in
      Fetch.Header.[range, bytes [ `Range (Int64.of_int off, last) ]]
  | Suffix n -> Fetch.Header.[range, bytes [ `Suffix (Int64.of_int n) ]]

let identity = Fetch.Header.[accept_encoding, [pref "identity"]]

let conditions : meta option -> Fetch.Header.headers = function
  | None -> Fetch.Header.[]
  | Some m ->
      match m.etag with
      | Some tag when not (String.starts_with ~prefix:"W/" tag) ->
          Fetch.Header.[raw "If-Match" tag]
      | _ -> match m.modified with
        | Some modified_text -> Fetch.Header.[if_unmodified_since, modified_text]
        | None -> Fetch.Header.[]

(* Data reaches stable storage before coverage is published. If a body fails
   or a request is cancelled, its uncommitted bytes remain uncovered. *)
let receive t m ~off ~expected resp =
  let path = data_path t m in
  let got = ref 0 in
  Eio.Path.with_open_out ~create:(`If_missing 0o600) path (fun file ->
      let buffer = Cstruct.create 65536 in
      (try while true do
          let n = Eio.Flow.single_read (Fetch.body resp) buffer in
          if !got > max_int - n - off then error "Object too large";
          Option.iter (fun total -> if !got + n > total then
              error "Upstream body exceeds declared range length") expected;
          Eio.File.pwrite_all file
            ~file_offset:(Optint.Int63.of_int (off + !got))
            [ Cstruct.sub buffer 0 n ];
          got := !got + n
        done with End_of_file -> ());
      Option.iter (fun total -> if !got <> total then
          error "Upstream body truncated: %d of %d bytes" !got total) expected;
      Eio.File.sync file);
  !got

let fetch t url old selection =
  let headers = Fetch.Header.append identity
      (Fetch.Header.append (range_header selection) (conditions old)) in
  Fetch.with_response ~headers ~redirects:5 t.client `GET url (fun resp ->
      check_coding resp;
      match Fetch.status resp with
      | 404 | 410 ->
          save t { (new_meta t url 0 resp) with absent = true };
          None
      | 412 -> raise Changed
      | 416 ->
          let size = match Fetch.header Fetch.Header.content_range resp with
            | Some { range = None; complete_length = Some n; _ } -> size64 n
            | _ -> error "Upstream 416 without object length" in
          raise (Unsatisfiable size)
      | (200 | 206) as status ->
          let off, size, expected =
            if status = 200 then 0, declared resp, declared resp else
            match Fetch.header Fetch.Header.content_range resp with
            | Some { unit = "bytes"; range = Some (first, last);
                     complete_length = Some total } ->
                let total = size64 total and first = size64 first
                and last = size64 last in
                let want = resolve total selection in
                if first <> want.start || last + 1 <> want.stop then
                  error "Upstream returned a different byte range";
                let len = last - first + 1 in
                Option.iter (fun n -> if n <> len then
                    error "Content-Length disagrees with Content-Range") (declared resp);
                first, Some total, Some len
            | _ -> error "Upstream 206 without a complete Content-Range" in
          Option.iter (fun m -> same_version m resp
              (Option.value ~default:m.size size)) old;
          let m = match old with
            | Some m when status = 206 -> m
            | _ -> new_meta t url (Option.value ~default:0 size) resp in
          let got = receive t m ~off ~expected resp in
          let total = Option.value ~default:got size in
          if old <> None && m.size <> total then raise Changed;
          let m = { m with size = total;
                           ranges = R.merge m.ranges (R.v off (off + got)) } in
          save t m;
          Some m
      | status -> error "GET %s: HTTP %d" url status)

let head t url =
  Fetch.with_response ~headers:identity ~redirects:5 t.client `HEAD url (fun resp ->
      check_coding resp;
      match Fetch.status resp with
      | 404 | 410 ->
          save t { (new_meta t url 0 resp) with absent = true };
          None
      | 200 ->
          let size = match declared resp with
            | Some n -> n | None -> error "HEAD %s: missing Content-Length" url in
          let m = new_meta t url size resp in
          save t m;
          Some m
      | status -> error "HEAD %s: HTTP %d" url status)

let rec fill t url m wanted =
  match R.missing m.ranges wanted with
  | [] -> Some m
  | r :: _ ->
      let n = min chunk_bytes (r.stop - r.start) in
      match fetch t url (Some m) (From (r.start, Some n)) with
      | None -> None
      | Some m -> fill t url m wanted

let ensure t ~url ~head_only selection =
  locked t url (fun () ->
      let rec run attempts =
        try
          let m = match load t url with
            | Some m -> Some m
            | None -> if head_only then head t url else fetch t url None selection in
          match m with
          | None -> None
          | Some m when m.absent -> None
          | Some m ->
              let wanted = if head_only then R.v 0 0 else resolve m.size selection in
              let m = if head_only then Some m else fill t url m wanted in
              Option.map (fun meta -> { meta; path = data_path t meta }, wanted) m
        with Changed ->
          Hashtbl.remove t.manifests url;
          if exists (manifest t url) then Eio.Path.unlink (manifest t url);
          if attempts = 0 then error "Upstream changed repeatedly: %s" url;
          run (attempts - 1)
      in
      run 1)

let size e = e.meta.size
let content_type e = e.meta.content_type
let etag e = e.meta.etag
let modified e = e.meta.modified
let revision e = e.meta.generation
let complete e = R.covers e.meta.ranges (R.v 0 e.meta.size)

let read e r =
  if not (R.covers e.meta.ranges r) then invalid_arg "Cache.read: uncovered bytes";
  if r.start = r.stop then "" else
  Eio.Path.with_open_in e.path (fun file ->
      let b = Cstruct.create (r.stop - r.start) in
      Eio.File.pread_exact file ~file_offset:(Optint.Int63.of_int r.start) [b];
      Cstruct.to_string b)

let write e r sink =
  if not (R.covers e.meta.ranges r) then invalid_arg "Cache.write: uncovered bytes";
  if r.start <> r.stop then Eio.Path.with_open_in e.path (fun file ->
      let b = Cstruct.create 65536 in
      let rec loop pos =
        if pos < r.stop then begin
          let n = min (Cstruct.length b) (r.stop - pos) in
          let piece = Cstruct.sub b 0 n in
          Eio.File.pread_exact file ~file_offset:(Optint.Int63.of_int pos) [piece];
          Proffer.Body.Sink.write sink (Cstruct.to_string piece);
          loop (pos + n)
        end in
      loop r.start)

let cached t ~url = locked t url (fun () ->
    Option.bind (load t url) (fun meta ->
        if meta.absent then None else Some { meta; path = data_path t meta }))
let covered e r = R.covers e.meta.ranges r
