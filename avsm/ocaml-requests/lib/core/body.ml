(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "requests.body" ~doc:"HTTP Request/Response Body"
module Log = (val Logs.src_log src : Logs.LOG)

type 'a part = {
  name : string;
  filename : string option;
  content_type : Mime.t;
  content : [`String of string | `Stream of Eio.Flow.source_ty Eio.Resource.t | `File of 'a Eio.Path.t];
}

type t =
  | Empty
  | String of { content : string; mime : Mime.t }
  | Stream of { source : Eio.Flow.source_ty Eio.Resource.t; mime : Mime.t; length : int64 option }
  | File : { file : 'a Eio.Path.t; mime : Mime.t } -> t
  | Multipart : { parts : 'a part list; boundary : string } -> t

let empty = Empty

let of_string mime content =
  String { content; mime }

let of_stream ?length mime source =
  Stream { source; mime; length }

let of_file ?mime file =
  let path = Eio.Path.native_exn file in
  let mime = Option.value mime ~default:(
    (* Use magic-mime library to guess MIME type from file extension *)
    let guessed_str = Magic_mime.lookup path in
    let guessed = Mime.of_string guessed_str in
    Log.debug (fun m -> m "Guessed MIME type %s for file %s" (Mime.to_string guessed) path);
    guessed
  ) in
  Log.debug (fun m -> m "Creating file body from %s with MIME type %s"
    path (Mime.to_string mime));
  File { file; mime }

let json_encoding_error e =
  let msg = Jsont.Error.to_string e in
  raise (Error.err (Error.Json_encode_error { reason = msg }))

(* For simple JSON encoding, we just take a Jsont.json value and encode it *)
let json (json_value : Jsont.json) =
  let content = match Jsont_bytesrw.encode_string' ~format:Jsont.Minify Jsont.json json_value with
    | Ok s -> s
    | Error e -> json_encoding_error e
  in
  String { content; mime = Mime.json }

(* Typed JSON encoding using a Jsont.t codec *)
let jsonv (type a) (codec : a Jsont.t) (value : a) =
  let content = match Jsont_bytesrw.encode_string' ~format:Jsont.Minify codec value with
    | Ok s -> s
    | Error e -> json_encoding_error e
  in
  String { content; mime = Mime.json }

(* JSON streaming using jsont - we encode the value to string and stream it *)
module Json_stream_source = struct
  type t = {
    content : string;
    mutable offset : int;
  }

  let single_read t dst =
    if t.offset >= String.length t.content then
      raise End_of_file
    else begin
      let available = String.length t.content - t.offset in
      let to_copy = min (Cstruct.length dst) available in
      Cstruct.blit_from_string t.content t.offset dst 0 to_copy;
      t.offset <- t.offset + to_copy;
      to_copy
    end

  let read_methods = []
end

let json_stream_source_create json_value =
  (* Encode the entire JSON value to string with minified format *)
  let content = match Jsont_bytesrw.encode_string' ~format:Jsont.Minify Jsont.json json_value with
    | Ok s -> s
    | Error e -> json_encoding_error e
  in
  let t = { Json_stream_source.content; offset = 0 } in
  let ops = Eio.Flow.Pi.source (module Json_stream_source) in
  Eio.Resource.T (t, ops)

let json_stream json_value =
  let source = json_stream_source_create json_value in
  Stream { source; mime = Mime.json; length = None }

let text content =
  String { content; mime = Mime.text }

let form params =
  let encode_param (k, v) =
    Printf.sprintf "%s=%s"
      (Uri.pct_encode ~component:`Query_value k)
      (Uri.pct_encode ~component:`Query_value v)
  in
  let content = String.concat "&" (List.map encode_param params) in
  String { content; mime = Mime.form }

let generate_boundary () =
  let random_bytes = Mirage_crypto_rng.generate 16 in
  (* Mirage_crypto_rng.generate returns a string, convert to Cstruct for hex encoding *)
  let random_part = Cstruct.to_hex_string (Cstruct.of_string random_bytes) in
  Printf.sprintf "----WebKitFormBoundary%s" random_part

let multipart parts =
  let boundary = generate_boundary () in
  Multipart { parts; boundary }

let content_type = function
  | Empty -> None
  | String { mime; _ } -> Some mime
  | Stream { mime; _ } -> Some mime
  | File { mime; _ } -> Some mime
  | Multipart { boundary; _ } ->
      Some (Mime.multipart_form |> Mime.with_param "boundary" boundary)

let content_length = function
  | Empty -> Some 0L
  | String { content; _ } -> Some (Int64.of_int (String.length content))
  | Stream { length; _ } -> length
  | File { file; _ } ->
      (* Try to get file size *)
      (try
        let stat = Eio.Path.stat ~follow:true file in
        Some (Optint.Int63.to_int64 stat.size)
      with _ -> None)
  | Multipart _ ->
      (* Complex to calculate, handled during sending *)
      None

(* Strings_source - A flow source that streams from a doubly-linked list of strings/flows *)
module Strings_source = struct
  type element =
    | String of string
    | Flow of Eio.Flow.source_ty Eio.Resource.t

  type t = {
    dllist : element Lwt_dllist.t;
    mutable current_element : element option;
    mutable string_offset : int;
  }

  let rec single_read t dst =
    match t.current_element with
    | None ->
        (* Try to get the first element from the list *)
        if Lwt_dllist.is_empty t.dllist then
          raise End_of_file
        else begin
          t.current_element <- Some (Lwt_dllist.take_l t.dllist);
          single_read t dst
        end
    | Some (String s) when t.string_offset >= String.length s ->
        (* Current string exhausted, move to next element *)
        t.current_element <- None;
        t.string_offset <- 0;
        single_read t dst
    | Some (String s) ->
        (* Read from current string *)
        let available = String.length s - t.string_offset in
        let to_read = min (Cstruct.length dst) available in
        Cstruct.blit_from_string s t.string_offset dst 0 to_read;
        t.string_offset <- t.string_offset + to_read;
        to_read
    | Some (Flow flow) ->
        (* Read from flow *)
        (try
           let n = Eio.Flow.single_read flow dst in
           if n = 0 then begin
             (* Flow exhausted, move to next element *)
             t.current_element <- None;
             single_read t dst
           end else n
         with End_of_file ->
           t.current_element <- None;
           single_read t dst)

  let read_methods = []  (* No special read methods *)

  let create () = {
    dllist = Lwt_dllist.create ();
    current_element = None;
    string_offset = 0;
  }

  let add_string t s =
    ignore (Lwt_dllist.add_r (String s) t.dllist)

  let add_flow t flow =
    ignore (Lwt_dllist.add_r (Flow flow) t.dllist)
end

let strings_source_create () =
  let t = Strings_source.create () in
  let ops = Eio.Flow.Pi.source (module Strings_source) in
  (t, Eio.Resource.T (t, ops))

let to_flow_source ~sw = function
  | Empty -> None
  | String { content; _ } -> Some (Eio.Flow.string_source content)
  | Stream { source; _ } -> Some source
  | File { file; _ } ->
      (* Open file and stream it directly without loading into memory *)
      let flow = Eio.Path.open_in ~sw file in
      Some (flow :> Eio.Flow.source_ty Eio.Resource.t)
  | Multipart { parts; boundary } ->
      (* Create a single strings_source with dllist for streaming *)
      let source, flow = strings_source_create () in

      List.iter (fun part ->
        (* Add boundary *)
        Strings_source.add_string source "--";
        Strings_source.add_string source boundary;
        Strings_source.add_string source "\r\n";

        (* Add Content-Disposition header *)
        Strings_source.add_string source "Content-Disposition: form-data; name=\"";
        Strings_source.add_string source part.name;
        Strings_source.add_string source "\"";
        (match part.filename with
         | Some f ->
             Strings_source.add_string source "; filename=\"";
             Strings_source.add_string source f;
             Strings_source.add_string source "\""
         | None -> ());
        Strings_source.add_string source "\r\n";

        (* Add Content-Type header *)
        Strings_source.add_string source "Content-Type: ";
        Strings_source.add_string source (Mime.to_string part.content_type);
        Strings_source.add_string source "\r\n\r\n";

        (* Add content *)
        (match part.content with
         | `String s ->
             Strings_source.add_string source s
         | `File file ->
             (* Open file and add as flow *)
             let file_flow = Eio.Path.open_in ~sw file in
             Strings_source.add_flow source (file_flow :> Eio.Flow.source_ty Eio.Resource.t)
         | `Stream stream ->
             (* Add stream directly *)
             Strings_source.add_flow source stream);

        (* Add trailing newline *)
        Strings_source.add_string source "\r\n"
      ) parts;

      (* Add final boundary *)
      Strings_source.add_string source "--";
      Strings_source.add_string source boundary;
      Strings_source.add_string source "--\r\n";

      Some flow

(* Private module *)
module Private = struct
  let to_flow_source = to_flow_source

  let to_string = function
    | Empty -> ""
    | String { content; _ } -> content
    | Stream _ -> invalid_arg "Body.Private.to_string: cannot convert streaming body (must be materialized first)"
    | File _ -> invalid_arg "Body.Private.to_string: cannot convert file body (must be read first)"
    | Multipart _ -> invalid_arg "Body.Private.to_string: cannot convert multipart body (must be encoded first)"

  let is_empty = function
    | Empty -> true
    | _ -> false

  let is_chunked = function
    | Empty -> false
    | String _ -> false
    | Stream { length = Some _; _ } -> false
    | Stream { length = None; _ } -> true
    | File _ -> false
    | Multipart _ -> true

  module Write = Eio.Buf_write

  let crlf w = Write.string w "\r\n"

  (** Copy from a flow source to the writer *)
  let write_stream w source =
    let buf = Cstruct.create 8192 in
    let rec copy () =
      match Eio.Flow.single_read source buf with
      | n ->
          Write.cstruct w (Cstruct.sub buf 0 n);
          copy ()
      | exception End_of_file -> ()
    in
    copy ()

  (** Write a chunk with hex size prefix *)
  let write_chunk w data len =
    Write.printf w "%x" len;
    crlf w;
    Write.cstruct w (Cstruct.sub data 0 len);
    crlf w

  (** Copy from a flow source using chunked transfer encoding *)
  let write_stream_chunked w source =
    let buf = Cstruct.create 8192 in
    let rec copy () =
      match Eio.Flow.single_read source buf with
      | n ->
          write_chunk w buf n;
          copy ()
      | exception End_of_file ->
          (* Final chunk *)
          Write.string w "0";
          crlf w;
          crlf w
    in
    copy ()

  let write ~sw w = function
    | Empty -> ()
    | String { content; _ } ->
        if content <> "" then Write.string w content
    | Stream { source; _ } ->
        write_stream w source
    | File { file; _ } ->
        let flow = Eio.Path.open_in ~sw file in
        write_stream w (flow :> Eio.Flow.source_ty Eio.Resource.t)
    | Multipart _ as body ->
        (* For multipart, get the flow source and write it *)
        (match to_flow_source ~sw body with
         | Some source -> write_stream w source
         | None -> ())

  let write_chunked ~sw w = function
    | Empty ->
        (* Empty body with chunked encoding is just final chunk *)
        Write.string w "0";
        crlf w;
        crlf w
    | String { content; _ } ->
        if content <> "" then begin
          Write.printf w "%x" (String.length content);
          crlf w;
          Write.string w content;
          crlf w
        end;
        Write.string w "0";
        crlf w;
        crlf w
    | Stream { source; _ } ->
        write_stream_chunked w source
    | File { file; _ } ->
        let flow = Eio.Path.open_in ~sw file in
        write_stream_chunked w (flow :> Eio.Flow.source_ty Eio.Resource.t)
    | Multipart _ as body ->
        (match to_flow_source ~sw body with
         | Some source -> write_stream_chunked w source
         | None ->
             Write.string w "0";
             crlf w;
             crlf w)
end
