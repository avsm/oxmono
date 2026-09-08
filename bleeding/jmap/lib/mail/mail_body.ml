(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Value = struct
  type t = { value : string; is_encoding_problem : bool; is_truncated : bool }

  let v ?(is_encoding_problem = false) ?(is_truncated = false) value =
    { value; is_encoding_problem; is_truncated }

  let jsont =
    let kind = "EmailBodyValue" in
    let make value is_encoding_problem is_truncated =
      { value; is_encoding_problem; is_truncated }
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "value" Jsont.string ~enc:(fun t -> t.value)
    |> Jsont.Object.mem "isEncodingProblem" Jsont.bool ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.is_encoding_problem)
         ~enc_omit:(fun b -> not b)
    |> Jsont.Object.mem "isTruncated" Jsont.bool ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.is_truncated)
         ~enc_omit:(fun b -> not b)
    |> Jsont.Object.finish
end

module Part = struct
  type t = {
    part_id : string option;
    blob_id : Proto_id.t option;
    size : int64 option;
    headers : Mail_header.t list option;
    name : string option;
    type_ : string option;
    charset : string option;
    disposition : string option;
    cid : string option;
    language : string list option;
    location : string option;
    sub_parts : t list option;
    unknown : Proto_unknown.t;
  }

  let v ?part_id ?blob_id ?size ?headers ?name ?type_ ?charset ?disposition ?cid
      ?language ?location ?sub_parts ?(unknown = Proto_unknown.empty) () =
    {
      part_id;
      blob_id;
      size;
      headers;
      name;
      type_;
      charset;
      disposition;
      cid;
      language;
      location;
      sub_parts;
      unknown;
    }

  let unknown_member t name = Proto_unknown.find t.unknown name

  let header_property t name =
    match unknown_member t name with
    | Some (Jsont.String (s, _)) -> Some s
    | _ -> None

  let jsont =
    let kind = "EmailBodyPart" in
    let make part_id blob_id size headers name type_ charset disposition cid
        language location sub_parts unknown =
      {
        part_id;
        blob_id;
        size;
        headers;
        name;
        type_;
        charset;
        disposition;
        cid;
        language;
        location;
        sub_parts;
        unknown;
      }
    in
    (* Many members are [T|null] (RFC 8621 Section 4.1.4). *)
    Jsont.Portable_lazy.from_fun_fixed (fun jsont ->
      Jsont.Object.map ~kind make
      |> Proto_json_map.nullable_mem "partId" Jsont.string ~enc:(fun t ->
          t.part_id)
      |> Proto_json_map.nullable_mem "blobId" Proto_id.jsont ~enc:(fun t ->
          t.blob_id)
      |> Jsont.Object.opt_mem "size" Proto_int53.Unsigned.jsont ~enc:(fun t ->
          t.size)
      |> Jsont.Object.opt_mem "headers" (Jsont.list Mail_header.jsont)
           ~enc:(fun t -> t.headers)
      |> Proto_json_map.nullable_mem "name" Jsont.string ~enc:(fun t -> t.name)
      (* [type] is a required response member but a client may restrict
         bodyProperties to exclude it (RFC 8621 Section 4.2). *)
      |> Jsont.Object.opt_mem "type" Jsont.string ~enc:(fun t -> t.type_)
      |> Proto_json_map.nullable_mem "charset" Jsont.string ~enc:(fun t ->
          t.charset)
      |> Proto_json_map.nullable_mem "disposition" Jsont.string ~enc:(fun t ->
          t.disposition)
      |> Proto_json_map.nullable_mem "cid" Jsont.string ~enc:(fun t -> t.cid)
      |> Proto_json_map.nullable_mem "language" (Jsont.list Jsont.string)
           ~enc:(fun t -> t.language)
      |> Proto_json_map.nullable_mem "location" Jsont.string ~enc:(fun t ->
          t.location)
      |> Proto_json_map.nullable_mem "subParts"
           (Jsont.list (Jsont.rec' jsont))
           ~enc:(fun t -> t.sub_parts)
      (* RFC 8621 Section 4.1.4: [bodyProperties] may name "header:*"
         properties, which are not members of the type above; keeping the
         unknown members is what makes them reachable and re-encodable. *)
      |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
      |> Jsont.Object.finish)

  let jsont = Jsont.Portable_lazy.force jsont
end
