(*---------------------------------------------------------------------------
   Copyright (c) 2024 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Unknown = struct
  type t = Jsont.json

  let empty = Jsont.Object ([], Jsont.Meta.none)
  let is_empty = function Jsont.Object ([], _) -> true | _ -> false
end

type t = {
  url : string;
  mime_type : string;
  title : string option;
  size_in_bytes : int64 option;
  duration_in_seconds : int option;
  unknown : Unknown.t;
}

let create ~url ~mime_type ?title ?size_in_bytes ?duration_in_seconds
    ?(unknown = Unknown.empty) () =
  { url; mime_type; title; size_in_bytes; duration_in_seconds; unknown }

let url t = t.url
let mime_type t = t.mime_type
let title t = t.title
let size_in_bytes t = t.size_in_bytes
let duration_in_seconds t = t.duration_in_seconds
let unknown t = t.unknown

let equal a b =
  a.url = b.url && a.mime_type = b.mime_type && a.title = b.title
  && a.size_in_bytes = b.size_in_bytes
  && a.duration_in_seconds = b.duration_in_seconds

let pp ppf t =
  (* Extract filename from URL *)
  let filename =
    try
      let parts = String.split_on_char '/' t.url in
      List.nth parts (List.length parts - 1)
    with _ -> t.url
  in

  Format.fprintf ppf "%s (%s" filename t.mime_type;

  Option.iter
    (fun size ->
      let mb = Int64.to_float size /. (1024. *. 1024.) in
      Format.fprintf ppf ", %.1f MB" mb)
    t.size_in_bytes;

  Option.iter
    (fun duration ->
      let mins = duration / 60 in
      let secs = duration mod 60 in
      Format.fprintf ppf ", %dm%ds" mins secs)
    t.duration_in_seconds;

  Format.fprintf ppf ")"

let jsont =
  let kind = "Attachment" in
  let doc = "An attachment object" in
  let create_obj url mime_type title size_in_bytes duration_in_seconds unknown =
    create ~url ~mime_type ?title ?size_in_bytes ?duration_in_seconds ~unknown
      ()
  in
  Jsont.Object.map ~kind ~doc create_obj
  |> Jsont.Object.mem "url" Jsont.string ~enc:url
  |> Jsont.Object.mem "mime_type" Jsont.string ~enc:mime_type
  |> Jsont.Object.opt_mem "title" Jsont.string ~enc:title
  |> Jsont.Object.opt_mem "size_in_bytes" Jsont.int64 ~enc:size_in_bytes
  |> Jsont.Object.opt_mem "duration_in_seconds" Jsont.int
       ~enc:duration_in_seconds
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
  |> Jsont.Object.finish
