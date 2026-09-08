module Msgtype = struct
  type t =
    | Text
    | Emote
    | Notice
    | Image
    | File
    | Audio
    | Video
    | Location
    | Custom of string

  let to_string = function
    | Text -> "m.text"
    | Emote -> "m.emote"
    | Notice -> "m.notice"
    | Image -> "m.image"
    | File -> "m.file"
    | Audio -> "m.audio"
    | Video -> "m.video"
    | Location -> "m.location"
    | Custom s -> s

  let of_string = function
    | "m.text" -> Text
    | "m.emote" -> Emote
    | "m.notice" -> Notice
    | "m.image" -> Image
    | "m.file" -> File
    | "m.audio" -> Audio
    | "m.video" -> Video
    | "m.location" -> Location
    | s -> Custom s

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.of_of_string ~kind:"msgtype" ~enc:to_string (fun s ->
        Ok (of_string s))
end

module Text_message_content = struct
  type t = {
    body : string;
    msgtype : Msgtype.t;
    format : string option;
    formatted_body : string option;
  }

  let make ~body ?(msgtype = Msgtype.Text) ?(format = "org.matrix.custom.html")
      ?formatted_body () =
    match formatted_body with
    | None -> { body; msgtype; format = None; formatted_body = None }
    | Some fb ->
        { body; msgtype; format = Some format; formatted_body = Some fb }

  let body t = t.body
  let msgtype t = t.msgtype
  let format t = t.format
  let formatted_body t = t.formatted_body

  let pp ppf t =
    Format.fprintf ppf "@[<v>msgtype: %a@,body: %s@]" Msgtype.pp t.msgtype
      t.body

  let jsont =
    Jsont.Object.(
      map (fun body msgtype format formatted_body ->
          { body; msgtype; format; formatted_body })
      |> mem "body" Matrix_json.Codec.string ~enc:(fun t -> t.body)
      |> mem "msgtype" Msgtype.jsont ~enc:(fun t -> t.msgtype)
      |> opt_mem "format" Matrix_json.Codec.string ~enc:(fun t -> t.format)
      |> opt_mem "formatted_body" Matrix_json.Codec.string ~enc:(fun t ->
          t.formatted_body)
      |> finish)
end

module Media_info = struct
  type t = {
    mimetype : string option;
    size : int option;
    duration : int option;
    h : int option;
    w : int option;
    thumbnail_url : string option;
    thumbnail_info : Matrix_event_core.Image_info.t option;
  }

  let v ?mimetype ?size ?duration ?h ?w ?thumbnail_url ?thumbnail_info () =
    { mimetype; size; duration; h; w; thumbnail_url; thumbnail_info }

  let jsont =
    Jsont.Object.(
      map (fun mimetype size duration h w thumbnail_url thumbnail_info ->
          { mimetype; size; duration; h; w; thumbnail_url; thumbnail_info })
      |> opt_mem "mimetype" Matrix_json.Codec.string ~enc:(fun t -> t.mimetype)
      |> opt_mem "size" Matrix_json.Codec.int ~enc:(fun t -> t.size)
      |> opt_mem "duration" Matrix_json.Codec.int ~enc:(fun t -> t.duration)
      |> opt_mem "h" Matrix_json.Codec.int ~enc:(fun t -> t.h)
      |> opt_mem "w" Matrix_json.Codec.int ~enc:(fun t -> t.w)
      |> opt_mem "thumbnail_url" Matrix_json.Codec.string ~enc:(fun t ->
          t.thumbnail_url)
      |> opt_mem "thumbnail_info" Matrix_event_core.Image_info.jsont
           ~enc:(fun t -> t.thumbnail_info)
      |> finish)
end

module Media_message_content = struct
  type t = {
    body : string;
    msgtype : Msgtype.t;
    url : string option;
    info : Media_info.t option;
    file : encrypted_file option;
  }

  and encrypted_file = {
    url : string;
    key : Jsont.json;
    iv : string;
    hashes : (string * string) list;
    v : string;
  }

  let encrypted_file_jsont : encrypted_file Jsont.t =
    Jsont.Object.(
      map (fun url key iv hashes v -> { url; key; iv; hashes; v })
      |> mem "url" Matrix_json.Codec.string ~enc:(fun (f : encrypted_file) ->
          f.url)
      |> mem "key" Matrix_json.Codec.json ~enc:(fun (f : encrypted_file) ->
          f.key)
      |> mem "iv" Matrix_json.Codec.string ~enc:(fun (f : encrypted_file) ->
          f.iv)
      |> mem "hashes" (Matrix_string_map.jsont Matrix_json.Codec.string)
           ~enc:(fun (f : encrypted_file) -> f.hashes)
      |> mem "v" Matrix_json.Codec.string ~enc:(fun (f : encrypted_file) -> f.v)
      |> finish)

  let jsont =
    Jsont.Object.(
      map (fun body msgtype url info file -> { body; msgtype; url; info; file })
      |> mem "body" Matrix_json.Codec.string ~enc:(fun t -> t.body)
      |> mem "msgtype" Msgtype.jsont ~enc:(fun t -> t.msgtype)
      |> opt_mem "url" Matrix_json.Codec.string ~enc:(fun t -> t.url)
      |> opt_mem "info" Media_info.jsont ~enc:(fun t -> t.info)
      |> opt_mem "file" encrypted_file_jsont ~enc:(fun t -> t.file)
      |> finish)
end

module Sticker_content = struct
  type t = { body : string; info : Media_info.t option; url : string }

  let jsont =
    Jsont.Object.(
      map (fun body info url -> { body; info; url })
      |> mem "body" Matrix_json.Codec.string ~enc:(fun t -> t.body)
      |> opt_mem "info" Media_info.jsont ~enc:(fun t -> t.info)
      |> mem "url" Matrix_json.Codec.string ~enc:(fun t -> t.url)
      |> finish)
end

module Location_message_content = struct
  type location_info = { uri : string; description : string option }

  let location_info_jsont =
    Jsont.Object.(
      map (fun uri description -> { uri; description })
      |> mem "uri" Matrix_json.Codec.string ~enc:(fun t -> t.uri)
      |> opt_mem "description" Matrix_json.Codec.string ~enc:(fun t ->
          t.description)
      |> finish)

  type t = {
    body : string;
    msgtype : Msgtype.t;
    geo_uri : string;
    info : location_info option;
  }

  let jsont =
    Jsont.Object.(
      map (fun body msgtype geo_uri info -> { body; msgtype; geo_uri; info })
      |> mem "body" Matrix_json.Codec.string ~enc:(fun t -> t.body)
      |> mem "msgtype" Msgtype.jsont ~enc:(fun t -> t.msgtype)
      |> mem "geo_uri" Matrix_json.Codec.string ~enc:(fun t -> t.geo_uri)
      |> opt_mem "info" location_info_jsont ~enc:(fun t -> t.info)
      |> finish)
end
