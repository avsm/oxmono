module Id = Matrix_proto.Id

type draft_type = New_message | Reply of Id.Event_id.t | Edit of Id.Event_id.t

type thumbnail = {
  filename : string;
  data : string;
  mimetype : string option;
  width : int64 option;
  height : int64 option;
  size : int64 option;
}

type attachment_content =
  | Image of {
      data : string;
      mimetype : string option;
      size : int64 option;
      width : int64 option;
      height : int64 option;
      blurhash : string option;
      thumbnail : thumbnail option;
    }
  | Video of {
      data : string;
      mimetype : string option;
      size : int64 option;
      width : int64 option;
      height : int64 option;
      duration_ms : int64 option;
      blurhash : string option;
      thumbnail : thumbnail option;
    }
  | Audio of {
      data : string;
      mimetype : string option;
      size : int64 option;
      duration_ms : int64 option;
    }
  | File of { data : string; mimetype : string option; size : int64 option }

type attachment = { filename : string; content : attachment_content }

type t = {
  plain_text : string;
  html_text : string option;
  draft_type : draft_type;
  attachments : attachment list;
}

let json_string = Jsont.Json.string
let json_int64 = Jsont.Json.int64
let json_null = Jsont.Json.null ()
let member name value = Jsont.Json.mem (Jsont.Json.name name) value

let optional name = function
  | None -> [ member name json_null ]
  | Some value -> [ member name value ]

let object' members = Jsont.Json.object' members

let find name json =
  match json with
  | Jsont.Object (members, _) ->
      Option.map snd (Jsont.Json.find_mem name members)
  | _ -> None

let required name json =
  match find name json with
  | Some value -> value
  | None ->
      Jsont.Error.msgf Jsont.Meta.none "missing composer draft member %S" name

let string name json =
  match required name json with
  | Jsont.String (value, _) -> value
  | _ ->
      Jsont.Error.msgf Jsont.Meta.none
        "composer draft member %S is not a string" name

let optional_string name json =
  match find name json with
  | None | Some (Jsont.Null _) -> None
  | Some (Jsont.String (value, _)) -> Some value
  | Some _ ->
      Jsont.Error.msgf Jsont.Meta.none
        "composer draft member %S is not a string" name

let int64 name json =
  match required name json with
  | Jsont.Number (value, _) when Float.is_integer value -> Int64.of_float value
  | Jsont.String (value, _) -> (
      match Int64.of_string_opt value with
      | Some value -> value
      | None ->
          Jsont.Error.msgf Jsont.Meta.none
            "composer draft member %S is not an integer" name)
  | _ ->
      Jsont.Error.msgf Jsont.Meta.none
        "composer draft member %S is not an integer" name

let optional_int64 name json =
  match find name json with
  | None | Some (Jsont.Null _) -> None
  | Some value -> Some (int64 name (object' [ member name value ]))

let base64_encode = Matrix_proto.Base64.encode

let base64_decode name json =
  let value = string name json in
  match Matrix_proto.Base64.decode value with
  | Ok value -> value
  | Error (`Msg message) ->
      Jsont.Error.msgf Jsont.Meta.none "invalid base64 in composer draft %S: %s"
        name message

let encode_thumbnail (t : thumbnail) =
  object'
    (member "filename" (json_string t.filename)
     :: member "data" (json_string (base64_encode t.data))
     :: optional "mimetype" (Option.map json_string t.mimetype)
    @ optional "width" (Option.map json_int64 t.width)
    @ optional "height" (Option.map json_int64 t.height)
    @ optional "size" (Option.map json_int64 t.size))

let decode_thumbnail json =
  {
    filename = string "filename" json;
    data = base64_decode "data" json;
    mimetype = optional_string "mimetype" json;
    width = optional_int64 "width" json;
    height = optional_int64 "height" json;
    size = optional_int64 "size" json;
  }

let optional_thumbnail name json =
  match find name json with
  | None | Some (Jsont.Null _) -> None
  | Some value -> Some (decode_thumbnail value)

let encode_content = function
  | Image x ->
      object'
        (member "type" (json_string "Image")
         :: member "data" (json_string (base64_encode x.data))
         :: optional "mimetype" (Option.map json_string x.mimetype)
        @ optional "size" (Option.map json_int64 x.size)
        @ optional "width" (Option.map json_int64 x.width)
        @ optional "height" (Option.map json_int64 x.height)
        @ optional "blurhash" (Option.map json_string x.blurhash)
        @ optional "thumbnail" (Option.map encode_thumbnail x.thumbnail))
  | Video x ->
      object'
        (member "type" (json_string "Video")
         :: member "data" (json_string (base64_encode x.data))
         :: optional "mimetype" (Option.map json_string x.mimetype)
        @ optional "size" (Option.map json_int64 x.size)
        @ optional "width" (Option.map json_int64 x.width)
        @ optional "height" (Option.map json_int64 x.height)
        @ optional "duration_ms" (Option.map json_int64 x.duration_ms)
        @ optional "blurhash" (Option.map json_string x.blurhash)
        @ optional "thumbnail" (Option.map encode_thumbnail x.thumbnail))
  | Audio x ->
      object'
        (member "type" (json_string "Audio")
         :: member "data" (json_string (base64_encode x.data))
         :: optional "mimetype" (Option.map json_string x.mimetype)
        @ optional "size" (Option.map json_int64 x.size)
        @ optional "duration_ms" (Option.map json_int64 x.duration_ms))
  | File x ->
      object'
        (member "type" (json_string "File")
         :: member "data" (json_string (base64_encode x.data))
         :: optional "mimetype" (Option.map json_string x.mimetype)
        @ optional "size" (Option.map json_int64 x.size))

let decode_content json =
  match string "type" json with
  | "Image" ->
      Image
        {
          data = base64_decode "data" json;
          mimetype = optional_string "mimetype" json;
          size = optional_int64 "size" json;
          width = optional_int64 "width" json;
          height = optional_int64 "height" json;
          blurhash = optional_string "blurhash" json;
          thumbnail = optional_thumbnail "thumbnail" json;
        }
  | "Video" ->
      Video
        {
          data = base64_decode "data" json;
          mimetype = optional_string "mimetype" json;
          size = optional_int64 "size" json;
          width = optional_int64 "width" json;
          height = optional_int64 "height" json;
          duration_ms = optional_int64 "duration_ms" json;
          blurhash = optional_string "blurhash" json;
          thumbnail = optional_thumbnail "thumbnail" json;
        }
  | "Audio" ->
      Audio
        {
          data = base64_decode "data" json;
          mimetype = optional_string "mimetype" json;
          size = optional_int64 "size" json;
          duration_ms = optional_int64 "duration_ms" json;
        }
  | "File" ->
      File
        {
          data = base64_decode "data" json;
          mimetype = optional_string "mimetype" json;
          size = optional_int64 "size" json;
        }
  | value ->
      Jsont.Error.msgf Jsont.Meta.none "unknown composer attachment type %S"
        value

let encode_attachment t =
  object'
    [
      member "filename" (json_string t.filename);
      member "content" (encode_content t.content);
    ]

let decode_attachment json =
  {
    filename = string "filename" json;
    content = decode_content (required "content" json);
  }

let encode_draft t =
  object'
    (member "plain_text" (json_string t.plain_text)
     :: optional "html_text" (Option.map json_string t.html_text)
    @ [
        member "draft_type"
          (match t.draft_type with
          | New_message -> json_string "NewMessage"
          | Reply id ->
              object'
                [
                  member "Reply"
                    (object'
                       [
                         member "event_id"
                           (json_string (Id.Event_id.to_string id));
                       ]);
                ]
          | Edit id ->
              object'
                [
                  member "Edit"
                    (object'
                       [
                         member "event_id"
                           (json_string (Id.Event_id.to_string id));
                       ]);
                ]);
        member "attachments"
          (Jsont.Json.list (List.map encode_attachment t.attachments));
      ])

let decode_draft json =
  let draft_type =
    match required "draft_type" json with
    | Jsont.String ("NewMessage", _) -> New_message
    | value -> (
        let parse_external details make =
          let event_id =
            match Id.Event_id.of_string (string "event_id" details) with
            | Ok id -> id
            | Error (`Msg message) -> Jsont.Error.msg Jsont.Meta.none message
          in
          make event_id
        in
        match (find "Reply" value, find "Edit" value, find "type" value) with
        | Some details, _, _ -> parse_external details (fun id -> Reply id)
        | _, Some details, _ -> parse_external details (fun id -> Edit id)
        | _, _, Some _ -> (
            (* Accept the short internal representation written by an early
               development version of this slot. *)
            match string "type" value with
            | "Reply" -> parse_external value (fun id -> Reply id)
            | "Edit" -> parse_external value (fun id -> Edit id)
            | kind ->
                Jsont.Error.msgf Jsont.Meta.none
                  "unknown composer draft type %S" kind)
        | _ -> Jsont.Error.msgf Jsont.Meta.none "unknown composer draft type")
  in
  let attachments =
    match find "attachments" json with
    | None -> []
    | Some (Jsont.Array (values, _)) -> List.map decode_attachment values
    | Some _ ->
        Jsont.Error.msg Jsont.Meta.none "composer attachments is not an array"
  in
  {
    plain_text = string "plain_text" json;
    html_text = optional_string "html_text" json;
    draft_type;
    attachments;
  }

let draft_jsont =
  Jsont.map ~kind:"composer draft" ~dec:decode_draft ~enc:encode_draft
    Matrix_proto.Json.Codec.json

type entry = { room_id : string; thread_root : string option; draft : t }

let encode_entry e =
  object'
    (member "room_id" (json_string e.room_id)
     :: optional "thread_root" (Option.map json_string e.thread_root)
    @ [ member "draft" (encode_draft e.draft) ])

let decode_entry json =
  {
    room_id = string "room_id" json;
    thread_root = optional_string "thread_root" json;
    draft = decode_draft (required "draft" json);
  }

let entries_jsont =
  Jsont.map ~kind:"composer drafts"
    ~dec:(fun values -> List.map decode_entry values)
    ~enc:(fun values -> List.map encode_entry values)
    (Jsont.list Matrix_proto.Json.Codec.json)

let store_slot = Store.Slot.v ~name:"composer_draft" entries_jsont

let key room_id thread_root =
  (Id.Room_id.to_string room_id, Option.map Id.Event_id.to_string thread_root)

let load_entries store =
  let open Result.Syntax in
  let+ entries = Store.Slot.find store store_slot in
  Option.value entries ~default:[]

let save store ~room_id ?thread_root draft =
  let open Result.Syntax in
  let* entries = load_entries store in
  let room_id, thread_root = key room_id thread_root in
  let entries =
    { room_id; thread_root; draft }
    :: List.filter
         (fun e ->
           not
             (String.equal e.room_id room_id
             && Option.equal String.equal e.thread_root thread_root))
         entries
  in
  Store.Slot.set store store_slot entries

let load store ~room_id ?thread_root () =
  let open Result.Syntax in
  let+ entries = load_entries store in
  let room_id, thread_root = key room_id thread_root in
  List.find_opt
    (fun e ->
      String.equal e.room_id room_id
      && Option.equal String.equal e.thread_root thread_root)
    entries
  |> Option.map (fun e -> e.draft)

let clear store ~room_id ?thread_root () =
  let open Result.Syntax in
  let* entries = load_entries store in
  let room_id, thread_root = key room_id thread_root in
  let entries =
    List.filter
      (fun e ->
        not
          (String.equal e.room_id room_id
          && Option.equal String.equal e.thread_root thread_root))
      entries
  in
  Store.Slot.set store store_slot entries
