let obj members =
  Jsont.Json.object'
    (List.map (fun (n, v) -> Jsont.Json.mem (Jsont.Json.name n) v) members)

let merge_extra_content base ?extra_content () =
  let members what =
    match what with
    | Jsont.Object (members, _) -> members
    | _ ->
        invalid_arg
          "Matrix_client.Json_codec.merge_extra_content: object expected"
  in
  let base_members = members base in
  match extra_content with
  | None -> base
  | Some extra ->
      let extra_members = members extra in
      let base_names = Jsont.Json.object_names base_members in
      let extra_members =
        List.filter
          (fun (name, _) -> not (List.mem (fst name) base_names))
          extra_members
      in
      Jsont.Json.object' (base_members @ extra_members)

let string_map = Matrix_proto.Json.Codec.string_map

let keyed_map ?skip_invalid ~what ~of_string ~to_string codec =
  Matrix_proto.Json.Codec.keyed_map ?skip_invalid ~what ~of_string ~to_string
    codec

let uri : Uriz.t Jsont.t =
  Jsont.of_of_string ~kind:"uri" ~enc:Uriz.to_string (fun s ->
      match Uriz.of_string s with
      | This uri -> Ok uri
      | Null -> Error "invalid URI reference")

let ptime = Matrix_proto.Json.Codec.ptime

let persisted_timestamp =
  Jsont.map ~dec:Matrix_proto.Event.Timestamp.of_ms
    ~enc:Matrix_proto.Event.Timestamp.to_ms Matrix_proto.Json.Codec.Legacy.int64
