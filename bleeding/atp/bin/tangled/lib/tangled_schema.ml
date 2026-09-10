(* SPDX-License-Identifier: ISC *)
let documents = Tangled_documents.documents
let get codec json = Tangled_api.decode codec json

let member key = function
  | Jsont.Object (members, _) ->
      List.find_map
        (fun ((name, _), value) -> if name = key then Some value else None)
        members
  | _ -> None

let field key json =
  match member key json with
  | Some value -> value
  | None -> invalid_arg ("Missing " ^ key)

let text json = get Jsont.string json
let strings json = get (Jsont.list Jsont.string) json

let document nsid =
  match List.assoc_opt nsid documents with
  | None -> invalid_arg ("Unknown vendored lexicon: " ^ nsid)
  | Some source -> (
      match Jsont_bytesrw.decode_string Jsont.json source with
      | Ok value -> value
      | Error message -> failwith message)

let main nsid = field "main" (field "defs" (document nsid))

let kind nsid =
  match member "main" (field "defs" (document nsid)) with
  | None -> "defs"
  | Some main -> text (field "type" main)

let rec validate ?(depth = 0) ~nsid schema value =
  if depth > 64 then invalid_arg "Schema validation exceeds 64 levels";
  let recurse = validate ~depth:(depth + 1) ~nsid in
  let bound key actual compare =
    Option.iter
      (fun n ->
        if compare actual (get Jsont.int n) then
          invalid_arg ("Value violates " ^ key))
      (member key schema)
  in
  let reference ref_ value =
    let target, definition =
      match String.split_on_char '#' ref_ with
      | [ target ] -> (target, "main")
      | [ target; definition ] ->
          ((if target = "" then nsid else target), definition)
      | _ -> invalid_arg "Invalid lexicon reference"
    in
    validate ~depth:(depth + 1) ~nsid:target
      (field definition (field "defs" (document target)))
      value
  in
  match text (field "type" schema) with
  | "record" -> recurse (field "record" schema) value
  | "object" | "params" ->
      let properties =
        match member "properties" schema with
        | Some (Jsont.Object (members, _)) -> members
        | _ -> []
      in
      let required =
        Option.fold ~none:[] ~some:strings (member "required" schema)
      in
      let nullable =
        Option.fold ~none:[] ~some:strings (member "nullable" schema)
      in
      (match value with
      | Jsont.Object _ -> ()
      | _ -> invalid_arg "Expected object");
      List.iter
        (fun key ->
          if member key value = None then invalid_arg ("Missing " ^ key))
        required;
      List.iter
        (fun ((name, _), spec) ->
          match member name value with
          | None -> ()
          | Some (Jsont.Null _) when List.mem name nullable -> ()
          | Some value -> recurse spec value)
        properties
  | "string" | "token" ->
      let value = text value in
      bound "minLength" (String.length value) ( < );
      bound "maxLength" (String.length value) ( > );
      Option.iter
        (fun values ->
          if not (List.mem value (strings values)) then
            invalid_arg ("Value is not in enum: " ^ value))
        (member "enum" schema);
      Option.iter
        (fun expected ->
          if value <> text expected then invalid_arg "Value differs from const")
        (member "const" schema);
      Option.iter
        (fun format ->
          let valid =
            match text format with
            | "did" -> Atp.Did.is_valid value
            | "at-uri" -> Atp.At_uri.is_valid value
            | "record-key" -> Atp.Record_key.is_valid value
            | "tid" -> Atp.Tid.is_valid value
            | "nsid" -> Atp.Nsid.is_valid value
            | "handle" -> Atp.Handle.is_valid value
            | "datetime" -> (
                match Ptime.of_rfc3339 value with
                | Ok _ -> true
                | Error _ -> false)
            | _ -> true
          in
          if not valid then invalid_arg ("Invalid " ^ text format ^ ": " ^ value))
        (member "format" schema)
  | "integer" ->
      let number = get Jsont.int value in
      bound "minimum" number ( < );
      bound "maximum" number ( > )
  | "boolean" -> ignore (get Jsont.bool value)
  | "array" ->
      let values = get (Jsont.list Jsont.json) value in
      bound "minLength" (List.length values) ( < );
      bound "maxLength" (List.length values) ( > );
      List.iter (recurse (field "items" schema)) values
  | "ref" -> reference (text (field "ref" schema)) value
  | "union" ->
      let type_ = text (field "$type" value) in
      let refs =
        strings (field "refs" schema)
        |> List.map (fun ref_ ->
            if String.starts_with ~prefix:"#" ref_ then nsid ^ ref_ else ref_)
      in
      if List.mem type_ refs then reference type_ value
      else if
        Option.fold ~none:false ~some:(get Jsont.bool) (member "closed" schema)
      then invalid_arg ("Unknown closed union type: " ^ type_)
  | "blob" -> ignore (get Atp.Blob_ref.jsont value)
  | "bytes" -> ignore (get Atp.Lex.bytes_jsont value)
  | "cid-link" -> ignore (get Atp.Cid.jsont value)
  | "unknown" -> ()
  | type_ -> invalid_arg ("Cannot validate schema type: " ^ type_)

let validate_input nsid value =
  validate ~nsid (field "schema" (field "input" (main nsid))) value

let params nsid pairs =
  match member "parameters" (main nsid) with
  | None -> if pairs <> [] then invalid_arg "Endpoint takes no parameters"
  | Some schema ->
      let properties = field "properties" schema in
      let scalar spec value =
        match text (field "type" spec) with
        | "integer" -> (
            match int_of_string_opt value with
            | Some n -> Jsont.Json.int n
            | None -> invalid_arg "Expected integer parameter")
        | "boolean" -> (
            match value with
            | "true" -> Jsont.Json.bool true
            | "false" -> Jsont.Json.bool false
            | _ -> invalid_arg "Expected true or false")
        | _ -> Jsont.Json.string value
      in
      let names = List.sort_uniq String.compare (List.map fst pairs) in
      let members =
        List.map
          (fun name ->
            let spec = field name properties in
            let values =
              List.filter_map
                (fun (key, value) -> if name = key then Some value else None)
                pairs
            in
            let value =
              if text (field "type" spec) = "array" then
                Jsont.Json.list (List.map (scalar (field "items" spec)) values)
              else
                match values with
                | [ value ] -> scalar spec value
                | _ -> invalid_arg ("Repeated scalar parameter: " ^ name)
            in
            Jsont.Json.mem (name, Jsont.Meta.none) value)
          names
      in
      validate ~nsid schema (Jsont.Json.object' members)
