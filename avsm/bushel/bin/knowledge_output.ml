module Model = Apple_mlx_embeddings.Model

let member key = function
  | Jsont.Object (fields, _) -> (
      match List.find_opt (fun ((name, _), _) -> name = key) fields with
      | Some (_, value) -> value
      | None -> Jsont.Json.null ())
  | _ -> Jsont.Json.null ()

let raw_string = function Jsont.String (text, _) -> text | _ -> ""
let string value = Console.sanitize ~keep_newlines:false (raw_string value)
let field key value = string (member key value)
let items = function Jsont.Array (items, _) -> items | _ -> []
let bool = function Jsont.Bool (value, _) -> value | _ -> false
let number = function Jsont.Number (n, _) -> n | _ -> 0.

let integer value =
  let n = number value in
  if Float.is_finite n && n >= 0. && n < float_of_int max_int then
    int_of_float n
  else 0

let count key value = integer (member key value)

let grouped n =
  let value = string_of_int n in
  let length = String.length value in
  let buffer = Buffer.create (length + (length / 3)) in
  String.iteri
    (fun index c ->
      if index > 0 && (length - index) mod 3 = 0 then Buffer.add_char buffer ',';
      Buffer.add_char buffer c)
    value;
  Buffer.contents buffer

let line ?(ppf = Fmt.stdout) ?(bold = false) text =
  let style = if bold then Console.Style.bold else Console.Style.none in
  let span =
    Console.Span.styled style (Console.sanitize ~keep_newlines:false text)
  in
  Console.Span.wrap 78 span
  |> List.iter (fun span -> Fmt.pf ppf "%a@." Console.Span.pp span)

let blank () = Fmt.pf Fmt.stdout "@."

let command_line text =
  Fmt.pf Fmt.stdout "%s@." (Console.sanitize ~keep_newlines:false text)

let prefix images =
  if images then "bushel knowledge images " else "bushel knowledge "

let units images = if images then "images" else "passages"
let get_command ~images id = prefix images ^ "get " ^ Filename.quote id

let coverage value =
  match member "embedding_coverage" value with
  | Jsont.Object _ as coverage ->
      Some (count "indexed" coverage, count "total" coverage)
  | _ -> None

let skipped_images value =
  let skipped = count "skipped_images" value in
  if skipped > 0 then
    line
      (Printf.sprintf "Skipped %s %s smaller than 32 pixels on either side."
         (grouped skipped)
         (if skipped = 1 then "image" else "images"))

let coverage_line ~images ~indexed ~total =
  let remaining = max 0 (total - indexed) in
  let percent =
    if total = 0 then 100
    else
      min 100
        (int_of_float (100. *. float_of_int indexed /. float_of_int total))
  in
  if remaining = 0 then
    line (Printf.sprintf "All %s %s ready." (grouped total) (units images))
  else
    line
      (Printf.sprintf "%s of %s ready (%d%%) · %s remaining" (grouped indexed)
         (grouped total) percent (grouped remaining))

let index ~images ?elapsed value =
  let added = count "newly_indexed" value in
  match coverage value with
  | Some (_, 0) -> line ("No " ^ units images ^ " to index.")
  | Some (indexed, total) when added = 0 && indexed >= total ->
      line ~bold:true "Already up to date.";
      coverage_line ~images ~indexed ~total
  | Some (indexed, total) ->
      let duration =
        match elapsed with
        | None -> ""
        | Some elapsed -> " in " ^ Console.Display.span elapsed
      in
      line ~bold:true
        (Printf.sprintf "Indexed %s %s%s." (grouped added) (units images)
           duration);
      coverage_line ~images ~indexed ~total;
      if indexed < total then command_line ("Resume: " ^ prefix images ^ "index")
  | None ->
      line (Printf.sprintf "Indexed %s %s." (grouped added) (units images))

let provider_name provider =
  if List.mem (field "backend" provider) [ "mlx"; "mlx-vision" ] then
    let path = field "model_dir" provider in
    let components = String.split_on_char '/' path in
    let model =
      List.find_opt
        (fun (model : Model.t) ->
          let directory =
            "models--"
            ^ String.concat "--" (String.split_on_char '/' model.repo)
          in
          List.mem directory components)
        (Model.all @ Model.vision_all)
    in
    match model with
    | Some model -> model.name ^ " (MLX)"
    | None -> "Local MLX model"
  else
    let model = field "model" provider in
    if model = "" then "HTTP provider" else model ^ " (HTTP)"

let status ~images value =
  if images then
    let total =
      match member "images" value with
      | Jsont.Number _ as n -> integer n
      | _ -> count "documents" value
    in
    line ~bold:true (Printf.sprintf "%s images" (grouped total))
  else
    line ~bold:true
      (Printf.sprintf "%s documents · %s passages"
         (grouped (count "documents" value))
         (grouped (count "passages" value)));
  (match member "embedding_provider" value with
  | Jsont.Object _ as provider -> (
      line
        (Printf.sprintf "Model: %s · %s dimensions" (provider_name provider)
           (grouped (count "dimensions" provider)));
      match coverage value with
      | Some (indexed, total) ->
          coverage_line ~images ~indexed ~total;
          if indexed < total then
            command_line ("Index: " ^ prefix images ^ "index")
      | None -> ())
  | _ ->
      line
        (if images then "Image embeddings are not configured."
         else "Embeddings are not configured. Lexical search is available.");
      command_line
        ("Configure: " ^ prefix images ^ "configure"
        ^ if images then "" else " --backend mlx"));
  let database = field "database" value in
  if database <> "" then line ("Database: " ^ database)

let models ~images value =
  let rows =
    items (member "models" value)
    |> List.map (fun model ->
           let bytes = number (member "size_bytes" model) in
           let size =
             if
               Float.is_finite bytes && bytes >= 0.
               && bytes < Int64.to_float Int64.max_int
             then Console.Display.bytes (Int64.of_float bytes)
             else "unknown"
           in
           [
             field "name" model;
             string_of_int (count "dimensions" model);
             size;
             (if bool (member "cached" model) then "Ready" else "Not downloaded");
             (if bool (member "default" model) then "default" else "");
           ])
  in
  let module T = Console.Table in
  let table =
    T.of_string_rows ~border:Console.Border.none
      [
        T.column "Model";
        T.column ~align:`Right "Dimensions";
        T.column ~align:`Right "Download";
        T.column "Cache";
        T.column "";
      ]
      rows
  in
  let old_margin = Format.pp_get_margin Fmt.stdout () in
  Fun.protect
    ~finally:(fun () -> Format.pp_set_margin Fmt.stdout old_margin)
    (fun () ->
      Format.pp_set_margin Fmt.stdout 78;
      Fmt.pf Fmt.stdout "%a@." T.pp table);
  blank ();
  command_line
    ("Configure default: " ^ prefix images ^ "configure"
    ^ if images then "" else " --backend mlx");
  line "Choose another model with --model NAME."

let title ?prefix value =
  let title = field "title" value in
  let title = if title = "" then "Untitled" else title in
  let kind = field "kind" value in
  let prefix = Option.value prefix ~default:"" in
  line ~bold:true (prefix ^ title ^ if kind = "" then "" else " [" ^ kind ^ "]");
  let url = field "url" value in
  if url <> "" then line url

let page_hint ~images command value =
  match member "next_offset" value with
  | Jsont.Number _ as offset -> (
      blank ();
      let offset = string_of_int (integer offset) in
      match command with
      | "get" ->
          command_line
            ("Continue: "
            ^ get_command ~images (field "id" value)
            ^ " --offset " ^ offset)
      | "contacts" ->
          line
            ("More contacts: repeat the command with --offset " ^ offset ^ ".")
      | _ ->
          line
            ("More results: repeat the search with the same filters and "
           ^ "--offset " ^ offset ^ "."))
  | _ -> ()

let image_metadata value =
  let path =
    List.find_map
      (fun key ->
        let value = field key value in
        if value = "" then None else Some value)
      [ "pixel_path"; "file_path"; "filepath"; "path" ]
  in
  Option.iter (fun path -> line ("Image: " ^ path)) path;
  let source = field "source_path" value in
  if source <> "" && Some source <> path then line ("Source: " ^ source);
  let width = count "width" value and height = count "height" value in
  if width > 0 && height > 0 then
    line (Printf.sprintf "%s × %s pixels" (grouped width) (grouped height));
  let media_type = field "media_type" value in
  if media_type <> "" then line media_type

let search ~images value =
  let hits = items (member "items" value) in
  if hits = [] then line ("No matching " ^ units images ^ ".")
  else
    List.iteri
      (fun index hit ->
        if index > 0 then blank ();
        title ~prefix:(Printf.sprintf "%d. " (index + 1)) hit;
        if images then image_metadata hit;
        let snippet = field "snippet" hit in
        if snippet <> "" then line snippet;
        let id = field "id" hit in
        if id <> "" then command_line ("Read: " ^ get_command ~images id))
      hits;
  (match coverage value with
  | Some (indexed, total) when indexed < total ->
      blank ();
      line
        (Printf.sprintf "Semantic coverage: %s of %s %s ready."
           (grouped indexed) (grouped total) (units images))
  | _ -> ());
  page_hint ~images "search" value

let get ~images value =
  title value;
  if images then image_metadata value;
  blank ();
  let text = Console.sanitize (raw_string (member "text" value)) in
  String.split_on_char '\n' text
  |> List.iter (fun text -> if text = "" then blank () else line text);
  page_hint ~images "get" value

let contacts value =
  let contacts = items (member "items" value) in
  (match field "status" value with
  | "ambiguous" ->
      line ~bold:true "Multiple matching contacts. Choose a person."
  | "candidates" ->
      line "Possible contacts. Confirm the identity before using it."
  | _ -> ());
  if contacts = [] then line "No matching contacts."
  else
    List.iteri
      (fun index contact ->
        if index > 0 then blank ();
        let handle = field "handle" contact in
        let name = field "name" contact in
        line ~bold:true
          (Printf.sprintf "%d. %s%s" (index + 1) name
             (if handle = "" then "" else " (" ^ handle ^ ")"));
        let emails =
          items (member "emails" contact)
          |> List.map string
          |> List.filter (fun value -> value <> "")
        in
        if emails <> [] then line (String.concat ", " emails);
        let matched = field "matched_on" contact in
        if String.contains matched '@' then line matched;
        let id = field "id" contact in
        if id <> "" then command_line ("Read: " ^ get_command ~images:false id))
      contacts;
  page_hint ~images:false "contacts" value

let print ?elapsed ~command response =
  match Jsont_bytesrw.decode_string Jsont.json response with
  | Error _ -> failwith "Could not read the knowledge response."
  | Ok (Jsont.Object _ as value) -> (
      match member "error" value with
      | Jsont.String _ as error ->
          line ~ppf:Fmt.stderr ("Error: " ^ string error)
      | _ -> (
          match command with
          | "configure" | "images-configure" ->
              line ~bold:true
                (if command = "images-configure" then
                   "Image embedding provider configured."
                 else "Embedding provider configured.");
              let path = field "config_file" value in
              if path <> "" then line ("Config: " ^ path)
          | "models" -> models ~images:false value
          | "images-models" -> models ~images:true value
          | "status" -> status ~images:false value
          | "images-status" ->
              status ~images:true value;
              skipped_images value
          | "index" -> index ~images:false ?elapsed value
          | "images-index" ->
              index ~images:true ?elapsed value;
              skipped_images value
          | "search" -> search ~images:false value
          | "images-search" -> search ~images:true value
          | "get" -> get ~images:false value
          | "images-get" -> get ~images:true value
          | "contacts" -> contacts value
          | _ -> failwith "Unknown knowledge command."))
  | Ok _ -> failwith "Knowledge response must be an object."
