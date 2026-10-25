module Response = Bushel_knowledge_store.Response

type model = {
  name : string;
  repo : string;
  revision : string;
  dimensions : int;
  size_bytes : int64;
  cached : bool;
  model_dir : string;
  is_default : bool;
}

type response =
  | Error of string
  | Configured of string
  | Models of { default : string; models : model list }
  | Status of Response.status
  | Result of Response.t

let obj fields =
  Jsont.Json.object'
    (List.map
       (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
       fields)

let json = function
  | Error message -> obj [ ("error", Jsont.Json.string message) ]
  | Configured path ->
      obj
        [
          ("configured", Jsont.Json.bool true);
          ("config_file", Jsont.Json.string path);
        ]
  | Status status -> Response.status_json status
  | Result result -> Response.json result
  | Models { default; models } ->
      obj
        [
          ("default", Jsont.Json.string default);
          ( "models",
            Jsont.Json.list
              (List.map
                 (fun model ->
                   obj
                     [
                       ("name", Jsont.Json.string model.name);
                       ("repo", Jsont.Json.string model.repo);
                       ("revision", Jsont.Json.string model.revision);
                       ("dimensions", Jsont.Json.int model.dimensions);
                       ( "size_bytes",
                         Jsont.Json.number (Int64.to_float model.size_bytes) );
                       ("cached", Jsont.Json.bool model.cached);
                       ("model_dir", Jsont.Json.string model.model_dir);
                       ("default", Jsont.Json.bool model.is_default);
                     ])
                 models) );
        ]

let encode response =
  match Jsont_bytesrw.encode_string Jsont.json (json response) with
  | Ok value -> value
  | Error message -> failwith message

module Model = Apple_mlx_embeddings.Model

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

let coverage_line ~images ({ Response.indexed; total } : Response.coverage) =
  let remaining = total - indexed in
  let percent =
    if total = 0 then 100
    else int_of_float (100. *. float indexed /. float total)
  in
  if remaining = 0 then
    line (Printf.sprintf "All %s %s ready." (grouped total) (units images))
  else
    line
      (Printf.sprintf "%s of %s ready (%d%%) · %s remaining" (grouped indexed)
         (grouped total) percent (grouped remaining))

let skipped status =
  Option.iter
    (fun count ->
      if count > 0 then
        line
          (Printf.sprintf "Skipped %s %s smaller than 32 pixels on either side."
             (grouped count)
             (if count = 1 then "image" else "images")))
    status.Response.skipped_images

let index ~images ?elapsed (status : Response.status) =
  let added =
    match status.newly_indexed with
    | Some count -> count
    | None -> failwith "Index result lacks newly_indexed"
  in
  (match status.coverage with
  | Some { total = 0; _ } -> line ("No " ^ units images ^ " to index.")
  | Some coverage when added = 0 && coverage.indexed = coverage.total ->
      line ~bold:true "Already up to date.";
      coverage_line ~images coverage
  | Some coverage ->
      let duration =
        Option.fold ~none:""
          ~some:(fun seconds -> " in " ^ Console.Display.span seconds)
          elapsed
      in
      line ~bold:true
        (Printf.sprintf "Indexed %s %s%s." (grouped added) (units images)
           duration);
      coverage_line ~images coverage;
      if coverage.indexed < coverage.total then
        command_line ("Resume: " ^ prefix images ^ "index")
  | None -> failwith "Index result lacks embedding coverage");
  if images then skipped status

let provider_name = function
  | Response.Http { model; _ } -> model ^ " (HTTP)"
  | Response.Mlx { model_dir; _ } -> (
      let components = String.split_on_char '/' model_dir in
      match
        List.find_opt
          (fun (model : Model.t) ->
            let directory =
              "models--"
              ^ String.concat "--" (String.split_on_char '/' model.repo)
            in
            List.mem directory components)
          (Model.all @ Model.vision_all)
      with
      | Some model -> model.name ^ " (MLX)"
      | None -> "Local MLX model")

let status ~images (status : Response.status) =
  if images then
    line ~bold:true (Printf.sprintf "%s images" (grouped status.documents))
  else
    line ~bold:true
      (Printf.sprintf "%s documents · %s passages" (grouped status.documents)
         (grouped status.passages));
  (match status.provider with
  | Some provider ->
      let dimensions =
        match provider with
        | Response.Http p -> p.dimensions
        | Response.Mlx p -> p.dimensions
      in
      line
        (Printf.sprintf "Model: %s · %s dimensions" (provider_name provider)
           (grouped dimensions));
      Option.iter
        (fun (coverage : Response.coverage) ->
          coverage_line ~images coverage;
          if coverage.indexed < coverage.total then
            command_line ("Index: " ^ prefix images ^ "index"))
        status.coverage
  | None ->
      line
        (if images then "Image embeddings are not configured."
         else "Embeddings are not configured. Lexical search is available.");
      command_line
        ("Configure: " ^ prefix images ^ "configure"
        ^ if images then "" else " --backend mlx"));
  if status.database <> "" then line ("Database: " ^ status.database);
  if images then skipped status

let models ~images models =
  let rows =
    List.map
      (fun model ->
        [
          model.name;
          string_of_int model.dimensions;
          Console.Display.bytes model.size_bytes;
          (if model.cached then "Ready" else "Not downloaded");
          (if model.is_default then "default" else "");
        ])
      models
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

let title ?(prefix = "") ~title ~kind ~url () =
  let title = if title = "" then "Untitled" else title in
  line ~bold:true (prefix ^ title ^ if kind = "" then "" else " [" ^ kind ^ "]");
  if url <> "" then line url

let image_paths ~pixel_path ~source_path =
  Option.iter (fun path -> line ("Image: " ^ path)) pixel_path;
  Option.iter
    (fun source -> if Some source <> pixel_path then line ("Source: " ^ source))
    source_path

let page_hint ~images ~command ?id = function
  | None -> ()
  | Some offset -> (
      blank ();
      let offset = string_of_int offset in
      match (command, id) with
      | "get", Some id ->
          command_line
            ("Continue: " ^ get_command ~images id ^ " --offset " ^ offset)
      | "contacts", _ ->
          line
            ("More contacts: repeat the command with --offset " ^ offset ^ ".")
      | _ ->
          line
            ("More results: repeat the search with the same filters and \
              --offset " ^ offset ^ "."))

let search ~images (result : Response.search) =
  if result.page.items = [] then line ("No matching " ^ units images ^ ".")
  else
    List.iteri
      (fun index (hit : Response.hit) ->
        if index > 0 then blank ();
        title
          ~prefix:(Printf.sprintf "%d. " (index + 1))
          ~title:hit.title ~kind:hit.kind ~url:hit.url ();
        if images then
          image_paths ~pixel_path:hit.pixel_path ~source_path:hit.source_path;
        if hit.snippet <> "" then line hit.snippet;
        command_line ("Read: " ^ get_command ~images hit.id))
      result.page.items;
  Option.iter
    (fun (coverage : Response.coverage) ->
      if coverage.indexed < coverage.total then (
        blank ();
        line
          (Printf.sprintf "Semantic coverage: %s of %s %s ready."
             (grouped coverage.indexed) (grouped coverage.total) (units images))))
    result.coverage;
  page_hint ~images ~command:"search" result.page.next_offset

let get ~images (source : Response.source) =
  title ~title:source.title ~kind:source.kind ~url:source.url ();
  if images then (
    image_paths ~pixel_path:source.pixel_path ~source_path:source.source_path;
    (match (source.width, source.height) with
    | Some width, Some height ->
        line (Printf.sprintf "%s × %s pixels" (grouped width) (grouped height))
    | _ -> ());
    Option.iter line source.media_type);
  blank ();
  Console.sanitize source.text
  |> String.split_on_char '\n'
  |> List.iter (fun text -> if text = "" then blank () else line text);
  page_hint ~images ~command:"get" ~id:source.id source.next_offset

let contacts status (page : Response.candidate Response.page) =
  (match status with
  | "ambiguous" ->
      line ~bold:true "Multiple matching contacts. Choose a person."
  | "candidates" ->
      line "Possible contacts. Confirm the identity before using it."
  | _ -> ());
  if page.items = [] then line "No matching contacts."
  else
    List.iteri
      (fun index (contact : Response.candidate) ->
        if index > 0 then blank ();
        line ~bold:true
          (Printf.sprintf "%d. %s%s" (index + 1) contact.name
             (if contact.handle = "" then "" else " (" ^ contact.handle ^ ")"));
        if contact.emails <> [] then line (String.concat ", " contact.emails);
        command_line ("Read: " ^ get_command ~images:false contact.id))
      page.items;
  page_hint ~images:false ~command:"contacts" page.next_offset

let print_value ?elapsed ~command response =
  match (command, response) with
  | _, Error message -> line ~ppf:Fmt.stderr ("Error: " ^ message)
  | ("configure" | "images-configure"), Configured path ->
      line ~bold:true
        (if command = "images-configure" then
           "Image embedding provider configured."
         else "Embedding provider configured.");
      line ("Config: " ^ path)
  | ("models" | "images-models"), Models { models = values; _ } ->
      models ~images:(command = "images-models") values
  | ("status" | "images-status"), Status value ->
      status ~images:(command = "images-status") value
  | ("index" | "images-index"), Status value ->
      index ~images:(command = "images-index") ?elapsed value
  | ("search" | "images-search"), Result (Response.Search value) ->
      search ~images:(command = "images-search") value
  | ("get" | "images-get"), Result (Response.Source value) ->
      get ~images:(command = "images-get") value
  | "contacts", Result (Response.Contacts { page; status }) ->
      contacts status page
  | _ -> failwith "Knowledge command and result do not match."

let print ?elapsed ~command encoded =
  let decode codec =
    try
      match Jsont_bytesrw.decode_string codec encoded with
      | Ok value -> value
      | Error message -> failwith message
    with Invalid_argument message -> failwith message
  in
  let error_codec =
    Jsont.Object.map Fun.id
    |> Jsont.Object.mem "error" Jsont.string ~enc:Fun.id
    |> Jsont.Object.finish
  in
  match Jsont_bytesrw.decode_string error_codec encoded with
  | Ok message -> print_value ?elapsed ~command (Error message)
  | Error _ ->
      let response =
        match command with
        | "index" | "images-index" | "status" | "images-status" ->
            Status (decode Response.status_codec)
        | "search" | "images-search" | "get" | "images-get" | "contacts" ->
            Result (decode Response.codec)
        | _ -> failwith "Use typed results for configuration and model lists"
      in
      print_value ?elapsed ~command response
