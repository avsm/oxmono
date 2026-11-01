(** Typed results shared by the command line and agent adapters. *)

type coverage = { indexed : int; total : int }

type hit = {
  id : string;
  revision : string;
  kind : string;
  title : string;
  url : string;
  date : string;
  passage : int;
  snippet : string;
  pixel_path : string option;
  source_path : string option;
}

type source = {
  id : string;
  revision : string;
  title : string;
  kind : string;
  url : string;
  text : string;
  next_offset : int option;
  offset : int option;
  width : int option;
  height : int option;
  media_type : string option;
  pixel_path : string option;
  source_path : string option;
}

type candidate = {
  id : string;
  handle : string;
  name : string;
  matched_on : string;
  emails : string list;
}

type 'a page = {
  generation : string;
  items : 'a list;
  next_offset : int option;
}

type search = {
  page : hit page;
  mode : string option;
  coverage : coverage option;
  source : string option;
  order : string option;
  as_of : string option;
  source_ids : string list option;
}

type t =
  | Search of search
  | Source of source
  | Contacts of { page : candidate page; status : string }
  | Contact of { id : string; contact : Sortal_schema.Contact.t }
  | Context of { page : Bushel_knowledge.edge page; source_ids : string list }

type provider =
  | Http of { url : string; model : string; dimensions : int }
  | Mlx of {
      backend : string;
      model_dir : string;
      dimensions : int;
      max_tokens : int;
      query_instruction : string;
      model_identity : string;
    }

type status = {
  database : string;
  generation : string;
  documents : int;
  passages : int;
  provider : provider option;
  coverage : coverage option;
  skipped_images : int option;
  newly_indexed : int option;
}

let obj fields =
  Jsont.Json.object'
    (List.map
       (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
       fields)

let str = Jsont.Json.string
let num = Jsont.Json.int
let opt f = function None -> Jsont.Json.null () | Some x -> f x
let optional name f = function None -> [] | Some x -> [ (name, f x) ]

let fields = function
  | Jsont.Object (fields, _) ->
      List.map (fun ((name, _), value) -> (name, value)) fields
  | _ -> invalid_arg "Knowledge result must be an object"

let required name fields =
  match List.assoc_opt name fields with
  | Some value -> value
  | None -> invalid_arg ("Missing knowledge result field: " ^ name)

let string = function
  | Jsont.String (value, _) -> value
  | _ -> invalid_arg "Knowledge result field must be a string"

let integer = function
  | Jsont.Number (value, _)
    when Float.is_finite value && value >= 0.
         && floor value = value
         && value < float_of_int max_int ->
      int_of_float value
  | _ -> invalid_arg "Knowledge result field must be a nonnegative integer"

let option decode = function
  | Jsont.Null _ -> None
  | value -> Some (decode value)

let absent decode name fields =
  Option.bind (List.assoc_opt name fields) (option decode)

let list decode = function
  | Jsont.Array (items, _) -> List.map decode items
  | _ -> invalid_arg "Knowledge result field must be an array"

let get decode name fields = decode (required name fields)

let coverage_json { indexed; total } =
  obj [ ("indexed", num indexed); ("total", num total) ]

let coverage_of_json value =
  let f = fields value in
  let indexed = get integer "indexed" f and total = get integer "total" f in
  if indexed > total then invalid_arg "Embedding coverage exceeds corpus size";
  { indexed; total }

let hit_json (h : hit) =
  obj
    ([
       ("id", str h.id);
       ("revision", str h.revision);
       ("kind", str h.kind);
       ("title", str h.title);
       ("url", str h.url);
       ("date", str h.date);
       ("passage", num h.passage);
       ("snippet", str h.snippet);
     ]
    @ optional "pixel_path" str h.pixel_path
    @ optional "source_path" str h.source_path)

let hit_of_json value =
  let f = fields value in
  {
    id = get string "id" f;
    revision = get string "revision" f;
    kind = get string "kind" f;
    title = get string "title" f;
    url = get string "url" f;
    date = get string "date" f;
    passage = get integer "passage" f;
    snippet = get string "snippet" f;
    pixel_path = absent string "pixel_path" f;
    source_path = absent string "source_path" f;
  }

let candidate_json (c : candidate) =
  obj
    ([
       ("id", str c.id);
       ("handle", str c.handle);
       ("name", str c.name);
       ("matched_on", str c.matched_on);
     ]
    @
    if c.emails = [] then []
    else [ ("emails", Jsont.Json.list (List.map str c.emails)) ])

let candidate_of_json value =
  let f = fields value in
  {
    id = get string "id" f;
    handle = get string "handle" f;
    name = get string "name" f;
    matched_on = get string "matched_on" f;
    emails = Option.value ~default:[] (absent (list string) "emails" f);
  }

let edge_json (e : Bushel_knowledge.edge) =
  obj
    [
      ("source", str e.source);
      ("target", str e.target);
      ("relation", str e.relation);
      ("evidence", str e.evidence);
    ]

let edge_of_json value =
  let f = fields value in
  {
    Bushel_knowledge.source = get string "source" f;
    target = get string "target" f;
    relation = get string "relation" f;
    evidence = get string "evidence" f;
  }

let page_fields encode (page : _ page) =
  [
    ("generation", str page.generation);
    ("items", Jsont.Json.list (List.map encode page.items));
    ("next_offset", opt num page.next_offset);
  ]

let page_of_fields decode f =
  {
    generation = get string "generation" f;
    items = get (list decode) "items" f;
    next_offset = get (option integer) "next_offset" f;
  }

let source_json (s : source) =
  obj
    ([
       ("id", str s.id);
       ("revision", str s.revision);
       ("title", str s.title);
       ("kind", str s.kind);
       ("url", str s.url);
       ("text", str s.text);
       ("next_offset", opt num s.next_offset);
     ]
    @ optional "offset" num s.offset
    @ optional "width" num s.width
    @ optional "height" num s.height
    @ optional "media_type" str s.media_type
    @ optional "pixel_path" str s.pixel_path
    @ optional "source_path" str s.source_path)

let source_of_json value =
  let f = fields value in
  {
    id = get string "id" f;
    revision = get string "revision" f;
    title = get string "title" f;
    kind = get string "kind" f;
    url = get string "url" f;
    text = get string "text" f;
    next_offset = get (option integer) "next_offset" f;
    offset = absent integer "offset" f;
    width = absent integer "width" f;
    height = absent integer "height" f;
    media_type = absent string "media_type" f;
    pixel_path = absent string "pixel_path" f;
    source_path = absent string "source_path" f;
  }

let search_json (s : search) =
  obj
    (page_fields hit_json s.page
    @ [ ("bounded_results", Jsont.Json.bool true) ]
    @ optional "mode" str s.mode
    @ optional "embedding_coverage" coverage_json s.coverage
    @ optional "source" str s.source
    @ optional "order" str s.order
    @ optional "as_of" str s.as_of
    @ optional "source_ids"
        (fun xs -> Jsont.Json.list (List.map str xs))
        s.source_ids)

let search_of_json value =
  let f = fields value in
  {
    page = page_of_fields hit_of_json f;
    mode = absent string "mode" f;
    coverage = absent coverage_of_json "embedding_coverage" f;
    source = absent string "source" f;
    order = absent string "order" f;
    as_of = absent string "as_of" f;
    source_ids = absent (list string) "source_ids" f;
  }

let json = function
  | Search s -> search_json s
  | Source s -> source_json s
  | Contacts { page; status } ->
      obj (("status", str status) :: page_fields candidate_json page)
  | Context { page; source_ids } ->
      obj
        (("source_ids", Jsont.Json.list (List.map str source_ids))
        :: page_fields edge_json page)
  | Contact { id; contact } ->
      obj
        [
          ("id", str id);
          ( "contact",
            Retrieval.Json.decode Jsont.json
              (Retrieval.Json.encode Sortal_schema.Contact.json_t contact) );
        ]

let encode response = Retrieval.Json.encode Jsont.json (json response)

let codec =
  Jsont.map ~kind:"knowledge response" ~enc:json
    ~dec:(fun value ->
      let f = fields value in
      if List.mem_assoc "text" f then Source (source_of_json value)
      else if List.mem_assoc "contact" f then
        Contact
          {
            id = get string "id" f;
            contact =
              Retrieval.Json.decode Sortal_schema.Contact.json_t
                (Retrieval.Json.encode Jsont.json (required "contact" f));
          }
      else if List.mem_assoc "status" f then
        Contacts
          {
            page = page_of_fields candidate_of_json f;
            status = get string "status" f;
          }
      else if List.mem_assoc "bounded_results" f then
        Search (search_of_json value)
      else
        Context
          {
            page = page_of_fields edge_of_json f;
            source_ids = get (list string) "source_ids" f;
          })
    Jsont.json

let provider_json = function
  | Http { url; model; dimensions } ->
      obj
        [
          ("backend", str "http");
          ("url", str url);
          ("model", str model);
          ("dimensions", num dimensions);
        ]
  | Mlx
      {
        backend;
        model_dir;
        dimensions;
        max_tokens;
        query_instruction;
        model_identity;
      } ->
      obj
        [
          ("backend", str backend);
          ("model_dir", str model_dir);
          ("dimensions", num dimensions);
          ("max_tokens", num max_tokens);
          ("query_instruction", str query_instruction);
          ("model_identity", str model_identity);
        ]

let provider_of_json value =
  let f = fields value in
  match get string "backend" f with
  | "http" ->
      Http
        {
          url = get string "url" f;
          model = get string "model" f;
          dimensions = get integer "dimensions" f;
        }
  | ("mlx" | "mlx-vision") as backend ->
      Mlx
        {
          backend;
          model_dir = get string "model_dir" f;
          dimensions = get integer "dimensions" f;
          max_tokens = get integer "max_tokens" f;
          query_instruction = get string "query_instruction" f;
          model_identity = get string "model_identity" f;
        }
  | _ -> invalid_arg "Unknown knowledge provider backend"

let status_json (s : status) =
  obj
    ([
       ("database", str s.database);
       ("generation", str s.generation);
       ("documents", num s.documents);
       ("passages", num s.passages);
       ("embedding_provider", opt provider_json s.provider);
       ("embedding_coverage", opt coverage_json s.coverage);
     ]
    @ optional "skipped_images" num s.skipped_images
    @ optional "newly_indexed" num s.newly_indexed)

let status_of_json value =
  let f = fields value in
  {
    database = get string "database" f;
    generation = get string "generation" f;
    documents = get integer "documents" f;
    passages = get integer "passages" f;
    provider = get (option provider_of_json) "embedding_provider" f;
    coverage = get (option coverage_of_json) "embedding_coverage" f;
    skipped_images = absent integer "skipped_images" f;
    newly_indexed = absent integer "newly_indexed" f;
  }

let status_codec =
  Jsont.map ~kind:"knowledge status" ~enc:status_json ~dec:status_of_json
    Jsont.json
