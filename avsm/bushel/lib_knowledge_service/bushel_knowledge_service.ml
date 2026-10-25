module K = Bushel_knowledge
module I = Retrieval.Index
module J = Retrieval.Json

let str = Jsont.Json.string
let num = Jsont.Json.int
let arr = Jsont.Json.list

let obj fields =
  Jsont.Json.object'
    (List.map (fun (k, v) -> Jsont.Json.mem (Jsont.Json.name k) v) fields)

let nullable = function None -> Jsont.Json.null () | Some n -> num n

module Response = Bushel_knowledge_store.Response

type t = {
  corpus : K.t;
  provider : Retrieval.Embedding.provider option Eio.Lazy.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  refresh : unit -> unit;
  max_bytes : int;
}

let create ~clock ~corpus ?provider ?(refresh = fun () -> ()) ~max_bytes () =
  if max_bytes < 1024 then
    invalid_arg "Knowledge tool budget must be at least 1024 bytes";
  {
    corpus;
    provider = Eio.Lazy.from_val provider;
    clock :> float Eio.Time.clock_ty Eio.Resource.t;
    refresh;
    max_bytes;
  }

let of_store store ~max_bytes =
  let agent =
    create
      ~clock:(Bushel_knowledge_store.clock store)
      ~corpus:(Bushel_knowledge_store.corpus store)
      ~max_bytes ()
  in
  {
    agent with
    provider =
      Eio.Lazy.from_fun ~cancel:`Record (fun () ->
          Bushel_knowledge_store.provider store);
  }

let integer =
  Jsont.map ~kind:"non-negative integer"
    ~dec:(fun f ->
      if
        (not (Float.is_finite f))
        || f < 0.
        || floor f <> f
        || f >= float_of_int max_int
      then invalid_arg "Offset must be a non-negative integer";
      int_of_float f)
    ~enc:float_of_int Jsont.number

let read_args =
  Jsont.Object.map (fun id offset -> (id, offset))
  |> Jsont.Object.mem "id" Jsont.string ~enc:fst
  |> Jsont.Object.mem "offset" integer ~dec_absent:(fun () -> 0) ~enc:snd
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let id_args =
  Jsont.Object.map Fun.id
  |> Jsont.Object.mem "id" Jsont.string ~enc:Fun.id
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let resolve_args =
  Jsont.Object.map (fun query offset -> (query, offset))
  |> Jsont.Object.mem "query" Jsont.string ~enc:fst
  |> Jsont.Object.mem "offset" integer ~dec_absent:(fun () -> 0) ~enc:snd
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let context_args =
  Jsont.Object.map (fun id url offset -> (id, url, offset))
  |> Jsont.Object.mem "id"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun (id, _, _) -> id)
  |> Jsont.Object.mem "url"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun (_, url, _) -> url)
  |> Jsont.Object.mem "offset" integer
       ~dec_absent:(fun () -> 0)
       ~enc:(fun (_, _, offset) -> offset)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

type search = {
  query : string;
  mode : string;
  kind : string option;
  tag : string option;
  contact : string option;
  project : string option;
  since : string option;
  until : string option;
  offset : int;
}

let search_args =
  Jsont.Object.map
    (fun query mode kind tag contact project since until offset ->
      { query; mode; kind; tag; contact; project; since; until; offset })
  |> Jsont.Object.mem "query" Jsont.string ~enc:(fun a -> a.query)
  |> Jsont.Object.mem "mode" Jsont.string
       ~dec_absent:(fun () -> "lexical")
       ~enc:(fun a -> a.mode)
  |> Jsont.Object.mem "kind"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun a -> a.kind)
  |> Jsont.Object.mem "tag"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun a -> a.tag)
  |> Jsont.Object.mem "contact"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun a -> a.contact)
  |> Jsont.Object.mem "project"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun a -> a.project)
  |> Jsont.Object.mem "since"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun a -> a.since)
  |> Jsont.Object.mem "until"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun a -> a.until)
  |> Jsont.Object.mem "offset" integer
       ~dec_absent:(fun () -> 0)
       ~enc:(fun a -> a.offset)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let latest_args =
  Jsont.Object.map Fun.id
  |> Jsont.Object.mem "offset" integer ~dec_absent:(fun () -> 0) ~enc:Fun.id
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let encode t json =
  let s = J.encode Jsont.json json in
  if String.length s > t.max_bytes then
    invalid_arg "Knowledge metadata exceeds result budget";
  json

let paged ?generation t ~offset ~extra values =
  let generation = Option.value ~default:(K.generation t.corpus) generation in
  if offset < 0 || offset > List.length values then
    invalid_arg "Invalid page offset";
  let remaining = List.filteri (fun i _ -> i >= offset) values in
  let rec page count =
    let items = List.filteri (fun i _ -> i < count) remaining in
    let next =
      if offset + List.length items < List.length values then
        Some (offset + List.length items)
      else None
    in
    let result =
      obj
        (extra
        @ [
            ("generation", str generation);
            ("items", arr items);
            ("next_offset", nullable next);
          ])
    in
    let s = J.encode Jsont.json result in
    if String.length s <= t.max_bytes then result
    else if count <= 1 then invalid_arg "Knowledge item exceeds result budget"
    else page (count / 2)
  in
  page (min 20 (List.length remaining))

let edge e =
  obj
    [
      ("source", str e.K.source);
      ("target", str e.target);
      ("relation", str e.relation);
      ("evidence", str e.evidence);
    ]

let hit (c : I.chunk) =
  obj
    [
      ("id", str c.document.id);
      ("revision", str c.document.revision);
      ("kind", str c.document.kind);
      ("title", str c.document.title);
      ("url", str c.document.url);
      ("date", str c.document.date);
      ("passage", num c.ordinal);
      ("snippet", str (Retrieval.Embedding.clip 1000 c.text));
    ]

let get t id =
  match K.get t.corpus id with
  | Some d -> d
  | None -> invalid_arg "Unknown source ID"

let read t id offset =
  let d = get t id in
  let length = String.length d.text in
  let continuation n = n < length && Char.code d.text.[n] land 0xc0 = 0x80 in
  if offset < 0 || offset > length || continuation offset then
    invalid_arg "Offset must be a UTF-8 boundary within the source";
  let rec page n =
    let text =
      Retrieval.Embedding.clip n (String.sub d.text offset (length - offset))
    in
    let next = offset + String.length text in
    let json =
      obj
        [
          ("id", str id);
          ("revision", str d.revision);
          ("title", str d.title);
          ("kind", str d.kind);
          ("url", str d.url);
          ("text", str text);
          ("next_offset", nullable (if next < length then Some next else None));
        ]
    in
    if String.length (J.encode Jsont.json json) <= t.max_bytes then
      encode t json
    else if n < 8 then invalid_arg "Source metadata exceeds result budget"
    else page (n / 2)
  in
  page (min 12000 (length - offset))

let valid_date date =
  let digit index =
    let c = date.[index] in
    c >= '0' && c <= '9'
  in
  if
    String.length date <> 10
    || date.[4] <> '-'
    || date.[7] <> '-'
    || (not (List.for_all digit [ 0; 1; 2; 3; 5; 6; 8; 9 ]))
    || Option.is_none
         (Ptime.of_date
            ( int_of_string (String.sub date 0 4),
              int_of_string (String.sub date 5 2),
              int_of_string (String.sub date 8 2) ))
  then invalid_arg "Dates must use YYYY-MM-DD"

let today clock =
  match Ptime.of_float_s (Eio.Time.now clock) with
  | None -> invalid_arg "Clock is outside the supported date range"
  | Some now ->
      let year, month, day = Ptime.to_date now in
      Printf.sprintf "%04d-%02d-%02d" year month day

type request =
  | Search of { bushel : bool; search : search }
  | Latest_posts of int
  | Resolve of { query : string; offset : int }
  | Contact of string
  | Read of { id : string; offset : int }
  | Context of { id : string option; url : string option; offset : int }

let request name arguments =
  if String.length arguments > 4096 then
    invalid_arg "Oversized knowledge tool arguments";
  match name with
  | "bushel_search" | "knowledge_search" ->
      Search
        {
          bushel = name = "bushel_search";
          search = J.decode search_args arguments;
        }
  | "bushel_latest_posts" -> Latest_posts (J.decode latest_args arguments)
  | "contacts_resolve" ->
      let query, offset = J.decode resolve_args arguments in
      Resolve { query; offset }
  | "contacts_get" -> Contact (J.decode id_args arguments)
  | "knowledge_get" ->
      let id, offset = J.decode read_args arguments in
      Read { id; offset }
  | "feeds_context" ->
      let id, url, offset = J.decode context_args arguments in
      Context { id; url; offset }
  | _ -> invalid_arg "Unknown knowledge tool"

let call t ~check request =
  try
    check ();
    t.refresh ();
    check ();
    let result =
      match request with
      | Resolve { query; offset } ->
          let candidates = K.resolve t.corpus query in
          let status =
            match candidates with
            | [] -> "missing"
            | [ c ] when c.exact -> "resolved"
            | c :: _ when c.exact -> "ambiguous"
            | _ -> "candidates"
          in
          paged t ~offset
            ~extra:[ ("status", str status) ]
            (List.map
               (fun (c : K.candidate) ->
                 obj
                   [
                     ("id", str c.contact.id);
                     ("handle", str (K.C.handle c.contact.value));
                     ("name", str (K.C.name c.contact.value));
                     ("matched_on", str c.matched_on);
                   ])
               candidates)
      | Contact id -> (
          match
            List.find_opt
              (fun (c : K.contact) -> c.id = id)
              (K.contacts t.corpus)
          with
          | None -> invalid_arg "Unknown contact ID"
          | Some c ->
              encode t
                (obj
                   [
                     ("id", str id);
                     ( "contact",
                       J.decode Jsont.json (J.encode K.C.json_t c.value) );
                   ]))
      | Read { id; offset } -> read t id offset
      | Context { id; url; offset } ->
          let documents =
            match (id, url) with
            | Some id, None -> [ get t id ]
            | None, Some url -> K.find_url t.corpus url
            | _ -> invalid_arg "Supply exactly one of id or url"
          in
          let edges =
            List.concat_map
              (fun (d : I.document) -> K.related t.corpus d.id)
              documents
            |> List.sort_uniq compare
          in
          paged t ~offset
            ~extra:
              [
                ( "source_ids",
                  arr (List.map (fun (d : I.document) -> str d.id) documents) );
              ]
            (List.map edge edges)
      | Latest_posts offset ->
          let until = today t.clock in
          let posts = K.latest_posts t.corpus ~until in
          let generation =
            Retrieval.Embedding.hash (K.key [ K.generation t.corpus; until ])
          in
          paged ~generation t ~offset
            ~extra:
              [
                ("source", str "bushel");
                ("order", str "published_desc");
                ("as_of", str until);
                ("bounded_results", Jsont.Json.bool true);
              ]
            (List.map
               (fun (document : I.document) ->
                 hit
                   I.
                     {
                       document;
                       id = document.id;
                       ordinal = 0;
                       text = document.text;
                     })
               posts)
      | Search { bushel; search = a } ->
          let a =
            if bushel then
              { a with kind = Some (Option.value ~default:"note" a.kind) }
            else a
          in
          if
            bushel
            && not
                 (List.mem (Option.get a.kind)
                    [ "note"; "paper"; "project"; "idea"; "video" ])
          then
            invalid_arg
              "Bushel kind must be note, paper, project, idea or video";
          Option.iter valid_date a.since;
          Option.iter valid_date a.until;
          (match (a.since, a.until) with
          | Some first, Some last when first > last ->
              invalid_arg "since must not be after until"
          | _ -> ());
          Option.iter
            (fun kind ->
              if
                not
                  (List.mem kind
                     [
                       "note";
                       "paper";
                       "project";
                       "idea";
                       "video";
                       "contact";
                       "feed";
                       "link";
                     ])
              then invalid_arg "Unknown knowledge kind")
            a.kind;
          let facets =
            List.filter_map Fun.id
              [
                Option.map (fun v -> ("tag", v)) a.tag;
                Option.map (fun v -> ("contact", v)) a.contact;
                Option.map (fun v -> ("project", v)) a.project;
              ]
          in
          let filter =
            I.{ kind = a.kind; since = a.since; until = a.until; facets }
          in
          let provider =
            match a.mode with
            | "semantic" | "hybrid" -> Eio.Lazy.force t.provider
            | _ -> None
          in
          let coverage =
            match (a.mode, provider) with
            | ("semantic" | "hybrid"), Some p ->
                let filter = if bushel then filter else I.all in
                let indexed, total = K.coverage ~filter t.corpus p in
                [
                  ( "embedding_coverage",
                    obj [ ("indexed", num indexed); ("total", num total) ] );
                ]
            | _ -> []
          in
          let hits =
            K.search t.corpus ?provider ~check ~mode:a.mode ~filter a.query
          in
          paged t ~offset:a.offset
            ~extra:
              (coverage
              @ (if bushel then [ ("source", str "bushel") ] else [])
              @ [
                  ("mode", str a.mode); ("bounded_results", Jsont.Json.bool true);
                ])
            (List.map hit hits)
    in
    check ();
    Jsont.Json.decode Response.codec result
  with Invalid_argument e | Failure e -> Error e
