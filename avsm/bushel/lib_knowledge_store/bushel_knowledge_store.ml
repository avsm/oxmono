module K = Bushel_knowledge
module E = Retrieval.Embedding
module J = Retrieval.Json

type images = {
  corpus : K.t;
  provider : E.provider option Eio.Lazy.t;
  settings : Bushel_config.mlx_embeddings option;
  database : string;
}

type t = {
  corpus : K.t;
  provider : E.provider option Eio.Lazy.t;
  settings : Bushel_config.embeddings option;
  images : images Eio.Lazy.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  database : string;
}

let corpus (t : t) = t.corpus
let provider (t : t) = Eio.Lazy.force t.provider
let database (t : t) = t.database
let clock (t : t) = t.clock
let image_corpus t = (Eio.Lazy.force t.images).corpus
let image_provider t = Eio.Lazy.force (Eio.Lazy.force t.images).provider

let config config_file =
  Eio_unix.run_in_systhread (fun () ->
      match
        match config_file with
        | None -> Bushel_config.load ()
        | Some path -> Bushel_config.load_file path
      with
      | Ok config -> config
      | Error error -> failwith ("Bushel configuration: " ^ error))

let roots ~fs config =
  let sortal = Sortal.Store.create fs "sortal" in
  ( config.Bushel_config.data_dir,
    Eio.Path.native_exn (Sortal.Store.data_dir sortal) )

let sources ?config_file ~fs () = roots ~fs (config config_file)

let is_directory path =
  try
    if (Unix.stat path).st_kind = Unix.S_DIR then true
    else invalid_arg ("Knowledge source is not a directory: " ^ path)
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let available ?config_file ~fs () =
  let bushel, sortal = sources ?config_file ~fs () in
  let bushel = Eio.Path.native_exn Eio.Path.(fs / bushel) in
  Eio_unix.run_in_systhread (fun () ->
      let first = is_directory bushel and second = is_directory sortal in
      first && second)

let rec mkdirs path =
  if path <> Filename.dirname path then (
    (try Unix.mkdir path 0o700 with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> (
        mkdirs (Filename.dirname path);
        try Unix.mkdir path 0o700
        with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    if (Unix.stat path).st_kind <> Unix.S_DIR then
      invalid_arg ("Not a directory: " ^ path))

let private_stat kind stat =
  stat.Unix.st_kind = kind
  && stat.st_uid = Unix.getuid ()
  && stat.st_perm land 0o077 = 0

let private_directory path =
  mkdirs path;
  if not (private_stat Unix.S_DIR (Unix.lstat path)) then
    invalid_arg
      ("Knowledge directory must be owned by you with mode 0700: " ^ path)

let private_file path =
  (match Unix.lstat path with
  | stat when private_stat Unix.S_REG stat -> ()
  | _ ->
      invalid_arg
        ("Knowledge file must be an owned private regular file: " ^ path)
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  let fd =
    Unix.openfile path
      [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC; Unix.O_NONBLOCK ]
      0o600
  in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      if
        (not (private_stat Unix.S_REG (Unix.lstat path)))
        || not (private_stat Unix.S_REG (Unix.fstat fd))
      then
        invalid_arg
          ("Knowledge file must be owned by you with mode 0600: " ^ path))

let data_home () =
  match Sys.getenv_opt "XDG_DATA_HOME" with
  | Some path when path <> "" -> path
  | _ ->
      Filename.concat
        (Option.value ~default:"." (Sys.getenv_opt "HOME"))
        ".local/share"

let location ~fs ~bushel ~sortal =
  let canonical path = Unix.realpath Eio.Path.(native_exn (fs / path)) in
  let bushel = canonical bushel and sortal = canonical sortal in
  let identity =
    E.hash (J.encode (Jsont.list Jsont.string) [ bushel; sortal ])
  in
  let base = Filename.concat (data_home ()) "bushel/knowledge" in
  private_directory base;
  let dir = Filename.concat base identity in
  private_directory dir;
  (bushel, sortal, Filename.concat (Unix.realpath dir) "knowledge.sqlite3")

(* POSIX record locks are process scoped. A second fiber must not open and
   close another descriptor for a lock held by this process. *)
let registry_lock = Mutex.create ()
let locks = Hashtbl.create 4

let local_lock path =
  Mutex.lock registry_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock registry_lock)
    (fun () ->
      match Hashtbl.find_opt locks path with
      | Some lock -> lock
      | None ->
          let lock = Eio.Mutex.create () in
          Hashtbl.add locks path lock;
          lock)

let with_lock ~clock ~check path f =
  Eio.Mutex.use_ro (local_lock path) (fun () ->
      let fd =
        Eio_unix.run_in_systhread (fun () ->
            private_file path;
            Unix.openfile path [ Unix.O_RDWR; Unix.O_CLOEXEC ] 0)
      in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          let rec acquire () =
            check ();
            match Unix.lockf fd Unix.F_TLOCK 0 with
            | () -> ()
            | exception Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
                Eio.Time.sleep clock 0.05;
                acquire ()
            | exception Unix.Unix_error (Unix.EINTR, _, _) -> acquire ()
          in
          acquire ();
          Fun.protect
            ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0)
            (fun () ->
              check ();
              f ())))

let read_key path =
  if not (private_stat Unix.S_REG (Unix.lstat path)) then
    invalid_arg "Embedding API key must be an owned private regular file";
  let fd =
    Unix.openfile path [ Unix.O_RDONLY; Unix.O_CLOEXEC; Unix.O_NONBLOCK ] 0
  in
  let ic = Unix.in_channel_of_descr fd in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let stat = Unix.fstat fd in
      if
        (not (private_stat Unix.S_REG stat))
        || not (private_stat Unix.S_REG (Unix.lstat path))
      then invalid_arg "Embedding API key must be an owned private regular file";
      if stat.st_size > 16384 then invalid_arg "Embedding API key is too large";
      let key = really_input_string ic stat.st_size |> String.trim in
      if key = "" || String.contains key '\n' || String.contains key '\r' then
        invalid_arg "Embedding API key must contain one nonempty line";
      key)

let http_provider ~sw ~fetch (settings : Bushel_config.http_embeddings) =
  let make ?api_key () =
    E.http ~sw ~fetch ~url:settings.url ~model:settings.model
      ~dimensions:settings.dimensions ?api_key ()
  in
  let identity = make () in
  let configured =
    Eio.Lazy.from_fun ~cancel:`Record (fun () ->
        let api_key =
          Eio_unix.run_in_systhread (fun () ->
              Option.map read_key settings.api_key_file)
        in
        make ?api_key ())
  in
  {
    identity with
    embed =
      (fun ~task ~check inputs ->
        check ();
        (Eio.Lazy.force configured).embed ~task ~check inputs);
  }

let make_provider ~sw ~fetch ~check = function
  | Bushel_config.Http settings -> http_provider ~sw ~fetch settings
  | Bushel_config.Mlx settings ->
      Retrieval_mlx.create ~model_dir:settings.model_dir
        ~dimensions:settings.dimensions ~max_tokens:settings.max_tokens
        ~query_instruction:settings.query_instruction ~check

let with_store ?config_file ~fs ~clock ~fetch ~check f =
  check ();
  let clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t) in
  let settings = config config_file in
  let bushel, sortal = roots ~fs settings in
  let bushel, sortal, database =
    Eio_unix.run_in_systhread (fun () -> location ~fs ~bushel ~sortal)
  in
  with_lock ~clock ~check (database ^ ".lock") (fun () ->
      Eio.Switch.run @@ fun sw ->
      Eio_unix.run_in_systhread (fun () -> private_file database);
      let index = Retrieval.Index.open_db ~sw Eio.Path.(fs / database) in
      let corpus = K.create index in
      (try K.load corpus ~fs ~bushel_dir:bushel ~sortal_dir:sortal
       with Sortal_carddav.Common.Error message ->
         failwith ("Sortal source: " ^ message));
      check ();
      let provider =
        Eio.Lazy.from_fun ~cancel:`Record (fun () ->
            Option.map (make_provider ~sw ~fetch ~check) settings.embeddings)
      in
      let images =
        Eio.Lazy.from_fun ~cancel:`Record (fun () ->
            check ();
            let output_dir =
              Eio_unix.run_in_systhread (fun () ->
                  Unix.realpath
                    Eio.Path.(native_exn (fs / settings.images_output_dir)))
            in
            let image_database =
              Filename.concat
                (Filename.dirname database)
                ("images-" ^ E.hash output_dir ^ ".sqlite3")
            in
            Eio_unix.run_in_systhread (fun () -> private_file image_database);
            let image_index =
              Retrieval.Index.open_db ~sw Eio.Path.(fs / image_database)
            in
            let image_corpus = K.create image_index in
            K.load_images image_corpus ~fs ~output_dir ~check;
            let image_provider =
              Eio.Lazy.from_fun ~cancel:`Record (fun () ->
                  Option.map
                    (fun (s : Bushel_config.mlx_embeddings) ->
                      Retrieval_mlx.Vision.create ~model_dir:s.model_dir
                        ~dimensions:s.dimensions ~max_tokens:s.max_tokens
                        ~query_instruction:s.query_instruction ~check)
                    settings.image_embeddings)
            in
            {
              corpus = image_corpus;
              provider = image_provider;
              settings = settings.image_embeddings;
              database = image_database;
            })
      in
      let result =
        f
          {
            corpus;
            provider;
            settings = settings.embeddings;
            database;
            images;
            clock;
          }
      in
      check ();
      result)

let obj fields =
  Jsont.Json.object'
    (List.map
       (fun (key, value) -> Jsont.Json.mem (Jsont.Json.name key) value)
       fields)

let status_fields (t : t) =
  let documents, passages = K.counts t.corpus in
  let coverage =
    match Eio.Lazy.force t.provider with
    | None -> Jsont.Json.null ()
    | Some provider ->
        let indexed, total = K.coverage t.corpus provider in
        obj
          [
            ("indexed", Jsont.Json.int indexed); ("total", Jsont.Json.int total);
          ]
  in
  let provider =
    match t.settings with
    | None -> Jsont.Json.null ()
    | Some (Bushel_config.Http settings) ->
        obj
          [
            ("backend", Jsont.Json.string "http");
            ("url", Jsont.Json.string settings.url);
            ("model", Jsont.Json.string settings.model);
            ("dimensions", Jsont.Json.int settings.dimensions);
          ]
    | Some (Bushel_config.Mlx settings) ->
        obj
          [
            ("backend", Jsont.Json.string "mlx");
            ("model_dir", Jsont.Json.string settings.model_dir);
            ("dimensions", Jsont.Json.int settings.dimensions);
            ("max_tokens", Jsont.Json.int settings.max_tokens);
            ("query_instruction", Jsont.Json.string settings.query_instruction);
            ( "model_identity",
              Jsont.Json.string (Option.get (Eio.Lazy.force t.provider)).model
            );
          ]
  in
  [
    ("database", Jsont.Json.string t.database);
    ("generation", Jsont.Json.string (K.generation t.corpus));
    ("documents", Jsont.Json.int documents);
    ("passages", Jsont.Json.int passages);
    ("embedding_provider", provider);
    ("embedding_coverage", coverage);
  ]

let status t = J.encode Jsont.json (obj (status_fields t))

let index_provider (t : t) =
  match Eio.Lazy.force t.provider with
  | Some provider -> provider
  | None -> invalid_arg "Configure embeddings with bushel knowledge configure"

let index_progress (t : t) provider on_progress =
  Option.map
    (fun progress ->
      let indexed, total = K.coverage t.corpus provider in
      progress ~indexed ~total;
      fun count -> progress ~indexed:(indexed + count) ~total)
    on_progress

let index_result t count =
  J.encode Jsont.json
    (obj (("newly_indexed", Jsont.Json.int count) :: status_fields t))

let index ?on_progress t ~check ~max_batches =
  if max_batches < 1 || max_batches > 10000 then
    invalid_arg "Embedding batch budget must be between 1 and 10000";
  let provider = index_provider t in
  let on_progress = index_progress t provider on_progress in
  let count = K.index ?on_progress t.corpus provider ~check ~max_batches in
  index_result t count

let index_all ?on_progress t ~check =
  let provider = index_provider t in
  let on_progress = index_progress t provider on_progress in
  let max_batches = 10000 in
  let rec loop count =
    check ();
    let batch_progress =
      Option.map (fun progress n -> progress (count + n)) on_progress
    in
    let added =
      K.index ?on_progress:batch_progress t.corpus provider ~check ~max_batches
    in
    let count = count + added in
    if added = 0 || added < provider.max_batch * max_batches then count
    else loop count
  in
  index_result t (loop 0)

let image_status_fields t =
  let images = Eio.Lazy.force t.images in
  let documents, passages = K.counts images.corpus in
  let provider = Eio.Lazy.force images.provider in
  let coverage =
    match provider with
    | None -> Jsont.Json.null ()
    | Some provider ->
        let indexed, total = K.image_coverage images.corpus provider in
        obj
          [
            ("indexed", Jsont.Json.int indexed); ("total", Jsont.Json.int total);
          ]
  in
  let configured =
    match (images.settings, provider) with
    | Some s, Some provider ->
        obj
          [
            ("backend", Jsont.Json.string "mlx-vision");
            ("model_dir", Jsont.Json.string s.model_dir);
            ("dimensions", Jsont.Json.int s.dimensions);
            ("max_tokens", Jsont.Json.int s.max_tokens);
            ("query_instruction", Jsont.Json.string s.query_instruction);
            ("model_identity", Jsont.Json.string provider.model);
          ]
    | _ -> Jsont.Json.null ()
  in
  [
    ("database", Jsont.Json.string images.database);
    ("generation", Jsont.Json.string (K.generation images.corpus));
    ("documents", Jsont.Json.int documents);
    ("passages", Jsont.Json.int passages);
    ("skipped_images", Jsont.Json.int (K.skipped_images images.corpus));
    ("embedding_provider", configured);
    ("embedding_coverage", coverage);
  ]

let image_status t = J.encode Jsont.json (obj (image_status_fields t))

let run_image_index ?on_progress t ~check ~max_batches =
  check ();
  let images = Eio.Lazy.force t.images in
  let provider =
    match Eio.Lazy.force images.provider with
    | Some provider -> provider
    | None -> invalid_arg "Configure image embeddings before indexing images"
  in
  let on_progress =
    Option.map
      (fun progress ->
        let indexed, total = K.image_coverage images.corpus provider in
        progress ~indexed ~total;
        fun count -> progress ~indexed:(indexed + count) ~total)
      on_progress
  in
  let budget = Option.value ~default:10000 max_batches in
  let rec loop count =
    check ();
    let batch_progress =
      Option.map (fun progress added -> progress (count + added)) on_progress
    in
    let added =
      K.index_images ?on_progress:batch_progress images.corpus provider ~check
        ~max_batches:budget
    in
    let count = count + added in
    if max_batches <> None || added = 0 || added < provider.max_batch * budget
    then count
    else loop count
  in
  let count = loop 0 in
  J.encode Jsont.json
    (obj (("newly_indexed", Jsont.Json.int count) :: image_status_fields t))

let index_images ?on_progress t ~check ~max_batches =
  if max_batches < 1 || max_batches > 10000 then
    invalid_arg "Embedding batch budget must be between 1 and 10000";
  run_image_index ?on_progress t ~check ~max_batches:(Some max_batches)

let index_images_all ?on_progress t ~check =
  run_image_index ?on_progress t ~check ~max_batches:None

let image_hit (chunk : Retrieval.Index.chunk) =
  let d = chunk.document in
  let str = Jsont.Json.string in
  obj
    [
      ("id", str d.id);
      ("revision", str d.revision);
      ("kind", str d.kind);
      ("title", str d.title);
      ("url", str d.url);
      ("date", str d.date);
      ("passage", Jsont.Json.int chunk.ordinal);
      ("snippet", str (E.clip 1000 chunk.text));
      ("pixel_path", str (List.assoc "pixel_path" d.facets));
      ("source_path", str (List.assoc "source_path" d.facets));
    ]

let images_search ?(mode = "semantic") ?(offset = 0) t ~check ~query =
  check ();
  let images = Eio.Lazy.force t.images in
  let provider =
    if mode = "lexical" then None else Eio.Lazy.force images.provider
  in
  if offset < 0 then invalid_arg "Image offset must be nonnegative";
  let hits = K.search_images images.corpus ?provider ~check ~mode query in
  let total = List.length hits in
  let hits = List.filteri (fun n _ -> n >= offset && n - offset < 20) hits in
  let coverage =
    match provider with
    | None -> Jsont.Json.null ()
    | Some provider ->
        let indexed, total = K.image_coverage images.corpus provider in
        obj
          [
            ("indexed", Jsont.Json.int indexed); ("total", Jsont.Json.int total);
          ]
  in
  check ();
  J.encode Jsont.json
    (obj
       [
         ("items", Jsont.Json.list (List.map image_hit hits));
         ("embedding_coverage", coverage);
         ("mode", Jsont.Json.string mode);
         ("bounded_results", Jsont.Json.bool true);
         ( "source_ids",
           Jsont.Json.list
             (List.map
                (fun (c : Retrieval.Index.chunk) ->
                  Jsont.Json.string c.document.id)
                hits) );
         ( "next_offset",
           if offset + List.length hits < total then
             Jsont.Json.int (offset + List.length hits)
           else Jsont.Json.null () );
       ])

let images_get t ~check ~id ~offset =
  check ();
  if offset < 0 then invalid_arg "Image offset must be nonnegative";
  let images = Eio.Lazy.force t.images in
  let document =
    match K.get images.corpus id with
    | Some document -> document
    | None -> invalid_arg "Unknown image source ID"
  in
  let length = String.length document.text in
  if
    offset > length
    || (offset < length && Char.code document.text.[offset] land 0xc0 = 0x80)
  then invalid_arg "Offset must be a UTF-8 boundary within the source";
  let text = E.clip 8192 (String.sub document.text offset (length - offset)) in
  let next = offset + String.length text in
  check ();
  J.encode Jsont.json
    (obj
       [
         ("id", Jsont.Json.string id);
         ("revision", Jsont.Json.string document.revision);
         ("title", Jsont.Json.string document.title);
         ("kind", Jsont.Json.string "image");
         ("url", Jsont.Json.string document.url);
         ( "pixel_path",
           Jsont.Json.string (List.assoc "pixel_path" document.facets) );
         ( "source_path",
           Jsont.Json.string (List.assoc "source_path" document.facets) );
         ("text", Jsont.Json.string text);
         ("offset", Jsont.Json.int offset);
         ( "next_offset",
           if next < length then Jsont.Json.int next else Jsont.Json.null () );
       ])
