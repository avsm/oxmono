module Response = Response
module K = Bushel_knowledge
module E = Retrieval.Embedding
module J = Retrieval.Json

type images = {
  path : Eio.Fs.dir_ty Eio.Path.t;
  write : bool;
  corpus : K.t;
  provider : E.provider option Eio.Lazy.t;
  settings : Bushel_config.mlx_embeddings option;
  database : string;
}

type t = {
  path : Eio.Fs.dir_ty Eio.Path.t;
  write : bool;
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

let data_home = Xdge.Paths.data_home

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

let with_lock ~clock ~check path f =
  let wait () =
    check ();
    Eio.Time.sleep clock 0.05
  in
  Eioext.Native.with_lock ~wait ~path (fun () ->
      check ();
      f ())

let read_key path =
  try Sekrit.read_reference ~max_bytes:16384 path
  with Sekrit.Error message -> invalid_arg message

let manifest roots =
  Eio_unix.run_in_systhread (fun () ->
      let rec scan ancestors path acc =
        let stat = Unix.stat path in
        match stat.Unix.st_kind with
        | Unix.S_DIR ->
            let identity = (stat.st_dev, stat.st_ino) in
            if List.mem identity ancestors then
              invalid_arg ("Source directory cycle: " ^ path);
            Sys.readdir path |> Array.to_list |> List.sort String.compare
            |> List.fold_left
                 (fun acc name ->
                   if name = ".git" then acc
                   else
                     scan (identity :: ancestors)
                       (Filename.concat path name)
                       acc)
                 acc
        | Unix.S_REG ->
            Printf.sprintf "%s:%d:%d:%d:%.17g:%.17g" path stat.st_dev
              stat.st_ino stat.st_size stat.st_mtime stat.st_ctime
            :: acc
        | _ -> acc
      in
      List.fold_left (fun acc root -> scan [] root acc) [] roots
      |> List.rev
      |> J.encode (Jsont.list Jsont.string)
      |> E.hash)

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

let make_provider ~runtime ~sw ~fetch ~check = function
  | Bushel_config.Http settings -> http_provider ~sw ~fetch settings
  | Bushel_config.Mlx settings ->
      Retrieval_mlx.create ~runtime ~model_dir:settings.model_dir
        ~dimensions:settings.dimensions ~max_tokens:settings.max_tokens
        ~query_instruction:settings.query_instruction ~check

let with_store ?config_file ?runtime ?(refresh = false) ?(write = false) ~fs
    ~clock ~fetch ~check f =
  check ();
  let clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t) in
  let settings = config config_file in
  let bushel, sortal = roots ~fs settings in
  let bushel, sortal, database =
    Eio_unix.run_in_systhread (fun () -> location ~fs ~bushel ~sortal)
  in
  Eio.Switch.run @@ fun sw ->
  let runtime =
    match runtime with
    | Some runtime -> runtime
    | None -> Retrieval_mlx.Runtime.create ~sw
  in
  let open_corpus ~refresh database load roots =
    let path = Eio.Path.(fs / database) in
    let update () =
      with_lock ~clock ~check (database ^ ".lock") (fun () ->
          Eio.Switch.run @@ fun update_sw ->
          Eio_unix.run_in_systhread (fun () -> private_file database);
          let index = Retrieval.Index.open_db ~sw:update_sw path in
          let corpus = K.create index in
          let restored = K.restore corpus in
          let fingerprint = manifest roots in
          if
            (not restored)
            || Retrieval.Index.metadata index "source_manifest_v1"
               <> Some fingerprint
          then load corpus [ ("source_manifest_v1", fingerprint) ])
    in
    let initialized () =
      Eio_unix.run_in_systhread (fun () ->
          match Unix.lstat database with
          | stat when private_stat Unix.S_REG stat -> stat.st_size > 0
          | _ ->
              invalid_arg
                "Knowledge index must be an owned private regular file"
          | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false)
    in
    if refresh || not (initialized ()) then update ();
    let open_index () =
      if write then Retrieval.Index.open_db ~sw path
      else Retrieval.Index.open_snapshot ~sw path
    in
    let index = open_index () in
    let corpus = K.create index in
    if not (K.restore corpus) then (
      (* Old indexes have vectors but no domain snapshot. Publish metadata once
         before opening a new pinned reader, preserving their existing vectors. *)
      update ();
      let index = open_index () in
      let corpus = K.create index in
      if not (K.restore corpus) then failwith "Knowledge snapshot is missing";
      corpus)
    else corpus
  in
  let corpus =
    open_corpus ~refresh database
      (fun corpus metadata ->
        try K.load ~metadata corpus ~fs ~bushel_dir:bushel ~sortal_dir:sortal
        with Sortal_carddav.Common.Error message ->
          failwith ("Sortal source: " ^ message))
      [ bushel; sortal ]
  in
  check ();
  let provider =
    Eio.Lazy.from_fun ~cancel:`Record (fun () ->
        Option.map
          (make_provider ~runtime ~sw ~fetch ~check)
          settings.embeddings)
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
        let image_corpus =
          open_corpus ~refresh image_database
            (fun corpus metadata ->
              K.load_images ~metadata corpus ~fs ~output_dir ~check)
            [ output_dir ]
        in
        let image_provider =
          Eio.Lazy.from_fun ~cancel:`Record (fun () ->
              Option.map
                (fun (s : Bushel_config.mlx_embeddings) ->
                  Retrieval_mlx.Vision.create ~runtime ~model_dir:s.model_dir
                    ~dimensions:s.dimensions ~max_tokens:s.max_tokens
                    ~query_instruction:s.query_instruction ~check)
                settings.image_embeddings)
        in
        {
          path = Eio.Path.(fs / image_database);
          write;
          corpus = image_corpus;
          provider = image_provider;
          settings = settings.image_embeddings;
          database = image_database;
        })
  in
  let result =
    f
      {
        path = Eio.Path.(fs / database);
        write;
        corpus;
        provider;
        settings = settings.embeddings;
        images;
        database;
        clock;
      }
  in
  check ();
  result

let provider_details ?(vision = false) settings provider =
  match (settings, provider) with
  | Some (Bushel_config.Http s), _ ->
      Some
        (Response.Http
           { url = s.url; model = s.model; dimensions = s.dimensions })
  | Some (Bushel_config.Mlx s), Some provider ->
      Some
        (Response.Mlx
           {
             backend = (if vision then "mlx-vision" else "mlx");
             model_dir = s.model_dir;
             dimensions = s.dimensions;
             max_tokens = s.max_tokens;
             query_instruction = s.query_instruction;
             model_identity = provider.E.model;
           })
  | _ -> None

let with_read_snapshot ~path ~write corpus f =
  if not write then f corpus
  else
    Eio.Switch.run (fun sw ->
        let index = Retrieval.Index.open_snapshot ~sw path in
        let corpus = K.create index in
        if not (K.restore corpus) then failwith "Knowledge snapshot is missing";
        f corpus)

let status (t : t) =
  with_read_snapshot ~path:t.path ~write:t.write t.corpus (fun corpus ->
      let documents, passages = K.counts corpus in
      let provider = Eio.Lazy.force t.provider in
      let coverage =
        Option.map
          (fun p ->
            let indexed, total = K.coverage corpus p in
            Response.{ indexed; total })
          provider
      in
      Response.
        {
          database = t.database;
          generation = K.generation corpus;
          documents;
          passages;
          provider = provider_details t.settings provider;
          coverage;
          skipped_images = None;
          newly_indexed = None;
        })

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
  { (status t) with Response.newly_indexed = Some count }

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

let image_status t =
  let images = Eio.Lazy.force t.images in
  with_read_snapshot ~path:images.path ~write:images.write images.corpus
    (fun corpus ->
      let documents, passages = K.counts corpus in
      let provider = Eio.Lazy.force images.provider in
      let coverage =
        Option.map
          (fun p ->
            let indexed, total = K.image_coverage corpus p in
            Response.{ indexed; total })
          provider
      in
      Response.
        {
          database = images.database;
          generation = K.generation corpus;
          documents;
          passages;
          provider =
            provider_details ~vision:true
              (Option.map (fun s -> Bushel_config.Mlx s) images.settings)
              provider;
          coverage;
          skipped_images = Some (K.skipped_images corpus);
          newly_indexed = None;
        })

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
  { (image_status t) with Response.newly_indexed = Some count }

let index_images ?on_progress t ~check ~max_batches =
  if max_batches < 1 || max_batches > 10000 then
    invalid_arg "Embedding batch budget must be between 1 and 10000";
  run_image_index ?on_progress t ~check ~max_batches:(Some max_batches)

let index_images_all ?on_progress t ~check =
  run_image_index ?on_progress t ~check ~max_batches:None

let image_hit (chunk : Retrieval.Index.chunk) =
  let d = chunk.document in
  Response.
    {
      id = d.id;
      revision = d.revision;
      kind = d.kind;
      title = d.title;
      url = d.url;
      date = d.date;
      passage = chunk.ordinal;
      snippet = E.clip 1000 chunk.text;
      pixel_path = List.assoc_opt "pixel_path" d.facets;
      source_path = List.assoc_opt "source_path" d.facets;
    }

let images_search ?(mode = "semantic") ?(offset = 0) t ~check ~query =
  check ();
  let images = Eio.Lazy.force t.images in
  let provider =
    if mode = "lexical" then None else Eio.Lazy.force images.provider
  in
  if offset < 0 then invalid_arg "Image offset must be nonnegative";
  let all_hits = K.search_images images.corpus ?provider ~check ~mode query in
  let total = List.length all_hits in
  let hits =
    List.filteri (fun n _ -> n >= offset && n - offset < 20) all_hits
  in
  let coverage =
    Option.map
      (fun p ->
        let indexed, total = K.image_coverage images.corpus p in
        Response.{ indexed; total })
      provider
  in
  check ();
  Response.
    {
      page =
        {
          generation = K.generation images.corpus;
          items = List.map image_hit hits;
          next_offset =
            (if offset + List.length hits < total then
               Some (offset + List.length hits)
             else None);
        };
      mode = Some mode;
      coverage;
      source = None;
      order = None;
      as_of = None;
      source_ids =
        Some (List.map (fun (c : Retrieval.Index.chunk) -> c.document.id) hits);
    }

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
  Response.
    {
      id;
      revision = document.revision;
      title = document.title;
      kind = "image";
      url = document.url;
      text;
      offset = Some offset;
      width = Option.map int_of_string (List.assoc_opt "width" document.facets);
      height =
        Option.map int_of_string (List.assoc_opt "height" document.facets);
      media_type = None;
      next_offset = (if next < length then Some next else None);
      pixel_path = List.assoc_opt "pixel_path" document.facets;
      source_path = List.assoc_opt "source_path" document.facets;
    }
