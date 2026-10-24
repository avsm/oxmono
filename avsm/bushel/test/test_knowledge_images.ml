module K = Bushel_knowledge
module I = Retrieval.Index
module E = Retrieval.Embedding
module S = Bushel_knowledge_store
module J = Retrieval.Json

let check name value = if not value then failwith name
let noop () = ()

let write path value =
  Out_channel.with_open_bin path (fun oc -> output_string oc value)

let rec remove path =
  if (Unix.lstat path).st_kind = Unix.S_DIR then (
    Array.iter
      (fun name -> remove (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Sys.remove path

let field name codec =
  Jsont.Object.map Fun.id
  |> Jsont.Object.mem name codec ~enc:Fun.id
  |> Jsont.Object.finish

let number name text = J.decode (field name Jsont.int) text
let string name text = J.decode (field name Jsont.string) text

let write_images root entries =
  write
    (Filename.concat root "index.json")
    (match Srcsetter.list_to_json entries with
    | Ok s -> s
    | Error e -> failwith e)

let images root =
  let entries =
    List.map
      (fun name ->
        write (Filename.concat root (name ^ ".webp")) ("pixels:" ^ name);
        Srcsetter.v (name ^ ".webp")
          ("café " ^ name ^ " " ^ String.make 7000 'x')
          (name ^ String.make 7000 'y' ^ ".jpg")
          Srcsetter.MS.empty (640, 480))
      [ "garden"; "compiler" ]
  in
  let tiny =
    Srcsetter.v "tiny.webp" "tiny" "tiny.png" Srcsetter.MS.empty (30, 30)
  in
  write_images root (tiny :: entries)

let duplicate_outputs ~fs ~sw ~root provider =
  let output = Filename.concat root "duplicate-images" in
  Unix.mkdir output 0o700;
  Unix.mkdir (Filename.concat output "other") 0o700;
  let pixels = Filename.concat output "shared.webp" in
  write pixels "shared pixels";
  write (Filename.concat output "other/shared.webp") "other pixels";
  let png =
    Srcsetter.v "shared.webp" "pngalias" "shared.png" Srcsetter.MS.empty
      (640, 480)
  in
  let jpg =
    Srcsetter.v "shared.webp" "jpgalias" "shared.jpg"
      (Srcsetter.MS.of_list [ ("shared.320.webp", (320, 240)) ])
      (640, 480)
  in
  let other = { png with name = "other/shared.webp" } in
  let corpus = K.create (I.memory ~sw ()) in
  let load entries =
    write_images output entries;
    K.load_images corpus ~fs ~output_dir:output ~check:noop
  in
  let index () = K.index_images corpus provider ~check:noop ~max_batches:10 in
  let shared () = Option.get (K.get corpus (K.image_id "shared.webp")) in
  load [ png; other ];
  check "same basename in different directories stays distinct"
    (K.counts corpus = (2, 2) && index () = 2);
  let original = shared () and generation = K.generation corpus in
  load [ other; png; png ];
  check "identical duplicate rows preserve cached vectors and generation"
    (K.counts corpus = (2, 2)
    && index () = 0
    && (shared ()).revision = original.revision
    && K.generation corpus = generation);
  load [ jpg; png; other; png ];
  check "different source extensions share one output document"
    (K.counts corpus = (2, 2) && index () = 1);
  let shared = shared () in
  check "shared output preserves all source paths"
    (List.mem ("source_path", "shared.png") shared.facets
    && List.mem ("source_path", "shared.jpg") shared.facets);
  List.iter
    (fun query ->
      check "shared output retains searchable source aliases"
        (K.search_images corpus ~check:noop ~mode:"lexical" query
        |> List.exists (fun (hit : I.chunk) -> hit.document.id = shared.id)))
    [ "pngalias"; "jpgalias" ];
  let generation = K.generation corpus in
  load [ png; other; { jpg with variants = Srcsetter.MS.empty }; jpg ];
  check "alias order, repeated rows and variants do not invalidate vectors"
    (K.counts corpus = (2, 2)
    && index () = 0
    && K.generation corpus = generation);
  write pixels "changed shared pixels";
  load [ other; jpg; png ];
  check "shared pixel changes invalidate only one image"
    (K.image_coverage corpus provider = (1, 2) && index () = 1);
  let small =
    List.mapi
      (fun n dims ->
        Srcsetter.v
          (Printf.sprintf "small-%d.webp" n)
          "small" "small.png" Srcsetter.MS.empty dims)
      [ (30, 30); (1, 238); (238, 1); (31, 32); (32, 31) ]
  in
  let entries = [ other; jpg; png ] @ small @ [ List.hd small ] in
  load entries;
  check "small outputs are skipped once without reading their pixel files"
    (K.skipped_images corpus = 5 && K.counts corpus = (2, 2) && index () = 0);
  let edge =
    Srcsetter.v "edge.webp" "edge" "edge.png" Srcsetter.MS.empty (32, 32)
  in
  write (Filename.concat output edge.name) "edge pixels";
  load (edge :: entries);
  check "the exact 32-pixel boundary remains eligible"
    (K.skipped_images corpus = 5 && K.counts corpus = (3, 3) && index () = 1);
  load ({ edge with dims = (32, 31) } :: entries);
  check "shrinking below the threshold removes an existing image"
    (K.skipped_images corpus = 6
    && K.counts corpus = (2, 2)
    && K.get corpus (K.image_id edge.name) = None
    && index () = 0);
  load (edge :: entries);
  check "growing above the threshold indexes only the newly eligible image"
    (K.skipped_images corpus = 5 && index () = 1)

let incremental_images ~fs ~clock ~root ~bushel =
  let output = Filename.concat root "incremental-images" in
  Unix.mkdir output 0o700;
  let entries = ref [] in
  let add name =
    let filename = name ^ ".webp" in
    write (Filename.concat output filename) ("pixels:" ^ name);
    entries :=
      Srcsetter.v filename name (name ^ ".jpg") Srcsetter.MS.empty (640, 480)
      :: !entries
  in
  List.iter add [ "a"; "b"; "c"; "d"; "e" ];
  entries :=
    Srcsetter.v "tiny.webp" "tiny" "tiny.png" Srcsetter.MS.empty (30, 30)
    :: !entries;
  let save () = write_images output !entries in
  save ();
  let config = Filename.concat root "incremental-images.toml" in
  write config
    (Printf.sprintf "[data]\nlocal_dir=%S\n[images]\nimages_output_dir=%S\n"
       bushel output);
  let fetch =
    Fetch_mock.client (fun _ -> failwith "incremental fixture needs no HTTP")
  in
  let session f =
    S.with_store ~config_file:config ~fs ~clock ~fetch ~check:noop (fun store ->
        f (S.image_corpus store))
  in
  let batches = ref [] and progress = ref [] and round = ref 0 in
  let cancelled = ref false and mode = ref `Complete in
  let reset next =
    batches := [];
    progress := [];
    round := 0;
    cancelled := false;
    mode := next
  in
  let check_call () = if !cancelled then raise (Eio.Cancel.Cancelled Exit) in
  let provider : E.provider =
    {
      model = "incremental-image-fixture";
      dimensions = 2;
      max_batch = 2;
      embed =
        (fun ~task ~check:check_provider inputs ->
          check_provider ();
          match task with
          | `Query -> List.map (fun _ -> [ 1.; 0. ]) inputs
          | `Document ->
              let names =
                List.map
                  (fun input ->
                    match J.decode (Jsont.list Jsont.string) input with
                    | [ path; _ ] -> Filename.basename path
                    | _ -> failwith "invalid incremental image input")
                  inputs
              in
              batches := names :: !batches;
              incr round;
              (if !round = 2 then
                 match !mode with
                 | `Fail -> failwith "second image batch failed"
                 | `Cancel -> cancelled := true
                 | `Complete -> ());
              List.map (fun _ -> [ 1.; 0. ]) inputs);
    }
  in
  let run ?(max_batches = 10) provider corpus =
    K.index_images corpus provider ~check:check_call ~max_batches
      ~on_progress:(fun count -> progress := count :: !progress)
  in
  let inferred () = List.concat (List.rev !batches) in
  reset `Fail;
  check "image provider failure preserves completed batch progress"
    (try
       session (fun corpus -> ignore (run provider corpus));
       false
     with Failure message ->
       message = "second image batch failed" && !progress = [ 2 ]);
  check "failure only attempted its first two image batches"
    (List.rev !batches = [ [ "a.webp"; "b.webp" ]; [ "c.webp"; "d.webp" ] ]);
  reset `Complete;
  session (fun corpus ->
      check "undersized images do not enter incremental batches"
        (K.skipped_images corpus = 1);
      check "completed image batch survives closing its store session"
        (K.image_coverage corpus provider = (2, 5));
      check "bounded restart only indexes the next missing batch"
        (run ~max_batches:1 provider corpus = 2
        && inferred () = [ "c.webp"; "d.webp" ]
        && !progress = [ 2 ]
        && K.image_coverage corpus provider = (4, 5)));
  reset `Complete;
  session (fun corpus ->
      check "next session resumes the remaining partial batch"
        (run provider corpus = 1
        && inferred () = [ "e.webp" ]
        && !progress = [ 1 ]
        && K.image_coverage corpus provider = (5, 5)));
  let cancelled_provider =
    { provider with model = "cancelled-image-fixture" }
  in
  reset `Cancel;
  check "cancellation after inference preserves only completed batches"
    (try
       session (fun corpus -> ignore (run cancelled_provider corpus));
       false
     with Eio.Cancel.Cancelled Exit -> !progress = [ 2 ]);
  reset `Complete;
  session (fun corpus ->
      check "cancelled batch is absent from persisted image coverage"
        (K.image_coverage corpus cancelled_provider = (2, 5));
      check "cancelled run resumes only missing images"
        (run cancelled_provider corpus = 3
        && inferred () = [ "c.webp"; "d.webp"; "e.webp" ]
        && List.rev !progress = [ 2; 3 ]));
  let generation = session K.generation in
  entries := List.rev !entries;
  save ();
  reset `Complete;
  session (fun corpus ->
      check "reordered manifest retains generation and image embeddings"
        (K.generation corpus = generation
        && run provider corpus = 0
        && !batches = [] && !progress = []));
  add "f";
  save ();
  reset `Complete;
  session (fun corpus ->
      check "new image leaves previous vectors current"
        (K.image_coverage corpus provider = (5, 6));
      check "adding an image embeds that image only"
        (run provider corpus = 1 && inferred () = [ "f.webp" ]));
  let changed = Filename.concat output "c.webp" in
  let stat = Unix.stat changed in
  write changed "pixels:C";
  Unix.utimes changed stat.st_atime stat.st_mtime;
  reset `Complete;
  session (fun corpus ->
      check "changed pixels invalidate just one persisted image"
        (K.image_coverage corpus provider = (5, 6));
      check "changed pixels reembed just that image"
        (run provider corpus = 1 && inferred () = [ "c.webp" ]));
  entries :=
    List.filter (fun (image : Srcsetter.t) -> image.name <> "b.webp") !entries;
  Sys.remove (Filename.concat output "b.webp");
  save ();
  reset `Complete;
  session (fun corpus ->
      check "removed image disappears without reembedding remaining images"
        (K.get corpus (K.image_id "b.webp") = None
        && K.image_coverage corpus provider = (5, 5)
        && run provider corpus = 0
        && !batches = []);
      let hits =
        K.search_images corpus ~provider ~check:noop ~mode:"semantic" "image"
      in
      check "removed image vector is absent from visual search"
        (List.length hits = 5
        && List.for_all
             (fun (hit : I.chunk) -> hit.document.id <> K.image_id "b.webp")
             hits));
  reset `Complete;
  session (fun corpus ->
      check "another independent store session reuses the completed disk index"
        (K.image_coverage corpus provider = (5, 5)
        && run provider corpus = 0
        && !batches = [] && !progress = []))

let () =
  let root = Filename.temp_file "bushel-image-corpus" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let dir name =
    let path = Filename.concat root name in
    Unix.mkdir path 0o700;
    path
  in
  let output = dir "images" in
  images output;
  let data = dir "data" and bushel = dir "bushel" in
  let notes = Filename.concat bushel "notes" in
  Unix.mkdir notes 0o700;
  write
    (Filename.concat notes "note.md")
    "---\n\
     title: Text note\n\
     date: 2026-01-01\n\
     slug: text-note\n\
     ---\n\
     text corpus\n";
  ignore (dir "sortal");
  let saved =
    List.map
      (fun name -> (name, Sys.getenv_opt name))
      [ "XDG_DATA_HOME"; "SORTAL_DATA_DIR" ]
  in
  Unix.putenv "XDG_DATA_HOME" data;
  Unix.putenv "SORTAL_DATA_DIR" root;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (name, value) -> Unix.putenv name (Option.value ~default:"" value))
        saved;
      remove root)
    (fun () ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let fs = Eio.Stdenv.fs env and clock = Eio.Stdenv.clock env in
      let corpus = K.create (I.memory ~sw ()) in
      let load () = K.load_images corpus ~fs ~output_dir:output ~check:noop in
      load ();
      check "one document per image, variants not separate"
        (K.counts corpus = (2, 2));
      let calls = ref [] in
      let encode = function "garden" -> [ 1.; 0. ] | _ -> [ 0.; 1. ] in
      let provider : E.provider =
        {
          model = "visual-fixture";
          dimensions = 2;
          max_batch = 1;
          embed =
            (fun ~task ~check:check_call inputs ->
              check_call ();
              calls := (task, inputs) :: !calls;
              List.map
                (fun input ->
                  match task with
                  | `Query -> encode input
                  | `Document -> (
                      match J.decode (Jsont.list Jsont.string) input with
                      | [ path; digest ] ->
                          check "pixel hash describes exact document input"
                            (digest
                            = E.hash
                                (In_channel.with_open_bin path
                                   In_channel.input_all));
                          encode
                            (Filename.basename path |> Filename.remove_extension)
                      | _ -> failwith "bad image provider input"))
                inputs);
        }
      in
      let progress = ref [] in
      let indexed =
        K.index_images
          ~on_progress:(fun n -> progress := n :: !progress)
          corpus provider ~check:noop ~max_batches:10
      in
      check "images index in bounded batches"
        (indexed = 2 && List.rev !progress = [ 1; 2 ]);
      check "visual coverage" (K.image_coverage corpus provider = (2, 2));
      let found =
        K.search_images corpus ~provider ~check:noop ~mode:"semantic" "garden"
      in
      check "query uses common image vector space"
        ((List.hd found).document.id = K.image_id "garden.webp"
        && fst (List.hd !calls) = `Query);
      let before = List.length !calls in
      check "metadata lexical search uses no image inference"
        ((List.hd
            (K.search_images corpus ~check:noop ~mode:"lexical" "compiler"))
           .document
           .id = K.image_id "compiler.webp"
        && List.length !calls = before);
      load ();
      check "unchanged images reuse cached vectors"
        (K.index_images corpus provider ~check:noop ~max_batches:10 = 0);
      let garden = Filename.concat output "garden.webp" in
      let old = Unix.stat garden in
      write garden "pixels:GARDEN";
      Unix.utimes garden old.st_atime old.st_mtime;
      load ();
      check "same-length pixel change invalidates only changed image"
        (K.image_coverage corpus provider = (1, 2));
      let stale =
        {
          provider with
          embed =
            (fun ~task:_ ~check:_ inputs ->
              write garden "pixels:mutate";
              List.map (fun _ -> [ 1.; 0. ]) inputs);
        }
      in
      progress := [];
      check "pixel mutation during inference rejects writes and progress"
        (try
           ignore
             (K.index_images
                ~on_progress:(fun n -> progress := n :: !progress)
                corpus stale ~check:noop ~max_batches:1);
           false
         with I.Conflict -> !progress = []);
      load ();
      check "changed image can be retried"
        (K.index_images corpus provider ~check:noop ~max_batches:10 = 1);
      check "different visual models cannot reuse vectors"
        (K.image_coverage corpus { provider with model = "other" } = (0, 2));
      duplicate_outputs ~fs ~sw ~root provider;
      incremental_images ~fs ~clock ~root ~bushel;
      let config = Filename.concat root "config.toml" in
      let configure output =
        write config
          (Printf.sprintf
             "[data]\nlocal_dir=%S\n[images]\nimages_output_dir=%S\n" bushel
             output)
      in
      let fetch =
        Fetch_mock.client (fun _ -> failwith "unexpected HTTP inference")
      in
      let session f =
        S.with_store ~config_file:config ~fs ~clock ~fetch ~check:noop f
      in
      configure (Filename.concat root "absent-images");
      let add_model path =
        let existing = In_channel.with_open_bin config In_channel.input_all in
        write config
          (existing
          ^ Printf.sprintf
              "[image_embeddings]\nbackend='mlx'\nmodel_dir=%S\ndimensions=64\n"
              path)
      in
      add_model (Filename.concat root "absent-model");
      session (fun store ->
          check "text commands do not touch missing image directory"
            (number "documents" (S.status store) = 1));
      configure output;
      let model = dir "vision-model" in
      write (Filename.concat model "config.json") "{}";
      write (Filename.concat model "model.safetensors") "not native weights";
      add_model model;
      let db =
        session (fun store ->
            check "text counts exclude images"
              (number "documents" (S.status store) = 1);
            let status = S.image_status store in
            check "separate image counts" (number "documents" status = 2);
            check "image status reports excluded small outputs"
              (number "skipped_images" status = 1);
            let result =
              S.images_search ~mode:"lexical" store ~check:noop ~query:"garden"
            in
            let ids =
              J.decode (field "source_ids" (Jsont.list Jsont.string)) result
            in
            check "image search exposes stable source IDs"
              (ids = [ K.image_id "garden.webp" ]);
            let read =
              S.images_get store ~check:noop ~id:(List.hd ids) ~offset:0
            in
            check "image get refuses offsets within a UTF-8 codepoint"
              (try
                 ignore
                   (S.images_get store ~check:noop ~id:(List.hd ids) ~offset:4);
                 false
               with Invalid_argument _ -> true);
            let beyond =
              S.images_search ~mode:"lexical" ~offset:max_int store ~check:noop
                ~query:"garden"
            in
            check "large image offsets do not overflow"
              (J.decode (field "source_ids" (Jsont.list Jsont.string)) beyond
              = []);

            check "image get exposes pixel path"
              (string "pixel_path" read = Unix.realpath garden);
            string "database" status)
      in
      let existing = In_channel.with_open_bin config In_channel.input_all in
      write config
        (existing
        ^ Printf.sprintf
            "[embeddings]\nbackend='mlx'\nmodel_dir=%S\ndimensions=32\n"
            (Filename.concat root "missing-text-model"));
      session (fun store ->
          check "image status ignores an unavailable text model"
            (number "documents" (S.image_status store) = 2);
          check "image provider is independent of unavailable text weights"
            (Option.is_some (S.image_provider store));
          ignore
            (S.images_search ~mode:"lexical" store ~check:noop ~query:"garden");
          ignore
            (S.images_get store ~check:noop ~id:(K.image_id "garden.webp")
               ~offset:0);
          check "explicit text provider access still validates text model"
            (try
               ignore (S.provider store);
               false
             with Unix.Unix_error (Unix.ENOENT, _, _) -> true));
      let other = dir "other-images" in
      images other;
      configure other;
      session (fun store ->
          check "different image roots have separate databases"
            (string "database" (S.image_status store) <> db));
      let bad =
        Srcsetter.v "../escape.webp" "escape" "escape.jpg" Srcsetter.MS.empty
          (32, 32)
      in
      write
        (Filename.concat output "index.json")
        (match Srcsetter.list_to_json [ bad ] with
        | Ok s -> s
        | Error e -> failwith e);
      check "image paths cannot escape their output root"
        (try
           load ();
           false
         with Invalid_argument _ -> true);
      check "invalid refresh preserves last image snapshot"
        (K.counts corpus = (2, 2) && K.skipped_images corpus = 1));
  print_endline "image corpus isolation, pixel invalidation and search passed"
