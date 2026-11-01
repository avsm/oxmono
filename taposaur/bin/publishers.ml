open Taposaur
module M = Taposaur_mirrors
module C = Taposaur_catalog
module R = Taposaur_review
module S = Taposaur_review_store
module P = Taposaur_paper
let ( / ) = Eio.Path.( / )
let get = function Ok x -> x | Error e -> failwith e
let run db reviews mirrors snapshot glutton datacite csv jobs limit =
  try Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
    if jobs < 1 || jobs > 16 then invalid_arg "jobs must be 1..16";
    if Option.fold ~none:false ~some:(fun n -> n<1) limit then invalid_arg "limit must be positive";
    let audit = Taposaur_audit_cli.open_session ~sw ~fs:env#fs
      ~component:"taposaur.publishers" in
    Taposaur_audit.with_operation ~session:audit ~action:"mirrors.publisher.batch"
      ~subject:("snapshot:" ^ string_of_int snapshot)
      ~details:["catalogue",db;"mirrors",mirrors;"jobs",string_of_int jobs]
      (fun scoped ->
    let audit=Option.get scoped in
    let store = M.open_write ~audit ~sw (env#fs / mirrors) in
    Option.iter (fun path ->
      let body=Eio.Path.with_open_in (env#fs / path) (fun flow ->
        Eio.Buf_read.(parse_exn take_all ~max_size:(32*1024*1024) flow)) in
      M.import_csv store ~path body) csv;
    let reviews = S.open_write ~audit ~sw (env#fs / reviews) in
    let parent = S.snapshot reviews snapshot in
    let catalog = C.open_write ~sw (env#fs / db) in
    let providers = Option.fold ~none:[] ~some:(fun x -> [M.Glutton x]) glutton @
      Option.fold ~none:[] ~some:(fun x -> [M.Datacite_clickhouse x]) datacite in
    let http=Fetch_main.std ~sw ~max_concurrent:jobs
      ~retry:(Fetch.Retry.v ~max_retries:0 ()) env in
    let client=M.client ~audit ~clock:env#clock ~timeout:30. ~providers http in
    let rec collect after acc =
      let rows=S.browse ?after ~publisher:"" ~limit:100 reviews snapshot in
      match List.rev rows with [] -> List.rev acc
      | last::_ -> collect (Some last.R.doi) (List.rev_append rows acc) in
    let rows=collect None [] in
    let rows=match limit with None -> rows | Some n -> List.filteri (fun i _ -> i<n) rows in
    let examined=ref 0 and resolved=ref 0 and applied=ref 0 in
    Eio.Fiber.List.iter ~max_fibers:jobs (fun (row:R.observation) ->
      let doi=get (Doi.of_string row.doi) in
      let uid=get (Uid.of_input row.doi) in
      let known=Option.bind (C.find catalog uid) (fun s -> s.Summary.bibliography.publisher) in
      let evidence=match known with
        | Some p when String.trim p<>"" -> None
        | _ -> match M.lookup client store doi with Some e -> Some e
          | None -> M.fallback store doi in
      Option.iter (fun (e:Publisher_evidence.t) ->
        let changed=Taposaur_audit.with_operation ~session:audit
          ~action:"catalog.publisher.fill" ~subject:(Uid.to_string uid)
          ~details:["provider",e.provider;"sha256",e.sha256]
          ~result_details:(fun changed -> ["changed",string_of_bool changed])
          (fun _ -> C.Metadata.fill_publisher catalog ~uid ~publisher:e.publisher
            ~update_detail:(fun index raw ->
              get (P.cache_string (P.with_publisher e (get (P.of_cache_string ~index raw)))))) in
        if changed then incr applied) evidence;
      if evidence<>None then incr resolved;
      incr examined;
      if !examined mod 100=0 || !examined=List.length rows then
        Printf.printf "examined=%d/%d resolved=%d catalogue_updated=%d\n%!"
          !examined (List.length rows) !resolved !applied) rows;
    match limit with
    | Some _ -> Printf.printf "Limited run: no snapshot published.\n%!"
    | None ->
      let publishers=List.filter_map (fun (m:R.member) ->
        M.find store (get (Doi.of_string m.doi))) (S.members reviews parent.run_id) in
      let catalog_read=C.open_read ~sw (env#fs / db) in
      let id=Taposaur_review_run.snapshot ~publishers reviews catalog_read
        ~run_id:parent.run_id ~parent:(Some snapshot)
        ~label:"Publisher recovery from DOI mirrors and original CSV"
        ~actor:(Option.value ~default:"system" (Sys.getenv_opt "TAPOSAUR_ACTOR")) in
      Printf.printf "snapshot=%d examined=%d resolved=%d catalogue_updated=%d\n%!"
        id !examined !resolved !applied)));
    Ok ()
  with ex -> Error (Printexc.to_string ex)
let () =
  let open Cmdliner in
  let opt name default doc = Arg.(value & opt string default & info [name] ~doc) in
  let optional name doc = Arg.(value & opt (some string) None & info [name] ~doc) in
  let term=Term.(const run $
    opt "db" "taposaur.db" "Catalogue database." $
    opt "reviews-db" "taposaur-reviews.db" "Review database." $
    opt "mirrors-db" (M.default_path ()) "Durable publisher evidence database." $
    Arg.(required & opt (some int) None & info ["snapshot"] ~doc:"Parent snapshot ID.") $
    optional "glutton" "Crossref Glutton service URL." $
    optional "datacite-clickhouse" "ClickHouse DataCite mirror URL." $
    optional "fallback-csv" "Original CSV with doi,publisher columns." $
    Arg.(value & opt int 4 & info ["jobs"] ~doc:"Concurrent lookups (1..16).") $
    Arg.(value & opt (some int) None & info ["limit"] ~doc:"Limit attempts; do not publish a snapshot.")) in
  exit (Cmd.eval_result (Cmd.v (Cmd.info "taposaur-publishers"
    ~doc:"Recover missing publishers with retained evidence and audited updates.") term))
