(* Abstract evidence: fetch OpenAlex abstracts for listed DOIs into the
   evidence store, and fill catalogue rows that lack an abstract. *)
open Cmdliner
module B = Taposaur_abstracts
module C = Taposaur_catalog
module P = Taposaur_paper
let ( / ) = Eio.Path.( / )
let get = function Ok x -> x | Error e -> failwith e

let execute ~action ~subject ?(details=[]) f =
  try
    Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
      let audit = Taposaur_audit_cli.open_session ~sw ~fs:env#fs
        ~component:"taposaur.abstracts" in
      Taposaur_audit.with_operation ~session:audit ~details ~action ~subject
        (fun scoped -> f env sw (Option.get scoped))));
    Ok ()
  with
  | Failure e | Invalid_argument e -> Error e
  | Eio.Io _ as e -> Error (Printexc.to_string e)
  | Sqlite3.Error e | Sqlite3.SqliteError e -> Error e

let dois_of_file env file =
  let lines = String.split_on_char '\n' (Eio.Path.load (env#fs / file)) in
  List.filter_map (fun line -> match String.trim line with
    | "" -> None | s when s.[0] = '#' -> None
    | s -> let s = if String.starts_with ~prefix:"doi:" s then
        String.sub s 4 (String.length s - 4) else s in
      match Taposaur.Doi.of_string s with
      | Ok d -> Some d
      | Error e -> Printf.eprintf "%s: %s\n%!" s e; None) lines

(* Batches of up to 100 cost one OpenAlex credit each. When the daily
   budget or the rate limit refuses a batch, remaining DOIs continue as
   single-work reads, which are free, after a pause. *)
let fetch store_path doi_file retry limit interval api_key_env =
  let api_key = Option.bind api_key_env (fun name ->
    match Sys.getenv_opt name with None | Some "" -> None | k -> k) in
  execute ~action:"abstracts.fetch" ~subject:store_path
    ~details:["dois",doi_file;"retry",string_of_bool retry;
      "keyed",string_of_bool (api_key <> None)] (fun env sw audit ->
  let store = B.open_write ~audit ~sw (env#fs / store_path) in
  let http = Fetch.with_limits ~clock:env#mono_clock ~max_concurrent:1
    ~min_interval:(Duration.of_f interval) (Clients.http ~sw env) in
  let client = B.client ~audit ~clock:env#clock ?api_key http in
  let dois = dois_of_file env doi_file in
  (* A DOI is pending until its work evidence is stored: an abstract
     accepted before schema 2 is queried again for its bibliography. *)
  let pending = List.filter (fun d -> not (B.known store d) &&
    (retry || B.find store d <> None || not (B.attempted store d))) dois in
  let pending = match limit with None -> pending
    | Some n -> List.filteri (fun i _ -> i < n) pending in
  Printf.eprintf "Listed %d; pending %d; accepted so far %d; works %d\n%!"
    (List.length dois) (List.length pending) (B.count store) (B.count_works store);
  let accepted = ref 0 and examined = ref 0 and singles = ref false in
  let refusals = ref 0 in
  let report () = Printf.eprintf "Examined %d; accepted %d; store holds %d abstracts, %d works\n%!"
    !examined !accepted (B.count store) (B.count_works store) in
  let rec batches = function
    | [] -> ()
    | dois ->
        let batch, rest = match dois with
          | d :: rest when !singles -> [d], rest
          | _ -> match B.batches dois with
            | [] -> [], []
            | batch :: more -> batch, List.concat more in
        let result = if !singles then
            Result.map (fun e -> Option.to_list (Option.map (fun e -> List.hd batch, e) e))
              (B.lookup client store (List.hd batch))
          else B.lookup_many client store batch in
        (match result with
         | Ok found ->
             accepted := !accepted + List.length found;
             examined := !examined + List.length batch; refusals := 0;
             if Stdlib.(!examined / 1000 > (!examined - List.length batch) / 1000)
             then report ();
             batches rest
         | Error (B.Budget | B.Rate_limited as r) ->
             incr refusals;
             if not !singles then begin
               Printf.eprintf "%s: continuing one DOI at a time\n%!" (B.refusal_name r);
               singles := true
             end;
             if !refusals > 5 then failwith ("Stopped after repeated " ^ B.refusal_name r);
             Eio.Time.sleep env#clock (10. *. float_of_int !refusals);
             batches (batch @ rest)
         | Error r when List.length batch > 1 ->
             (* Retry the members singly so one bad DOI is attributed to itself. *)
             Printf.eprintf "batch of %d: %s; retrying singly\n%!"
               (List.length batch) (B.refusal_name r);
             List.iter (fun d -> match B.lookup client store d with
               | Ok (Some _) -> incr accepted
               | Ok None -> ()
               | Error r -> Printf.eprintf "%s: %s\n%!" (Taposaur.Doi.to_string d)
                   (B.refusal_name r)) batch;
             examined := !examined + List.length batch; batches rest
         | Error r ->
             Printf.eprintf "%s: %s\n%!" (Taposaur.Doi.to_string (List.hd batch)) (B.refusal_name r);
             examined := !examined + List.length batch; batches rest) in
  batches pending;
  report ();
  Taposaur_audit.record audit ~action:"abstracts.fetch.result" ~subject:store_path
    ~details:["examined",string_of_int !examined;"accepted",string_of_int !accepted;
      "total",string_of_int (B.count store);"works",string_of_int (B.count_works store)] ();
  Printf.printf "Examined %d; accepted %d; store holds %d abstracts and %d works\n"
    !examined !accepted (B.count store) (B.count_works store))

let fill store_path db doi_file =
  execute ~action:"abstracts.fill" ~subject:db ~details:["store",store_path]
    (fun env sw audit ->
  let store = B.open_read ~sw (env#fs / store_path) in
  let catalog = C.open_write ~sw (env#fs / db) in
  let dois = match doi_file with
    | Some file -> dois_of_file env file
    | None -> let acc = ref [] in
        B.iter store (fun e -> acc := get (Taposaur.Doi.of_string e.doi) :: !acc);
        List.rev !acc in
  let filled = ref 0 and examined = ref 0 in
  List.iter (fun doi ->
    incr examined;
    match B.find store doi with
    | None -> ()
    | Some e ->
        let uid = get (Taposaur.Uid.of_input ("doi:" ^ Taposaur.Doi.to_string doi)) in
        let changed = Taposaur_audit.with_operation ~session:audit
          ~action:"catalog.abstract.fill" ~subject:(Taposaur.Uid.to_string uid)
          ~details:["provider",e.provider;"sha256",e.sha256]
          ~result_details:(fun changed -> ["changed",string_of_bool changed])
          (fun _ -> C.Metadata.fill_abstract catalog ~uid ~abstract:e.abstract
            ~update_detail:(fun index raw ->
              get (P.cache_string (P.with_abstract e (get (P.of_cache_string ~index raw)))))) in
        if changed then incr filled;
        if !examined mod 1000 = 0 then
          Printf.eprintf "Examined %d; filled %d\n%!" !examined !filled) dois;
  Taposaur_audit.record audit ~action:"abstracts.fill.result" ~subject:db
    ~details:["examined",string_of_int !examined;"filled",string_of_int !filled] ();
  Printf.printf "Examined %d; filled %d catalogue rows\n" !examined !filled)

let store = Arg.(value & opt string "taposaur-abstracts.db" & info ["store"]
  ~doc:"Abstract evidence SQLite database. Also TAPOSAUR_ABSTRACTS_DB for readers.")
let db = Arg.(value & opt string "taposaur.db" & info ["db"] ~doc:"Local catalogue.")
let command = Cmd.group (Cmd.info "abstracts"
  ~doc:"Fetch and apply exact-DOI abstract evidence from OpenAlex.") [
  Cmd.v (Cmd.info "fetch" ~doc:"Look up abstracts and bibliographies for the DOIs listed in a file.")
    Term.(const fetch $ store $
      Arg.(required & pos 0 (some string) None & info [] ~docv:"DOIS"
        ~doc:"File of DOIs, one per line; blank lines and # comments are ignored.") $
      Arg.(value & flag & info ["retry"]
        ~doc:"Query DOIs again whose earlier lookups found no work or no abstract.") $
      Arg.(value & opt (some int) None & info ["limit"] ~doc:"Maximum DOIs to query.") $
      Arg.(value & opt float 0.1 & info ["interval"]
        ~doc:"Seconds between requests; OpenAlex allows 10 a second without a key, 30 with one.") $
      Arg.(value & opt (some string) None & info ["api-key-env"]
        ~doc:"Environment variable holding an OpenAlex API key. The value is never printed."));
  Cmd.v (Cmd.info "fill" ~doc:"Fill catalogue rows that lack an abstract from stored evidence.")
    Term.(const fill $ store $ db $
      Arg.(value & opt (some string) None & info ["dois"]
        ~doc:"Restrict to the DOIs listed in this file; default is every stored abstract."))]
