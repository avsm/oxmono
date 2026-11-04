(* SPDX-License-Identifier: ISC *)
module Api = Tangled.Api
module M = Tangled.Matrix

type t = { repo : Api.repository; rooms : string list }

let log_error what e =
  Logs.warn (fun m -> m "Skipping %s: %s" what (Printexc.to_string e))

let rooms ~repo ~owner ~collaborators wanted =
  List.filter_map
    (fun (r, (author, room)) ->
      if
        r = repo
        && (author = owner || List.mem author (Lazy.force collaborators))
      then Some room
      else None)
    wanted
  |> List.sort_uniq String.compare

(* A spindle is named by DID or by host, which is its [did:web]. *)
let spindle_did s =
  if String.starts_with ~prefix:"did:" s then s else "did:web:" ^ s

let served ~spindles (repo : Api.repository) =
  match repo.manifest.spindle with
  | None -> false
  | Some s -> spindles = [] || List.mem s (List.map spindle_did spindles)

let read api ~accounts ~bot ?(everything = []) ?(spindles = [])
    ?(resolve = fun did -> Api.resolve_repo api did) ?(on_error = log_error) ()
    =
  let guard what f =
    try f () with
    | Eio.Cancel.Cancelled _ as e -> raise e
    | e ->
        on_error what e;
        []
  in
  (* [(repository DID, (author, room))] for every binding naming [bot]. *)
  let wanted =
    List.concat_map
      (fun did ->
        guard did (fun () ->
            List.filter_map
              (fun (b : M.t) ->
                if b.bot = bot then Some (b.repo, (did, b.room)) else None)
              (M.list api ~did)))
      (List.sort_uniq String.compare accounts)
  in
  (* The accounts' own repositories go to the firehose rooms. Each is its
     account's own, so the operator's choice binds it. *)
  let wanted =
    if everything = [] then wanted
    else
      wanted
      @ List.concat_map
          (fun did ->
            guard did (fun () ->
                List.concat_map
                  (fun (_, (d : Api.Lex.Repo.Declaration.main)) ->
                    List.map (fun room -> (d.repo, (did, room))) everything)
                  (Api.list_declarations api ~did ())))
          (List.sort_uniq String.compare accounts)
  in
  let repos = List.sort_uniq String.compare (List.map fst wanted) in
  (* An account may have many repositories, each a few requests to resolve,
     so several are resolved at once. *)
  List.concat
  @@ Eio.Fiber.List.map ~max_fibers:8
       (fun did ->
         guard did (fun () ->
             let repo = resolve did in
             if not (served ~spindles repo) then []
             else
               (* Collaborators are read once per repository, and only when an
               author is not its owner. *)
               let collaborators = lazy (Api.collaborators api repo) in
               let rooms =
                 rooms ~repo:did ~owner:repo.owner ~collaborators wanted
               in
               if rooms = [] then [] else [ { repo; rooms } ]))
       repos
