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

let read api ~accounts ~bot ?(on_error = log_error) () =
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
  let repos = List.sort_uniq String.compare (List.map fst wanted) in
  List.concat_map
    (fun did ->
      guard did (fun () ->
          let repo = Api.resolve_repo api did in
          if repo.manifest.spindle = None then []
          else
            (* Collaborators are read once per repository, and only when an
               author is not its owner. *)
            let collaborators = lazy (Api.collaborators api repo) in
            let rooms =
              rooms ~repo:did ~owner:repo.owner ~collaborators wanted
            in
            if rooms = [] then [] else [ { repo; rooms } ]))
    repos
