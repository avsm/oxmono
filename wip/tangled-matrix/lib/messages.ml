(* SPDX-License-Identifier: ISC *)

type entry = {
  repo : string;
  pipeline : string;
  room : string;
  mutable event : string option;
  mutable finished : bool;
  mutable shown : string;
}

type t = { path : Eio.Fs.dir_ty Eio.Path.t; mutable entries : entry list }
(* Newest first. *)

let entry_jsont =
  Jsont.Object.map ~kind:"entry" (fun repo pipeline room event finished shown ->
      { repo; pipeline; room; event; finished; shown })
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun e -> e.repo)
  |> Jsont.Object.mem "pipeline" Jsont.string ~enc:(fun e -> e.pipeline)
  |> Jsont.Object.mem "room" Jsont.string ~enc:(fun e -> e.room)
  |> Jsont.Object.opt_mem "event" Jsont.string ~enc:(fun e -> e.event)
  |> Jsont.Object.mem "finished" Jsont.bool ~enc:(fun e -> e.finished)
  |> Jsont.Object.mem "shown" Jsont.string
       ~dec_absent:(fun () -> "")
       ~enc:(fun e -> e.shown)
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let file_jsont = Jsont.list entry_jsont

let load path =
  let path = (path :> Eio.Fs.dir_ty Eio.Path.t) in
  match Eio.Path.load path with
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
      { path; entries = [] }
  | text -> (
      match Jsont_bytesrw.decode_string file_jsont text with
      | Ok entries -> { path; entries }
      | Error e ->
          let dir, name = Option.get (Eio.Path.split path) in
          Logs.err (fun m ->
              m "%a does not parse, so it is moved aside: %s" Eio.Path.pp path e);
          Eio.Path.rename path Eio.Path.(dir / (name ^ ".bad"));
          { path; entries = [] })

let save t =
  match Jsont_bytesrw.encode_string file_jsont t.entries with
  | Error e -> failwith e
  | Ok data -> (
      match Matrix_client.Profile_store.atomic_write ~path:t.path ~data with
      | Ok () -> ()
      | Error e -> failwith (Matrix_client.Error.to_string e))

let find t ~repo ~pipeline ~room =
  List.find_opt
    (fun e -> e.repo = repo && e.pipeline = pipeline && e.room = room)
    t.entries

let limit = 1000

let add t e =
  let entries = e :: t.entries in
  t.entries <-
    (if List.length entries <= limit then entries
     else
       (* The oldest finished entries go first. *)
       let excess = ref (List.length entries - limit) in
       List.rev entries
       |> List.filter (fun x ->
           if !excess > 0 && x.finished then (
             decr excess;
             false)
           else true)
       |> List.rev)

let unfinished t =
  List.sort_uniq compare
    (List.filter_map
       (fun e -> if e.finished then None else Some (e.repo, e.pipeline))
       t.entries)

let entries t = t.entries
