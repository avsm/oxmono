module Smap = Map.MakePortable (String)
module Log = (val Logs.src_log Logging.src : Logs.LOG)

type error = Codec of string | Backend of string

let pp_error ppf = function
  | Codec message ->
      Format.fprintf ppf "cannot encode or decode a value: %s" message
  | Backend message -> Format.fprintf ppf "cannot write the store: %s" message

let error_to_string e = Format.asprintf "%a" pp_error e

(* [{ "plugin": { "<room or *>": { "key": <value> } } }]. The room key is a
   literal ["*"] rather than an absent level, so that a plugin's global and
   per-room values are read back by the same two lookups. *)
type store = Jsont.json Smap.t Smap.t Smap.t

let store_jsont : store Jsont.t =
  Matrix_proto.Json.Codec.as_string_map
    (Matrix_proto.Json.Codec.as_string_map
       (Matrix_proto.Json.Codec.as_string_map Matrix_proto.Json.Codec.json))

type backing = Memory | File of Eio.Fs.dir_ty Eio.Path.t
type t = { backing : backing; mutex : Eio.Mutex.t; mutable store : store }

let global = "*"

let scope = function
  | None -> global
  | Some room -> Matrix_proto.Id.Room_id.to_string room

let memory () =
  { backing = Memory; mutex = Eio.Mutex.create (); store = Smap.empty }

let rename_aside path =
  match Eio.Path.split path with
  | None -> Eio.Path.unlink ~missing_ok:true path
  | Some (dir, base) ->
      let stamp = Printf.sprintf "%.0f" (Unix.gettimeofday ()) in
      Eio.Path.rename path Eio.Path.(dir / (base ^ ".broken-" ^ stamp))

let load path =
  match Eio.Path.load path with
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Smap.empty
  | exception (Eio.Io _ as exn) ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context exn bt "loading plugin store"
  | text -> (
      match Jsont_bytesrw.decode_string store_jsont text with
      | Ok store -> store
      | Error _message ->
          Log.warn (fun m ->
              m
                "plugin store does not parse; it is renamed aside and the \
                 store starts empty");
          (try rename_aside path
           with Eio.Io _ as exn ->
             let contextual =
               Eio.Exn.add_context exn "renaming corrupt plugin store aside"
             in
             Log.warn (fun m ->
                 m "Cannot rename plugin store aside: %a" Eio.Exn.pp contextual));
          Smap.empty)

let open_file path =
  { backing = File path; mutex = Eio.Mutex.create (); store = load path }

(* A crash between the two must leave the reader with one whole file. The
   shared profile writer uses a unique same-directory name, syncs the file,
   and moves it over the target. *)
let flush t =
  match t.backing with
  | Memory -> Ok ()
  | File path -> (
      match Jsont_bytesrw.encode_string store_jsont t.store with
      | Error message -> Error (Codec message)
      | Ok text -> (
          try
            match Matrix_client.Profile_store.atomic_write ~path ~data:text with
            | Ok () -> Ok ()
            | Error error ->
                Error (Backend (Matrix_client.Error.to_string error))
          with Eio.Io _ as exn ->
            let contextual = Eio.Exn.add_context exn "flushing plugin store" in
            Error (Backend (Format.asprintf "%a" Eio.Exn.pp contextual))))

let get t ~room ~plugin key =
  Option.bind (Smap.find_opt plugin t.store) @@ fun rooms ->
  Option.bind (Smap.find_opt (scope room) rooms) @@ fun keys ->
  Smap.find_opt key keys

let put t ~room ~plugin key value =
  let rooms = Option.value ~default:Smap.empty (Smap.find_opt plugin t.store) in
  let keys =
    Option.value ~default:Smap.empty (Smap.find_opt (scope room) rooms)
  in
  let keys =
    match value with
    | None -> Smap.remove key keys
    | Some json -> Smap.add key json keys
  in
  let rooms = Smap.add (scope room) keys rooms in
  t.store <- Smap.add plugin rooms t.store;
  flush t

let decode codec json =
  Result.map_error (fun message -> Codec message) (Jsont.Json.decode codec json)

let encode codec value =
  Result.map_error
    (fun message -> Codec message)
    (Jsont.Json.encode codec value)

let find t ?room ~plugin ~key codec =
  Eio.Mutex.use_ro t.mutex @@ fun () ->
  match get t ~room ~plugin key with
  | None -> Ok None
  | Some json -> Result.map Option.some (decode codec json)

let set t ?room ~plugin ~key codec value =
  Eio.Mutex.use_rw ~protect:true t.mutex @@ fun () ->
  Result.bind (encode codec value) @@ fun json ->
  put t ~room ~plugin key (Some json)

let update t ?room ~plugin ~key codec f =
  Eio.Mutex.use_rw ~protect:true t.mutex @@ fun () ->
  let current =
    Option.bind (get t ~room ~plugin key) (fun json ->
        Result.to_option (decode codec json))
  in
  let value = f current in
  Result.bind (encode codec value) @@ fun json ->
  Result.map (fun () -> value) (put t ~room ~plugin key (Some json))

let remove t ?room ~plugin ~key () =
  Eio.Mutex.use_rw ~protect:true t.mutex @@ fun () ->
  put t ~room ~plugin key None

let keys t ?room ~plugin () =
  Eio.Mutex.use_ro t.mutex @@ fun () ->
  match Smap.find_opt plugin t.store with
  | None -> []
  | Some rooms -> (
      match Smap.find_opt (scope room) rooms with
      | None -> []
      | Some keys -> List.map fst (Smap.bindings keys))
