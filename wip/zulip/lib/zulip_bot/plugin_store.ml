module Smap = Map.Make (String)

type error = Codec of string | Backend of string
type store = Jsont.json Smap.t Smap.t Smap.t
type backing = Memory | File of Eio.Fs.dir_ty Eio.Path.t
type t = { backing : backing; mutex : Eio.Mutex.t; mutable values : store }

let max_file_bytes = 1024 * 1024
let max_members = 4096

let pp_error ppf = function
  | Codec s -> Format.fprintf ppf "JSON codec error: %s" s
  | Backend s -> Format.fprintf ppf "plugin store error: %s" s

let error_to_string e = Format.asprintf "%a" pp_error e
let global = "*"
let scope = function None -> global | Some room -> "room:" ^ room

let memory () =
  { backing = Memory; mutex = Eio.Mutex.create (); values = Smap.empty }

let json_of_store store =
  let object_of_map map =
    Jsont.Json.object'
      (Smap.bindings map
      |> List.map (fun (name, value) ->
          Jsont.Json.mem (Jsont.Json.name name) value))
  in
  object_of_map
    (Smap.map (fun rooms -> object_of_map (Smap.map object_of_map rooms)) store)

let map_of_object json =
  match json with
  | Jsont.Object (members, _) ->
      if List.length members > max_members then
        Error "object has too many members"
      else
        Ok
          (List.fold_left
             (fun map ((name, _), value) -> Smap.add name value map)
             Smap.empty members)
  | _ -> Error "expected a JSON object"

let store_of_json json =
  Result.bind (map_of_object json) @@ fun plugins ->
  Smap.fold
    (fun plugin rooms result ->
      Result.bind result @@ fun store ->
      Result.bind (map_of_object rooms) @@ fun rooms ->
      let decoded =
        Smap.fold
          (fun room keys result ->
            Result.bind result @@ fun rooms ->
            Result.map
              (fun keys -> Smap.add room keys rooms)
              (map_of_object keys))
          rooms (Ok Smap.empty)
      in
      Result.map (fun rooms -> Smap.add plugin rooms store) decoded)
    plugins (Ok Smap.empty)

let load path =
  match
    Eio.Path.with_open_in path (fun flow ->
        let stat = Eio.File.stat flow in
        if stat.kind <> `Regular_file || stat.perm land 0o077 <> 0 then
          failwith "plugin store must be a regular private file (0600)";
        Eio.Buf_read.(take_all (of_flow ~max_size:(max_file_bytes + 1) flow)))
  with
  | text -> (
      if String.length text > max_file_bytes then
        Error (Backend "plugin store exceeds 1 MiB")
      else
        match Fetch.Json.decode_string' Jsont.json text with
        | Error error ->
            Error
              (Backend ("cannot decode store: " ^ Jsont.Error.to_string error))
        | Ok json -> Result.map_error (fun e -> Backend e) (store_of_json json))
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Ok Smap.empty
  | exception Eio.Buf_read.Buffer_limit_exceeded ->
      Error (Backend "plugin store exceeds 1 MiB")

let open_file path =
  try
    Result.map
      (fun values ->
        { backing = File path; mutex = Eio.Mutex.create (); values })
      (load path)
  with
  | Eio.Io _ as exn -> Error (Backend (Printexc.to_string exn))
  | Failure message -> Error (Backend message)

let counter = Atomic.make 0

let atomic_write path data =
  match Eio.Path.split path with
  | None -> Error (Backend "plugin store requires a file basename")
  | Some (dir, base) ->
      let rec write attempt =
        if attempt = 100 then
          Error (Backend "cannot reserve plugin-store temporary file")
        else
          let suffix =
            Printf.sprintf ".%d.%d.tmp" (Unix.getpid ())
              (Atomic.fetch_and_add counter 1)
          in
          let temp = Eio.Path.(dir / (base ^ suffix)) in
          try
            Eio.Path.with_open_out ~create:(`Exclusive 0o600) temp
            @@ fun flow ->
            Eio.Path.chmod ~follow:false ~perm:0o600 temp;
            Eio.Flow.copy_string data flow;
            Eio.File.sync flow;
            Eio.Path.rename temp path;
            Ok ()
          with
          | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> write (attempt + 1)
          | Eio.Cancel.Cancelled _ as exn ->
              (try
                 Eio.Cancel.protect (fun () ->
                     Eio.Path.unlink ~missing_ok:true temp)
               with _ -> ());
              raise exn
          | exn ->
              (try
                 Eio.Cancel.protect (fun () ->
                     Eio.Path.unlink ~missing_ok:true temp)
               with _ -> ());
              Error (Backend (Printexc.to_string exn))
      in
      write 0

let flush t values =
  match t.backing with
  | Memory -> Ok ()
  | File path -> (
      match
        Jsont_bytesrw.encode_string ~format:Jsont.Indent Jsont.json
          (json_of_store values)
      with
      | Ok text when String.length text > max_file_bytes ->
          Error (Backend "plugin store exceeds 1 MiB")
      | Ok text -> (
          (* Validate the same limits used on reopen before committing a file. *)
          match Fetch.Json.decode_string' Jsont.json text with
          | Error error -> Error (Codec (Jsont.Error.to_string error))
          | Ok json ->
              Result.bind
                (Result.map_error (fun e -> Backend e) (store_of_json json))
                (fun _ -> atomic_write path text))
      | Error message -> Error (Codec message))

let get t ~room ~plugin key =
  Option.bind (Smap.find_opt plugin t.values) @@ fun rooms ->
  Option.bind (Smap.find_opt (scope room) rooms) @@ fun keys ->
  Smap.find_opt key keys

let put t ~room ~plugin key value =
  let rooms =
    Option.value ~default:Smap.empty (Smap.find_opt plugin t.values)
  in
  let keys =
    Option.value ~default:Smap.empty (Smap.find_opt (scope room) rooms)
  in
  let keys =
    match value with
    | None -> Smap.remove key keys
    | Some v -> Smap.add key v keys
  in
  let rooms =
    if Smap.is_empty keys then Smap.remove (scope room) rooms
    else Smap.add (scope room) keys rooms
  in
  let values =
    if Smap.is_empty rooms then Smap.remove plugin t.values
    else Smap.add plugin rooms t.values
  in
  Result.map (fun () -> t.values <- values) (flush t values)

let decode codec json =
  Result.map_error (fun s -> Codec s) (Jsont.Json.decode codec json)

let encode codec value =
  Result.map_error (fun s -> Codec s) (Jsont.Json.encode codec value)

let find t ?room ~plugin ~key codec =
  Eio.Mutex.use_ro t.mutex @@ fun () ->
  match get t ~room ~plugin key with
  | None -> Ok None
  | Some json -> Result.map Option.some (decode codec json)

let set t ?room ~plugin ~key codec value =
  Eio.Mutex.use_rw ~protect:true t.mutex @@ fun () ->
  Result.bind (encode codec value) (fun json ->
      put t ~room ~plugin key (Some json))

let update t ?room ~plugin ~key codec f =
  Eio.Mutex.use_rw ~protect:true t.mutex @@ fun () ->
  let previous =
    match get t ~room ~plugin key with
    | None -> Ok None
    | Some json -> Result.map Option.some (decode codec json)
  in
  Result.bind previous @@ fun previous ->
  let value = f previous in
  Result.bind (encode codec value) @@ fun json ->
  Result.map (fun () -> value) (put t ~room ~plugin key (Some json))

let remove t ?room ~plugin ~key () =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      put t ~room ~plugin key None)

let keys t ?room ~plugin () =
  Eio.Mutex.use_ro t.mutex @@ fun () ->
  match
    Option.bind (Smap.find_opt plugin t.values) (fun r ->
        Smap.find_opt (scope room) r)
  with
  | None -> []
  | Some keys -> List.map fst (Smap.bindings keys)
