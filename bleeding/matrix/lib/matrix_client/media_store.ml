type format =
  | File
  | Thumbnail of {
      width : int;
      height : int;
      resize : [ `Crop | `Scale ] option;
    }

type key = { uri : Media.Mxc.t; format : format }

type retention_policy = {
  max_file_size : int option;
  max_total_size : int option;
  expiry : Ptime.Span.t option;
  cleanup_frequency : Ptime.Span.t option;
}

let src = Logs.Src.create "matrix.media_store" ~doc:"Matrix media cache"

module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig
  type t

  val retention : t -> retention_policy
  val set_retention : t -> retention_policy -> (unit, Error.t) result

  val add :
    ?ignore_retention:bool ->
    ?protected:bool ->
    ?owner:string ->
    ?now:Ptime.t ->
    t ->
    key ->
    data:string ->
    (unit, Error.t) result

  val get : now:Ptime.t -> t -> key -> (string option, Error.t) result
  val protect : t -> key -> (unit, Error.t) result
  val unprotect : t -> key -> (unit, Error.t) result
  val is_protected : t -> key -> (bool, Error.t) result
  val set_ignore_retention : t -> key -> bool -> (unit, Error.t) result
  val replace_key : t -> from_:key -> to_:key -> (unit, Error.t) result
  val remove : t -> key -> (unit, Error.t) result
  val remove_uri : t -> Media.Mxc.t -> (unit, Error.t) result

  val prune_local :
    owner:string ->
    keep:key list ->
    older_than:Ptime.t ->
    t ->
    (unit, Error.t) result

  val clean : now:Ptime.t -> t -> (unit, Error.t) result
  val last_cleanup : t -> Ptime.t option
  val set_last_cleanup : t -> Ptime.t option -> (unit, Error.t) result
  val close : t -> unit
end

type entry = {
  key : key;
  data : string;
  mutable ignore_retention : bool;
  mutable protected_ : bool;
  owner : string option;
  mutable last_access : Ptime.t;
  sequence : int;
}

type memory = {
  mutable policy : retention_policy;
  entries : (string, entry) Hashtbl.t;
  mutable next_sequence : int;
  mutable last_cleanup : Ptime.t option;
}

let local_server = "send-queue.localhost"

let default_policy =
  {
    max_file_size = Some (20 * 1024 * 1024);
    max_total_size = Some (400 * 1024 * 1024);
    expiry = Some (Ptime.Span.of_int_s (60 * 24 * 60 * 60));
    cleanup_frequency = Some (Ptime.Span.of_int_s (24 * 60 * 60));
  }

let validate_policy policy =
  let non_negative name = function
    | Some value when value < 0 -> invalid_arg ("Media_store: negative " ^ name)
    | _ -> ()
  in
  non_negative "max_file_size" policy.max_file_size;
  non_negative "max_total_size" policy.max_total_size;
  match policy.expiry with
  | Some expiry when Ptime.Span.compare expiry Ptime.Span.zero < 0 ->
      invalid_arg "Media_store: negative expiry"
  | _ -> (
      ();
      match policy.cleanup_frequency with
      | Some frequency when Ptime.Span.compare frequency Ptime.Span.zero < 0 ->
          invalid_arg "Media_store: negative cleanup frequency"
      | _ -> ())

let memory_create ?(retention = default_policy) () =
  validate_policy retention;
  {
    policy = retention;
    entries = Hashtbl.create 17;
    next_sequence = 0;
    last_cleanup = None;
  }

let memory_retention t = t.policy

let memory_set_retention t policy =
  try
    validate_policy policy;
    t.policy <- policy;
    Ok ()
  with Invalid_argument message -> Error (Error.Policy_denied message)

let local_uri ~txn_id =
  (* A Matrix transaction id is only required to be non-empty; it need not be
     a legal MXC media id.  Hash its exact bytes into a fixed, versioned local
     identity instead of accidentally imposing the media-id grammar on queue
     persistence.  Existing pre-v2 URIs remain recognizable through
     [is_local_uri] and are restored from their persisted [upload_cache_uri]. *)
  let media_id =
    "v2_"
    ^ Digestif.SHA256.(
        digest_string ("matrix-send-queue-v2\000" ^ txn_id) |> to_hex)
  in
  match Media.Mxc.of_string ("mxc://" ^ local_server ^ "/" ^ media_id) with
  | Ok uri -> uri
  | Error (`Msg message) ->
      invalid_arg ("Media_store.local_uri: internal identity: " ^ message)

let is_local_uri uri =
  String.equal
    (Matrix_proto.Id.Server_name.to_string (Media.Mxc.server_name uri))
    local_server

let derived_key ~namespace ~identity format =
  let digest =
    Digestif.SHA256.(digest_string (namespace ^ "\000" ^ identity) |> to_hex)
  in
  let uri =
    match Media.Mxc.of_string ("mxc://media-cache.localhost/" ^ digest) with
    | Ok uri -> uri
    | Error (`Msg message) -> invalid_arg ("Media_store.derived_key: " ^ message)
  in
  { uri; format }

let format_key = function
  | File -> "f"
  | Thumbnail { width; height; resize } ->
      let resize =
        match resize with None -> "-" | Some `Crop -> "c" | Some `Scale -> "s"
      in
      Printf.sprintf "t:%d:%d:%s" width height resize

let key_id key = Media.Mxc.to_string key.uri ^ "\000" ^ format_key key.format
let now () = Ptime_clock.now ()

let next_sequence t =
  let sequence = t.next_sequence in
  t.next_sequence <- sequence + 1;
  sequence

let computed_max_file_size policy =
  match (policy.max_file_size, policy.max_total_size) with
  | None, None -> None
  | Some limit, None | None, Some limit -> Some limit
  | Some file_limit, Some total_limit -> Some (min file_limit total_limit)

let exceeds_max_file_size policy size =
  match computed_max_file_size policy with
  | None -> false
  | Some limit -> size > limit

let memory_add ?(ignore_retention = false) ?(protected = false) ?owner ?now:at t
    key ~data =
  let size = String.length data in
  (* As in matrix-rust-sdk, an ordinary over-sized file is simply not cached.
     Check before replacing so a rejected replacement retains its old value. *)
  if (not ignore_retention) && exceeds_max_file_size t.policy size then Ok ()
  else
    let id = key_id key in
    Hashtbl.remove t.entries id;
    let last_access = Option.value at ~default:(now ()) in
    let entry =
      {
        key;
        data;
        ignore_retention;
        protected_ = protected;
        owner;
        last_access;
        sequence = next_sequence t;
      }
    in
    Hashtbl.replace t.entries id entry;
    Ok ()

let memory_get ~now t key =
  match Hashtbl.find_opt t.entries (key_id key) with
  | None -> Ok None
  | Some entry ->
      entry.last_access <- now;
      Ok (Some entry.data)

let memory_protect t key =
  match Hashtbl.find_opt t.entries (key_id key) with
  | None -> ()
  | Some entry -> entry.protected_ <- true

let memory_unprotect t key =
  match Hashtbl.find_opt t.entries (key_id key) with
  | None -> ()
  | Some entry -> entry.protected_ <- false

let memory_is_protected t key =
  match Hashtbl.find_opt t.entries (key_id key) with
  | None -> false
  | Some entry -> entry.protected_

let memory_set_ignore_retention t key value =
  match Hashtbl.find_opt t.entries (key_id key) with
  | None -> ()
  | Some entry -> entry.ignore_retention <- value

let memory_remove t key =
  Hashtbl.remove t.entries (key_id key);
  Ok ()

let memory_remove_uri t uri =
  let ids =
    Hashtbl.fold
      (fun id entry ids ->
        if Media.Mxc.equal entry.key.uri uri then id :: ids else ids)
      t.entries []
  in
  List.iter (Hashtbl.remove t.entries) ids;
  Ok ()

let memory_prune_local ~owner ~keep ~older_than t =
  let keep = List.map key_id keep in
  let stale entry =
    is_local_uri entry.key.uri && entry.owner = Some owner
    && Ptime.compare entry.last_access older_than < 0
    && not (List.mem (key_id entry.key) keep)
  in
  let ids =
    Hashtbl.fold
      (fun id entry ids -> if stale entry then id :: ids else ids)
      t.entries []
  in
  List.iter (Hashtbl.remove t.entries) ids;
  Ok ()

let memory_replace_key t ~from_ ~to_ =
  let from_id = key_id from_ and to_id = key_id to_ in
  if String.equal from_id to_id then Ok ()
  else
    match Hashtbl.find_opt t.entries from_id with
    | None -> Ok ()
    | Some entry ->
        Hashtbl.remove t.entries to_id;
        Hashtbl.remove t.entries from_id;
        Hashtbl.replace t.entries to_id { entry with key = to_ };
        Ok ()

let memory_clean ~now t =
  let oversized entry =
    (not entry.ignore_retention)
    && (not entry.protected_)
    && exceeds_max_file_size t.policy (String.length entry.data)
  in
  let expired entry =
    if entry.ignore_retention || entry.protected_ then false
    else
      match t.policy.expiry with
      | None -> false
      | Some expiry ->
          Ptime.compare now entry.last_access >= 0
          && Ptime.Span.compare (Ptime.diff now entry.last_access) expiry >= 0
  in
  let to_remove =
    Hashtbl.fold
      (fun id entry ids ->
        if oversized entry || expired entry then id :: ids else ids)
      t.entries []
  in
  List.iter (Hashtbl.remove t.entries) to_remove;
  let total_size () =
    Hashtbl.fold
      (fun _ entry total ->
        if entry.ignore_retention then total
        else
          let size = String.length entry.data in
          if total > max_int - size then max_int else total + size)
      t.entries 0
  in
  let over_limit () =
    match t.policy.max_total_size with
    | Some limit -> total_size () > limit
    | None -> false
  in
  if over_limit () then begin
    let candidates =
      Hashtbl.fold
        (fun id entry candidates ->
          if entry.ignore_retention || entry.protected_ then candidates
          else entry :: candidates)
        t.entries []
    in
    let compare_oldest a b =
      let by_time = Ptime.compare a.last_access b.last_access in
      if by_time <> 0 then by_time else Int.compare a.sequence b.sequence
    in
    let candidates = List.sort compare_oldest candidates in
    let rec evict = function
      | _ when not (over_limit ()) -> ()
      | [] -> ()
      | entry :: rest ->
          Hashtbl.remove t.entries (key_id entry.key);
          evict rest
    in
    evict candidates
  end;
  Ok ()

let memory_last_cleanup t = t.last_cleanup

let memory_set_last_cleanup t value =
  t.last_cleanup <- value;
  Ok ()

module Memory : S with type t = memory = struct
  type t = memory

  let retention = memory_retention
  let set_retention = memory_set_retention
  let add = memory_add
  let get = memory_get

  let protect t key =
    memory_protect t key;
    Ok ()

  let unprotect t key =
    memory_unprotect t key;
    Ok ()

  let is_protected t key = Ok (memory_is_protected t key)

  let set_ignore_retention t key value =
    memory_set_ignore_retention t key value;
    Ok ()

  let replace_key = memory_replace_key
  let remove = memory_remove
  let remove_uri = memory_remove_uri
  let prune_local = memory_prune_local
  let clean = memory_clean
  let last_cleanup = memory_last_cleanup
  let set_last_cleanup = memory_set_last_cleanup
  let close _ = ()
end

type t = {
  mutex : Eio.Mutex.t;
  mutable retention : retention_policy;
  set_retention_ : retention_policy -> (unit, Error.t) result;
  add_ :
    bool ->
    bool ->
    string option ->
    Ptime.t option ->
    key ->
    data:string ->
    (unit, Error.t) result;
  get_ : Ptime.t -> key -> (string option, Error.t) result;
  protect_ : key -> (unit, Error.t) result;
  unprotect_ : key -> (unit, Error.t) result;
  is_protected_ : key -> (bool, Error.t) result;
  set_ignore_retention_ : key -> bool -> (unit, Error.t) result;
  replace_key_ : from_:key -> to_:key -> (unit, Error.t) result;
  remove_ : key -> (unit, Error.t) result;
  remove_uri_ : Media.Mxc.t -> (unit, Error.t) result;
  prune_local_ :
    owner:string ->
    keep:key list ->
    older_than:Ptime.t ->
    (unit, Error.t) result;
  clean_ : Ptime.t -> (unit, Error.t) result;
  last_cleanup_ : unit -> Ptime.t option;
  set_last_cleanup_ : Ptime.t option -> (unit, Error.t) result;
  mutable last_cleanup : Ptime.t option;
  close_ : unit -> unit;
  mutable closed : bool;
}

let closed_error = Error.Policy_denied "media store is closed"

let v (type backend) (module Backend : S with type t = backend) backend =
  {
    mutex = Eio.Mutex.create ();
    retention = Backend.retention backend;
    last_cleanup = Backend.last_cleanup backend;
    set_retention_ = (fun policy -> Backend.set_retention backend policy);
    add_ =
      (fun ignore_retention protected owner now key ~data ->
        Backend.add ~ignore_retention ~protected ?owner ?now backend key ~data);
    get_ = (fun now key -> Backend.get ~now backend key);
    protect_ = (fun key -> Backend.protect backend key);
    unprotect_ = (fun key -> Backend.unprotect backend key);
    is_protected_ = (fun key -> Backend.is_protected backend key);
    set_ignore_retention_ =
      (fun key value -> Backend.set_ignore_retention backend key value);
    replace_key_ = (fun ~from_ ~to_ -> Backend.replace_key backend ~from_ ~to_);
    remove_ = (fun key -> Backend.remove backend key);
    remove_uri_ = (fun uri -> Backend.remove_uri backend uri);
    prune_local_ =
      (fun ~owner ~keep ~older_than ->
        Backend.prune_local ~owner ~keep ~older_than backend);
    clean_ = (fun now -> Backend.clean ~now backend);
    last_cleanup_ = (fun () -> Backend.last_cleanup backend);
    set_last_cleanup_ = (fun value -> Backend.set_last_cleanup backend value);
    close_ = (fun () -> Backend.close backend);
    closed = false;
  }

let with_store t f =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if t.closed then Error closed_error else f ())

let retention t = Eio.Mutex.use_ro t.mutex (fun () -> t.retention)
let current_time () = Ptime_clock.now ()

let should_auto_clean t at =
  match (t.retention.cleanup_frequency, t.last_cleanup) with
  | Some _, None -> true
  | Some frequency, Some last ->
      Ptime.compare at last >= 0
      && Ptime.Span.compare (Ptime.diff at last) frequency >= 0
  | None, _ -> false

let has_limitations policy =
  Option.is_some policy.max_file_size
  || Option.is_some policy.max_total_size
  || Option.is_some policy.expiry

let maybe_clean_locked t at =
  if should_auto_clean t at && has_limitations t.retention then
    match t.clean_ at with
    | Error _ as error -> error
    | Ok () -> (
        match t.set_last_cleanup_ (Some at) with
        | Ok () ->
            t.last_cleanup <- Some at;
            Ok ()
        | Error _ as error -> error)
  else Ok ()

let best_effort_cleanup t at =
  match maybe_clean_locked t at with
  | Ok () -> ()
  | Error error ->
      Log.warn (fun m ->
          m "opportunistic media cleanup failed: %s" (Error.to_string error))

let last_cleanup t = Eio.Mutex.use_ro t.mutex (fun () -> t.last_cleanup)

let set_last_cleanup t value =
  with_store t (fun () ->
      match t.set_last_cleanup_ value with
      | Ok () ->
          t.last_cleanup <- value;
          Ok ()
      | Error _ as error -> error)

let set_retention t policy =
  with_store t (fun () ->
      match t.set_retention_ policy with
      | Error _ as error -> error
      | Ok () ->
          t.retention <- policy;
          best_effort_cleanup t (current_time ());
          Ok ())

let add ?(ignore_retention = false) ?(protected = false) ?owner ?now t key ~data
    =
  with_store t (fun () ->
      let at = Option.value now ~default:(current_time ()) in
      if
        (not ignore_retention)
        && exceeds_max_file_size t.retention (String.length data)
      then Ok ()
      else
        match t.add_ ignore_retention protected owner now key ~data with
        | Error _ as error -> error
        | Ok () ->
            best_effort_cleanup t at;
            Ok ())

let get ~now t key =
  with_store t (fun () ->
      match t.get_ now key with
      | Error _ as error -> error
      | Ok value ->
          best_effort_cleanup t now;
          Ok value)

let protect t key = with_store t (fun () -> t.protect_ key)
let unprotect t key = with_store t (fun () -> t.unprotect_ key)
let is_protected t key = with_store t (fun () -> t.is_protected_ key)

let set_ignore_retention t key value =
  with_store t (fun () -> t.set_ignore_retention_ key value)

let replace_key t ~from_ ~to_ =
  with_store t (fun () -> t.replace_key_ ~from_ ~to_)

let remove t key = with_store t (fun () -> t.remove_ key)
let remove_uri t uri = with_store t (fun () -> t.remove_uri_ uri)

let prune_local ~owner ~keep ~older_than t =
  with_store t (fun () -> t.prune_local_ ~owner ~keep ~older_than)

let clean ~now t =
  with_store t (fun () ->
      if not (has_limitations t.retention) then Ok ()
      else
        match t.clean_ now with
        | Error _ as error -> error
        | Ok () -> (
            match t.set_last_cleanup_ (Some now) with
            | Ok () ->
                t.last_cleanup <- Some now;
                Ok ()
            | Error _ as error -> error))

let close t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if not t.closed then begin
        t.closed <- true;
        t.close_ ()
      end)

let memory ?retention () = v (module Memory) (memory_create ?retention ())
let create ?retention () = memory ?retention ()
