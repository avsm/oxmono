open! Core
open! Async_kernel
open! Import
include Raw_log

(* This implementation file mainly includes helper functions for specific message formats.
   The core logging functionality, independent of format, etc., is in [Raw_log]. *)

let sexp_of_t (_ : t) = Sexp.Atom "<opaque>"
let async_trace_hook = ref None

let message t msg =
  if would_log t (Message.level msg)
  then push_message_event t (Message_event.of_serialized_message msg)
;;

let message_event t msg =
  if would_log t (Message_event.level msg) then push_message_event t msg
;;

let push_message_event t data source ~level ~time ~legacy_tags =
  let time =
    Option.value_or_thunk time ~default:(fun () ->
      Synchronous_time_source.now (get_time_source t)
      |> Time_ns.to_time_float_round_nearest)
  in
  let legacy_tags = Option.value legacy_tags ~default:[] in
  let async_trace_span =
    match !async_trace_hook with
    | None -> None
    | Some hook -> hook ()
  in
  Message_event.Private.create
    data
    source
    ~level
    ~time
    ~legacy_tags
    ~user_scope:None
    ~function_name:None
    ~async_trace_span
  |> push_message_event t
;;

let push_message t msg ~level ~time ~tags =
  push_message_event
    t
    msg
    (Manually_constructed "from async log")
    ~level
    ~time
    ~legacy_tags:tags
;;

let sexp ?level ?time ?tags t sexp =
  if would_log t level then push_message t (`Sexp sexp) ~level ~time ~tags
;;

let string ?level ?time ?tags t s =
  if would_log t level then push_message t (`String s) ~level ~time ~tags
;;

let structured_message ?level ?time ?tags t data source =
  if would_log t level
  then push_message_event t data source ~level ~time ~legacy_tags:tags
;;

let printf ?level ?time ?tags t fmt =
  if would_log t level
  then ksprintf (fun msg -> push_message t (`String msg) ~level ~time ~tags) fmt
  else ifprintf () fmt
;;

let add_uuid_to_tags tags =
  let uuid =
    match Base.Exported_for_specific_uses.am_testing with
    | true -> Uuid.Stable.V1.for_testing
    | false -> Uuid.create_random Random.State.default
  in
  ("Log.surround_id", Uuid.to_string uuid) :: tags
;;

let surround_s_gen
  ?(tags = [])
  ~try_with
  ~map_return
  ~(log_sexp : ?tags:(string * string) list -> Sexp.t -> unit)
  ~f
  msg
  =
  let tags = add_uuid_to_tags tags in
  log_sexp ~tags [%message "Enter" ~_:(msg : Sexp.t)];
  map_return (try_with f) ~f:(function
    | Ok x ->
      log_sexp ~tags [%message "Exit" ~_:(msg : Sexp.t)];
      x
    | Error exn ->
      log_sexp ~tags [%message "Raised while " ~_:(msg : Sexp.t) (exn : exn)];
      Exn.reraise exn (sprintf !"%{sexp:Sexp.t}" msg))
;;

let surroundf_gen
  ?(tags = [])
  ~try_with
  ~map_return
  ~(log_string : ?tags:(string * string) list -> string -> unit)
  =
  ksprintf (fun msg f ->
    let tags = add_uuid_to_tags tags in
    log_string ~tags ("Enter " ^ msg);
    map_return (try_with f) ~f:(function
      | Ok x ->
        log_string ~tags ("Exit " ^ msg);
        x
      | Error exn ->
        log_string ~tags ("Raised while " ^ msg ^ ":" ^ Exn.to_string exn);
        Exn.reraise exn msg))
;;

let surround_s ~on_subsequent_errors ?level ?time ?tags t msg f =
  surround_s_gen
    ?tags
    ~try_with:(Monitor.try_with ~run:`Schedule ~rest:on_subsequent_errors)
    ~map_return:Deferred.map
    ~log_sexp:(fun ?tags s -> sexp ?tags ?level ?time t s)
    ~f
    msg
;;

let surroundf ~on_subsequent_errors ?level ?time ?tags t fmt =
  surroundf_gen
    ?tags
    ~try_with:(Monitor.try_with ~run:`Schedule ~rest:on_subsequent_errors)
    ~map_return:Deferred.map
    ~log_string:(fun ?tags -> string ?tags ?level ?time t)
    fmt
;;

let set_level_via_param_helper ~default ~f =
  let open Command.Param in
  map
    (flag "log-level" (optional Level.arg) ~doc:"LEVEL The log level")
    ~f:(fun level -> Option.first_some level default |> Option.iter ~f)
;;

let set_level_via_param ?default log =
  set_level_via_param_helper ~default ~f:(set_level log)
;;

let set_level_via_param_lazy log ~default =
  set_level_via_param_helper ~default ~f:(fun level -> set_level (Lazy.force log) level)
;;

let raw ?time ?tags t fmt = printf ?time ?tags t fmt
let debug ?time ?tags t fmt = printf ~level:`Debug ?time ?tags t fmt
let info ?time ?tags t fmt = printf ~level:`Info ?time ?tags t fmt
let error ?time ?tags t fmt = printf ~level:`Error ?time ?tags t fmt
let raw_s ?time ?tags t the_sexp = sexp ?time ?tags t the_sexp
let debug_s ?time ?tags t the_sexp = sexp ~level:`Debug ?time ?tags t the_sexp
let info_s ?time ?tags t the_sexp = sexp ~level:`Info ?time ?tags t the_sexp
let error_s ?time ?tags t the_sexp = sexp ~level:`Error ?time ?tags t the_sexp

module For_testing = struct
  let create_output
    ?(map_output = Fn.id)
    ?(time = `Omit)
    ?(tags = `Omit)
    ?(level = `Omit)
    ()
    =
    Output.For_testing.create ~map_output ~time ~tags ~level ()
  ;;

  let create
    ?(map_output = Fn.id)
    ?(time : [ `Keep | `Omit ] = `Omit)
    ?(tags : [ `Keep | `Omit ] = `Omit)
    ?(level : [ `Keep | `Omit ] = `Omit)
    (log_level : Level.t)
    =
    let default_outputs = [ create_output ~map_output ~time ~tags ~level () ] in
    let named_outputs = Output_name.Map.empty in
    create
      ~default_outputs
      ~named_outputs
      ~level:log_level
      ~on_error:`Raise
      ~time_source:None
      ~transforms:[]
  ;;

  let transform = For_testing.transform
end

module Private = struct
  include Private

  let push_message_event = Raw_log.push_message_event
  let set_async_trace_hook f = async_trace_hook := Some f
  let set_level_via_param_lazy = set_level_via_param_lazy
  let all_live_logs_flushed = Raw_log.all_live_logs_flushed
end

let create ~level ~output ~on_error ?time_source ?transform () =
  let named_outputs = Output_name.Map.empty in
  let transforms =
    match transform with
    | None -> []
    | Some transform -> [ (fun msg -> Some (transform msg)) ]
  in
  create ~level ~default_outputs:output ~named_outputs ~on_error ~time_source ~transforms
;;

module Transform = struct
  include Transform

  let append' t f = Transform.add t f `After
  let prepend' t f = Transform.add t f `Before
  let append t f = ignore (Transform.add t (fun msg -> Some (f msg)) `After : Transform.t)

  let prepend t f =
    ignore (Transform.add t (fun msg -> Some (f msg)) `Before : Transform.t)
  ;;
end

let add_tags t ~tags = Transform.prepend t (Message_event.add_tags ~tags)

let add_tags' t ~tags =
  Transform.prepend' t (fun message_event ->
    Some (Message_event.add_tags message_event ~tags))
;;

let copy ?level ?on_error ?output ?extra_tags t =
  let t = copy t in
  Option.iter level ~f:(set_level t);
  Option.iter on_error ~f:(set_on_error t);
  Option.iter output ~f:(set_output t);
  Option.iter extra_tags ~f:(fun tags -> add_tags t ~tags);
  t
;;

let create_null () =
  create ~level:`Error ~output:[] ~on_error:(`Call (fun (_ : Error.t) -> ())) ()
;;
