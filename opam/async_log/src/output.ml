open! Core
open! Async_kernel
open! Import

module Message = struct
  type t = Message.t

  let write_write_only_text t wr =
    Writer.write wr (Message.to_write_only_text t);
    Writer.newline wr
  ;;

  let write_sexp t ~hum wr =
    Writer.write_sexp ~hum wr (Message.Stable.V2.sexp_of_t t);
    Writer.newline wr
  ;;

  let write_bin_prot t wr = Writer.write_bin_prot wr Message.Stable.V2.bin_writer_t t
end

include Async_log_kernel.Output

let now ~time_source =
  match time_source with
  | Some time_source ->
    Synchronous_time_source.now time_source |> Time_ns.to_time_float_round_nearest
  | None -> Time_float.now ()
;;

let basic_write format w msg =
  match format with
  | `Sexp -> Message.write_sexp msg ~hum:false w
  | `Sexp_hum -> Message.write_sexp msg ~hum:true w
  | `Bin_prot -> Message.write_bin_prot msg w
  | `Text -> Message.write_write_only_text msg w
;;

let open_file ?perm filename =
  (* guard the open_file with a unit deferred to prevent any work from happening before
     async spins up. Without this no real work will be done, but async will be
     initialized, which will raise if we later call Scheduler.go_main. *)
  let%bind () = Deferred.unit in
  Writer.open_file ~append:true filename ?perm
;;

let open_writer ~filename ~perm =
  (* the lazy pushes evaluation to the first place we use it, which keeps writer creation
     errors within the error handlers for the log. *)
  lazy
    (let%map w = open_file filename ?perm in
     (* if we are writing to a slow device, or a temporarily disconnected device it's
        better to push back on memory in the hopes that the disconnection will resolve
        than to blow up after a timeout. If we had a better logging error reporting
        mechanism we could potentially deal with it that way, but we currently don't. *)
     Writer.set_buffer_age_limit w `Unlimited;
     w)
;;

let write_immediately w format msgs =
  Queue.iter msgs ~f:(fun msg -> basic_write format w msg);
  Writer.bytes_received w
;;

let write' w format msgs =
  let%map w in
  write_immediately w format msgs
;;

module File : sig
  val create : ?perm:Unix.file_perm -> Format.t -> filename:string -> t
end = struct
  let create ?perm format ~filename =
    let w = open_writer ~filename ~perm in
    create
      ~finalize:(fun () -> if Lazy.is_val w then force w >>= Writer.close else return ())
      ~flush:(fun () -> if Lazy.is_val w then force w >>= Writer.flushed else return ())
      (fun msgs ->
        let%map (_ : Int63.t) = write' (force w) format msgs in
        ())
  ;;
end

module Log_writer : sig
  val create : Format.t -> Writer.t -> t
end = struct
  let create format w =
    create
      ~flush:(fun () -> Writer.flushed w)
      (fun msgs ->
        Queue.iter msgs ~f:(fun msg -> basic_write format w msg);
        return ())
  ;;
end

module Rotating_file : sig
  val create
    :  ?perm:Unix.file_perm
    -> ?time_source:Synchronous_time_source.t
    -> Format.t
    -> basename:string
    -> ?suffix:string
    -> Rotation.t
    -> log_on_rotation:(unit -> Message.t list) option
    -> t * string Tail.t
end = struct
  module Make (Id : Rotation_id.S) = struct
    let make_filename ~dirname ~basename ~suffix id =
      match Id.to_string_opt id with
      | None -> dirname ^/ sprintf "%s%s" basename suffix
      | Some s -> dirname ^/ sprintf "%s.%s%s" basename s suffix
    ;;

    let parse_filename_id ~basename ~suffix filename =
      if String.equal (Filename.basename filename) (basename ^ suffix)
      then Id.of_string_opt None
      else
        let open Option.Let_syntax in
        let%bind id_dot_log =
          String.chop_prefix (Filename.basename filename) ~prefix:(basename ^ ".")
        in
        let%bind id = String.chop_suffix id_dot_log ~suffix in
        Id.of_string_opt (Some id)
    ;;

    let current_log_files ~dirname ~basename ~suffix =
      let%map files = Sys.readdir dirname in
      List.filter_map (Array.to_list files) ~f:(fun filename ->
        let filename = dirname ^/ filename in
        let%map.Option id = parse_filename_id ~basename ~suffix filename in
        id, filename)
    ;;

    (* errors from this function should be ignored. If this function fails to run, the
       disk may fill up with old logs, but external monitoring should catch that, and the
       core function of the Log module will be unaffected. *)
    let maybe_delete_old_logs ~dirname ~basename ~suffix keep =
      let%map (_ : unit Or_error.t list) =
        (match keep with
         | `All -> return []
         | `Newer_than span ->
           let%bind files = current_log_files ~dirname ~basename ~suffix in
           (* This will be compared to the mtime of the file, so we should always use now
              (wall-clock time) instead a different time source. *)
           let now = Time_float.now () in
           let cutoff = Time_float.sub now span in
           Deferred.List.filter ~how:`Sequential files ~f:(fun (_, filename) ->
             match%map
               Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
                 Unix.stat filename)
             with
             | Error _ -> false
             | Ok stats -> Time_float.( < ) stats.mtime cutoff)
         | `At_least i ->
           let%map files = current_log_files ~dirname ~basename ~suffix in
           let files =
             List.sort files ~compare:(fun (i1, _) (i2, _) -> Id.cmp_newest_first i1 i2)
           in
           List.drop files i)
        >>= Deferred.List.map ~how:`Sequential ~f:(fun (_i, filename) ->
          Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
            Unix.unlink filename))
      in
      ()
    ;;

    type t =
      { basename : string
      ; suffix : string
      ; dirname : string
      ; rotation : Rotation.t
      ; format : Format.t
      ; mutable writer : Writer.t Deferred.t Lazy.t
      ; mutable filename : string
      ; mutable last_messages : int
      ; mutable last_size : int
      ; mutable last_time : Time_float_unix.t
      ; log_files : string Tail.t
      ; log_on_rotation : unit -> Message.t list
      ; perm : int option
      }
    [@@deriving sexp_of]

    let we_have_written_to_the_current_writer t = Lazy.is_val t.writer

    let close_writer t =
      if we_have_written_to_the_current_writer t
      then (
        let%bind w = Lazy.force t.writer in
        Writer.close w)
      else return ()
    ;;

    let rotate t ~time_source =
      let { basename; dirname; suffix; _ } = t in
      let%bind () = close_writer t in
      let%bind files = current_log_files ~dirname ~basename ~suffix in
      let files =
        List.rev
          (List.sort files ~compare:(fun (i1, _) (i2, _) -> Id.cmp_newest_first i1 i2))
      in
      let%bind () =
        Deferred.List.iter ~how:`Sequential files ~f:(fun (id, src) ->
          let id' = Id.rotate_one id in
          let dst = make_filename ~dirname ~basename ~suffix id' in
          if String.equal src t.filename then Tail.extend t.log_files dst;
          if Id.cmp_newest_first id id' <> 0 then Unix.rename ~src ~dst else return ())
      in
      let%map () = maybe_delete_old_logs ~dirname ~basename ~suffix t.rotation.keep in
      let filename =
        make_filename
          ~dirname
          ~basename
          ~suffix
          (Id.create ?time_source (Rotation.zone t.rotation))
      in
      t.last_size <- 0;
      t.last_messages <- 0;
      t.last_time <- now ~time_source;
      t.filename <- filename;
      t.writer <- open_writer ~filename ~perm:t.perm
    ;;

    let write t ~time_source msgs =
      let current_time = now ~time_source in
      let%bind rotation_msgs, on_rotation_log_size =
        if Rotation.should_rotate
             t.rotation
             ~last_messages:t.last_messages
             ~last_size:(Byte_units.of_bytes_int t.last_size)
             ~last_time:t.last_time
             ~current_time
        then (
          let%bind () = rotate ~time_source t in
          let msgs = t.log_on_rotation () |> Queue.of_list in
          let rotation_msgs = Queue.length msgs in
          let%map size = write' (Lazy.force t.writer) t.format msgs in
          rotation_msgs, size)
        else return (0, Int63.zero)
      in
      let%map size = write' (Lazy.force t.writer) t.format msgs in
      t.last_messages <- t.last_messages + rotation_msgs + Queue.length msgs;
      t.last_size <- Int63.to_int_exn size + Int63.to_int_exn on_rotation_log_size;
      t.last_time <- current_time
    ;;

    let create ?perm ?time_source ~log_on_rotation format ~basename ~suffix rotation =
      let log_on_rotation =
        match log_on_rotation with
        | None -> Fn.const []
        | Some f -> f
      in
      let absolute_basename =
        (* make dirname absolute, because cwd may change *)
        match Filename.is_absolute basename with
        | true ->
          (* don't call Sys.getcwd if we don't have to, because that errors out when cwd
             is a directory that's been removed *)
          return basename
        | false ->
          let%map cwd = Sys.getcwd () in
          Filename.to_absolute_exn basename ~relative_to:cwd
      in
      let log_files = Tail.create () in
      let t_deferred =
        let%map absolute_basename in
        let dirname = Filename.dirname absolute_basename in
        let basename = Filename.basename absolute_basename in
        let filename =
          make_filename
            ~dirname
            ~basename
            ~suffix
            (Id.create ?time_source (Rotation.zone rotation))
        in
        { basename
        ; suffix
        ; dirname
        ; rotation
        ; format
        ; writer = open_writer ~filename ~perm
        ; filename
        ; last_size = 0
        ; last_messages = 0
        ; last_time = now ~time_source
        ; log_files
        ; log_on_rotation
        ; perm
        }
      in
      let first_rotate_scheduled = ref false in
      let finalize () =
        let%bind t = t_deferred in
        close_writer t
      in
      let flush () =
        let%bind t = t_deferred in
        if Lazy.is_val t.writer then force t.writer >>= Writer.flushed else return ()
      in
      ( create
          ~finalize
          ~flush
          ~rotate:(fun () -> t_deferred >>= rotate ~time_source)
          (fun msgs ->
            let%bind t = t_deferred in
            if not !first_rotate_scheduled
            then (
              first_rotate_scheduled := true;
              let%bind () = rotate t ~time_source in
              write t ~time_source msgs)
            else write t ~time_source msgs)
      , log_files )
    ;;
  end

  module Numbered = Make (struct
      type t = int

      let create ?time_source:_ _ = 0
      let rotate_one = ( + ) 1

      let to_string_opt = function
        | 0 -> None
        | x -> Some (Int.to_string x)
      ;;

      let cmp_newest_first = Int.ascending

      let of_string_opt = function
        | None -> Some 0
        | Some s ->
          (try Some (Int.of_string s) with
           | _ -> None)
      ;;
    end)

  module Timestamped = Make (struct
      type t = Time_float.t

      let create ?time_source _zone = now ~time_source
      let rotate_one = Fn.id

      let to_string_opt ts =
        Some (Time_float.to_filename_string ~zone:(force Time_float_unix.Zone.local) ts)
      ;;

      let cmp_newest_first = Time_float.descending

      let of_string_opt = function
        | None -> None
        | Some s ->
          (try
             Some
               (Time_float.of_filename_string ~zone:(force Time_float_unix.Zone.local) s)
           with
           | _ -> None)
      ;;
    end)

  module Dated = Make (struct
      type t = Date.t

      let create ?time_source zone = Date.of_time (now ~time_source) ~zone
      let rotate_one = Fn.id
      let to_string_opt date = Some (Date.to_string date)
      let cmp_newest_first = Date.descending

      let of_string_opt = function
        | None -> None
        | Some str -> Option.try_with (fun () -> Date.of_string str)
      ;;
    end)

  let create
    ?perm
    ?time_source
    format
    ~basename
    ?(suffix = ".log")
    (rotation : Rotation.t)
    =
    match rotation.naming_scheme with
    | `Numbered -> Numbered.create format ~basename ~suffix rotation ?perm ?time_source
    | `Timestamped ->
      Timestamped.create format ~basename ~suffix rotation ?perm ?time_source
    | `Dated -> Dated.create format ~basename ~suffix rotation ?perm ?time_source
    | `User_defined id ->
      let module Id = (val id : Rotation_id.S) in
      let module User_defined = Make (Id) in
      User_defined.create format ~basename ~suffix rotation ?perm ?time_source
  ;;
end

let rotating_file ?perm ?time_source ?log_on_rotation format ~basename ?suffix rotation =
  fst
    (Rotating_file.create
       format
       ~basename
       ?suffix
       ~log_on_rotation
       rotation
       ?perm
       ?time_source)
;;

let rotating_file_with_tail
  ?perm
  ?time_source
  ?log_on_rotation
  format
  ~basename
  ?suffix
  rotation
  =
  Rotating_file.create
    format
    ~basename
    ?suffix
    ~log_on_rotation
    rotation
    ?perm
    ?time_source
;;

let file = File.create
let writer = Log_writer.create

let stdout =
  let make =
    Memo.general (fun format -> Log_writer.create format (Lazy.force Writer.stdout))
  in
  fun ?(format = `Text) () -> make format
;;

let stderr =
  let make =
    Memo.general (fun format -> Log_writer.create format (Lazy.force Writer.stderr))
  in
  fun ?(format = `Text) () -> make format
;;
