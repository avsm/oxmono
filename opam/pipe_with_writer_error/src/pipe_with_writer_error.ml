open Core
open Async_kernel

module T = struct
  type ('a, 'error) t =
    { reader : 'a Pipe.Reader.t
    ; writer_error : (unit, 'error) Result.t Deferred.t
    }
  [@@deriving sexp_of]
end

include T

let of_reader ~writer_error reader' =
  let reader, writer = Pipe.create () in
  let () =
    don't_wait_for (Pipe.transfer_id reader' writer >>| fun () -> Pipe.close writer);
    don't_wait_for
      (match%bind writer_error with
       | Ok () -> return ()
       | Error _ ->
         (* If we close the writer we created we might not have pulled down all the values
            upstream that were written prior to the error and are ready to be consumed. We
            need to flush these values before closing the writer. *)
         let%map (`Ok | `Reader_closed) = Pipe.upstream_flushed reader' in
         Pipe.close writer)
  in
  { reader; writer_error }
;;

module Expert = struct
  include T

  let get = Fn.id
  let lift_map t ~f = { t with reader = f t.reader }

  let lift_read t ~f =
    let open Deferred.Result.Let_syntax in
    let%bind result = Deferred.ok (f t.reader) in
    match result with
    | `Ok _ -> return result
    | `Eof ->
      let%map () = t.writer_error in
      result
  ;;

  let lift_consume t ~f =
    let%map result = f t.reader
    and writer_error = t.writer_error in
    Result.map writer_error ~f:(fun () -> result)
  ;;

  let lift_concat ts ~f ~combine_errors =
    let readers, writer_errors =
      ts
      |> List.map ~f:(fun { reader; writer_error } -> reader, writer_error)
      |> List.unzip
    in
    let reader = f readers in
    let writer_error =
      writer_errors
      |> Deferred.all
      >>| Result.combine_errors_unit
      >>| Result.map_error ~f:combine_errors
    in
    of_reader reader ~writer_error
  ;;

  (* Unlike [lift_map], this creates a well-behaved [t] even if [f] returns a pipe that
     never closes in case of error. *)
  let lift_map_with_deadlock_protection t ~f =
    let { writer_error; reader } = t in
    of_reader (f reader) ~writer_error
  ;;
end

let create_reader ?size_budget f =
  let writer_error = Ivar.create () in
  let reader, writer = Pipe.create ?size_budget () in
  let () =
    don't_wait_for
      (let%bind result = f writer in
       Ivar.fill_exn writer_error result;
       (* We don't need to call [upstream_flushed] here like we did in [of_reader] because
          the caller expects the writer to be closed when [f] returns and must ensure that
          all values of interest are in the writer before it does. Since [writer_error] is
          only determined after [f] returns, we don't have the case where it gets filled
          with an error before the writer has all the values of interest. *)
       Pipe.close writer;
       return ())
  in
  { reader; writer_error = Ivar.read writer_error }
;;

let of_reader_with_errors ~writer_error reader =
  create_reader (fun writer ->
    let iter_result =
      let error_element = ref None in
      let%map () =
        Pipe.transfer' reader writer ~f:(fun elems ->
          let unwrapped_elems = Queue.create () in
          Queue.iter elems ~f:(fun elem ->
            if Option.is_none !error_element
            then (
              match elem with
              | Ok elem -> Queue.enqueue unwrapped_elems elem
              | Error error ->
                error_element := Some error;
                Pipe.close_read reader));
          return unwrapped_elems)
      in
      match !error_element with
      | None -> Ok ()
      | Some error -> Error error
    in
    match%bind writer_error with
    | Ok () -> iter_result
    | Error _ as error ->
      (* Ensure that everything has made it out of [reader] (i.e. into [writer]) *)
      let%bind (`Ok | `Reader_closed) = Pipe.upstream_flushed reader in
      return error)
;;

let empty ~writer_error = of_reader ~writer_error (Pipe.empty ())
let of_list ~writer_error list = of_reader ~writer_error (Pipe.of_list list)
let singleton ~writer_error x = of_reader ~writer_error (Pipe.singleton x)
let close t = Pipe.close_read t.reader
let is_closed t = Pipe.is_closed t.reader
let closed t = Expert.lift_consume t ~f:Pipe.closed
let length t = Pipe.length t.reader
let is_empty t = Pipe.is_empty t.reader
let read t = Expert.lift_read t ~f:Pipe.read
let read' t = Expert.lift_read t ~f:Pipe.read'

let read_exactly t ~num_values =
  let%bind result = Pipe.read_exactly t.reader ~num_values in
  match result with
  | `Exactly _ -> return (Ok result)
  | (`Eof | `Fewer _) as result ->
    (match%map t.writer_error with
     | Ok () -> Ok result
     | Error error ->
       let queue =
         match result with
         | `Eof -> Queue.create ~capacity:0 ()
         | `Fewer queue -> queue
       in
       Error (error, queue))
;;

let peek t =
  match Deferred.peek t.writer_error with
  | Some (Error _ as error) -> error
  | None | Some (Ok ()) -> Ok (Pipe.peek t.reader)
;;

let read_now t =
  match Deferred.peek t.writer_error with
  | Some (Error _ as error) -> error
  | None | Some (Ok ()) -> Ok (Pipe.read_now t.reader)
;;

let read_all t = Expert.lift_consume t ~f:Pipe.read_all

let values_available t =
  let f pipe =
    match%map Pipe.values_available pipe with
    | `Eof -> `Eof
    | `Ok -> `Ok ()
  in
  match%bind Expert.lift_read t ~f with
  | (Error _ | Ok `Eof) as result -> return result
  | Ok (`Ok ()) -> return (Ok `Ok)
;;

let fold t ~init ~f = Expert.lift_consume t ~f:(Pipe.fold ~init ~f)
let fold' t ~init ~f = Expert.lift_consume t ~f:(Pipe.fold' ~init ~f)

let fold_without_pushback t ~init ~f =
  Expert.lift_consume t ~f:(Pipe.fold_without_pushback ~init ~f)
;;

let iter t ~f = Expert.lift_consume t ~f:(Pipe.iter ~f)
let iter' t ~f = Expert.lift_consume t ~f:(Pipe.iter' ~f)

let iter_without_pushback ?max_iterations_per_job t ~f =
  Expert.lift_consume t ~f:(Pipe.iter_without_pushback ?max_iterations_per_job ~f)
;;

let iter_parallel ?continue_on_error t ~max_concurrent_jobs ~f =
  Expert.lift_consume t ~f:(Pipe.iter_parallel ?continue_on_error ~max_concurrent_jobs ~f)
;;

let transfer t writer ~f = Expert.lift_consume t ~f:(Fn.flip Pipe.transfer writer ~f)
let transfer_id = transfer ~f:Fn.id
let map ?max_batch_size t ~f = Expert.lift_map t ~f:(Pipe.map ?max_batch_size ~f)
let map' ?max_queue_length t ~f = Expert.lift_map t ~f:(Pipe.map' ?max_queue_length ~f)

let map_error t ~f =
  { t with writer_error = Deferred.map t.writer_error ~f:(Result.map_error ~f) }
;;

let map_error' t ~f =
  { t with
    writer_error =
      Deferred.bind t.writer_error ~f:(function
        | Ok _ as x -> return x
        | Error e ->
          let%map e = f e in
          Error e)
  }
;;

let folding_map t ~init ~f = Expert.lift_map t ~f:(Pipe.folding_map ~init ~f)
let filter_map t ~f = Expert.lift_map t ~f:(Pipe.filter_map ~f)
let filter_map' t ~f = Expert.lift_map t ~f:(Pipe.filter_map' ~f)
let filter t ~f = Expert.lift_map t ~f:(Pipe.filter ~f)

let merge_custom ts ~compare ~combine_errors =
  Expert.lift_concat ts ~f:(Pipe.merge ~compare) ~combine_errors
;;

let merge ts ~compare = merge_custom ts ~compare ~combine_errors:Error.of_list

let concat_custom ts ~combine_errors =
  Expert.lift_concat ts ~f:Pipe.concat ~combine_errors
;;

let concat ts = concat_custom ts ~combine_errors:Error.of_list
let concat_plain_pipes t = Expert.lift_map_with_deadlock_protection t ~f:Pipe.concat_pipe

let interleave_custom ts ~combine_errors =
  Expert.lift_concat ts ~f:Pipe.interleave ~combine_errors
;;

let interleave ts = interleave_custom ts ~combine_errors:Error.of_list

let fork t ~pushback_uses =
  t.reader
  |> Pipe.fork ~pushback_uses
  |> Tuple2.map ~f:(of_reader ~writer_error:t.writer_error)
;;

let to_list t = Expert.lift_consume t ~f:Pipe.to_list
