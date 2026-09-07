(* Own responder lifetime and exception containment. Protocol work uses private
   modules with the same local values and zero-allocation contracts. *)
module F64 = Stdlib_upstream_compatible.Float_u

type body = Response.body =
  | Empty
  | String of string @@ global
  | Stream of {
      length : int64 option;
      write : (Body.Sink.t -> unit) @@ global;
      trailers : Headers.t;
    }
  | Handoff of {
      kind : Body.handoff_kind;
      run : (Body.Socket.t -> unit) @@ global;
    }

type outcome = Response.outcome = {
  status : Status.t;
  headers : Headers.t;
  last_modified : float option;
  body : body;
  content_length : int64 option;
}

type writer = outcome @ local -> unit

let sink ?emit_sub emit = Body.Sink.v ?emit_sub emit
let socket ~read ~write ~shutdown = Body.Socket.v ~read ~write ~shutdown

let[@zero_alloc] request ~meth ~version ~connection_upgrade
    ~(target : string @ local)
    ~(path : string @ local) ~(query : string @ local)
    (headers : Headers.t @ local) ~(body : string @ local) =
  exclave_
  Req.backend ~meth ~version ~connection_upgrade ~target ~path ~query headers
    ~body
;;

let[@inline never][@zero_alloc assume] call_error (on_error : exn -> unit) exn
    =
  try on_error exn with _ -> ()

let[@inline never][@zero_alloc assume strict] call_describe
    (describe : (Resp.respond @ local -> unit) @ local)
    (respond : Resp.respond @ local) =
  describe respond

(* Built once, since they are reported rather than raised. *)
let responded_twice =
  Invalid_argument "Proffer.Backend: the handler responded more than once"

let never_responded =
  Invalid_argument "Proffer.Backend: the handler returned without responding"

(* [handle] is [run] plus dispatch, and a test reaches [run] through
   [proffer.mock] to exercise one response without a site. *)
let[@zero_alloc] run_core ~on_error ~has_now (now : float#)
    (req : Req.t @ local)
    (describe : (Resp.respond @ local -> unit) @ local)
    (write : writer @ local) =
  (* [responded] detects missing or duplicate responder calls. [sent] is
     separate because a delayed body can fail after the response is described
     but before anything reaches the writer. *)
  let local_ responded = ref false in
  let local_ sent = ref false in
  let local_ w : writer =
   fun o ->
    sent := true;
    Response.call_writer write o
  in
  let local_ respond : Resp.respond =
   fun d ->
    if !responded then call_error on_error responded_twice
    else begin
      responded := true;
      let () = Response.decide ~has_now now req d w in
      ()
    end
  in
  (match
     if Conditional.reject_conditional_write ~has_now now req then begin
       responded := true;
       Response.write_precondition_failed Headers.empty req w
     end
     else call_describe describe respond
   with
  | () -> if not !responded then call_error on_error never_responded
  | exception exn -> call_error on_error exn);
  if not !sent then begin
    (match Response.write_internal_error req w
     with
    | () -> ()
    | exception exn -> call_error on_error exn)
  end

let ignore_error _ = ()

let run ?on_error ?now (req : Req.t @ local)
    (describe : (Resp.respond @ local -> unit) @ local)
    (write : writer @ local) =
  let on_error = Option.value on_error ~default:ignore_error in
  let #(has_now, now) =
    match now with
    | None -> #(false, #0.)
    | Some now -> #(true, F64.of_float now)
  in
  let () = run_core ~on_error ~has_now now req describe write in
  ()

let[@zero_alloc] handle_core ~on_error ~has_now (now : float#) site env
    (req : Req.t @ local) (write : writer @ local) =
  let local_ describe (respond : Resp.respond @ local) =
    let () = Dispatch.run site env req respond in
    ()
  in
  let () = run_core ~on_error ~has_now now req describe write in
  ()

let[@zero_alloc] handle_unboxed ~on_error ~(now : float#) site env
    (req : Req.t @ local) (write : writer @ local) =
  handle_core ~on_error ~has_now:true now site env req write

let handle ?on_error ?now site env (req : Req.t @ local)
    (write : writer @ local) =
  let on_error = Option.value on_error ~default:ignore_error in
  let #(has_now, now) =
    match now with
    | None -> #(false, #0.)
    | Some now -> #(true, F64.of_float now)
  in
  let () = handle_core ~on_error ~has_now now site env req write in
  ()
