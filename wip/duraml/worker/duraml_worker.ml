module D = Duraml
exception Lost_lease

let transient = function
  | Eio.Io (D.E (D.Storage _), _) -> true
  | _ -> false

let run ~mono_clock ~queue ?(concurrency=4) ?(lease=Ptime.Span.of_int_s 60)
    ?(max_attempts=5) ?stop ?(drain=Ptime.Span.of_int_s 30)
    ?(on_settled=fun _ _ -> ()) ?on_exn t handler =
  if concurrency<1 || concurrency>1024 then
    invalid_arg "Concurrency must be 1..1024";
  if max_attempts<1 then invalid_arg "Attempt budget must be positive";
  if Ptime.Span.compare lease Ptime.Span.zero<=0 then
    invalid_arg "Lease must be positive and finite";
  if Ptime.Span.compare drain Ptime.Span.zero<0 then
    invalid_arg "Drain duration must be nonnegative and finite";
  let recovering = Option.is_some on_exn in
  let sleep s = Eio.Time.Mono.sleep mono_clock s in
  (* [storage ~tries f] retries [f] after a transient storage failure, at
     most [tries] times in all when [tries] is given, doubling a pause of
     one second up to thirty. *)
  let storage ?tries f =
    let rec go n pause =
      match f () with
      | v -> v
      | exception e when recovering && transient e
                         && Option.fold ~none:true ~some:(( < ) n) tries ->
          sleep pause;
          go (n + 1) (Float.min 30. (pause *. 2.))
    in
    go 1 1. in
  let exhausted ()=D.complete (D.Failed {code="attempt_limit";
    detail="Worker attempt budget exhausted"}) in
  let handle ~sw claim =
    match handler ~sw claim with
    | result -> result
    | exception (Eio.Cancel.Cancelled _ as e) -> raise e
    | exception e ->
        match on_exn with
        | None -> raise e
        | Some f -> f claim e in
  let process claim =
    let rec heartbeat () =
      sleep (Ptime.Span.to_float_s lease/.3.);
      (* A renewal that cannot reach storage may have lost the claim, so
         the handler stops and the claim is left to expire. *)
      match D.renew t claim ~lease with
      | true -> heartbeat ()
      | false -> raise Lost_lease
      | exception e when recovering && transient e -> raise Lost_lease in
    let settled =
      try
        let result=if D.Claim.attempt claim>max_attempts then exhausted ()
          else Eio.Fiber.first
            (fun () -> Eio.Switch.run (fun sw -> handle ~sw claim))
            heartbeat in
        let result=match D.Completion.outcome result with
          | D.Retry _ when D.Claim.attempt claim>=max_attempts -> exhausted ()
          | _ -> result in
        Some (storage ~tries:3 (fun () -> D.settle t claim result))
      with
      | Lost_lease -> None
      | e when recovering && transient e -> None in
    Option.iter (fun outcome ->
      on_settled claim outcome;
      match outcome with
      | D.Settlement_conflict | D.Followup_conflict _ ->
          raise (D.err (D.Storage
            "Worker completion conflicts with stored work"))
      | D.Applied | D.Already_applied | D.Lost_claim -> ()) settled in
  let stopped () = Option.fold ~none:false ~some:Eio.Promise.is_resolved stop in
  let take () = storage (fun () -> D.take t ~queue ~lease) in
  let next () = if stopped () then None else match stop with
    | None -> Some (take ())
    | Some stop -> Eio.Fiber.first
        (fun () -> Some (take ()))
        (fun () -> Eio.Promise.await stop; None) in
  let rec worker () = match next () with
    | None -> ()
    | Some claim ->
        if not (stopped ()) then begin
          process claim;
          Eio.Fiber.yield ();
          worker ()
        end in
  let pool () = Eio.Fiber.List.iter ~max_fibers:concurrency
    (fun () -> worker ()) (List.init concurrency (fun _ -> ())) in
  match stop with
  | None -> pool ()
  | Some stop -> Eio.Fiber.first pool (fun () ->
      Eio.Promise.await stop;
      sleep (Ptime.Span.to_float_s drain))
