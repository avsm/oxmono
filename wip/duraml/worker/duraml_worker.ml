module D = Duraml
exception Lost_lease

let transient = function
  | Eio.Io (D.E (D.Storage _), _) -> true
  | _ -> false

let run ~mono_clock ~queue ?(concurrency=4) ?(lease=Ptime.Span.of_int_s 60)
    ?(max_attempts=5) ?stop ?(drain=Ptime.Span.of_int_s 30)
    ?(on_settled=fun _ _ -> ()) ?on_exn ?(check=ignore) t handler =
  if concurrency<1 || concurrency>1024 then
    invalid_arg "Concurrency must be 1..1024";
  if max_attempts<1 then invalid_arg "Attempt budget must be positive";
  if Ptime.Span.compare lease Ptime.Span.zero<=0 then
    invalid_arg "Lease must be positive";
  if Ptime.Span.compare drain Ptime.Span.zero<0 then
    invalid_arg "Drain duration must be nonnegative";
  let recovering = Option.is_some on_exn in
  let lease_s = Ptime.Span.to_float_s lease in
  let sleep s = Eio.Time.Mono.sleep mono_clock s in
  let seconds () =
    Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now mono_clock)) /. 1e9 in
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
  (* The store set the claim's deadline before [take] returned, so the
     local deadline [expires] errs late by one request at first. Later
     deadlines count from the start of each renewal, which errs early. A
     renewal still pending at the local deadline counts as lost, so a slow
     store cannot leave the handler running beside a later claimant. *)
  let process claim =
    let rec heartbeat ~began expires =
      sleep (Float.max 0. (began +. lease_s /. 3. -. seconds ()));
      let began = seconds () in
      let remaining = expires -. began in
      if remaining <= 0. then raise Lost_lease;
      match Eio.Fiber.first
              (fun () -> Some (D.renew t claim ~lease))
              (fun () -> sleep remaining; None) with
      | Some true -> heartbeat ~began (began +. lease_s)
      | Some false | None -> raise Lost_lease
      | exception e when recovering && transient e -> raise Lost_lease in
    let began = seconds () in
    check claim;
    let settled =
      try
        let result=if D.Claim.attempt claim>max_attempts then exhausted ()
          else Eio.Fiber.first
            (fun () -> Eio.Switch.run (fun sw -> handle ~sw claim))
            (fun () -> heartbeat ~began (began +. lease_s)) in
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
          raise (D.err (D.Conflict
            "Worker completion conflicts with stored work"))
      | D.Applied | D.Already_applied | D.Lost_claim -> ()) settled in
  (* A claim taken as the pool stops goes back at once, eligible from its
     claim time, instead of waiting out its lease. *)
  let release claim =
    let at = Option.value ~default:Ptime.epoch
      (Ptime.sub_span (D.Claim.deadline claim) lease) in
    try ignore (D.settle t claim
      (D.complete (D.Retry {at;reason="worker stopped"})))
    with e when transient e -> () in
  let stopped () = Option.fold ~none:false ~some:Eio.Promise.is_resolved stop in
  let take () = storage (fun () -> D.take t ~queue ~lease) in
  let next () = if stopped () then None else match stop with
    | None -> Some (take ())
    | Some stop -> Eio.Fiber.first
        (fun () -> Some (take ()))
        (fun () -> Eio.Promise.await stop; None) in
  let rec worker () = match next () with
    | None -> ()
    | Some claim when stopped () -> release claim
    | Some claim ->
        process claim;
        Eio.Fiber.yield ();
        worker () in
  let pool () = Eio.Fiber.all (List.init concurrency (fun _ -> worker)) in
  match stop with
  | None -> pool ()
  | Some stop -> Eio.Fiber.first pool (fun () ->
      Eio.Promise.await stop;
      sleep (Ptime.Span.to_float_s drain))
