module D = Duraml
exception Lost_lease
let run ~mono_clock ~queue ?(concurrency=4) ?(lease=Ptime.Span.of_int_s 60)
    ?(max_attempts=5) ?stop ?(drain=Ptime.Span.of_int_s 30)
    ?(on_settled=fun _ _ -> ()) t handler =
  if concurrency<1 || concurrency>1024 then
    invalid_arg "Concurrency must be 1..1024";
  if max_attempts<1 then invalid_arg "Attempt budget must be positive";
  if Ptime.Span.compare lease Ptime.Span.zero<=0 then
    invalid_arg "Lease must be positive and finite";
  if Ptime.Span.compare drain Ptime.Span.zero<0 then
    invalid_arg "Drain duration must be nonnegative and finite";
  let exhausted ()=D.complete (D.Failed {code="attempt_limit";
    detail="Worker attempt budget exhausted"}) in
  let process claim =
    let rec heartbeat () =
      Eio.Time.Mono.sleep mono_clock (Ptime.Span.to_float_s lease/.3.);
      if not (D.renew t claim ~lease) then raise Lost_lease;
      heartbeat () in
    try
      let result=if D.Claim.attempt claim>max_attempts then exhausted ()
        else Eio.Fiber.first
          (fun () -> Eio.Switch.run (fun sw -> handler ~sw claim)) heartbeat in
      let result=match D.Completion.outcome result with
        | D.Retry _ when D.Claim.attempt claim>=max_attempts -> exhausted ()
        | _ -> result in
      let outcome=D.settle t claim result in
      on_settled claim outcome;
      (match outcome with
       | D.Settlement_conflict | D.Followup_conflict _ ->
           raise (D.err (D.Storage
             "Worker completion conflicts with stored work"))
       | D.Applied | D.Already_applied | D.Lost_claim -> ())
    with Lost_lease -> () in
  let stopped () = Option.fold ~none:false ~some:Eio.Promise.is_resolved stop in
  let next () = if stopped () then None else match stop with
    | None -> Some (D.take t ~queue ~lease)
    | Some stop -> Eio.Fiber.first
        (fun () -> Some (D.take t ~queue ~lease))
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
      Eio.Time.Mono.sleep mono_clock (Ptime.Span.to_float_s drain))
