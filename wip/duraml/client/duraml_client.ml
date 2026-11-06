module D = Duraml

module Codec = struct
  type 'a t = {
    encode : 'a -> string;
    decode : string -> ('a, string) result;
  }
  let v ~encode ~decode = {encode;decode}
  let utf8 = v ~encode:Fun.id ~decode:(fun s ->
    if String.is_valid_utf_8 s then Ok s else Error "Invalid UTF-8")
end

module Submission = struct
  type t = {identity : string; spec : D.Spec.t}
end

module Completion = struct
  type 'a outcome = Success of 'a | Retry of Ptime.t * string
    | Failure of string * string
  type 'a t = {outcome : 'a outcome; followups : Submission.t list}
  let validate followups outcome =
    ignore (D.complete ~followups:(List.map (fun s -> s.Submission.spec)
      followups) outcome)
  let succeed ?(followups=[]) value =
    validate followups (D.Succeeded "");
    {outcome=Success value;followups}
  let retry ~at ~reason =
    validate [] (D.Retry {at;reason});
    {outcome=Retry (at,reason);followups=[]}
  let fail ?(followups=[]) ~code ~detail () =
    validate followups (D.Failed {code;detail});
    {outcome=Failure (code,detail);followups}
end

module Job = struct
  type 'a t = {store : D.plain; id : D.Id.t; result : 'a Codec.t}
  type 'a status = Queued | Running | Done of 'a
    | Failed of {code : string; detail : string}
    | Cancelled | Invalid_result of string | Missing
  let id t = t.id
  let snapshot t = D.find t.store t.id
  let inspect t = match snapshot t with
    | None -> Missing
    | Some j -> match D.Job.state j with
      | D.Queued -> Queued | D.Running -> Running | D.Cancelled -> Cancelled
      | D.Done -> (match D.Job.outcome j with
          | Some (D.Succeeded data) -> (match t.result.decode data with
              | Ok value -> Done value | Error error -> Invalid_result error)
          | _ -> Invalid_result "Missing successful result")
      | D.Failed_job -> (match D.Job.outcome j with
          | Some (D.Failed {code;detail}) -> Failed {code;detail}
          | _ -> Invalid_result "Missing failure result")
  let cancel t = D.cancel t.store t.id
end

let context : (D.Id.t * int) Eio.Fiber.key = Eio.Fiber.create_key ()
let attempt () = Eio.Fiber.get context

module Queue = struct
  type ('request, 'result) t = {
    store : D.plain; identity : string; name : string; kind : string;
    version : int; request : 'request Codec.t; result : 'result Codec.t;
  }
  type 'a submission = Added of 'a Job.t | Existing of 'a Job.t
    | Conflict of D.Id.t
  exception Schema_mismatch of D.Id.t
  let v store ~name ~kind ~version ~request ~result =
    ignore (D.Spec.v ~queue:name ~kind ~version ~payload:"" ());
    {store=(store :> D.plain);identity=D.identity store;
      name;kind;version;request;result}
  let name t = t.name
  let handle t id = {Job.store=t.store;id;result=t.result}
  let matches t spec = D.Spec.queue spec=t.name && D.Spec.kind spec=t.kind &&
    D.Spec.version spec=t.version
  let stage ?key ?not_before ?lane t value =
    let spec=D.Spec.v ~queue:t.name ~kind:t.kind ~version:t.version
      ~payload:(t.request.encode value) ?key ?not_before ?lane () in
    {Submission.identity=t.identity;spec}
  let submit ?key ?not_before ?lane t value =
    let staged=stage ?key ?not_before ?lane t value in
    match D.enqueue t.store staged.spec with
    | D.Inserted id -> Added (handle t id)
    | D.Existing id -> Existing (handle t id)
    | D.Conflict id -> Conflict id
  let find t id = match D.find t.store id with
    | Some j when matches t (D.Job.spec j) -> Some (handle t id)
    | _ -> None
  let finish t (c : _ Completion.t) =
    List.iter (fun s -> if s.Submission.identity<>t.identity then
      invalid_arg "Successors must belong to the same Duraml store")
      c.followups;
    let outcome=match c.outcome with
      | Completion.Success value -> D.Succeeded (t.result.encode value)
      | Completion.Retry (at,reason) -> D.Retry {at;reason}
      | Completion.Failure (code,detail) -> D.Failed {code;detail} in
    D.complete ~followups:(List.map (fun s -> s.Submission.spec) c.followups)
      outcome
  let run ~mono_clock ?concurrency ?lease ?max_attempts ?stop ?drain
      ?(on_settled=fun _ _ -> ()) ?on_exn t handler =
    let on_exn = Option.map (fun f _ -> function
      | Schema_mismatch _ as e -> raise e
      | e -> finish t (f e)) on_exn in
    Duraml_worker.run ~mono_clock ~queue:t.name ?concurrency ?lease
      ?max_attempts ?stop ?drain ?on_exn
      ~on_settled:(fun c result -> on_settled (handle t (D.Claim.id c)) result)
      t.store (fun ~sw c ->
        let spec=D.Claim.spec c in
        if not (matches t spec) then raise (Schema_mismatch (D.Claim.id c));
        match t.request.decode (D.Spec.payload spec) with
        | Error _ -> D.complete (D.Failed {code="invalid_payload";
            detail="Request decoding failed"})
        | Ok request ->
            Eio.Fiber.with_binding context (D.Claim.id c, D.Claim.attempt c)
              (fun () -> finish t (handler ~sw request)))
  let start ~sw ~mono_clock ?concurrency ?lease ?max_attempts ?stop ?drain
      ?on_settled ?on_exn t handler =
    Eio.Fiber.fork ~sw (fun () ->
      run ~mono_clock ?concurrency ?lease ?max_attempts ?stop ?drain
        ?on_settled ?on_exn t handler)
end
