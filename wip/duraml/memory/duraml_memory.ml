module D = Duraml
module Jobs = Map.Make (D.Id)
module Keys = Map.Make (struct
  type t = string * string
  let compare = Stdlib.compare
end)
module Attempts = Map.Make (struct
  type t = D.Id.t * int
  let compare (a,n) (b,m) =
    let c=D.Id.compare a b in if c=0 then Int.compare n m else c
end)
module Events = Map.Make (Int64)
module Lanes = Keys
type state = {
  next_id : int64; revision : int64; jobs : D.job Jobs.t;
  keys : D.Id.t Keys.t; attempts : D.Pi.attempt Attempts.t;
  events : D.event Events.t; served : int Lanes.t;
}
type t = {
  identity : string; mutable state : state; mutable closed : bool;
  mutable wake_revision : int64;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  random : Eio.Flow.source_ty Eio.Resource.t;
  mutex : Eio.Mutex.t; changed : Eio.Condition.t;
}
let check t = if t.closed then raise (D.err D.Closed)
let storage t state =
  let find id = Jobs.find_opt id !state.jobs in
  let scheduled queue = Jobs.bindings !state.jobs |> List.map snd
    |> List.filter (fun (j:D.job) -> (D.Spec.queue (D.Job.spec j))=queue &&
      ((D.Job.state j)=D.Queued || (D.Job.state j)=D.Running)) in
  (* Lanes never served sort first; a lane's rank is its last service. *)
  let rank queue lane = Option.value ~default:0
    (Lanes.find_opt (queue,lane) !state.served) in
  let dispatch_order queue (a:D.job) (b:D.job) =
    let lane j = D.Spec.lane (D.Job.spec j) in
    let c=compare (rank queue (lane a),lane a) (rank queue (lane b),lane b) in
    if c<>0 then c else
    let c=Ptime.compare (D.Job.created_at a) (D.Job.created_at b) in
    if c=0 then D.Id.compare (D.Job.id a) (D.Job.id b) else c in
  let take n xs = List.filteri (fun i _ -> i<n) xs in
  let matching q = Jobs.bindings !state.jobs |> List.map snd
    |> List.filter (fun (j:D.job) ->
      Option.fold ~none:true ~some:((=) (D.Spec.queue (D.Job.spec j)))
        (D.Query.queue q) &&
      Option.fold ~none:true ~some:((=) (D.Job.state j)) (D.Query.state q) &&
      Option.fold ~none:true ~some:(fun id -> D.Id.compare (D.Job.id j) id>0)
        (D.Query.after q) &&
      Option.fold ~none:true ~some:(fun id ->
        Option.fold ~none:false ~some:(fun p -> D.Id.compare p id=0)
          (D.Job.parent j)) (D.Query.parent q)) in
  {D.Pi.identity=t.identity;
   allocate=(fun () ->
     let n= !state.next_id in
     if n=Int64.max_int then raise (D.err (D.Storage "Job IDs exhausted"));
     state:={!state with next_id=Int64.succ n};
     Result.get_ok (D.Id.of_string (Int64.to_string n)));
   find;
   find_key=(fun ~queue ~key ->
     Option.bind (Keys.find_opt (queue,key) !state.keys) find);
   put=(fun (j:D.job) ->
     let keys=match (D.Spec.key (D.Job.spec j)) with None -> !state.keys
       | Some key -> Keys.add ((D.Spec.queue (D.Job.spec j)),key) (D.Job.id j)
         !state.keys in
     state:={!state with jobs=Jobs.add (D.Job.id j) j !state.jobs;keys});
   eligible=(fun ~queue ~now -> scheduled queue
     |> List.filter (fun (j:D.job) -> Ptime.compare (D.Job.available_at j)
       now<=0)
     |> List.sort (dispatch_order queue)
     |> function [] -> None | j::_ -> Some j);
   served=(fun ~queue ~lane ->
     let latest=Lanes.fold (fun (q,_) n acc -> if q=queue then max n acc
       else acc) !state.served 0 in
     state:={!state with served=Lanes.add (queue,lane) (latest+1)
       !state.served});
   next_at=(fun ~queue -> scheduled queue
     |> List.fold_left (fun acc (j:D.job) -> match acc with
       | Some t when Ptime.compare t (D.Job.available_at j)<=0 -> acc
       | _ -> Some (D.Job.available_at j)) None);
   query=(fun q -> matching q |> take (D.Query.limit q));
   count=(fun q -> List.length (matching q));
   attempt=(fun id n -> Attempts.find_opt (id,n) !state.attempts);
   put_attempt=(fun a ->
     let c=D.Pi.attempt_claim a in
     state:={!state with attempts=Attempts.add
       (D.Claim.id c,D.Claim.attempt c) a !state.attempts});
   emit=(fun ~time ~kind ~job ~attempt ~detail ->
     let sequence=Int64.succ !state.revision in
     let e=D.Pi.restore_event ~sequence ~time ~kind ~job ~attempt ~detail in
     state:={!state with revision=sequence;
       events=Events.add sequence e !state.events});
   revision=(fun () -> t.wake_revision);
   events=(fun ~after ~limit -> Events.bindings !state.events
     |> List.filter_map (fun (seq,e) -> if seq>after then Some e else None)
     |> take limit);
   tail=(fun ~before ~limit -> List.rev (Events.bindings !state.events)
     |> List.filter_map (fun (seq,e) ->
       if Option.fold ~none:true ~some:(fun b -> seq<b) before then Some e
       else None)
     |> take limit)}
let call : type a. t -> a D.Pi.request -> a = fun t request ->
  check t;
  (* Randomness can yield. Consume it before entering the transaction. *)
  let token=match request with D.Pi.Try_take _ -> D.Pi.random_id t.random
    | _ -> "" in
  let result=Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
    try
      check t;
      let state=ref t.state in
      let result=D.Pi.execute ~now:(D.Pi.time_of_float_s (Eio.Time.now
        t.clock)) ~token
        (storage t state) request in
      (* Renewals change eligibility without emitting lifecycle events. *)
      if !state != t.state then begin
        t.state <- !state;
        t.wake_revision <- Int64.succ t.wake_revision;
        Eio.Condition.broadcast t.changed
      end;
      Ok result
    with ex -> Error (ex,Printexc.get_raw_backtrace ())) in
  match result with Ok x -> x
  | Error (ex,bt) -> Printexc.raise_with_backtrace ex bt
let await t (w:D.waiting) =
  let wait () = Eio.Mutex.use_ro t.mutex (fun () ->
    check t;
    while t.wake_revision=(D.Waiting.revision w) do
      Eio.Condition.await t.changed t.mutex; check t
    done) in
  match (D.Waiting.next_at w) with None -> wait ()
  | Some at -> Eio.Fiber.first wait
      (fun () -> Eio.Time.sleep_until t.clock (D.Pi.clock_deadline at))
module Backend = struct
  type nonrec t = t
  let call = call
  let await = await
end
let create ~sw ~clock ~random () =
  let identity=D.Pi.random_id random in
  let t={identity;state={next_id=1L;revision=0L;jobs=Jobs.empty;
    keys=Keys.empty;attempts=Attempts.empty;events=Events.empty;
    served=Lanes.empty};closed=false;
    wake_revision=0L;
    clock=(clock :> float Eio.Time.clock_ty Eio.Resource.t);
    random=(random :> Eio.Flow.source_ty Eio.Resource.t);
    mutex=Eio.Mutex.create ();changed=Eio.Condition.create ()} in
  Eio.Switch.on_release sw (fun () ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.closed <- true; Eio.Condition.broadcast t.changed));
  Eio.Resource.T (t,D.Pi.handler (module Backend))
