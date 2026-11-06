module D = Duraml
module Jobs = Map.Make (D.Id)
module Names = Map.Make (String)
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
module Ids = Set.Make (D.Id)
module Lanes = Keys
(* [pending] holds each queue's queued and running jobs, and [lanes] counts
   them per lane. A lane is ranked in [served] while it holds such a job,
   and [latest] is each queue's highest rank. [counts] is the number of jobs
   in each queue and state. [by_queue] and [by_state] hold the job IDs of
   each queue, and of each queue and state, so that a listing confined to a
   queue reads only its jobs. *)
type state = {
  next_id : int64; revision : int64; jobs : D.job Jobs.t;
  keys : D.Id.t Keys.t; attempts : D.Pi.attempt Attempts.t;
  events : D.event Events.t; served : int Lanes.t; latest : int Names.t;
  pending : D.job Jobs.t Names.t; lanes : int Lanes.t; counts : int Keys.t;
  by_queue : Ids.t Names.t; by_state : Ids.t Keys.t;
}
type t = {
  identity : string; mutable state : state; mutable closed : bool;
  mutable wake_revision : int64;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  random : Eio.Flow.source_ty Eio.Resource.t;
  mutex : Eio.Mutex.t; changed : Eio.Condition.t;
}
let check t = if t.closed then raise (D.err D.Closed)
let active (j:D.job) = D.Job.state j=D.Queued || D.Job.state j=D.Running
let queue_of (j:D.job) = D.Spec.queue (D.Job.spec j)
let lane_of (j:D.job) = D.Spec.lane (D.Job.spec j)
let rec take n s () = if n<=0 then Seq.Nil else match s () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (x,s) -> Seq.Cons (x,take (n-1) s)
let storage t state =
  let find id = Jobs.find_opt id !state.jobs in
  let pending queue = Option.value ~default:Jobs.empty
    (Names.find_opt queue !state.pending) in
  let latest queue = Option.value ~default:0
    (Names.find_opt queue !state.latest) in
  let rank queue lane = Option.value ~default:(latest queue)
    (Lanes.find_opt (queue,lane) !state.served) in
  let order queue (a:D.job) (b:D.job) =
    let c=compare (rank queue (lane_of a),lane_of a)
      (rank queue (lane_of b),lane_of b) in
    if c<>0 then c else
    let c=Ptime.compare (D.Job.created_at a) (D.Job.created_at b) in
    if c=0 then D.Id.compare (D.Job.id a) (D.Job.id b) else c in
  let matching q =
    let from ids = match D.Query.after q with
      | None -> Ids.to_seq ids
      | Some after -> Ids.to_seq_from after ids
        |> Seq.drop_while (D.Id.equal after) in
    let ids set = Seq.filter_map (fun id ->
      Option.map (fun j -> (id,j)) (Jobs.find_opt id !state.jobs))
      (from set) in
    let jobs = match D.Query.queue q,D.Query.state q with
      | Some queue,Some st -> ids (Option.value ~default:Ids.empty
          (Keys.find_opt (queue,D.state_name st) !state.by_state))
      | Some queue,None -> ids (Option.value ~default:Ids.empty
          (Names.find_opt queue !state.by_queue))
      | None,_ -> (match D.Query.after q with
        | None -> Jobs.to_seq !state.jobs
        | Some after -> Jobs.to_seq_from after !state.jobs
          |> Seq.drop_while (fun (id,_) -> D.Id.equal id after)) in
    Seq.filter_map (fun (_,(j:D.job)) ->
      if Option.fold ~none:true ~some:((=) (queue_of j)) (D.Query.queue q) &&
        Option.fold ~none:true ~some:((=) (D.Job.state j)) (D.Query.state q) &&
        Option.fold ~none:true ~some:(fun id ->
          Option.fold ~none:false ~some:(D.Id.equal id) (D.Job.parent j))
          (D.Query.parent q)
      then Some j else None) jobs in
  let put (j:D.job) =
    let s= !state in
    let queue=queue_of j and lane=lane_of j and id=D.Job.id j in
    let old=find id in
    let was=Option.fold ~none:false ~some:active old in
    let bump state n counts =
      let k=(queue,D.state_name state) in
      match Option.value ~default:0 (Keys.find_opt k counts) + n with
      | 0 -> Keys.remove k counts
      | c -> Keys.add k c counts in
    let counts=bump (D.Job.state j) 1 (match old with
      | Some o -> bump (D.Job.state o) (-1) s.counts
      | None -> s.counts) in
    let keys=match D.Spec.key (D.Job.spec j) with None -> s.keys
      | Some key -> Keys.add (queue,key) id s.keys in
    let jobs_in=pending queue in
    let count=Option.value ~default:0 (Lanes.find_opt (queue,lane) s.lanes) in
    let pending_q,lanes,served = match was,active j with
      | _,true ->
          let lanes,served = if was then s.lanes,s.served else
            Lanes.add (queue,lane) (count+1) s.lanes,
            (if count=0 then Lanes.add (queue,lane) (latest queue) s.served
             else s.served) in
          Jobs.add id j jobs_in,lanes,served
      | true,false ->
          let lanes,served = if count<=1 then
            Lanes.remove (queue,lane) s.lanes,Lanes.remove (queue,lane) s.served
            else Lanes.add (queue,lane) (count-1) s.lanes,s.served in
          Jobs.remove id jobs_in,lanes,served
      | false,false -> jobs_in,s.lanes,s.served in
    let by_state =
      let k st = (queue,D.state_name st) in
      let set st m = Option.value ~default:Ids.empty (Keys.find_opt (k st) m) in
      let m = match old with
        | Some o -> Keys.add (k (D.Job.state o))
            (Ids.remove id (set (D.Job.state o) s.by_state)) s.by_state
        | None -> s.by_state in
      Keys.add (k (D.Job.state j)) (Ids.add id (set (D.Job.state j) m)) m in
    let by_queue = Names.add queue (Ids.add id (Option.value ~default:Ids.empty
      (Names.find_opt queue s.by_queue))) s.by_queue in
    state:={s with jobs=Jobs.add id j s.jobs;keys;lanes;served;counts;
      pending=Names.add queue pending_q s.pending;by_queue;by_state} in
  {D.Pi.identity=t.identity;
   allocate=(fun () ->
     let n= !state.next_id in
     if n=Int64.max_int then raise (D.err (D.Storage "Job IDs exhausted"));
     state:={!state with next_id=Int64.succ n};
     Result.get_ok (D.Id.of_string (Int64.to_string n)));
   find;
   find_key=(fun ~queue ~key ->
     Option.bind (Keys.find_opt (queue,key) !state.keys) find);
   put;
   eligible=(fun ~queue ~now -> Jobs.fold (fun _ (j:D.job) best ->
     if Ptime.compare (D.Job.available_at j) now>0 then best
     else match best with
       | Some b when order queue b j<=0 -> best
       | _ -> Some j) (pending queue) None);
   served=(fun ~queue ~lane ->
     let next=latest queue+1 in
     state:={!state with latest=Names.add queue next !state.latest;
       served=Lanes.add (queue,lane) next !state.served});
   next_at=(fun ~queue -> Jobs.fold (fun _ (j:D.job) acc -> match acc with
     | Some t when Ptime.compare t (D.Job.available_at j)<=0 -> acc
     | _ -> Some (D.Job.available_at j)) (pending queue) None);
   query=(fun q ->
     D.Pi.page (List.of_seq (take (D.Query.limit q) (matching q))));
   (* Without a cursor or parent the counts answer, otherwise the jobs are
      read. *)
   count=(fun q -> match D.Query.after q,D.Query.parent q with
     | None,None ->
         Keys.fold (fun (queue,state) n total ->
           if Option.fold ~none:true ~some:((=) queue) (D.Query.queue q) &&
             Option.fold ~none:true
               ~some:(fun s -> D.state_name s=state) (D.Query.state q)
           then total+n else total) !state.counts 0
     | _ -> Seq.length (matching q));
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
   events=(fun ~after ~limit ->
     Events.to_seq_from (Int64.succ after) !state.events
     |> Seq.map snd |> take limit |> List.of_seq);
   tail=(fun ~before ~limit ->
     let older = match before with None -> !state.events
       | Some b -> let older,_,_=Events.split b !state.events in older in
     Events.to_rev_seq older |> Seq.map snd |> take limit |> List.of_seq)}
(* A renewal wakes waiters only when it brings a deadline forward, the one
   way it can make work eligible sooner. *)
let wakes : type a. a D.Pi.request -> before:state -> after:state -> bool =
  fun request ~before ~after -> match request with
  | D.Pi.Renew (c,_) ->
      let deadline s = Option.map D.Job.available_at
        (Jobs.find_opt (D.Claim.id c) s.jobs) in
      (match deadline before,deadline after with
       | Some b,Some a -> Ptime.compare a b<0
       | _ -> true)
  | _ -> true
let locked t f =
  let result=Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
    try check t; Ok (f ())
    with ex -> Error (ex,Printexc.get_raw_backtrace ())) in
  match result with Ok x -> x
  | Error (ex,bt) -> Printexc.raise_with_backtrace ex bt
let now t = D.Pi.time_of_float_s (Eio.Time.now t.clock)
let call : type a. t -> a D.Pi.request -> a = fun t request ->
  check t;
  let idle = match request with
    | D.Pi.Try_take (queue,lease) -> locked t (fun () ->
        D.Pi.idle ~now:(now t) ~lease (storage t (ref t.state)) ~queue)
    | _ -> None in
  match idle, request with
  | Some waiting, D.Pi.Try_take _ -> D.Await waiting
  | _ ->
  (* Randomness can yield. Consume it before entering the transaction, and
     only when there is work to claim. *)
  let token=match request with D.Pi.Try_take _ -> D.Pi.random_id t.random
    | _ -> "" in
  locked t (fun () ->
    let state=ref t.state in
    let result=D.Pi.execute ~now:(now t) ~token (storage t state) request in
    if !state != t.state then begin
      let wake=wakes request ~before:t.state ~after:!state in
      t.state <- !state;
      if wake then begin
        t.wake_revision <- Int64.succ t.wake_revision;
        Eio.Condition.broadcast t.changed
      end
    end;
    result)
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
    served=Lanes.empty;latest=Names.empty;pending=Names.empty;
    lanes=Lanes.empty;counts=Keys.empty;by_queue=Names.empty;
    by_state=Keys.empty};closed=false;
    wake_revision=0L;
    clock=(clock :> float Eio.Time.clock_ty Eio.Resource.t);
    random=(random :> Eio.Flow.source_ty Eio.Resource.t);
    mutex=Eio.Mutex.create ();changed=Eio.Condition.create ()} in
  Eio.Switch.on_release sw (fun () ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.closed <- true; Eio.Condition.broadcast t.changed));
  Eio.Resource.T (t,D.Pi.handler (module Backend))
