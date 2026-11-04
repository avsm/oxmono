module Id = struct
  type t = int64
  let to_string = Int64.to_string
  let of_string s =
    match Int64.of_string_opt s with
    | Some n when n > 0L -> Ok n
    | _ -> Error "Invalid job ID"
  let compare = Int64.compare
end

let text name limit s =
  if String.length s > limit || not (String.is_valid_utf_8 s) then
    invalid_arg (name ^ " exceeds its UTF-8 byte budget")
let name field s =
  text field 256 s;
  if String.trim s = "" || String.contains s '\000' then
    invalid_arg (field ^ " must be nonempty and contain no NUL")
let timestamp n =
  if Ptime.compare n Ptime.epoch < 0 then invalid_arg "Invalid timestamp"
let duration n =
  if Ptime.Span.compare n Ptime.Span.zero <= 0 then invalid_arg
    "Invalid duration"

module Spec = struct
  type t = {
    queue : string; kind : string; version : int; payload : string;
    key : string option; not_before : Ptime.t option; lane : string;
  }
  let v ~queue ~kind ~version ~payload ?key ?not_before ?(lane="") () =
    name "queue" queue; name "kind" kind; text "payload" 1048576 payload;
    if version < 1 then invalid_arg "Version must be positive";
    Option.iter (name "key") key; Option.iter timestamp not_before;
    if lane <> "" then name "lane" lane;
    {queue;kind;version;payload;key;not_before;lane}
  let queue (x : t) = x.queue
  let kind (x : t) = x.kind
  let version (x : t) = x.version
  let payload (x : t) = x.payload
  let key (x : t) = x.key
  let not_before (x : t) = x.not_before
  let lane (x : t) = x.lane
  (* Lanes schedule work; they do not identify it. *)
  let same (a : t) (b : t) = {a with lane=""} = {b with lane=""}
end

type outcome =
  | Succeeded of string
  | Retry of {at : Ptime.t; reason : string}
  | Failed of {code : string; detail : string}
type completion = {outcome : outcome; followups : Spec.t list}

let validate_completion (c : completion) =
  (match c.outcome with
   | Succeeded s -> text "result" 1048576 s
   | Retry {at;reason} -> timestamp at; text "reason" 4096 reason;
       if c.followups <> [] then invalid_arg "Retry cannot enqueue successors"
   | Failed {code;detail} -> name "failure code" code;
       text "failure detail" 4096 detail);
  if List.length c.followups > 100 then invalid_arg "Too many successors";
  if List.fold_left (fun n s -> n + String.length s.Spec.payload)
      0 c.followups > 4*1048576 then invalid_arg "Successors exceed 4 MiB"

let complete ?(followups=[]) outcome =
  let c={outcome;followups} in validate_completion c; c
type state = Queued | Running | Done | Failed_job | Cancelled
let state_name = function Queued -> "queued" | Running -> "running"
  | Done -> "done" | Failed_job -> "failed" | Cancelled -> "cancelled"
type job = {
  id : Id.t; spec : Spec.t; state : state; attempt : int;
  available_at : Ptime.t; created_at : Ptime.t; updated_at : Ptime.t;
  outcome : outcome option; parent : Id.t option;
}
module Claim = struct
  type t = {
    identity : string; id : Id.t; spec : Spec.t;
    attempt : int; token : string; deadline : Ptime.t;
  }
  let id c = c.id
  let spec c = c.spec
  let attempt c = c.attempt
  let deadline c = c.deadline
end
type enqueue_result = Inserted of Id.t | Existing of Id.t | Conflict of Id.t
type settlement = Applied | Already_applied | Lost_claim
  | Settlement_conflict | Followup_conflict of Id.t
type waiting = {revision : int64; next_at : Ptime.t option}
type poll = Ready of Claim.t | Await of waiting
type query = {
  queue : string option; state : state option; after : Id.t option;
  parent : Id.t option; limit : int;
}
let query ?queue ?state ?after ?parent ?(limit=100) () =
  if limit < 1 || limit > 1000 then invalid_arg "Limit must be 1..1000";
  Option.iter (name "queue") queue;
  {queue;state;after;parent;limit}
type event = {
  sequence : int64; time : Ptime.t; kind : string; job : Id.t;
  attempt : int; detail : string;
}
type error = Closed | Storage of string
type Eio.Exn.err += E of error
let err e = Eio.Exn.create (E e)
type 'a t = 'a Eio.Resource.t constraint 'a = [> `Job_queue]
type plain = [ `Job_queue ] Eio.Resource.t

module Completion = struct
  type t = completion
  let outcome (x : completion) = x.outcome
  let followups (x : completion) = x.followups
end

module Job = struct
  type t = job
  let id (x : job) = x.id
  let spec (x : job) = x.spec
  let state (x : job) = x.state
  let attempt (x : job) = x.attempt
  let available_at (x : job) = x.available_at
  let created_at (x : job) = x.created_at
  let updated_at (x : job) = x.updated_at
  let outcome (x : job) = x.outcome
  let parent (x : job) = x.parent
end

module Query = struct
  type t = query
  let queue (x : query) = x.queue
  let state (x : query) = x.state
  let after (x : query) = x.after
  let parent (x : query) = x.parent
  let limit (x : query) = x.limit
end

module Event = struct
  type t = event
  let sequence (x : event) = x.sequence
  let time (x : event) = x.time
  let kind (x : event) = x.kind
  let job (x : event) = x.job
  let attempt (x : event) = x.attempt
  let detail (x : event) = x.detail
end

module Waiting = struct
  type t = waiting
  let revision (x : waiting) = x.revision
  let next_at (x : waiting) = x.next_at
end

module Pi = struct
  type _ request =
    | Identity : string request
    | Enqueue : Spec.t -> enqueue_result request
    | Try_take : string * Ptime.Span.t -> poll request
    | Renew : Claim.t * Ptime.Span.t -> bool request
    | Settle : Claim.t * completion -> settlement request
    | Cancel : Id.t -> bool request
    | Find : Id.t -> job option request
    | Query : query -> job list request
    | Count : query -> int request
    | Events : int64 * int -> event list request
    | Tail : int64 option * int -> event list request

  let operation : type a. a request -> string = function
    | Identity -> "identity" | Enqueue _ -> "enqueue"
    | Try_take _ -> "take" | Renew _ -> "renew"
    | Settle _ -> "settle" | Cancel _ -> "cancel" | Find _ -> "find"
    | Query _ -> "query" | Count _ -> "count" | Events _ -> "events"
    | Tail _ -> "tail"
  module type BACKEND = sig
    type t
    val call : t -> 'a request -> 'a
    val await : t -> waiting -> unit
  end
  type (_, _, _) Eio.Resource.pi +=
    | Queue : ('a, (module BACKEND with type t = 'a),
               [> `Job_queue]) Eio.Resource.pi
  let handler (type a) (module B : BACKEND with type t = a) :
      (a, [ `Job_queue ]) Eio.Resource.handler =
    Eio.Resource.handler [H (Queue, (module B))]
  let call (Eio.Resource.T (t, ops)) request =
    let module B = (val Eio.Resource.get ops Queue) in B.call t request
  let await (Eio.Resource.T (t, ops)) waiting =
    let module B = (val Eio.Resource.get ops Queue) in B.await t waiting

  type attempt = {
    claim : Claim.t; status : string; completion : completion option;
  }
  type storage = {
    identity : string;
    allocate : unit -> Id.t;
    find : Id.t -> job option;
    find_key : queue:string -> key:string -> job option;
    put : job -> unit;
    eligible : queue:string -> now:Ptime.t -> job option;
    served : queue:string -> lane:string -> unit;
    next_at : queue:string -> Ptime.t option;
    query : query -> job list;
    count : query -> int;
    attempt : Id.t -> int -> attempt option;
    put_attempt : attempt -> unit;
    emit : time:Ptime.t -> kind:string -> job:Id.t -> attempt:int ->
      detail:string -> unit;
    revision : unit -> int64;
    events : after:int64 -> limit:int -> event list;
    tail : before:int64 option -> limit:int -> event list;
  }
  exception Abort_followup of Id.t

  let execute (type a) ~now ~token s (request : a request) : a =
    timestamp now;
    let emit (j : job) kind detail = s.emit ~time:now ~kind ~job:j.id
      ~attempt:j.attempt ~detail in
    (* Successors without a lane of their own stay in their parent's. *)
    let enqueue ?parent ?(lane="") spec =
      let spec = if spec.Spec.lane = "" then {spec with lane} else spec in
      match Option.bind spec.Spec.key (fun key ->
        s.find_key ~queue:spec.queue ~key) with
      | Some j when Spec.same j.spec spec -> Existing j.id
      | Some j -> Conflict j.id
      | None ->
          let j = {id=s.allocate ();spec;state=Queued;attempt=0;
            available_at=Option.value ~default:now spec.not_before;
            created_at=now;updated_at=now;outcome=None;parent} in
          s.put j; emit j "enqueued" ""; Inserted j.id in
    let owned c =
      if c.Claim.identity <> s.identity then None else
      match s.find c.id, s.attempt c.id c.attempt with
      | Some j, Some a when j.state=Running && j.attempt=c.attempt &&
          a.status="running" && a.claim.token=c.token &&
          Ptime.compare j.available_at now > 0 -> Some (j,a)
      | _ -> None in
    match request with
    | Identity -> s.identity
    | Enqueue spec -> enqueue spec
    | Find id -> s.find id
    | Query q -> s.query q
    | Count q -> s.count q
    | Events (after,limit) -> s.events ~after ~limit
    | Tail (before,limit) -> s.tail ~before ~limit
    | Try_take (queue,lease) ->
        (match s.eligible ~queue ~now with
         | None -> Await {revision=s.revision ();next_at=s.next_at ~queue}
         | Some j ->
             if j.state=Running then begin
               let a = Option.get (s.attempt j.id j.attempt) in
               s.put_attempt {a with status="expired"};
               emit j "lease_expired" ""
             end;
             let deadline=match Ptime.add_span now lease with
               | Some t -> t
               | None -> invalid_arg "Lease exceeds timestamp range" in
             let j={j with state=Running;attempt=j.attempt+1;
               available_at=deadline;updated_at=now} in
             let c={Claim.identity=s.identity;id=j.id;spec=j.spec;
               attempt=j.attempt;token;deadline} in
             s.put j;
             s.put_attempt {claim=c;status="running";completion=None};
             s.served ~queue ~lane:j.spec.lane;
             emit j "claimed" ""; Ready c)
    | Renew (c,lease) ->
        (match owned c with None -> false | Some (j,a) ->
          let deadline=match Ptime.add_span now lease with
               | Some t -> t
               | None -> invalid_arg "Lease exceeds timestamp range" in
          s.put {j with available_at=deadline;updated_at=now};
          s.put_attempt {a with claim={a.claim with deadline}}; true)
    | Cancel id ->
        (match s.find id with
         | None -> false
         | Some j when j.state<>Queued && j.state<>Running -> false
         | Some j ->
             if j.state=Running then begin
               let a=Option.get (s.attempt id j.attempt) in
               s.put_attempt {a with status="cancelled"}
             end;
             let j={j with state=Cancelled;updated_at=now} in
             s.put j; emit j "cancelled" ""; true)
    | Settle (c,completion) ->
        let previous = if c.Claim.identity <> s.identity then None
          else s.attempt c.id c.attempt in
        (match previous with
         | Some a when a.claim.token=c.token && a.status="settled" ->
             if a.completion=Some completion then Already_applied
             else Settlement_conflict
         | _ -> match owned c with
           | None -> Lost_claim
           | Some (j,a) ->
               List.iter (fun spec ->
                 match enqueue ~parent:j.id ~lane:j.spec.lane spec with
                 | Conflict id -> raise (Abort_followup id)
                 | Inserted _ | Existing _ -> ()) completion.followups;
               let state,available_at,kind,detail = match completion.outcome
                 with
                 | Succeeded _ -> Done,j.available_at,"succeeded",""
                 | Retry {at;reason} -> Queued,at,"retry_scheduled",reason
                 | Failed {code;_} -> Failed_job,j.available_at,"failed",code in
               let j={j with state;available_at;updated_at=now;
                 outcome=Some completion.outcome} in
               s.put j;
               s.put_attempt {a with status="settled";
                 completion=Some completion};
               emit j kind detail; Applied)

  let time_of_float_s seconds =
    match Ptime.of_float_s seconds with
    | Some t -> timestamp t; t
    | None -> invalid_arg "Clock is outside the timestamp range"
  let clock_deadline t =
    let rec ceiling seconds = match Ptime.of_float_s seconds with
      | Some rounded when Ptime.compare rounded t < 0 ->
          ceiling (Float.next_after seconds infinity)
      | _ -> seconds in
    ceiling (Ptime.to_float_s t)
  let waiting ~revision ~next_at = {revision;next_at}
  let restore_event ~sequence ~time ~kind ~job ~attempt ~detail =
    {sequence;time;kind;job;attempt;detail}
  let restore_job ?parent ~id ~spec ~state ~attempt ~available_at
      ~created_at ~updated_at ~outcome () =
    {id;spec;state;attempt;available_at;created_at;updated_at;outcome;parent}
  let restore_claim ~identity ~id ~spec ~attempt ~token ~deadline =
    {Claim.identity;id;spec;attempt;token;deadline}
  let restore_attempt ~claim ~status ~completion = {claim;status;completion}
  let claim_identity c = c.Claim.identity
  let claim_token c = c.Claim.token
  let attempt_status a = a.status
  let attempt_completion a = a.completion
  let attempt_claim a = a.claim
  let random_id source =
    let bytes=Cstruct.create 16 in Eio.Flow.read_exact source bytes;
    let hex="0123456789abcdef" in
    String.init 32 (fun i ->
      let n=Cstruct.get_uint8 bytes (i/2) in
      hex.[if i mod 2=0 then n lsr 4 else n land 15])
end

let identity t = Pi.call t Pi.Identity
let enqueue t spec = Pi.call t (Pi.Enqueue spec)
let try_take t ~queue ~lease =
  name "queue" queue; duration lease;
  Pi.call t (Pi.Try_take (queue,lease))
let rec take t ~queue ~lease = match try_take t ~queue ~lease with
  | Ready c -> c | Await waiting -> Pi.await t waiting; take t ~queue ~lease
let renew t c ~lease = duration lease; Pi.call t (Pi.Renew (c,lease))
let settle t c completion =
  try Pi.call t (Pi.Settle (c,completion))
  with Pi.Abort_followup id -> Followup_conflict id
let cancel t id = Pi.call t (Pi.Cancel id)
let find t id = Pi.call t (Pi.Find id)
let list t q = Pi.call t (Pi.Query q)
let count t q = Pi.call t (Pi.Count q)
let events ?(after=0L) ?(limit=100) t =
  if after < 0L || limit<1 || limit>1000 then invalid_arg
    "Invalid event cursor";
  Pi.call t (Pi.Events (after,limit))
let tail ?before ?(limit=100) t =
  if limit < 1 || limit > 1000 then invalid_arg "Limit must be 1..1000";
  Option.iter (fun b -> if b < 1L then invalid_arg "Cursor must be positive")
    before;
  Pi.call t (Pi.Tail (before,limit))
