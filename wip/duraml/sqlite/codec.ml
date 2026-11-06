open Duraml
let time_string t = Ptime.to_rfc3339 ~frac_s:12 ~tz_offset_s:0 t
let time_of_string s = match Ptime.of_rfc3339 ~strict:true s with
  | Ok (t,_,_) -> t
  | Error _ -> invalid_arg "Invalid stored timestamp"
let time_codec = Jsont.map ~enc:time_string ~dec:time_of_string Jsont.string
let id_codec = Jsont.map ~enc:Id.to_string
  ~dec:(fun s -> Result.get_ok (Id.of_string s)) Jsont.string
let state_codec = Jsont.map ~enc:state_name ~dec:(function
  | "queued" -> Queued | "running" -> Running | "done" -> Done
  | "failed" -> Failed_job | "cancelled" -> Cancelled
  | _ -> invalid_arg "Invalid state") Jsont.string
let lane s = if Spec.lane s="" then None else Some (Spec.lane s)
let with_payload s payload =
  Spec.v ~queue:(Spec.queue s) ~kind:(Spec.kind s) ~version:(Spec.version s)
    ~payload ?key:(Spec.key s) ?not_before:(Spec.not_before s) ?lane:(lane s)
    ()
(* [spec ~payload] is the specification codec, which holds the payload when
   [payload] is set. Job records leave it to the payloads table. *)
let spec ~payload =
  Jsont.Object.map (fun queue kind version data key not_before lane ->
    Spec.v ~queue ~kind ~version ~payload:(Option.value ~default:"" data)
      ?key ?not_before ?lane ())
  |> Jsont.Object.mem "queue" Jsont.string ~enc:Spec.queue
  |> Jsont.Object.mem "kind" Jsont.string ~enc:Spec.kind
  |> Jsont.Object.mem "version" Jsont.int ~enc:Spec.version
  |> Jsont.Object.opt_mem "payload" Jsont.string
       ~enc:(fun s -> if payload then Some (Spec.payload s) else None)
  |> Jsont.Object.opt_mem "key" Jsont.string ~enc:Spec.key
  |> Jsont.Object.opt_mem "not_before" time_codec
       ~enc:Spec.not_before
  |> Jsont.Object.opt_mem "lane" Jsont.string ~enc:lane
  |> Jsont.Object.finish
let spec_codec = spec ~payload:true
let outcome_codec =
  let parts = function Succeeded s -> "success",s,"",Ptime.epoch
    | Retry {at;reason} -> "retry",reason,"",at
    | Failed {code;detail} -> "failure",detail,code,Ptime.epoch in
  Jsont.Object.map (fun kind data code at -> match kind with
    | "success" -> Succeeded data | "retry" -> Retry {at;reason=data}
    | "failure" -> Failed {code;detail=data}
    | _ -> invalid_arg "Invalid outcome")
  |> Jsont.Object.mem "kind" Jsont.string
       ~enc:(fun o -> let k,_,_,_=parts o in k)
  |> Jsont.Object.mem "data" Jsont.string
       ~enc:(fun o -> let _,s,_,_=parts o in s)
  |> Jsont.Object.mem "code" Jsont.string
       ~enc:(fun o -> let _,_,c,_=parts o in c)
  |> Jsont.Object.mem "at" time_codec
       ~enc:(fun o -> let _,_,_,at=parts o in at)
  |> Jsont.Object.finish
let completion_codec =
  Jsont.Object.map (fun outcome followups -> complete ~followups outcome)
  |> Jsont.Object.mem "outcome" outcome_codec ~enc:Completion.outcome
  |> Jsont.Object.mem "followups" (Jsont.list spec_codec)
       ~enc:Completion.followups
  |> Jsont.Object.finish
(* A job record holds its specification without the payload, which is
   restored by {!with_job_payload}. *)
let job_codec =
  Jsont.Object.map
    (fun id spec state attempt available_at created_at updated_at outcome
      parent ->
      Pi.restore_job ?parent ~id ~spec ~state ~attempt ~available_at
      ~created_at ~updated_at ~outcome ())
  |> Jsont.Object.mem "id" id_codec ~enc:Job.id
  |> Jsont.Object.mem "spec" (spec ~payload:false) ~enc:Job.spec
  |> Jsont.Object.mem "state" state_codec ~enc:Job.state
  |> Jsont.Object.mem "attempt" Jsont.int ~enc:Job.attempt
  |> Jsont.Object.mem "available_at" time_codec ~enc:Job.available_at
  |> Jsont.Object.mem "created_at" time_codec ~enc:Job.created_at
  |> Jsont.Object.mem "updated_at" time_codec ~enc:Job.updated_at
  |> Jsont.Object.opt_mem "outcome" outcome_codec ~enc:Job.outcome
  |> Jsont.Object.opt_mem "parent" id_codec ~enc:Job.parent
  |> Jsont.Object.finish
let with_job_payload j payload =
  Pi.restore_job ?parent:(Job.parent j) ~id:(Job.id j)
    ~spec:(with_payload (Job.spec j) payload) ~state:(Job.state j)
    ~attempt:(Job.attempt j) ~available_at:(Job.available_at j)
    ~created_at:(Job.created_at j) ~updated_at:(Job.updated_at j)
    ~outcome:(Job.outcome j) ()
(* An attempt record holds its claim without the specification, which is its
   job's. *)
type claim = {
  identity : string; id : Id.t; attempt : int; token : string;
  deadline : Ptime.t;
}
type attempt = {claim : claim; status : string; completion : completion option}
let claim_codec =
  Jsont.Object.map (fun identity id attempt token deadline ->
    {identity;id;attempt;token;deadline})
  |> Jsont.Object.mem "identity" Jsont.string ~enc:(fun c -> c.identity)
  |> Jsont.Object.mem "id" id_codec ~enc:(fun c -> c.id)
  |> Jsont.Object.mem "attempt" Jsont.int ~enc:(fun c -> c.attempt)
  |> Jsont.Object.mem "token" Jsont.string ~enc:(fun c -> c.token)
  |> Jsont.Object.mem "deadline" time_codec ~enc:(fun c -> c.deadline)
  |> Jsont.Object.finish
let attempt_codec =
  Jsont.Object.map (fun claim status completion -> {claim;status;completion})
  |> Jsont.Object.mem "claim" claim_codec ~enc:(fun a -> a.claim)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun a -> a.status)
  |> Jsont.Object.opt_mem "completion" completion_codec
       ~enc:(fun a -> a.completion)
  |> Jsont.Object.finish
let of_attempt a =
  let c=Pi.attempt_claim a in
  {claim={identity=Pi.claim_identity c;id=Claim.id c;attempt=Claim.attempt c;
     token=Pi.claim_token c;deadline=Claim.deadline c};
   status=Pi.attempt_status a;completion=Pi.attempt_completion a}
let to_attempt ~spec a =
  let c=a.claim in
  Pi.restore_attempt ~status:a.status ~completion:a.completion
    ~claim:(Pi.restore_claim ~identity:c.identity ~id:c.id ~spec
      ~attempt:c.attempt ~token:c.token ~deadline:c.deadline)
let event_codec =
  Jsont.Object.map (fun sequence time kind job attempt detail ->
    Pi.restore_event ~sequence ~time ~kind ~job ~attempt ~detail)
  |> Jsont.Object.mem "sequence" (Jsont.map ~enc:Int64.to_string
       ~dec:Int64.of_string Jsont.string) ~enc:Event.sequence
  |> Jsont.Object.mem "time" time_codec ~enc:Event.time
  |> Jsont.Object.mem "kind" Jsont.string ~enc:Event.kind
  |> Jsont.Object.mem "job" id_codec ~enc:Event.job
  |> Jsont.Object.mem "attempt" Jsont.int ~enc:Event.attempt
  |> Jsont.Object.mem "detail" Jsont.string ~enc:Event.detail
  |> Jsont.Object.finish
