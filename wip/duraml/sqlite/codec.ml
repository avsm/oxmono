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
let spec_codec =
  Jsont.Object.map (fun queue kind version payload key not_before lane ->
    Spec.v ~queue ~kind ~version ~payload ?key ?not_before ?lane ())
  |> Jsont.Object.mem "queue" Jsont.string ~enc:Spec.queue
  |> Jsont.Object.mem "kind" Jsont.string ~enc:Spec.kind
  |> Jsont.Object.mem "version" Jsont.int ~enc:Spec.version
  |> Jsont.Object.mem "payload" Jsont.string ~enc:Spec.payload
  |> Jsont.Object.opt_mem "key" Jsont.string ~enc:Spec.key
  |> Jsont.Object.opt_mem "not_before" time_codec
       ~enc:Spec.not_before
  |> Jsont.Object.opt_mem "lane" Jsont.string
       ~enc:(fun s -> if Spec.lane s="" then None else Some (Spec.lane s))
  |> Jsont.Object.finish
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
let job_codec =
  Jsont.Object.map
    (fun id spec state attempt available_at created_at updated_at outcome
      parent ->
      Pi.restore_job ?parent ~id ~spec ~state ~attempt ~available_at
      ~created_at ~updated_at ~outcome ())
  |> Jsont.Object.mem "id" id_codec ~enc:Job.id
  |> Jsont.Object.mem "spec" spec_codec ~enc:Job.spec
  |> Jsont.Object.mem "state" state_codec ~enc:Job.state
  |> Jsont.Object.mem "attempt" Jsont.int ~enc:Job.attempt
  |> Jsont.Object.mem "available_at" time_codec ~enc:Job.available_at
  |> Jsont.Object.mem "created_at" time_codec ~enc:Job.created_at
  |> Jsont.Object.mem "updated_at" time_codec ~enc:Job.updated_at
  |> Jsont.Object.opt_mem "outcome" outcome_codec ~enc:Job.outcome
  |> Jsont.Object.opt_mem "parent" id_codec ~enc:Job.parent
  |> Jsont.Object.finish
let claim_codec =
  Jsont.Object.map (fun identity id spec attempt token deadline ->
    Pi.restore_claim ~identity ~id ~spec ~attempt ~token ~deadline)
  |> Jsont.Object.mem "identity" Jsont.string ~enc:Pi.claim_identity
  |> Jsont.Object.mem "id" id_codec ~enc:Claim.id
  |> Jsont.Object.mem "spec" spec_codec ~enc:Claim.spec
  |> Jsont.Object.mem "attempt" Jsont.int ~enc:Claim.attempt
  |> Jsont.Object.mem "token" Jsont.string ~enc:Pi.claim_token
  |> Jsont.Object.mem "deadline" time_codec ~enc:Claim.deadline
  |> Jsont.Object.finish
let attempt_codec =
  Jsont.Object.map (fun claim status completion -> Pi.restore_attempt ~claim
    ~status ~completion)
  |> Jsont.Object.mem "claim" claim_codec ~enc:Pi.attempt_claim
  |> Jsont.Object.mem "status" Jsont.string ~enc:Pi.attempt_status
  |> Jsont.Object.opt_mem "completion" completion_codec
       ~enc:Pi.attempt_completion
  |> Jsont.Object.finish
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
