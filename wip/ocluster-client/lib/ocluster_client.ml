(* SPDX-License-Identifier: ISC *)
open Capnp_rpc
module Raw = Schema.MakeRPC (Capnp_rpc)

type submission = Raw.Client.Submission.t Capability.t
type ticket = Raw.Client.Ticket.t Capability.t
type job = Raw.Client.Job.t Capability.t

exception Error of string

let fail = function
  | Ok x -> x
  | Error (`Capnp e) -> raise (Error (Fmt.str "%a" Capnp_rpc.Error.pp e))

type scheduler = Raw.Client.Submission.t Capnp_rpc.Sturdy_ref.t

let scheduler ~sw net path =
  let vat = Capnp_rpc_unix.client_only_vat ~sw net in
  match Capnp_rpc_unix.Cap_file.load vat path with
  | Error (`Msg m) -> raise (Error ("loading " ^ path ^ ": " ^ m))
  | Ok sturdy -> sturdy

let submission sturdy =
  match Capnp_rpc.Sturdy_ref.connect sturdy with
  | Ok cap -> cap
  | Error e -> raise (Error (Fmt.str "%a" Capnp_rpc.Exception.pp e))

let connect ~sw net path = submission (scheduler ~sw net path)

let submit t ~pool ?(urgent = false) ~cache_hint ?src spec =
  let open Raw.Client.Submission.Submit in
  let module JD = Raw.Builder.JobDescr in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.pool_set params pool;
  Params.urgent_set params urgent;
  let descr = Params.descr_get params in
  let action = JD.action_init descr in
  Raw.Builder.OBuilder.spec_set (JD.Action.obuilder_init action) spec;
  JD.cache_hint_set descr cache_hint;
  Option.iter
    (fun (repo, commits) ->
      ignore (JD.commits_set_list descr commits : _ Capnp.Array.t);
      JD.repository_set descr repo)
    src;
  Capability.call_for_caps t method_id request Results.ticket_get_pipelined

let job t =
  let open Raw.Client.Ticket.Job in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_caps t method_id request Results.job_get_pipelined

let cancel_ticket t =
  let open Raw.Client.Ticket.Cancel in
  fail
    (Capability.call_for_unit t method_id
       (Capability.Request.create_no_args ()))

let log t start =
  let open Raw.Client.Job.Log in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.start_set params start;
  let results = fail (Capability.call_for_value t method_id request) in
  (Results.log_get results, Results.next_get results)

let stream_log t f =
  let rec loop start =
    match log t start with
    | "", _ -> ()
    | data, next ->
        f data;
        loop next
  in
  loop 0L

let result t =
  let open Raw.Client.Job.Result in
  match
    Capability.call_for_value t method_id (Capability.Request.create_no_args ())
  with
  | Ok results -> Ok (Results.output_get results)
  | Error (`Capnp e) -> Error (Fmt.str "%a" Capnp_rpc.Error.pp e)

let cancel t =
  let open Raw.Client.Job.Cancel in
  fail
    (Capability.call_for_unit t method_id
       (Capability.Request.create_no_args ()))

let release t = Capability.dec_ref t
let release_job t = Capability.dec_ref t
let release_submission t = Capability.dec_ref t
