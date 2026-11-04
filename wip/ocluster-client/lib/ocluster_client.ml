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

let submit_action t ~pool ~urgent ~cache_hint ?src set_action =
  let open Raw.Client.Submission.Submit in
  let module JD = Raw.Builder.JobDescr in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.pool_set params pool;
  Params.urgent_set params urgent;
  let descr = Params.descr_get params in
  set_action (JD.action_init descr);
  JD.cache_hint_set descr cache_hint;
  Option.iter
    (fun (repo, commits) ->
      ignore (JD.commits_set_list descr commits : _ Capnp.Array.t);
      JD.repository_set descr repo)
    src;
  Capability.call_for_caps t method_id request Results.ticket_get_pipelined

let submit t ~pool ?(urgent = false) ~cache_hint ?src spec =
  submit_action t ~pool ~urgent ~cache_hint ?src (fun action ->
      Raw.Builder.OBuilder.spec_set
        (Raw.Builder.JobDescr.Action.obuilder_init action)
        spec)

type day10 = {
  opam_repository : string;
  opam_repository_commit : string;
  ocaml_version : string;
  with_test : bool;
  dune_args : string list;
  only_packages : string list;
  arch : string;
  os : string;
  os_family : string;
  os_distribution : string;
  os_version : string;
}

(* A day10 job is a custom job, whose payload the fork's schema defines. *)
let submit_day10 t ~pool ?(urgent = false) ~cache_hint ~src (d : day10) =
  submit_action t ~pool ~urgent ~cache_hint ~src (fun action ->
      let module C = Raw.Builder.Custom in
      let module D = Raw.Builder.Day10 in
      let custom = Raw.Builder.JobDescr.Action.custom_init action in
      C.kind_set custom "day10";
      let p = D.init_pointer (C.payload_get custom) in
      D.verb_set p "build";
      D.opam_repository_set p d.opam_repository;
      D.opam_repository_commit_set p d.opam_repository_commit;
      D.ocaml_version_set p d.ocaml_version;
      D.with_test_set p d.with_test;
      ignore (D.dune_args_set_list p d.dune_args : _ Capnp.Array.t);
      ignore (D.only_packages_set_list p d.only_packages : _ Capnp.Array.t);
      D.arch_set p d.arch;
      D.os_set p d.os;
      D.os_family_set p d.os_family;
      D.os_distribution_set p d.os_distribution;
      D.os_version_set p d.os_version)

let job t =
  let open Raw.Client.Ticket.Job in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_caps t method_id request Results.job_get_pipelined

(* The scheduler answers [Ticket.job] with the job a worker made when it
   took the ticket from the pool's queue, so the promise settles then. *)
let started job =
  match Capability.await_settled job with
  | Ok () -> Ok ()
  | Error e -> Error (Fmt.str "%a" Capnp_rpc.Exception.pp e)

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
