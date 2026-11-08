(* SPDX-License-Identifier: ISC *)

module Executor = Spindle.Executor
module Job = Spindle.Job

type route = {
  name : string;
  accepts : Brief.t -> bool;
  jobs : Job.t list;
  executor : Executor.t;
}

type scan = Executor.context -> Executor.request -> string

let detect = "detect"
let command = "brief -json ."
let jobs = [ Job.v detect [ Job.step detect command ] ]
let route_var = "SPINDLE_ROUTE"

let logf (ctx : Executor.context) fmt =
  Printf.ksprintf (fun s -> ctx.log `Stdout s) fmt

let run_detect ~scan routes (ctx : Executor.context) (r : Executor.request) =
  let found = ref None in
  ctx.step ~command detect (fun () ->
      let json = scan ctx r in
      let report =
        match Brief.of_string json with
        | Ok report -> report
        | Error e -> failwith ("brief's report does not read: " ^ e)
      in
      ctx.log `Stdout (Brief.summary report);
      let accepted = List.filter (fun route -> route.accepts report) routes in
      (match accepted with
      | [] ->
          logf ctx
            "No spindle here tests this kind of project, so there is nothing \
             to do.\n"
      | _ ->
          List.iter
            (fun route ->
              List.iter
                (fun (job : Job.t) ->
                  logf ctx "Starting %s for %s.\n" job.name route.name;
                  ctx.spawn
                    ~env:((route_var, route.name) :: job.env)
                    job.name job.steps)
                route.jobs)
            accepted);
      found := Some json);
  Option.iter
    (fun json ->
      ctx.step ~kind:`System ~command "report" (fun () -> ctx.log `Stdout json))
    !found

let forward route (ctx : Executor.context) r =
  let spawn ?(env = []) name steps =
    ctx.spawn ~env:((route_var, route.name) :: env) name steps
  in
  route.executor { ctx with spawn } r

let executor ~scan routes : Executor.t =
 fun ctx (r : Executor.request) ->
  if r.definition <> None then
    failwith "this spindle runs only its own workflows, not the repository's"
  else
    match List.assoc_opt route_var r.env with
    | None when r.workflow = detect -> run_detect ~scan routes ctx r
    | None -> failwith ("no route for the workflow " ^ r.workflow)
    | Some name -> (
        match List.find_opt (fun route -> route.name = name) routes with
        | Some route -> forward route ctx r
        | None -> failwith ("this spindle has no route called " ^ name))
