(* SPDX-License-Identifier: ISC *)

let () =
  Eio_main.run @@ fun system ->
  let job : Spindle.Job.t =
    Spindle.Job.v "inspect" [ Spindle.Job.step "sleep 30" "sleep 30" ]
  in
  (* Each step sleeps for 30 seconds, so the harness can cancel it. *)
  let executor (ctx : Spindle.Executor.context) (r : Spindle.Executor.request) =
    List.iter
      (fun (s : Spindle.Job.step) ->
        ctx.step ~command:s.command s.name (fun () ->
            Eio.Time.sleep system#clock 30.))
      r.steps
  in
  Spindle.run ~addr:"0.0.0.0" system
    {
      hostname = "spindle.tangled.test";
      owner = Sys.getenv "SPINDLE_OWNER";
      admins = [];
      admission = Spindle.Catalog.Closed;
      repo = Some ("did:web:repo.tangled.test", "/fixture");
      plc = "http://oxmono-atp-plc-1:8080";
      state_dir = "/state";
      port = 9000;
      jobs = [ job ];
      jetstream = None;
      allow_http = true;
      executor;
      workflow_timeout = 60.;
      quotas = { repo = []; user = [] };
      private_webhooks = true;
      cors_origins = [];
    }
