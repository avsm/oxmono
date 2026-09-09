(* SPDX-License-Identifier: ISC *)

let () =
  Eio_main.run @@ fun system ->
  let job : Spindle.Job.t =
    Spindle.Job.v "inspect" [ Metadata; Command [ "sleep"; "30" ]; Metadata ]
  in
  Spindle.run ~addr:"0.0.0.0" system
    {
      hostname = "spindle.tangled.test";
      owner = Sys.getenv "SPINDLE_OWNER";
      repo = Some ("did:web:repo.tangled.test", "/fixture");
      plc = "http://oxmono-atp-plc-1:8080";
      state_dir = "/state";
      port = 9000;
      jobs = [ job ];
      jetstream = None;
      allow_http = true;
    }
