(* SPDX-License-Identifier: ISC *)

let () =
  Eio_main.run @@ fun system ->
  let job : Spindle.Job.t =
    { name = "inspect";
      steps = [Metadata; Command ["sleep"; "30"]; Metadata] } in
  Spindle.run ~addr:"0.0.0.0" system
    { hostname = "spindle.tangled.test";
      owner = Sys.getenv "SPINDLE_OWNER";
      repo = "did:web:repo.tangled.test";
      source = "/fixture";
      plc = "http://oxmono-atp-plc-1:8080";
      state_dir = "/state";
      port = 9000;
      job }
