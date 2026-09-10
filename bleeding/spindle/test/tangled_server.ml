(* SPDX-License-Identifier: ISC *)
let () =
  Eio_main.run @@ fun system ->
  let open Spindle.Job in
  let tracked =
    v "tracked-files"
      [ Metadata; Command [ "git"; "ls-files" ] ]
      ~accepts:(fun context -> context.kind <> Manual)
  in
  Spindle.run ~addr:"0.0.0.0"
    ~operations:(Spindle.Operations.v ~maintenance_seconds:1 ())
    system
    {
      hostname = "spindle.tangled.test";
      owner = Sys.getenv "SPINDLE_OWNER";
      repo = Some ("did:web:repo.tangled.test", "/fixture");
      plc = "https://plc.tangled.test";
      state_dir = "/state";
      port = 9000;
      jobs = [ inspect; tracked ];
      jetstream = Some "wss://jetstream.tangled.test/subscribe";
      allow_http = false;
    }
