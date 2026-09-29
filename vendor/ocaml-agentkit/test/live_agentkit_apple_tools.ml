(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let run _env =
  if Apple_fm.Availability.get () <> `Available then
    failwith "Apple Foundation Models is not available";
  Eio.Switch.run @@ fun sw ->
  let called = ref false in
  let arguments =
    Dsml.Codec.Invoke.(
      map "read" (fun cap path -> (cap, path))
      |> param ~enc:fst "cap" Dsml.Codec.string
      |> param ~enc:snd "path" Dsml.Codec.string
      |> seal)
  in
  let native =
    Ds4.Tool.v ~description:"Read a file from the workspace." arguments
      (fun (_cap, path) ->
        called := true;
        if path = "hello.txt" then "hello from the tool" else "wrong path")
  in
  let session, context_size =
    Agentkit_apple_tools.create ~sw ~model:"default"
      ~system:
        "Call the read tool with cap empty and path hello.txt, then report its \
         result."
      [ native ]
  in
  if context_size <= 0 then failwith "missing Apple context size";
  let events = ref [] in
  Agentkit.Driver.send session
    ~on_event:(fun event -> events := event :: !events)
    "Read hello.txt and say what it contains.";
  Agentkit.Driver.close session;
  if not !called then failwith "Apple did not call the specialized read tool";
  if
    not
      (List.exists
         (function Agentkit.Agent.Tool_result _ -> true | _ -> false)
         !events)
  then failwith "Apple tool result was not recorded"

let () =
  match Sys.getenv_opt "APPLE_FM_LIVE" with
  | None -> print_endline "SKIP - live Apple tool bridge test"
  | Some _ -> Eio_main.run run
