(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let fail message = failwith message
let check condition message = if not condition then fail message

let () =
  let invoked = ref false in
  let codec =
    let open Dsml.Codec in
    Invoke.map "read" (fun cap path -> (cap, path))
    |> Invoke.param ~enc:fst "cap" string
    |> Invoke.param ~enc:snd "path" string
    |> Invoke.seal
  in
  let native =
    Ds4.Tool.v ~description:"Read a path." codec (fun (cap, path) ->
        invoked := true;
        cap ^ ":" ^ path)
  in
  let apple = Agentkit_apple_tools.of_ds4 native in
  let events = ref [] in
  let output =
    Agentkit_apple_fm.Tool.invoke
      ~on_event:(fun event -> events := event :: !events)
      apple {|{"cap":"<src>","path":"lib/a.ml"}|}
  in
  check (output = "<src>:lib/a.ml") "Apple codec invokes the native handler";
  check !invoked "the native handler ran";
  check
    (match List.rev !events with
    | [
     Agentkit.Agent.Tool_call { name = "read"; arguments };
     Agentkit.Agent.Tool_result ("read", "<src>:lib/a.ml");
    ] ->
        arguments = {|{"cap":"<src>","path":"lib/a.ml"}|}
    | _ -> false)
    "Apple events contain canonical arguments and the complete result";
  invoked := false;
  let output = Agentkit_apple_fm.Tool.invoke apple {|{"cap":"<src>"}|} in
  check
    (String.starts_with ~prefix:"Error:" output)
    "missing required arguments are rejected";
  check (not !invoked) "a rejected call cannot run the native handler";
  check
    (match
       let wrong =
         Dsml.Codec.Invoke.(map "unknown" () |> seal) |> fun codec ->
         Ds4.Tool.v ~description:"unknown" codec (fun () -> "")
       in
       Agentkit_apple_tools.of_ds4 wrong
     with
    | exception Invalid_argument _ -> true
    | _ -> false)
    "unsupported tool names are rejected"
