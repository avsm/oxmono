(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Simple example showing structured output with explicit JSON Schema *)

module C = Claude

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info)

let simple_example env =
  Printf.printf "\n=== Simple Structured Output Example ===\n\n";

  (* Define a simple schema for a person's info *)
  let person_schema =
    let open Jsont in
    Object
      ( [
          (("type", Meta.none), String ("object", Meta.none));
          ( ("properties", Meta.none),
            Object
              ( [
                  ( ("name", Meta.none),
                    Object
                      ( [ (("type", Meta.none), String ("string", Meta.none)) ],
                        Meta.none ) );
                  ( ("age", Meta.none),
                    Object
                      ( [ (("type", Meta.none), String ("integer", Meta.none)) ],
                        Meta.none ) );
                  ( ("occupation", Meta.none),
                    Object
                      ( [ (("type", Meta.none), String ("string", Meta.none)) ],
                        Meta.none ) );
                ],
                Meta.none ) );
          ( ("required", Meta.none),
            Array
              ( [
                  String ("name", Meta.none);
                  String ("age", Meta.none);
                  String ("occupation", Meta.none);
                ],
                Meta.none ) );
        ],
        Meta.none )
  in

  let output_format =
    Claude.Proto.Structured_output.of_json_schema person_schema
  in

  let options =
    C.Options.default
    |> C.Options.with_output_format output_format
    |> C.Options.with_max_turns 1
  in

  Printf.printf "Asking Claude to provide structured data...\n\n";

  Eio.Switch.run @@ fun sw ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let client = C.Client.create ~sw ~process_mgr ~clock ~options () in

  C.Client.query client
    "Tell me about a famous computer scientist. Provide their name, age, and \
     occupation in the exact JSON structure I specified.";

  let responses = C.Client.receive_all client in
  List.iter
    (function
      | C.Response.Complete result -> (
          Printf.printf "Response received!\n";
          match C.Response.Complete.structured_output result with
          | Some json ->
              Printf.printf "\nStructured Output:\n%s\n"
                (Test_json_utils.to_string ~minify:false json)
          | None -> Printf.printf "No structured output\n")
      | C.Response.Error err ->
          Printf.printf "Error: %s\n" (C.Response.Error.message err)
      | _ -> ())
    responses

let () =
  Eio_main.run @@ fun env ->
  try simple_example env
  with exn ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string exn);
    exit 1
