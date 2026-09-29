(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Driver = Agentkit.Driver
module Event = Agentkit.Agent

let fail message = failwith message
let check condition message = if not condition then fail message

module Mock = struct
  type t = { name : string; mutable closed : bool }

  let send t ~on_event prompt =
    if t.closed then fail "send after close";
    on_event (Event.Content (t.name ^ ":" ^ prompt));
    on_event Event.Done

  let stats _ =
    {
      Event.ctx_used = 0;
      ctx_size = 0;
      prompt_tokens = 0;
      generated = 0;
      generate_seconds = 0.;
      prefill_seconds = 0.;
      tool_calls = 0;
      turns = 0;
      drafted = 0;
      total_generated = 0;
      total_generate_seconds = 0.;
    }

  let cancel _ = ()
  let close t = t.closed <- true
end

let () =
  let created = ref [] in
  let make name =
    Driver.v ~name
      ~models:(fun () -> [ { Driver.name = "default"; description = name } ])
      ~create:(fun model ->
        created := (name, model) :: !created;
        Driver.session (module Mock) { Mock.name = model; closed = false })
  in
  let registry = Driver.merge [ make "ds4"; make "apple" ] in
  check
    (List.map (fun (m : Driver.model) -> m.name) (Driver.models registry)
    = [ "ds4/default"; "apple/default" ])
    "models must retain driver prefixes";
  let selected =
    match Driver.select registry "ds4/path/to/model.gguf" with
    | Ok selected -> selected
    | Error message -> fail message
  in
  check (!created = []) "selection must not start the model";
  check
    (Driver.driver_name selected = "ds4"
    && Driver.model_name selected = "path/to/model.gguf")
    "selection must retain the driver and full model name";
  let session = Driver.start selected in
  check
    (!created = [ ("ds4", "path/to/model.gguf") ])
    "the selected driver must receive the whole model name";
  let events = ref [] in
  Driver.send session ~on_event:(fun event -> events := event :: !events) "hi";
  check
    (List.rev !events = [ Event.Content "path/to/model.gguf:hi"; Event.Done ])
    "the selected agent must emit common events";
  Driver.close session;
  check
    (match Driver.send session ~on_event:ignore "again" with
    | exception Failure message -> message = "send after close"
    | _ -> false)
    "close must reach the selected backend";
  let rejected choice =
    match Driver.create registry choice with Error _ -> true | Ok _ -> false
  in
  check (rejected "default") "an unqualified model must be rejected";
  check (rejected "ds4/") "an empty model must be rejected";
  check (rejected "other/default") "an unknown driver must be rejected";
  check
    (!created = [ ("ds4", "path/to/model.gguf") ])
    "a rejected choice must not construct an agent";
  check
    (match Driver.merge [ make "apple"; make "apple" ] with
    | exception Invalid_argument _ -> true
    | _ -> false)
    "duplicate drivers must be rejected";
  let fetched = ref [] in
  let managed =
    Driver.merge
      [
        Driver.manage
          ~availability:(fun _ -> Driver.Needs_download)
          ~canonical:(fun name -> if name = "short" then "default" else name)
          ~fetch:(fun model ~token ->
            fetched := (model, token) :: !fetched;
            Ok ())
          (make "downloadable");
      ]
  in
  check
    (match Driver.catalog managed with
    | [ { Driver.availability = Driver.Needs_download; _ } ] -> true
    | _ -> false)
    "catalog reports a driver's availability";
  check
    (match Driver.lookup managed "downloadable/short" with
    | Ok { Driver.model = { name = "downloadable/default"; _ }; _ } -> true
    | _ -> false)
    "lookup resolves a driver's model aliases";
  (match Driver.create managed "downloadable/default" with
  | Ok session -> Driver.close session
  | Error message -> fail message);
  check (!fetched = []) "listing and model selection must not fetch weights";
  check
    (Driver.fetch managed "downloadable/default" ~token:(Some "test") = Ok ())
    "fetch dispatches only on explicit request";
  check
    (!fetched = [ ("default", Some "test") ])
    "fetch forwards the model and token";
  check
    (match Driver.fetch registry "apple/default" ~token:None with
    | Error _ -> true
    | Ok () -> false)
    "a driver without fetch is refused";
  print_endline "Agentkit driver test passed."
