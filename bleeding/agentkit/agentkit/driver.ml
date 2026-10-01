(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type model = { name : string; description : string }
type availability = Ready | Needs_download | Unavailable | Managed
type listing = { model : model; availability : availability }

type session =
  | Session : {
      backend : (module Agent.S with type t = 'a);
      agent : 'a;
      prefill_progress : unit -> int * int;
    }
      -> session

let session ?(prefill_progress = fun () -> (0, 0)) backend agent =
  Session { backend; agent; prefill_progress }

let send (Session { backend = (module Backend); agent; _ }) ~on_event prompt =
  Backend.send agent ~on_event prompt

let stats (Session { backend = (module Backend); agent; _ }) =
  Backend.stats agent

let cancel (Session { backend = (module Backend); agent; _ }) =
  Backend.cancel agent

let close (Session { backend = (module Backend); agent; _ }) =
  Backend.close agent

let prefill_progress (Session session) = session.prefill_progress ()

type 'a t = {
  name : string;
  models : unit -> model list;
  availability : string -> availability;
  fetch : (string -> token:string option -> (unit, string) result) option;
  canonical : string -> string;
  create : string -> 'a;
}

let valid_name name =
  name <> ""
  && String.for_all
       (function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
       name

let v ~name ~models ~create =
  if not (valid_name name) then
    invalid_arg
      "Driver.v: name must contain only lowercase letters, digits, and hyphens";
  {
    name;
    models;
    availability = (fun _ -> Ready);
    fetch = None;
    canonical = Fun.id;
    create;
  }

let manage ?(availability = fun _ -> Ready) ?fetch ?(canonical = Fun.id) driver
    =
  { driver with availability; fetch; canonical }

let name t = t.name

type 'a registry = 'a t list

let merge drivers =
  let seen = Hashtbl.create (List.length drivers) in
  List.iter
    (fun driver ->
      if Hashtbl.mem seen driver.name then
        invalid_arg ("Driver.merge: duplicate driver " ^ driver.name);
      Hashtbl.add seen driver.name ())
    drivers;
  drivers

let models drivers =
  List.concat_map
    (fun driver ->
      List.map
        (fun (model : model) ->
          { model with name = driver.name ^ "/" ^ model.name })
        (driver.models ()))
    drivers

let catalog drivers =
  List.concat_map
    (fun driver ->
      List.map
        (fun (model : model) ->
          {
            model = { model with name = driver.name ^ "/" ^ model.name };
            availability = driver.availability model.name;
          })
        (driver.models ()))
    drivers

type 'a selection = { driver : 'a t; model : string }

let driver_name selection = selection.driver.name
let model_name selection = selection.model
let start selection = selection.driver.create selection.model

let select drivers choice =
  match String.index_opt choice '/' with
  | None ->
      Error "model must be DRIVER/MODEL, for example ds4/q4 or apple/default"
  | Some slash -> (
      let driver_name = String.sub choice 0 slash in
      let model =
        String.sub choice (slash + 1) (String.length choice - slash - 1)
      in
      if model = "" then
        Error "model must be DRIVER/MODEL, with a name after the slash"
      else
        match
          List.find_opt (fun driver -> driver.name = driver_name) drivers
        with
        | Some driver -> Ok { driver; model }
        | None ->
            let available =
              match drivers with
              | [] -> "no drivers are registered"
              | _ ->
                  "available drivers: "
                  ^ String.concat ", "
                      (List.map (fun driver -> driver.name) drivers)
            in
            Error ("unknown driver " ^ driver_name ^ ". " ^ available))

let create drivers choice = Result.map start (select drivers choice)

let lookup drivers choice =
  match select drivers choice with
  | Error _ as error -> error
  | Ok { driver; model } -> (
      let canonical = driver.name ^ "/" ^ driver.canonical model in
      match
        List.find_opt
          (fun (entry : listing) -> entry.model.name = canonical)
          (catalog drivers)
      with
      | Some entry -> Ok entry
      | None -> Error (choice ^ " is not listed. Run models list"))

let fetch drivers choice ~token =
  match select drivers choice with
  | Error _ as error -> error
  | Ok { driver = { fetch = None; _ }; _ } ->
      Error (choice ^ " is managed outside Agentkit and cannot be fetched")
  | Ok { driver = { fetch = Some fetch; _ }; model } -> fetch model ~token

let model_arg =
  let open Cmdliner in
  Arg.(
    value
    & opt (some string) None
    & info [ "model" ] ~docv:"DRIVER/MODEL"
        ~doc:
          "Select a model from a registered driver, such as ds4/q4 or \
           apple/default.")

let model_term ~default ?short_driver () =
  let open Cmdliner in
  let parse value =
    let value =
      match short_driver with
      | Some driver when String.index_opt value '/' = None ->
          driver ^ "/" ^ value
      | Some driver when String.starts_with ~prefix:"/" value ->
          driver ^ "/" ^ value
      | Some driver when String.starts_with ~prefix:"./" value ->
          driver ^ "/" ^ value
      | _ -> value
    in
    match String.index_opt value '/' with
    | Some slash when slash > 0 && slash < String.length value - 1 -> Ok value
    | _ -> Error (`Msg "expected DRIVER/MODEL, such as ds4/q4 or apple/default")
  in
  (match parse default with
  | Ok _ -> ()
  | Error _ -> invalid_arg "Driver.model_term: default must be DRIVER/MODEL");
  let model = Arg.conv ~docv:"DRIVER/MODEL" (parse, Format.pp_print_string) in
  Arg.(
    value & opt model default
    & info [ "model" ] ~docv:"DRIVER/MODEL"
        ~doc:
          (Printf.sprintf
             "Select a model from a linked driver, such as ds4/q4 or \
              apple/default. The default is $(b,%s). Run $(b,models list) to \
              see every choice. Selection never downloads weights."
             default))

module Cli = struct
  type color = Auto | Always | Never

  let color =
    let open Cmdliner in
    Arg.(
      value
      & opt (enum [ ("auto", Auto); ("always", Always); ("never", Never) ]) Auto
      & info [ "color" ] ~docv:"WHEN"
          ~doc:
            "Colorize model output. WHEN is auto, always, or never. Auto \
             colors a terminal unless NO_COLOR is set or TERM is dumb.")

  let colors = function
    | Always -> true
    | Never -> false
    | Auto ->
        Unix.isatty Unix.stdout
        && Sys.getenv_opt "NO_COLOR" = None
        && Sys.getenv_opt "TERM" <> Some "dumb"

  let paint enabled code value =
    if enabled then Printf.sprintf "\027[%sm%s\027[0m" code value else value

  let availability = function
    | Ready -> "ready"
    | Needs_download -> "download"
    | Unavailable -> "unavailable"
    | Managed -> "system"

  let status_code = function
    | Ready -> "32"
    | Needs_download -> "33"
    | Unavailable -> "31"
    | Managed -> "36"

  let status color state =
    let label = Printf.sprintf "%-11s" (availability state) in
    paint (colors color) (status_code state) label

  let choice =
    let open Cmdliner in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"DRIVER/MODEL")

  let token =
    let open Cmdliner in
    Arg.(
      value
      & opt (some string) None
      & info [ "token" ] ~docv:"TOKEN"
          ~doc:"Hugging Face token for a model that requires one.")

  let models_cmd registry =
    let open Cmdliner in
    let list color =
      Printf.printf "%s\n"
        (paint (colors color) "2"
           (Printf.sprintf "%-11s %-42s %s" "STATUS" "MODEL" "DESCRIPTION"));
      catalog registry
      |> List.iter (fun { model; availability = state } ->
          Printf.printf "%s %s %s\n" (status color state)
            (paint (colors color) "1" (Printf.sprintf "%-42s" model.name))
            model.description);
      Ok ()
    in
    let show choice color =
      match lookup registry choice with
      | Error _ as error -> error
      | Ok { model; availability = state } ->
          Printf.printf "%s\n%s %s\n%s %s\n"
            (paint (colors color) "1" model.name)
            (paint (colors color) "2" "Status:")
            (paint (colors color) (status_code state) (availability state))
            (paint (colors color) "2" "About: ")
            model.description;
          Ok ()
    in
    let fetch_choice choice token color =
      match fetch registry choice ~token with
      | Error _ as error -> error
      | Ok () ->
          Printf.printf "%s %s\n" (status color Ready)
            (paint (colors color) "1" choice);
          Ok ()
    in
    let list_cmd =
      Cmd.v
        (Cmd.info "list" ~doc:"List models and their availability.")
        Term.(const list $ color)
    in
    let show_cmd =
      Cmd.v
        (Cmd.info "show" ~doc:"Show one listed model.")
        Term.(const show $ choice $ color)
    in
    let fetch_cmd =
      Cmd.v
        (Cmd.info "fetch" ~doc:"Explicitly download a model.")
        Term.(const fetch_choice $ choice $ token $ color)
    in
    Cmd.group
      (Cmd.info "models" ~doc:"List and fetch models shared by every agent.")
      [ list_cmd; show_cmd; fetch_cmd ]
end
