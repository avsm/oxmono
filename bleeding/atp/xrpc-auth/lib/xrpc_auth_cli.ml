(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module V = Console.View
module Trace = Xrpc.Trace

let is_rich = ref false
let is_tracing = ref false
let rich () = !is_rich
let tracing () = !is_tracing
let mark m = Console.Theme.mark (Console.Theme.get Fmt.stderr) m
let pad n s = s ^ String.make (max 0 (n - String.length s)) ' '

let duration s =
  if s < 1. then Printf.sprintf "%4.0fms" (s *. 1000.)
  else Printf.sprintf "%5.1fs" s

(* One exchange: its status and time first, so that they line up, then the
   host and the XRPC method or the path. *)
let exchange tags =
  let get d = Logs.Tag.find d tags in
  let status =
    match get Trace.status with
    | Some s ->
        let role =
          if s < 300 then `Succeeded else if s < 400 then `Muted else `Failed
        in
        V.role role (V.text (string_of_int s))
    | None -> V.role `Failed (V.text "---")
  in
  let what =
    match get Trace.nsid with
    | Some nsid -> V.role `Accent (V.text nsid)
    | None -> V.text (Option.value ~default:"" (get Trace.target))
  in
  V.concat ~sep:(V.text " ")
    [
      V.role `Muted (V.text "→");
      V.styled Console.Style.bold
        (V.text (pad 4 (Option.value ~default:"" (get Trace.meth))));
      status;
      V.role `Muted
        (V.text (Option.fold ~none:"" ~some:duration (get Trace.elapsed)));
      V.role `Muted (V.text (Option.value ~default:"" (get Trace.host)));
      what;
    ]

let message src level text =
  let prefix =
    match level with
    | Logs.Error -> V.role `Failed (V.text (mark `Error ^ " "))
    | Logs.Warning -> V.role `Warning (V.text (mark `Warning ^ " "))
    | Logs.Debug -> V.role `Muted (V.text (mark `Debug ^ " "))
    | Logs.App | Logs.Info -> V.empty
  in
  let source =
    if Logs.Src.equal src Logs.default then V.empty
    else V.role `Muted (V.text ("[" ^ Logs.Src.name src ^ "] "))
  in
  let body =
    if level = Logs.Debug then V.role `Muted (V.text text) else V.text text
  in
  V.concat [ prefix; source; body ]

let reporter () =
  let report src level ~over k msgf =
    msgf @@ fun ?header:_ ?tags fmt ->
    Format.kasprintf
      (fun text ->
        let view =
          match (level, tags) with
          | Logs.Info, Some tags when Logs.Src.equal src Trace.src ->
              exchange tags
          | Logs.Debug, _ when Logs.Src.equal src Trace.src ->
              (* Request and response fields, under their exchange. *)
              V.role `Muted
                (V.vcat
                   (List.map
                      (fun l -> V.text ("      " ^ l))
                      (String.split_on_char '\n' text)))
          | _ -> message src level text
        in
        Fmt.epr "%a@." V.pp view;
        over ();
        k ())
      fmt
  in
  { Logs.report }

let setup style_renderer theme verbosity quiet =
  Console_tty.setup ?style_renderer ?theme ();
  is_rich :=
    Console_tty.is_terminal Unix.stdout || style_renderer = Some `Ansi_tty;
  (* Widgets are no wider than the formatter's margin, 80 by default. Through
     a pipe, take the width of the terminal the user is looking at. *)
  let size =
    match Console_eio.size Fmt.stdout with
    | Some size -> Some size
    | None when !is_rich -> Console_eio.size Fmt.stderr
    | None -> None
  in
  Option.iter
    (fun size -> Format.pp_set_margin Fmt.stdout (max 40 (fst (size ()))))
    size;
  let level =
    if quiet then None
    else
      match List.length verbosity with
      | 0 -> Some Logs.Warning
      | 1 -> Some Logs.Info
      | _ -> Some Logs.Debug
  in
  is_tracing :=
    (match level with Some (Logs.Info | Logs.Debug) -> true | _ -> false);
  Logs.set_level level;
  Logs.set_reporter (reporter ())

let term ?env () =
  let open Cmdliner in
  let docs = Manpage.s_common_options in
  let env_info name =
    Option.map (fun prefix -> Cmd.Env.info (prefix ^ "_" ^ name)) env
  in
  let color = Fmt_cli.style_renderer ~docs ?env:(env_info "COLOR") () in
  let theme =
    Arg.(
      value
      & opt (some (enum Console.Theme.all)) None
      & info [ "theme" ] ~docs ?env:(env_info "THEME") ~docv:"THEME"
          ~doc:
            (Printf.sprintf "Colours and borders of the output: %s."
               (String.concat ", " (List.map fst Console.Theme.all))))
  in
  let verbosity =
    Arg.(
      value & flag_all
      & info [ "v"; "verbose" ] ~docs
          ~doc:
            "Trace each HTTP exchange on standard error. Repeat to show \
             request and response fields, with secrets redacted.")
  in
  let quiet =
    Arg.(
      value & flag
      & info [ "quiet" ] ~docs ~doc:"Show no warnings on standard error.")
  in
  Term.(const setup $ color $ theme $ verbosity $ quiet)
