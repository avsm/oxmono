(* SPDX-License-Identifier: ISC *)
(* Terminal views. Each command keeps its plain output for pipes, which
   scripts and the tests read, and draws these only when standard output is
   a terminal or [--color=always] asks for them. *)

module C = Console
module V = Console.View

let tty () = Xrpc_auth.Cli.rich ()

(* Commands outside {!Common.cmd}, and errors, get the defaults. *)
let setup () = Console_tty.setup ()

(* The clock of the running Eio main loop, which spinners need. *)
let clock : Eio.Time.Mono.ty Eio.Resource.t option ref = ref None
let set_clock c = clock := Some (Console_eio.clock c)

let busy label f =
  match !clock with
  | Some clock
    when Console_tty.is_terminal Unix.stderr && not (Xrpc_auth.Cli.tracing ())
    ->
      (* Rows show only after [delay], so quick calls draw nothing. The
         finished task leaves nothing in the scrollback. *)
      Console_eio.run ~clock ~delay:0.2
        ~commit:(fun _ -> None)
        ~view:(fun root ->
          if C.Task.state root = `Running then C.Progress.tree root
          else V.empty)
        (fun root -> C.Task.run root label (fun _ -> f ()))
  | _ -> f ()

let text = V.text
let accent s = V.role `Accent (V.text s)
let muted s = V.role `Muted (V.text s)
let bold s = V.styled C.Style.bold (V.text s)
let title s = V.styled C.Style.bold (V.role `Accent (V.text s))

(* Prose, wrapped at words to the room it is laid out in. *)
let para s =
  V.fit (fun room ->
      match C.Room.width room with
      | Some w when w > 0 -> V.vcat (V.wrap w (V.text s))
      | _ -> V.text s)

let link url =
  if
    String.starts_with ~prefix:"https://" url
    || String.starts_with ~prefix:"http://" url
  then V.link ~uri:url (V.styled C.Style.underline (V.text url))
  else V.text url

let mark m = C.Theme.mark (C.Theme.get Fmt.stdout) m
let print view = Fmt.pr "%a@." V.pp view
let short_sha sha = if String.length sha > 7 then String.sub sha 0 7 else sha

(* [heading name n] is a title with the count of what follows. *)
let heading name n =
  V.concat ~sep:(V.text "  ")
    [ title name; muted (Printf.sprintf "%d total" n) ]

let column ?align name = C.Table.column ?align (V.text name)

let table ?heading:h columns rows =
  let table = if rows = [] then muted "None." else C.Table.view columns rows in
  print (match h with None -> table | Some h -> V.vcat [ h; table ])

(* Labelled values, one per line, with the labels aligned and each value in
   a column of its own. Absent values are left out. *)
let fields pairs =
  let pairs =
    List.filter_map (fun (k, v) -> Option.map (fun v -> (k, v)) v) pairs
  in
  let width =
    List.fold_left (fun w (k, _) -> max w (String.length k)) 0 pairs
  in
  V.vcat
    (List.map
       (fun (k, v) ->
         V.hcat ~gap:2
           [ muted (k ^ String.make (width - String.length k) ' '); v ])
       pairs)

(* An account by its handle, or by its DID when it has no verified one. *)
let account (did, handle) =
  match handle with Some h -> accent h | None -> accent did

(* A table of accounts, by handle with the DID beside it. *)
let accounts name rows =
  table
    ~heading:(heading name (List.length rows))
    [ column "Account"; column "DID" ]
    (List.map (fun ((did, _) as a) -> [ account a; muted did ]) rows)

let panel ?subtitle heading body =
  print (C.Panel.view ~title:heading ?subtitle body)

(* A service's description, for knots and spindles. *)
let service ~kind ~host ~did ~version ~capabilities ~contact =
  panel
    (V.concat [ muted (kind ^ " "); title host ])
    (fields
       [
         ("DID", Some (accent did));
         ("Version", Some (text version));
         ( "Capabilities",
           Option.map (fun c -> text (String.concat ", " c)) capabilities );
         ("Contact", Option.map text contact);
       ])

(* A state of a ticket or a workflow, in the theme's colour for it. *)
let state s =
  let role =
    match s with
    | "open" | "success" | "succeeded" -> `Succeeded
    | "closed" | "failed" | "error" | "timeout" -> `Failed
    | "completed" | "merged" -> `Accent
    | "pending" | "running" | "blocked" -> `Warning
    | _ -> `Muted
  in
  V.role role (V.text s)

let status_mark s =
  match s with
  | "success" -> V.role `Succeeded (V.text (mark `Succeeded))
  | "failed" | "error" | "timeout" -> V.role `Failed (V.text (mark `Failed))
  | "cancelled" -> V.role `Muted (V.text (mark `Cancelled))
  | _ -> V.role `Warning (V.text (mark `Running))

(* [ok fmt] confirms a change on a terminal. Pipes get nothing, as before. *)
let ok fmt =
  Fmt.kstr
    (fun message ->
      if tty () then
        print
          (V.concat
             [
               V.role `Succeeded (V.text (mark `Succeeded));
               V.text " ";
               V.text message;
             ]))
    fmt

let confirm question =
  if tty () then
    Fmt.pr "%a %a @?" V.pp
      (V.role `Warning (V.text "?"))
      V.pp
      (V.concat [ V.text question; muted " [y/N]" ])
  else Fmt.pr "%s [y/N] @?" question;
  match String.lowercase_ascii (String.trim (read_line ())) with
  | "y" | "yes" -> true
  | _ -> false
