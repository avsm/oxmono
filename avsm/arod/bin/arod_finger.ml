(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Finger protocol handler for Arod.

    Serves the latest weeknote as plain text via RFC 1288. *)

let src = Logs.Src.create "arod.finger" ~doc:"Arod finger protocol handler"
module Log = (val Logs.src_log src : Logs.LOG)

let format_date (y, m, d) =
  let month_name = [| ""; "January"; "February"; "March"; "April"; "May";
    "June"; "July"; "August"; "September"; "October"; "November";
    "December" |] in
  Printf.sprintf "%d %s %d" d month_name.(m) y

let latest_weeknote ctx =
  let weeknotes = List.filter Bushel.Note.weeknote (Arod.Ctx.notes ctx) in
  let sorted = List.sort (fun a b ->
    Bushel.Entry.compare (`Note b) (`Note a)
  ) weeknotes in
  match sorted with
  | n :: _ -> Some n
  | [] -> None

let banner = {|              __--_--_-_
               ( I wish I  )
              ( were a real )
              (    llama   )
               ( in Peru! )
              o (__--_--_)
           , o
          ~)
           (_---;
ejm 97      /|~|\
           / / / |
|}

let format_note ~ctx note =
  let cfg = Arod.Ctx.config ctx in
  let title = Bushel.Note.title note in
  let date_str = format_date (Bushel.Entry.date (`Note note)) in
  let contacts = Arod.Ctx.contacts ctx in
  let contact_name handle =
    List.find_map (fun c ->
      if Sortal_schema.Contact.handle c = handle
      then Some (Sortal_schema.Contact.name c)
      else None
    ) contacts
  in
  let plain_body = Bushel.Md.plain_text_of_markdown ~contact_name (Bushel.Note.body note) in
  let slug = Bushel.Note.slug note in
  let base_url = cfg.site.base_url in
  let hostname =
    Uri.host (Uri.of_string base_url) |> Option.value ~default:"localhost"
  in
  let buf = Buffer.create 2048 in
  let line s = Buffer.add_string buf s; Buffer.add_string buf "\r\n" in
  (* ASCII art banner *)
  String.iter (fun c ->
    if c = '\n' then Buffer.add_string buf "\r\n"
    else Buffer.add_char buf c
  ) banner;
  line "";
  line (Printf.sprintf "=== %s ===" cfg.site.name);
  line (Printf.sprintf "Date: %s" date_str);
  line (Printf.sprintf "Title: %s" title);
  (match Bushel.Note.synopsis note with
   | Some syn -> line (Printf.sprintf "Synopsis: %s" syn)
   | None -> ());
  line "";
  (* Convert LF to CRLF per RFC 1288 *)
  String.iter (fun c ->
    if c = '\n' then Buffer.add_string buf "\r\n"
    else Buffer.add_char buf c
  ) plain_body;
  line ""; line "";
  line "---";
  line (Printf.sprintf "Visit: %s/notes/%s" base_url slug);
  line (Printf.sprintf "Finger: finger @%s" hostname);
  Buffer.contents buf

let handler ~ctx (query : Finger.Query.t) =
  Log.info (fun m -> m "Finger query: %a" Finger.Query.pp query);
  match latest_weeknote ctx with
  | Some note -> format_note ~ctx note
  | None ->
    let cfg = Arod.Ctx.config ctx in
    Printf.sprintf "=== %s ===\r\nNo weeknotes available.\r\n" cfg.site.name
