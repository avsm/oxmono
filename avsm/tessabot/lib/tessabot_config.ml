(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type buffer = {
  api_key : string;
}

type t = {
  contacts : string list;
  buffer : buffer;
}

let buffer_codec =
  Tomlt.(Table.(
    obj (fun api_key -> { api_key })
    |> mem "api_key" string ~enc:(fun b -> b.api_key)
    |> finish
  ))

let config_codec =
  Tomlt.(Table.(
    obj (fun contacts buffer -> { contacts; buffer })
    |> mem "contacts" Tomlt.(list string) ~dec_absent:[] ~enc:(fun c -> c.contacts)
    |> mem "buffer" buffer_codec ~enc:(fun c -> c.buffer)
    |> finish
  ))

let contacts cfg all_contacts =
  match cfg.contacts with
  | [] -> List.filter (fun c ->
    match Sortal.Contact.feeds c with
    | Some (_ :: _) -> true
    | _ -> false
  ) all_contacts
  | handles ->
    List.filter (fun c ->
      let h = Sortal.Contact.handle c in
      List.mem h handles
    ) all_contacts

let config_file () =
  let xdg_config = Sys.getenv_opt "XDG_CONFIG_HOME" in
  let home = Sys.getenv_opt "HOME" in
  match xdg_config, home with
  | Some xdg, _ -> Filename.concat xdg "tessabot/config.toml"
  | None, Some h -> Filename.concat h ".config/tessabot/config.toml"
  | None, None -> "./config.toml"

let of_toml_string s =
  match Tomlt_bytesrw.decode_string config_codec s with
  | Ok cfg -> Ok cfg
  | Error e -> Error (Tomlt.Error.to_string e)

let load () =
  let path = config_file () in
  if Sys.file_exists path then begin
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    of_toml_string content
  end else
    Error (Fmt.str "Config file not found: %s" path)

let load_or_fail () =
  match load () with
  | Ok cfg -> cfg
  | Error msg -> failwith msg

let sample_config = {|# Tessabot Configuration

# Sortal contact handles to monitor (empty = all contacts with feeds)
contacts = []

[buffer]
# Get your API key from https://publish.buffer.com/settings/api
api_key = "your-buffer-api-key"
|}

let pp ppf t =
  Fmt.pf ppf "@[<v>Contacts: %a@,Buffer:@,  api_key: %s@]"
    Fmt.(list ~sep:(Fmt.any ", ") string)
    (match t.contacts with [] -> ["(all)"] | cs -> cs)
    (if String.length t.buffer.api_key > 8 then
       String.sub t.buffer.api_key 0 8 ^ "..."
     else "(not set)")
