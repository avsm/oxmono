(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { handle : string; pds : string }

let tomlt =
  Tomlt.(
    Table.(
      obj (fun handle pds -> { handle; pds })
      |> mem "handle" string ~enc:(fun c -> c.handle)
      |> mem "pds" string
           ~enc:(fun c -> c.pds)
           ~dec_absent:"https://bsky.social"
      |> finish))

let pp ppf config =
  Fmt.pf ppf "@[<v>Handle: %s@,PDS: %s@]" config.handle config.pds

let config_file xdg = Eio.Path.(Xdge.config_dir xdg / "config.toml")
let password_file xdg = Eio.Path.(Xdge.config_dir xdg / "password")

let load _fs xdg =
  let path = config_file xdg in
  try
    match Tomlt_eio.decode_file tomlt path with
    | Ok config -> Some config
    | Error _ -> None
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let save _fs xdg config =
  let dir = Xdge.config_dir xdg in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  let path = config_file xdg in
  Tomlt_eio.encode_file tomlt config path

let load_password _fs xdg =
  let path = password_file xdg in
  try Some (String.trim (Eio.Path.load path))
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let save_password _fs xdg password =
  let dir = Xdge.config_dir xdg in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  let path = password_file xdg in
  Eio.Path.save ~create:(`Or_truncate 0o600) path password

let clear_password _fs xdg =
  let path = password_file xdg in
  try Eio.Path.unlink path
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()
