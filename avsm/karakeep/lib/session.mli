(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type credentials = {
  api_key : string;
  base_url : string;
}

val app_name : string
val default_base_url : string
val default_profile : string

val base_config_dir : Eio.Fs.dir_ty Eio.Path.t -> Eio.Fs.dir_ty Eio.Path.t
val profiles_dir : Eio.Fs.dir_ty Eio.Path.t -> Eio.Fs.dir_ty Eio.Path.t
val profile_dir : Eio.Fs.dir_ty Eio.Path.t -> string -> Eio.Fs.dir_ty Eio.Path.t

val get_current_profile : Eio.Fs.dir_ty Eio.Path.t -> string
val set_current_profile : Eio.Fs.dir_ty Eio.Path.t -> string -> unit
val list_profiles : Eio.Fs.dir_ty Eio.Path.t -> string list

val load : Eio.Fs.dir_ty Eio.Path.t -> ?profile:string -> unit -> credentials option
val save : Eio.Fs.dir_ty Eio.Path.t -> ?profile:string -> credentials -> unit
val clear : Eio.Fs.dir_ty Eio.Path.t -> ?profile:string -> unit -> unit

val pp : Format.formatter -> credentials -> unit
val base_url : credentials -> string
val api_key : credentials -> string
val create : base_url:string -> api_key:string -> unit -> credentials
