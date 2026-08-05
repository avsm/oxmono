(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t

val login :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock
      ; net : _ Eio.Net.t
      ; fs : Eio.Fs.dir_ty Eio.Path.t
      ; .. > ->
  ?profile:string ->
  base_url:string ->
  api_key:string ->
  unit ->
  t

val resume :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock
      ; net : _ Eio.Net.t
      ; fs : Eio.Fs.dir_ty Eio.Path.t
      ; .. > ->
  ?profile:string ->
  session:Session.credentials ->
  unit ->
  t

val logout : t -> unit

val client : t -> Karakeep.t
val session : t -> Session.credentials
val profile : t -> string option
val fs : t -> Eio.Fs.dir_ty Eio.Path.t
