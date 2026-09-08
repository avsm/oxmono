(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type clock = float Eio.Time.clock_ty Eio.Resource.t
type mono = Eio.Time.Mono.ty Eio.Resource.t
type t = { fetch : Fetch.plain; clock : clock option; mono_clock : mono option }

let v ?https ?retry ?cookies ?max_concurrent ?min_interval env =
  let min_interval =
    Option.map
      (fun seconds ->
        if not (Float.is_finite seconds && seconds >= 0.) then
          invalid_arg
            "Transport.v: min_interval must be finite and non-negative";
        Duration.of_f seconds)
      min_interval
  in
  Error_context.with_io ~operation:"initializing JMAP transport" @@ fun () ->
  {
    fetch =
      Fetch_httpz.std ?https ?retry ?cookies ?max_concurrent ?min_interval env;
    clock = Some (Eio.Stdenv.clock env);
    mono_clock = Some (Eio.Stdenv.mono_clock env);
  }

let of_fetch ?(clock : [> float Eio.Time.clock_ty ] Eio.Resource.t option)
    ?(mono_clock : [> Eio.Time.Mono.ty ] Eio.Resource.t option) fetch =
  {
    (* Fetch offers no coercion from a backend-tagged client to [Fetch.plain],
       so the tag is erased by re-exposing the client's own handler. *)
    fetch = Fetch.Middleware.(of_handler (handler fetch));
    clock :> clock option;
    mono_clock :> mono option;
  }

let fetch t = t.fetch
let clock t = t.clock
let mono_clock t = t.mono_clock
let restrict ~under t = { t with fetch = Fetch.restrict ~under t.fetch }
