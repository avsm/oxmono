(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tangled - Decentralized Git Collaboration on AT Protocol.

    Tangled is a decentralized git collaboration platform built on AT Protocol.
    This library provides types and API operations for interacting with Tangled
    services.

    {2 Architecture}

    Tangled uses two types of servers:
    - {b PDS} (Personal Data Server): Stores AT Protocol records for repository
      metadata in the [sh.tangled.repo] collection
    - {b Knot}: Distributed git hosting servers that store actual git data

    {2 Quick Start}

    {[
      open Eio_main

      let () =
        run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        (* Create API client *)
        let api = Tangled.Api.create ~sw ~env ~pds:"https://bsky.social" () in

        (* Try to resume saved session *)
        (match Tangled.Session.load env#fs with
        | Some session -> Tangled.Api.resume api ~session
        | None ->
            (* Login *)
            Tangled.Api.login api ~identifier:"alice.bsky.social"
              ~password:"app-password");

        (* List repos *)
        let repos = Tangled.Api.list_repos api () in
        List.iter
          (fun (rkey, repo) -> Fmt.pr "%a@." Tangled.Types.pp_repo repo)
          repos;

        (* Create a new repo *)
        let _rkey =
          Tangled.Api.create_repo api ~name:"my-project" ~knot:"knot.tangled.sh"
            ~description:"My new project" ()
        in

        (* Clone *)
        Tangled.Api.clone api ~repo:"alice/my-project" ()
    ]}

    {2 Modules} *)

module Session = Xrpc_auth.Session
(** Session storage for Tangled CLI (shared with other AT Protocol CLIs). *)

module Types = Tangled_types
(** Tangled data types with jsont codecs. *)

module Api = Tangled_api
(** High-level Tangled API operations. *)

module Config = Tangled_config
(** Configuration management using TOML files with XDG paths. *)
