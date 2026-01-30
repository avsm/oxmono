(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
---------------------------------------------------------------------------*)

open Cmdliner

(* Argument terms *)

let domain_arg =
  let doc = "The domain name to query." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"DOMAIN" ~doc)

(* Term functions *)

let registrable_term psl =
  Term.(
    const (fun domain -> Publicsuffix.registrable_domain psl domain)
    $ domain_arg)

let suffix_term psl =
  Term.(
    const (fun domain -> Publicsuffix.public_suffix psl domain) $ domain_arg)

let is_suffix_term psl =
  Term.(
    const (fun domain -> Publicsuffix.is_public_suffix psl domain) $ domain_arg)

let is_registrable_term psl =
  Term.(
    const (fun domain -> Publicsuffix.is_registrable_domain psl domain)
    $ domain_arg)

let registrable_section_term psl =
  Term.(
    const (fun domain ->
        Publicsuffix.registrable_domain_with_section psl domain)
    $ domain_arg)

let suffix_section_term psl =
  Term.(
    const (fun domain -> Publicsuffix.public_suffix_with_section psl domain)
    $ domain_arg)

let stats_term psl =
  Term.(
    const (fun () ->
        ( Publicsuffix.rule_count psl,
          Publicsuffix.icann_rule_count psl,
          Publicsuffix.private_rule_count psl ))
    $ const ())

let version_term psl =
  Term.(
    const (fun () -> (Publicsuffix.version psl, Publicsuffix.commit psl))
    $ const ())
