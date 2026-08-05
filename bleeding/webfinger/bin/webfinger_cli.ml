(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** WebFinger command-line tool.

    A CLI for performing WebFinger (RFC 7033) queries against any host. *)

open Cmdliner

(** {1 Command-line Arguments} *)

let resource =
  let doc = "Resource URI to query (e.g., acct:user@example.com)" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"RESOURCE" ~doc)

let rels =
  let doc = "Filter links by relation type (can be specified multiple times)" in
  Arg.(value & opt_all string [] & info ["r"; "rel"] ~docv:"REL" ~doc)

let json_output =
  let doc = "Output raw JSON instead of formatted text" in
  Arg.(value & flag & info ["j"; "json"] ~doc)

let show_links_only =
  let doc = "Show only links, not subject/aliases/properties" in
  Arg.(value & flag & info ["l"; "links-only"] ~doc)

(** {1 Logging Setup} *)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  (* Return whether we should be quiet (log level is None) *)
  Option.is_none level

(** {1 Output Formatting} *)

let pp_link_compact ppf link =
  let href = Option.value ~default:"(no href)" (Webfinger.Link.href link) in
  Format.fprintf ppf "@[<h>%s@ ->@ %s" (Webfinger.Link.rel link) href;
  Option.iter (fun t -> Format.fprintf ppf "@ [%s]" t) (Webfinger.Link.type_ link);
  Format.fprintf ppf "@]"

let pp_jrd_compact ppf jrd =
  Option.iter (fun s -> Format.fprintf ppf "@[<v>Subject: %s@]@," s) (Webfinger.Jrd.subject jrd);
  let aliases = Webfinger.Jrd.aliases jrd in
  if aliases <> [] then begin
    Format.fprintf ppf "@[<v>Aliases:@,";
    List.iter (fun a -> Format.fprintf ppf "  %s@," a) aliases;
    Format.fprintf ppf "@]"
  end;
  let props = Webfinger.Jrd.properties jrd in
  if props <> [] then begin
    Format.fprintf ppf "@[<v>Properties:@,";
    List.iter (fun (k, v) ->
      let v = Option.value ~default:"null" v in
      Format.fprintf ppf "  %s: %s@," k v
    ) props;
    Format.fprintf ppf "@]"
  end;
  let links = Webfinger.Jrd.links jrd in
  if links <> [] then begin
    Format.fprintf ppf "@[<v>Links:@,";
    List.iter (fun link -> Format.fprintf ppf "  %a@," pp_link_compact link) links;
    Format.fprintf ppf "@]"
  end

let pp_links_only ppf jrd =
  List.iter (fun link -> Format.fprintf ppf "%a@," pp_link_compact link) (Webfinger.Jrd.links jrd)

(** {1 Main Command} *)

let run quiet resource rels json_output links_only =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let session = Requests.create ~sw env in
  match Webfinger.query session ~resource ~rels () with
  | Error e ->
      if not quiet then Format.eprintf "Error: %a@." Webfinger.pp_error e;
      `Error (false, Webfinger.error_to_string e)
  | Ok jrd ->
      if not quiet then begin
        if json_output then
          Format.printf "%s@." (Webfinger.Jrd.to_string jrd)
        else if links_only then
          Format.printf "%a" pp_links_only jrd
        else
          Format.printf "%a" pp_jrd_compact jrd
      end;
      `Ok ()

let run_term =
  let quiet = Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ()) in
  Term.(ret (const run $ quiet $ resource $ rels $ json_output $ show_links_only))

(** {1 Command Definition} *)

let cmd =
  let doc = "Query WebFinger (RFC 7033) resources" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) queries WebFinger servers to discover information about \
        resources identified by URIs. This is commonly used for discovering \
        ActivityPub profiles, OpenID Connect providers, and other federated \
        services.";
    `S Manpage.s_examples;
    `P "Query a Mastodon user's WebFinger record:";
    `Pre "  $(tname) acct:gargron@mastodon.social";
    `P "Get only ActivityPub-related links:";
    `Pre "  $(tname) --rel self acct:user@example.com";
    `P "Output raw JSON for processing:";
    `Pre "  $(tname) --json acct:user@example.com | jq .";
    `S Manpage.s_see_also;
    `P "RFC 7033 - WebFinger: $(b,https://datatracker.ietf.org/doc/html/rfc7033)";
  ] in
  let info = Cmd.info "webfinger" ~version:"0.1.0" ~doc ~man in
  Cmd.v info run_term

let () = exit (Cmd.eval cmd)
