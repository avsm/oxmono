(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Type alias for convenience *)
module Document = Atp_lexicon_standard_site.Site.Standard.Document

(* Format datetime for AT Protocol (RFC 3339 with millisecond precision and UTC Z suffix) *)
let now_rfc3339 () = Ptime.to_rfc3339 ~tz_offset_s:0 ~frac_s:3 (Ptime_clock.now ())

(* Normalize a datetime string to AT Protocol format *)
let normalize_datetime s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> Ptime.to_rfc3339 ~tz_offset_s:0 ~frac_s:3 t
  | Error _ -> s (* If parsing fails, return original and let server validate *)

(* Helper to load session and create API *)
let with_api env f =
  Eio.Switch.run @@ fun sw ->
  let fs = env#fs in
  match Xrpc_auth.Session.load fs ~app_name:"standard-site" () with
  | None ->
      Fmt.epr "Not logged in. Use 'standard-site auth login' first.@.";
      exit 1
  | Some session ->
      let api =
        Standard_site.Api.create ~sw ~env ~app_name:"standard-site"
          ~pds:session.pds ()
      in
      Standard_site.Api.resume api ~session;
      f api

(* Resolve a site argument to a full AT-URI.
   Accepts either a full AT-URI or just the publication rkey. *)
let resolve_site_uri ~did site =
  if String.starts_with ~prefix:"at://" site then site
  else
    (* Assume it's just the publication rkey *)
    Printf.sprintf "at://%s/site.standard.publication/%s" did site

(* Pretty printers *)
let pp_document ppf (rkey, (d : Document.main)) =
  Fmt.pf ppf "@[<v>%s@,  Title: %s@,  Site: %s%a%a@,  Published: %s%a%a%a@]" rkey
    d.title d.site
    Fmt.(option (fmt "@,  Path: %s"))
    d.path
    Fmt.(option (fmt "@,  Description: %s"))
    d.description d.published_at
    Fmt.(option (fmt "@,  Updated: %s"))
    d.updated_at
    (fun ppf tags ->
      match tags with
      | Some t when t <> [] -> Fmt.pf ppf "@,  Tags: %s" (String.concat ", " t)
      | _ -> ())
    d.tags
    (fun ppf bsky ->
      match bsky with
      | Some (r : Atp_lexicon_standard_site.Com.Atproto.Repo.StrongRef.main) ->
          Fmt.pf ppf "@,  Bluesky: %s" r.uri
      | None -> ())
    d.bsky_post_ref

let pp_document_detail ppf (rkey, (d : Document.main)) =
  Fmt.pf ppf "@[<v>Document: %s@,@," rkey;
  Fmt.pf ppf "Title: %s@," d.title;
  Fmt.pf ppf "Site: %s@," d.site;
  Fmt.(option (fmt "Path: %s@,")) ppf d.path;
  Fmt.(option (fmt "Description: %s@,")) ppf d.description;
  Fmt.pf ppf "Published: %s@," d.published_at;
  Fmt.(option (fmt "Updated: %s@,")) ppf d.updated_at;
  (match d.tags with
  | Some tags when tags <> [] ->
      Fmt.pf ppf "Tags: %s@," (String.concat ", " tags)
  | _ -> ());
  (match d.bsky_post_ref with
  | Some (r : Atp_lexicon_standard_site.Com.Atproto.Repo.StrongRef.main) ->
      Fmt.pf ppf "Bluesky post: %s@," r.uri;
      Fmt.pf ppf "Bluesky CID: %s@," r.cid
  | None -> ());
  (match d.text_content with
  | Some content ->
      let preview =
        if String.length content > 200 then String.sub content 0 200 ^ "..."
        else content
      in
      Fmt.pf ppf "@,Content preview:@,%s@," preview
  | None -> ());
  Fmt.pf ppf "@]"

(* List command *)

let user_arg =
  let doc = "User handle or DID (default: logged-in user)." in
  Arg.(value & opt (some string) None & info [ "user"; "u" ] ~docv:"USER" ~doc)

let list_action ~user env =
  with_api env @@ fun api ->
  let did =
    match user with
    | Some u ->
        if String.starts_with ~prefix:"did:" u then u
        else Standard_site.Api.resolve_handle api u
    | None -> Standard_site.Api.get_did api
  in
  let docs = Standard_site.Api.list_documents api ~did () in
  if docs = [] then Fmt.pr "No documents found.@."
  else begin
    Fmt.pr "Documents:@.@.";
    List.iter (fun d -> Fmt.pr "%a@.@." pp_document d) docs
  end

let list_cmd =
  let doc = "List documents." in
  let info = Cmd.info "list" ~doc in
  let list' user = Eio_main.run @@ fun env -> list_action ~user env in
  Cmd.v info Term.(const list' $ user_arg)

(* Show command *)

let rkey_arg =
  let doc = "Record key of the document." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"RKEY" ~doc)

let show_action ~rkey ~user env =
  with_api env @@ fun api ->
  let did =
    match user with
    | Some u ->
        if String.starts_with ~prefix:"did:" u then u
        else Standard_site.Api.resolve_handle api u
    | None -> Standard_site.Api.get_did api
  in
  match Standard_site.Api.get_document api ~did ~rkey with
  | Some doc -> Fmt.pr "%a@." pp_document_detail (rkey, doc)
  | None ->
      Fmt.epr "Document not found: %s@." rkey;
      exit 1

let show_cmd =
  let doc = "Show document details." in
  let info = Cmd.info "show" ~doc in
  let show' rkey user =
    Eio_main.run @@ fun env -> show_action ~rkey ~user env
  in
  Cmd.v info Term.(const show' $ rkey_arg $ user_arg)

(* Create command *)

let title_arg =
  let doc = "Document title." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"TITLE" ~doc)

let site_arg =
  let doc =
    "Publication rkey or full AT-URI. If just a rkey is given (e.g., \
     'myblog'), it will be expanded to an AT-URI using your DID."
  in
  Arg.(
    required & opt (some string) None & info [ "site"; "s" ] ~docv:"SITE" ~doc)

let path_arg =
  let doc = "URL path for the document (e.g., /posts/my-post)." in
  Arg.(value & opt (some string) None & info [ "path"; "p" ] ~docv:"PATH" ~doc)

let description_arg =
  let doc = "Document description or excerpt." in
  Arg.(
    value
    & opt (some string) None
    & info [ "description"; "d" ] ~docv:"DESC" ~doc)

let content_arg =
  let doc = "Plaintext content of the document." in
  Arg.(
    value & opt (some string) None & info [ "content"; "c" ] ~docv:"TEXT" ~doc)

let tags_arg =
  let doc = "Comma-separated tags." in
  Arg.(value & opt (some string) None & info [ "tags"; "t" ] ~docv:"TAGS" ~doc)

let create_rkey_arg =
  let doc = "Record key (auto-generated if not provided)." in
  Arg.(value & opt (some string) None & info [ "rkey" ] ~docv:"RKEY" ~doc)

let published_at_arg =
  let doc = "Publish timestamp (ISO 8601, default: now)." in
  Arg.(
    value
    & opt (some string) None
    & info [ "published-at" ] ~docv:"TIMESTAMP" ~doc)

let bsky_post_arg =
  let doc =
    "Link to a Bluesky post. Accepts web URLs (https://bsky.app/profile/handle/post/rkey) \
     or AT URIs (at://did/app.bsky.feed.post/rkey)."
  in
  Arg.(value & opt (some string) None & info [ "bsky-post" ] ~docv:"URL" ~doc)

let create_action ~title ~site ~path ~description ~content ~tags ~rkey
    ~published_at ~bsky_post env =
  with_api env @@ fun api ->
  let did = Standard_site.Api.get_did api in
  let site = resolve_site_uri ~did site in
  let published_at =
    match published_at with
    | Some t -> t
    | None -> now_rfc3339 ()
  in
  let tags =
    Option.map (String.split_on_char ',') tags
    |> Option.map (List.map String.trim)
  in
  let bsky_post_ref = Option.map (Standard_site.Api.resolve_bsky_post api) bsky_post in
  let rkey =
    Standard_site.Api.create_document api ~site ~title ~published_at ?path
      ?description ?text_content:content ?tags ?bsky_post_ref ?rkey ()
  in
  Fmt.pr "Created document: %s@." title;
  Fmt.pr "AT URI: at://%s/site.standard.document/%s@." did rkey

let create_cmd =
  let doc = "Create a new document." in
  let info = Cmd.info "create" ~doc in
  let create' title site path description content tags rkey published_at bsky_post =
    Eio_main.run @@ fun env ->
    create_action ~title ~site ~path ~description ~content ~tags ~rkey
      ~published_at ~bsky_post env
  in
  Cmd.v info
    Term.(
      const create' $ title_arg $ site_arg $ path_arg $ description_arg
      $ content_arg $ tags_arg $ create_rkey_arg $ published_at_arg
      $ bsky_post_arg)

(* Update command *)

let update_rkey_arg =
  let doc = "Record key of the document. If not provided, use --path to find the document." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"RKEY" ~doc)

let update_path_arg =
  let doc = "URL path to find the document by (e.g., /notes/my-post). Used to look up the document if RKEY is not provided, or to update the path if RKEY is provided." in
  Arg.(value & opt (some string) None & info [ "path"; "p" ] ~docv:"PATH" ~doc)

let update_action ~rkey ~title ~site ~path ~description ~content ~tags ~bsky_post env =
  with_api env @@ fun api ->
  let did = Standard_site.Api.get_did api in
  (* Track whether we looked up by path (to preserve path vs update it) *)
  let looked_up_by_path = Option.is_none rkey in
  (* Find the document - either by rkey or by path *)
  let rkey, existing =
    match rkey with
    | Some rkey -> (
        match Standard_site.Api.get_document api ~did ~rkey with
        | None ->
            Fmt.epr "Document not found: %s@." rkey;
            exit 1
        | Some doc -> (rkey, doc))
    | None -> (
        (* Look up by path *)
        match path with
        | None ->
            Fmt.epr "Error: Either RKEY or --path must be provided.@.";
            exit 1
        | Some search_path ->
            let docs = Standard_site.Api.list_documents api ~did () in
            let matching =
              List.find_opt
                (fun (_, (d : Document.main)) -> d.path = Some search_path)
                docs
            in
            match matching with
            | None ->
                Fmt.epr "No document found with path: %s@." search_path;
                exit 1
            | Some (rkey, doc) -> (rkey, doc))
  in
  let site =
    match site with
    | Some s -> resolve_site_uri ~did s
    | None -> existing.site
  in
  let tags =
    Option.map (String.split_on_char ',') tags
    |> Option.map (List.map String.trim)
  in
  let bsky_post_ref =
    match bsky_post with
    | Some url -> Some (Standard_site.Api.resolve_bsky_post api url)
    | None -> existing.bsky_post_ref
  in
  let updated_at = now_rfc3339 () in
  (* When looking up by path, preserve existing path; otherwise use new path if provided *)
  let new_path =
    if looked_up_by_path then existing.path
    else if Option.is_some path then path
    else existing.path
  in
  Standard_site.Api.update_document api ~rkey ~site
    ~title:(Option.value ~default:existing.title title)
    ~published_at:(normalize_datetime existing.published_at)
    ?path:new_path
    ?description:
      (if Option.is_some description then description else existing.description)
    ?text_content:
      (if Option.is_some content then content else existing.text_content)
    ?tags:(if Option.is_some tags then tags else existing.tags)
    ?bsky_post_ref ~updated_at ();
  Fmt.pr "Updated document: %s@." rkey

let update_title_arg =
  let doc = "New document title." in
  Arg.(value & opt (some string) None & info [ "title" ] ~docv:"TITLE" ~doc)

let update_site_arg =
  let doc = "New publication rkey or full AT-URI." in
  Arg.(value & opt (some string) None & info [ "site"; "s" ] ~docv:"SITE" ~doc)

let update_cmd =
  let doc = "Update an existing document. Specify RKEY as a positional argument, or use --path to find the document by its URL path." in
  let info = Cmd.info "update" ~doc in
  let update' rkey title site path description content tags bsky_post =
    Eio_main.run @@ fun env ->
    update_action ~rkey ~title ~site ~path ~description ~content ~tags
      ~bsky_post env
  in
  Cmd.v info
    Term.(
      const update' $ update_rkey_arg $ update_title_arg $ update_site_arg
      $ update_path_arg $ description_arg $ content_arg $ tags_arg $ bsky_post_arg)

(* Delete command *)

let force_arg =
  let doc = "Skip confirmation prompt." in
  Arg.(value & flag & info [ "force"; "f" ] ~doc)

let delete_action ~rkey ~force env =
  with_api env @@ fun api ->
  if not force then begin
    Fmt.pr "Delete document '%s'? [y/N] @?" rkey;
    let response = read_line () in
    if not (String.lowercase_ascii response = "y" || response = "yes") then begin
      Fmt.pr "Aborted.@.";
      exit 0
    end
  end;
  Standard_site.Api.delete_document api ~rkey;
  Fmt.pr "Deleted document: %s@." rkey

let delete_cmd =
  let doc = "Delete a document." in
  let info = Cmd.info "delete" ~doc in
  let delete' rkey force =
    Eio_main.run @@ fun env -> delete_action ~rkey ~force env
  in
  Cmd.v info Term.(const delete' $ rkey_arg $ force_arg)

(* Resolve command - lookup AT-URI from web URL *)

let web_url_arg =
  let doc =
    "Full web URL to resolve (e.g., https://anil.recoil.org/notes/hny2026)."
  in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"URL" ~doc)

let resolve_action ~url env =
  with_api env @@ fun api ->
  let did = Standard_site.Api.get_did api in
  (* Parse the URL to extract base and path *)
  let uri = Uri.of_string url in
  let base_url =
    let scheme = Option.value ~default:"https" (Uri.scheme uri) in
    let host = Option.value ~default:"" (Uri.host uri) in
    Printf.sprintf "%s://%s" scheme host
  in
  let path = Uri.path uri in
  (* Find the publication matching this base URL *)
  let pubs = Standard_site.Api.list_publications api ~did () in
  let matching_pub =
    List.find_opt
      (fun (_, (p : Atp_lexicon_standard_site.Site.Standard.Publication.main))
         ->
        (* Normalize URLs for comparison - remove trailing slashes *)
        let pub_url = String.trim p.url in
        let pub_url =
          if String.ends_with ~suffix:"/" pub_url then
            String.sub pub_url 0 (String.length pub_url - 1)
          else pub_url
        in
        let base =
          if String.ends_with ~suffix:"/" base_url then
            String.sub base_url 0 (String.length base_url - 1)
          else base_url
        in
        String.equal
          (String.lowercase_ascii pub_url)
          (String.lowercase_ascii base))
      pubs
  in
  match matching_pub with
  | None ->
      Fmt.epr "No publication found matching URL: %s@." base_url;
      exit 1
  | Some (pub_rkey, _pub) -> (
      let pub_at_uri =
        Printf.sprintf "at://%s/site.standard.publication/%s" did pub_rkey
      in
      (* Find the document matching this publication and path *)
      let docs = Standard_site.Api.list_documents api ~did () in
      let matching_doc =
        List.find_opt
          (fun (_, (d : Document.main)) ->
            (* Check if document's site matches our publication *)
            let site_matches = String.equal d.site pub_at_uri in
            (* Check if path matches *)
            let path_matches =
              match d.path with
              | Some doc_path ->
                  (* Normalize paths - ensure both have leading slash *)
                  let norm_doc =
                    if String.starts_with ~prefix:"/" doc_path then doc_path
                    else "/" ^ doc_path
                  in
                  let norm_path =
                    if String.starts_with ~prefix:"/" path then path
                    else "/" ^ path
                  in
                  String.equal norm_doc norm_path
              | None -> false
            in
            site_matches && path_matches)
          docs
      in
      match matching_doc with
      | None ->
          Fmt.epr "No document found with path '%s' in publication '%s'.@." path
            pub_rkey;
          exit 1
      | Some (doc_rkey, _doc) ->
          let doc_at_uri =
            Printf.sprintf "at://%s/site.standard.document/%s" did doc_rkey
          in
          Fmt.pr "%s@." doc_at_uri)

let resolve_cmd =
  let doc =
    "Resolve a web URL to its AT-URI. Useful for generating <link \
     rel=\"site.standard.document\" href=\"at://...\"> tags."
  in
  let info = Cmd.info "resolve" ~doc in
  let resolve' url = Eio_main.run @@ fun env -> resolve_action ~url env in
  Cmd.v info Term.(const resolve' $ web_url_arg)

(* Document command group *)

let cmd =
  let doc = "Document commands." in
  let info = Cmd.info "document" ~doc in
  Cmd.group info
    [ list_cmd; show_cmd; create_cmd; update_cmd; delete_cmd; resolve_cmd ]
