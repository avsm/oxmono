(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Type alias for convenience *)
module Publication = Atp_lexicon_standard_site.Site.Standard.Publication

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

(* Pretty printer *)
let pp_publication ~did ppf (rkey, (p : Publication.main)) =
  Fmt.pf ppf
    "@[<v>%s@,\
    \  Name: %s@,\
    \  URL: %s@,\
    \  AT-URI: at://%s/site.standard.publication/%s%a@]"
    rkey p.name p.url did rkey
    Fmt.(option (fmt "@,  Description: %s"))
    p.description

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
  let pubs = Standard_site.Api.list_publications api ~did () in
  if pubs = [] then Fmt.pr "No publications found.@."
  else begin
    Fmt.pr "Publications:@.@.";
    List.iter (fun p -> Fmt.pr "%a@.@." (pp_publication ~did) p) pubs
  end

let list_cmd =
  let doc = "List publications." in
  let info = Cmd.info "list" ~doc in
  let list' user = Eio_main.run @@ fun env -> list_action ~user env in
  Cmd.v info Term.(const list' $ user_arg)

(* Create command *)

let name_arg =
  let doc = "Publication name." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)

let url_arg =
  let doc = "Publication base URL." in
  Arg.(required & opt (some string) None & info [ "url" ] ~docv:"URL" ~doc)

let description_arg =
  let doc = "Publication description." in
  Arg.(
    value
    & opt (some string) None
    & info [ "description"; "d" ] ~docv:"DESC" ~doc)

let rkey_arg =
  let doc = "Record key (auto-generated if not provided)." in
  Arg.(value & opt (some string) None & info [ "rkey" ] ~docv:"RKEY" ~doc)

let create_action ~name ~url ~description ~rkey env =
  with_api env @@ fun api ->
  let rkey =
    Standard_site.Api.create_publication api ~name ~url ?description ?rkey ()
  in
  let did = Standard_site.Api.get_did api in
  Fmt.pr "Created publication: %s@." name;
  Fmt.pr "AT URI: at://%s/site.standard.publication/%s@." did rkey

let create_cmd =
  let doc = "Create a new publication." in
  let info = Cmd.info "create" ~doc in
  let create' name url description rkey =
    Eio_main.run @@ fun env -> create_action ~name ~url ~description ~rkey env
  in
  Cmd.v info
    Term.(const create' $ name_arg $ url_arg $ description_arg $ rkey_arg)

(* Update command *)

let update_rkey_arg =
  let doc = "Record key of publication to update." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"RKEY" ~doc)

let update_name_arg =
  let doc = "New publication name." in
  Arg.(value & opt (some string) None & info [ "name"; "n" ] ~docv:"NAME" ~doc)

let update_url_arg =
  let doc = "New publication base URL." in
  Arg.(value & opt (some string) None & info [ "url" ] ~docv:"URL" ~doc)

let update_action ~rkey ~name ~url ~description env =
  with_api env @@ fun api ->
  let did = Standard_site.Api.get_did api in
  (* First get the existing publication to preserve unchanged fields *)
  match Standard_site.Api.get_publication api ~did ~rkey with
  | None ->
      Fmt.epr "Publication not found: %s@." rkey;
      exit 1
  | Some existing ->
      Standard_site.Api.update_publication api ~rkey
        ~name:(Option.value ~default:existing.name name)
        ~url:(Option.value ~default:existing.url url)
        ?description:
          (if Option.is_some description then description
           else existing.description)
        ();
      Fmt.pr "Updated publication: %s@." rkey

let update_cmd =
  let doc = "Update an existing publication." in
  let info = Cmd.info "update" ~doc in
  let update' rkey name url description =
    Eio_main.run @@ fun env -> update_action ~rkey ~name ~url ~description env
  in
  Cmd.v info
    Term.(const update' $ update_rkey_arg $ update_name_arg $ update_url_arg $ description_arg)

(* Delete command *)

let delete_rkey_arg =
  let doc = "Record key of publication to delete." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"RKEY" ~doc)

let force_arg =
  let doc = "Skip confirmation prompt." in
  Arg.(value & flag & info [ "force"; "f" ] ~doc)

let delete_action ~rkey ~force env =
  with_api env @@ fun api ->
  if not force then begin
    Fmt.pr "Delete publication '%s'? [y/N] @?" rkey;
    let response = read_line () in
    if not (String.lowercase_ascii response = "y" || response = "yes") then begin
      Fmt.pr "Aborted.@.";
      exit 0
    end
  end;
  Standard_site.Api.delete_publication api ~rkey;
  Fmt.pr "Deleted publication: %s@." rkey

let delete_cmd =
  let doc = "Delete a publication." in
  let info = Cmd.info "delete" ~doc in
  let delete' rkey force =
    Eio_main.run @@ fun env -> delete_action ~rkey ~force env
  in
  Cmd.v info Term.(const delete' $ delete_rkey_arg $ force_arg)

(* URI lookup command *)

let site_url_arg =
  let doc = "Site URL to look up (matches publication base URL)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"URL" ~doc)

(* Check if a URL matches a publication's base URL *)
let url_matches ~base_url url =
  String.starts_with ~prefix:base_url url
  ||
  (* Also try without trailing slash *)
  let base_no_slash =
    if String.ends_with ~suffix:"/" base_url then
      String.sub base_url 0 (String.length base_url - 1)
    else base_url
  in
  String.starts_with ~prefix:base_no_slash url

let uri_action ~url env =
  with_api env @@ fun api ->
  let did = Standard_site.Api.get_did api in
  let pubs = Standard_site.Api.list_publications api ~did () in
  (* Find publication whose URL matches *)
  let matching =
    List.find_opt
      (fun (_, (p : Publication.main)) -> url_matches ~base_url:p.url url)
      pubs
  in
  match matching with
  | Some (rkey, p) ->
      Fmt.pr "at://%s/site.standard.publication/%s@." did rkey;
      Fmt.pr "Publication: %s@." p.name;
      Fmt.pr "Base URL: %s@." p.url
  | None ->
      Fmt.epr "No publication found matching URL: %s@." url;
      Fmt.epr "@.Available publications:@.";
      List.iter
        (fun (_, (p : Publication.main)) -> Fmt.epr "  - %s@." p.url)
        pubs;
      exit 1

let uri_cmd =
  let doc = "Look up AT URI for a publication given its site URL." in
  let info = Cmd.info "uri" ~doc in
  let uri' url = Eio_main.run @@ fun env -> uri_action ~url env in
  Cmd.v info Term.(const uri' $ site_url_arg)

(* Publication command group *)

let cmd =
  let doc = "Publication commands." in
  let info = Cmd.info "publication" ~doc in
  Cmd.group info [ list_cmd; create_cmd; update_cmd; delete_cmd; uri_cmd ]
