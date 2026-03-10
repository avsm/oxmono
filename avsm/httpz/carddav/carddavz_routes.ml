(* carddavz_routes.ml - httpz route integration for CardDAV *)

let routes ~store ~locks =
  let open Httpz_server.Route in
  let webdav_routes = Webdavz.Handler.routes (module Carddavz_store) store ~locks in
  let carddav_routes = [
    (* REPORT - addressbook-query and addressbook-multiget *)
    report tail (fun path_segs ctx respond ->
      let path = "/" ^ String.concat "/" path_segs in
      match body_string ctx with
      | None ->
        respond_string respond ~status:Httpz.Res.Bad_request "Missing REPORT body"
      | Some body_xml ->
        match Carddavz_report.parse_report body_xml with
        | None ->
          respond_string respond ~status:Httpz.Res.Bad_request "Invalid REPORT body"
        | Some (Carddavz_report.Addressbook_query { filter; props = _props }) ->
          (* List all vcf files, filter by query, return multistatus *)
          let children = Carddavz_store.children store ~path in
          let child_prefix = if String.equal path "/" then "/" else path ^ "/" in
          let responses = List.filter_map (fun name ->
            if not (Filename.check_suffix name ".vcf") then None
            else
              let child_path = child_prefix ^ name in
              match Carddavz_store.read store ~path:child_path with
              | None -> None
              | Some content ->
                match Carddavz_vcard.parse content with
                | Error _ -> None
                | Ok vcard ->
                  if Carddavz_report.vcard_matches_filter filter vcard then
                    let open Webdavz.Response in
                    Some {
                      href = child_path;
                      propstats = [propstat_ok [
                        Webdavz.Xml.dav_node "getetag"
                          [Webdavz.Xml.pcdata (Printf.sprintf "\"%08x\"" (Hashtbl.hash content))];
                        Webdavz.Xml.Node (Webdavz.Xml.carddav_ns, "address-data", [],
                          [Webdavz.Xml.pcdata content]);
                      ]];
                    }
                  else None)
            children
          in
          xml_multistatus respond (Webdavz.Response.multistatus responses)
        | Some (Carddavz_report.Addressbook_multiget { hrefs; props = _props }) ->
          let responses = List.filter_map (fun href ->
            match Carddavz_store.read store ~path:href with
            | None ->
              let open Webdavz.Response in
              Some { href; propstats = [propstat_not_found []] }
            | Some content ->
              let open Webdavz.Response in
              Some {
                href;
                propstats = [propstat_ok [
                  Webdavz.Xml.dav_node "getetag"
                    [Webdavz.Xml.pcdata (Printf.sprintf "\"%08x\"" (Hashtbl.hash content))];
                  Webdavz.Xml.Node (Webdavz.Xml.carddav_ns, "address-data", [],
                    [Webdavz.Xml.pcdata content]);
                ]];
              })
            hrefs
          in
          xml_multistatus respond (Webdavz.Response.multistatus responses));

    (* Override OPTIONS to advertise CardDAV compliance *)
    route Httpz.Method.Options tail h0
      (fun _path_segs () _ctx respond ->
        respond_string respond ~status:Httpz.Res.Success
          ~headers:[
            (Httpz.Header_name.Allow,
             "OPTIONS, GET, HEAD, PUT, DELETE, PROPFIND, PROPPATCH, MKCOL, REPORT");
            (Httpz.Header_name.Dav, "1, addressbook");
          ] "");
  ] in
  (* CardDAV-specific routes take priority over generic WebDAV *)
  carddav_routes @ webdav_routes
