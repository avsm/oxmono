(** {1 Karakeep}

    The API for the Karakeep app

    @version 1.0.0 *)

type t = {
  session : Requests.t;
  base_url : string;
}

let create ?session ~sw env ~base_url =
  let session = match session with
    | Some s -> s
    | None -> Requests.create ~sw env
  in
  { session; base_url }

let base_url t = t.base_url
let session t = t.session

module TagId = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module Tag = struct
  module Types = struct
    module T = struct
      type t = {
        id : string;
        name : string;
        num_bookmarks : float;
        num_bookmarks_by_attached_type : Jsont.json;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~id ~name ~num_bookmarks ~num_bookmarks_by_attached_type () = { id; name; num_bookmarks; num_bookmarks_by_attached_type }
    
    let id t = t.id
    let name t = t.name
    let num_bookmarks t = t.num_bookmarks
    let num_bookmarks_by_attached_type t = t.num_bookmarks_by_attached_type
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Tag"
        (fun id name num_bookmarks num_bookmarks_by_attached_type -> { id; name; num_bookmarks; num_bookmarks_by_attached_type })
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "numBookmarks" Jsont.number ~enc:(fun r -> r.num_bookmarks)
      |> Jsont.Object.mem "numBookmarksByAttachedType" Jsont.json ~enc:(fun r -> r.num_bookmarks_by_attached_type)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get a single tag
  
      Get tag by its id *)
  let get_tags ~tag_id client () =
    let op_name = "get_tags" in
    let url_path = Openapi.Runtime.Path.render ~params:[("tagId", tag_id)] "/tags/{tagId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ListId = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module List = struct
  module Types = struct
    module T = struct
      type t = {
        description : string option;
        has_collaborators : bool;
        icon : string;
        id : string;
        name : string;
        parent_id : string option;
        public : bool;
        query : string option;
        type_ : string;
        user_role : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~has_collaborators ~icon ~id ~name ~public ~user_role ?(type_="manual") ?description ?parent_id ?query () = { description; has_collaborators; icon; id; name; parent_id; public; query; type_; user_role }
    
    let description t = t.description
    let has_collaborators t = t.has_collaborators
    let icon t = t.icon
    let id t = t.id
    let name t = t.name
    let parent_id t = t.parent_id
    let public t = t.public
    let query t = t.query
    let type_ t = t.type_
    let user_role t = t.user_role
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"List"
        (fun description has_collaborators icon id name parent_id public query type_ user_role -> { description; has_collaborators; icon; id; name; parent_id; public; query; type_; user_role })
      |> Jsont.Object.mem "description" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "hasCollaborators" Jsont.bool ~enc:(fun r -> r.has_collaborators)
      |> Jsont.Object.mem "icon" Jsont.string ~enc:(fun r -> r.icon)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "parentId" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.parent_id)
      |> Jsont.Object.mem "public" Jsont.bool ~enc:(fun r -> r.public)
      |> Jsont.Object.mem "query" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.query)
      |> Jsont.Object.mem "type" Jsont.string ~dec_absent:"manual" ~enc:(fun r -> r.type_)
      |> Jsont.Object.mem "userRole" Jsont.string ~enc:(fun r -> r.user_role)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Create a new list
  
      Create a new list *)
  let post_lists ~body client () =
    let op_name = "post_lists" in
    let url_path = "/lists" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get a single list
  
      Get list by its id *)
  let get_lists ~list_id client () =
    let op_name = "get_lists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("listId", list_id)] "/lists/{listId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a list
  
      Update list by its id *)
  let patch_lists ~list_id ~body client () =
    let op_name = "patch_lists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("listId", list_id)] "/lists/{listId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.patch client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PATCH" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PATCH";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module HighlightId = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module Highlight = struct
  module Types = struct
    module T = struct
      type t = {
        bookmark_id : string;
        color : string;
        created_at : string;
        end_offset : float;
        id : string;
        note : string option;
        start_offset : float;
        text : string option;
        user_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~bookmark_id ~created_at ~end_offset ~id ~start_offset ~user_id ?(color="yellow") ?note ?text () = { bookmark_id; color; created_at; end_offset; id; note; start_offset; text; user_id }
    
    let bookmark_id t = t.bookmark_id
    let color t = t.color
    let created_at t = t.created_at
    let end_offset t = t.end_offset
    let id t = t.id
    let note t = t.note
    let start_offset t = t.start_offset
    let text t = t.text
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Highlight"
        (fun bookmark_id color created_at end_offset id note start_offset text user_id -> { bookmark_id; color; created_at; end_offset; id; note; start_offset; text; user_id })
      |> Jsont.Object.mem "bookmarkId" Jsont.string ~enc:(fun r -> r.bookmark_id)
      |> Jsont.Object.mem "color" Jsont.string ~dec_absent:"yellow" ~enc:(fun r -> r.color)
      |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "endOffset" Jsont.number ~enc:(fun r -> r.end_offset)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "note" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.note)
      |> Jsont.Object.mem "startOffset" Jsont.number ~enc:(fun r -> r.start_offset)
      |> Jsont.Object.mem "text" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.text)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Create a new highlight
  
      Create a new highlight *)
  let post_highlights ~body client () =
    let op_name = "post_highlights" in
    let url_path = "/highlights" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get a single highlight
  
      Get highlight by its id *)
  let get_highlights ~highlight_id client () =
    let op_name = "get_highlights" in
    let url_path = Openapi.Runtime.Path.render ~params:[("highlightId", highlight_id)] "/highlights/{highlightId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a highlight
  
      Delete highlight by its id *)
  let delete_highlights ~highlight_id client () =
    let op_name = "delete_highlights" in
    let url_path = Openapi.Runtime.Path.render ~params:[("highlightId", highlight_id)] "/highlights/{highlightId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a highlight
  
      Update highlight by its id *)
  let patch_highlights ~highlight_id ~body client () =
    let op_name = "patch_highlights" in
    let url_path = Openapi.Runtime.Path.render ~params:[("highlightId", highlight_id)] "/highlights/{highlightId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.patch client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PATCH" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PATCH";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module PaginatedHighlights = struct
  module Types = struct
    module T = struct
      type t = {
        highlights : Highlight.T.t list;
        next_cursor : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~highlights ?next_cursor () = { highlights; next_cursor }
    
    let highlights t = t.highlights
    let next_cursor t = t.next_cursor
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PaginatedHighlights"
        (fun highlights next_cursor -> { highlights; next_cursor })
      |> Jsont.Object.mem "highlights" (Jsont.list Highlight.T.jsont) ~enc:(fun r -> r.highlights)
      |> Jsont.Object.mem "nextCursor" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.next_cursor)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get all highlights
  
      Get all highlights *)
  let get_highlights ?limit ?cursor client () =
    let op_name = "get_highlights" in
    let url_path = "/highlights" in
    let query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"limit" ~value:limit; Openapi.Runtime.Query.optional ~key:"cursor" ~value:cursor]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module FileToBeUploaded = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module Cursor = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module Client = struct
  (** Update user
  
      Update a user's role, bookmark quota, or storage quota. Admin access required. *)
  let put_admin_users ~user_id ~body client () =
    let op_name = "put_admin_users" in
    let url_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/admin/users/{userId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get a single asset
  
      Get asset by its id *)
  let get_assets ~asset_id client () =
    let op_name = "get_assets" in
    let url_path = Openapi.Runtime.Path.render ~params:[("assetId", asset_id)] "/assets/{assetId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get all backups
  
      Get all backups *)
  let get_backups client () =
    let op_name = "get_backups" in
    let url_path = "/backups" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Trigger a new backup
  
      Trigger a new backup *)
  let post_backups client () =
    let op_name = "post_backups" in
    let url_path = "/backups" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get a single backup
  
      Get backup by its id *)
  let get_backups ~backup_id client () =
    let op_name = "get_backups" in
    let url_path = Openapi.Runtime.Path.render ~params:[("backupId", backup_id)] "/backups/{backupId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a backup
  
      Delete backup by its id *)
  let delete_backups ~backup_id client () =
    let op_name = "delete_backups" in
    let url_path = Openapi.Runtime.Path.render ~params:[("backupId", backup_id)] "/backups/{backupId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Download a backup
  
      Download backup file *)
  let get_backups_download ~backup_id client () =
    let op_name = "get_backups_download" in
    let url_path = Openapi.Runtime.Path.render ~params:[("backupId", backup_id)] "/backups/{backupId}/download" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a bookmark
  
      Delete bookmark by its id *)
  let delete_bookmarks ~bookmark_id client () =
    let op_name = "delete_bookmarks" in
    let url_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a bookmark
  
      Update bookmark by its id *)
  let patch_bookmarks ~bookmark_id ~body client () =
    let op_name = "patch_bookmarks" in
    let url_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.patch client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PATCH" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PATCH";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Attach asset
  
      Attach a new asset to a bookmark *)
  let post_bookmarks_assets ~bookmark_id ~body client () =
    let op_name = "post_bookmarks_assets" in
    let url_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}/assets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Replace asset
  
      Replace an existing asset with a new one *)
  let put_bookmarks_assets ~bookmark_id ~asset_id ~body client () =
    let op_name = "put_bookmarks_assets" in
    let url_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id); ("assetId", asset_id)] "/bookmarks/{bookmarkId}/assets/{assetId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Detach asset
  
      Detach an asset from a bookmark *)
  let delete_bookmarks_assets ~bookmark_id ~asset_id client () =
    let op_name = "delete_bookmarks_assets" in
    let url_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id); ("assetId", asset_id)] "/bookmarks/{bookmarkId}/assets/{assetId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get highlights of a bookmark
  
      Get highlights of a bookmark *)
  let get_bookmarks_highlights ~bookmark_id client () =
    let op_name = "get_bookmarks_highlights" in
    let url_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}/highlights" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get lists of a bookmark
  
      Get lists of a bookmark *)
  let get_bookmarks_lists ~bookmark_id client () =
    let op_name = "get_bookmarks_lists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}/lists" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Summarize a bookmark
  
      Attaches a summary to the bookmark and returns the updated record. *)
  let post_bookmarks_summarize ~bookmark_id client () =
    let op_name = "post_bookmarks_summarize" in
    let url_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}/summarize" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Attach tags to a bookmark
  
      Attach tags to a bookmark *)
  let post_bookmarks_tags ~bookmark_id ~body client () =
    let op_name = "post_bookmarks_tags" in
    let url_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}/tags" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Detach tags from a bookmark
  
      Detach tags from a bookmark *)
  let delete_bookmarks_tags ~bookmark_id client () =
    let op_name = "delete_bookmarks_tags" in
    let url_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}/tags" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get all lists
  
      Get all lists *)
  let get_lists client () =
    let op_name = "get_lists" in
    let url_path = "/lists" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a list
  
      Delete list by its id *)
  let delete_lists ~list_id client () =
    let op_name = "delete_lists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("listId", list_id)] "/lists/{listId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Add a bookmark to a list
  
      Add the bookmarks to a list *)
  let put_lists_bookmarks ~list_id ~bookmark_id client () =
    let op_name = "put_lists_bookmarks" in
    let url_path = Openapi.Runtime.Path.render ~params:[("listId", list_id); ("bookmarkId", bookmark_id)] "/lists/{listId}/bookmarks/{bookmarkId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Remove a bookmark from a list
  
      Remove the bookmarks from a list *)
  let delete_lists_bookmarks ~list_id ~bookmark_id client () =
    let op_name = "delete_lists_bookmarks" in
    let url_path = Openapi.Runtime.Path.render ~params:[("listId", list_id); ("bookmarkId", bookmark_id)] "/lists/{listId}/bookmarks/{bookmarkId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get all tags
  
      Get all tags *)
  let get_tags ?name_contains ?sort ?attached_by ?cursor ?limit client () =
    let op_name = "get_tags" in
    let url_path = "/tags" in
    let query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"nameContains" ~value:name_contains; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"attachedBy" ~value:attached_by; Openapi.Runtime.Query.optional ~key:"cursor" ~value:cursor; Openapi.Runtime.Query.optional ~key:"limit" ~value:limit]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a new tag
  
      Create a new tag *)
  let post_tags ~body client () =
    let op_name = "post_tags" in
    let url_path = "/tags" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a tag
  
      Delete tag by its id *)
  let delete_tags ~tag_id client () =
    let op_name = "delete_tags" in
    let url_path = Openapi.Runtime.Path.render ~params:[("tagId", tag_id)] "/tags/{tagId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a tag
  
      Update tag by its id *)
  let patch_tags ~tag_id ~body client () =
    let op_name = "patch_tags" in
    let url_path = Openapi.Runtime.Path.render ~params:[("tagId", tag_id)] "/tags/{tagId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.patch client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PATCH" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PATCH";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get current user info
  
      Returns info about the current user *)
  let get_users_me client () =
    let op_name = "get_users_me" in
    let url_path = "/users/me" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get current user stats
  
      Returns stats about the current user *)
  let get_users_me_stats client () =
    let op_name = "get_users_me_stats" in
    let url_path = "/users/me/stats" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module BookmarkId = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module Bookmark = struct
  module Types = struct
    module T = struct
      type t = {
        archived : bool;
        assets : Jsont.json list;
        content : Jsont.json;
        created_at : string;
        favourited : bool;
        id : string;
        modified_at : string option;
        note : string option;
        source : string option;
        summarization_status : string option;
        summary : string option;
        tagging_status : string option;
        tags : Jsont.json list;
        title : string option;
        user_id : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~archived ~assets ~content ~created_at ~favourited ~id ~tags ~user_id ?modified_at ?note ?source ?summarization_status ?summary ?tagging_status ?title () = { archived; assets; content; created_at; favourited; id; modified_at; note; source; summarization_status; summary; tagging_status; tags; title; user_id }
    
    let archived t = t.archived
    let assets t = t.assets
    let content t = t.content
    let created_at t = t.created_at
    let favourited t = t.favourited
    let id t = t.id
    let modified_at t = t.modified_at
    let note t = t.note
    let source t = t.source
    let summarization_status t = t.summarization_status
    let summary t = t.summary
    let tagging_status t = t.tagging_status
    let tags t = t.tags
    let title t = t.title
    let user_id t = t.user_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Bookmark"
        (fun archived assets content created_at favourited id modified_at note source summarization_status summary tagging_status tags title user_id -> { archived; assets; content; created_at; favourited; id; modified_at; note; source; summarization_status; summary; tagging_status; tags; title; user_id })
      |> Jsont.Object.mem "archived" Jsont.bool ~enc:(fun r -> r.archived)
      |> Jsont.Object.mem "assets" (Jsont.list Jsont.json) ~enc:(fun r -> r.assets)
      |> Jsont.Object.mem "content" Jsont.json ~enc:(fun r -> r.content)
      |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "favourited" Jsont.bool ~enc:(fun r -> r.favourited)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "modifiedAt" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.modified_at)
      |> Jsont.Object.mem "note" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.note)
      |> Jsont.Object.mem "source" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.source)
      |> Jsont.Object.mem "summarizationStatus" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.summarization_status)
      |> Jsont.Object.mem "summary" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.summary)
      |> Jsont.Object.mem "taggingStatus" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.tagging_status)
      |> Jsont.Object.mem "tags" (Jsont.list Jsont.json) ~enc:(fun r -> r.tags)
      |> Jsont.Object.mem "title" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.title)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Create a new bookmark
  
      Create a new bookmark *)
  let post_bookmarks ~body client () =
    let op_name = "post_bookmarks" in
    let url_path = "/bookmarks" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get a single bookmark
  
      Get bookmark by its id 
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  let get_bookmarks ~bookmark_id ?include_content client () =
    let op_name = "get_bookmarks" in
    let url_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}" in
    let query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"includeContent" ~value:include_content]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module PaginatedBookmarks = struct
  module Types = struct
    module T = struct
      type t = {
        bookmarks : Bookmark.T.t list;
        next_cursor : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~bookmarks ?next_cursor () = { bookmarks; next_cursor }
    
    let bookmarks t = t.bookmarks
    let next_cursor t = t.next_cursor
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PaginatedBookmarks"
        (fun bookmarks next_cursor -> { bookmarks; next_cursor })
      |> Jsont.Object.mem "bookmarks" (Jsont.list Bookmark.T.jsont) ~enc:(fun r -> r.bookmarks)
      |> Jsont.Object.mem "nextCursor" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.next_cursor)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get all bookmarks
  
      Get all bookmarks 
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  let get_bookmarks ?archived ?favourited ?sort_order ?limit ?cursor ?include_content client () =
    let op_name = "get_bookmarks" in
    let url_path = "/bookmarks" in
    let query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"archived" ~value:archived; Openapi.Runtime.Query.optional ~key:"favourited" ~value:favourited; Openapi.Runtime.Query.optional ~key:"sortOrder" ~value:sort_order; Openapi.Runtime.Query.optional ~key:"limit" ~value:limit; Openapi.Runtime.Query.optional ~key:"cursor" ~value:cursor; Openapi.Runtime.Query.optional ~key:"includeContent" ~value:include_content]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Search bookmarks
  
      Search bookmarks 
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  let get_bookmarks_search ~q ?sort_order ?limit ?cursor ?include_content client () =
    let op_name = "get_bookmarks_search" in
    let url_path = "/bookmarks/search" in
    let query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"q" ~value:q; Openapi.Runtime.Query.optional ~key:"sortOrder" ~value:sort_order; Openapi.Runtime.Query.optional ~key:"limit" ~value:limit; Openapi.Runtime.Query.optional ~key:"cursor" ~value:cursor; Openapi.Runtime.Query.optional ~key:"includeContent" ~value:include_content]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get bookmarks in the list
  
      Get bookmarks in the list 
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  let get_lists_bookmarks ~list_id ?sort_order ?limit ?cursor ?include_content client () =
    let op_name = "get_lists_bookmarks" in
    let url_path = Openapi.Runtime.Path.render ~params:[("listId", list_id)] "/lists/{listId}/bookmarks" in
    let query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"sortOrder" ~value:sort_order; Openapi.Runtime.Query.optional ~key:"limit" ~value:limit; Openapi.Runtime.Query.optional ~key:"cursor" ~value:cursor; Openapi.Runtime.Query.optional ~key:"includeContent" ~value:include_content]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get bookmarks with the tag
  
      Get bookmarks with the tag 
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  let get_tags_bookmarks ~tag_id ?sort_order ?limit ?cursor ?include_content client () =
    let op_name = "get_tags_bookmarks" in
    let url_path = Openapi.Runtime.Path.render ~params:[("tagId", tag_id)] "/tags/{tagId}/bookmarks" in
    let query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"sortOrder" ~value:sort_order; Openapi.Runtime.Query.optional ~key:"limit" ~value:limit; Openapi.Runtime.Query.optional ~key:"cursor" ~value:cursor; Openapi.Runtime.Query.optional ~key:"includeContent" ~value:include_content]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module BackupId = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module AssetId = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module Asset = struct
  module Types = struct
    module T = struct
      type t = {
        asset_id : string;
        content_type : string;
        file_name : string;
        size : float;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~asset_id ~content_type ~file_name ~size () = { asset_id; content_type; file_name; size }
    
    let asset_id t = t.asset_id
    let content_type t = t.content_type
    let file_name t = t.file_name
    let size t = t.size
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Asset"
        (fun asset_id content_type file_name size -> { asset_id; content_type; file_name; size })
      |> Jsont.Object.mem "assetId" Jsont.string ~enc:(fun r -> r.asset_id)
      |> Jsont.Object.mem "contentType" Jsont.string ~enc:(fun r -> r.content_type)
      |> Jsont.Object.mem "fileName" Jsont.string ~enc:(fun r -> r.file_name)
      |> Jsont.Object.mem "size" Jsont.number ~enc:(fun r -> r.size)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Upload a new asset
  
      Upload a new asset *)
  let post_assets ~body client () =
    let op_name = "post_assets" in
    let url_path = "/assets" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json body) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end
