(** {1 Karakeep}

    The API for the Karakeep app

    @version 1.0.0 *)

let __openapi_schemas = Openapi.Schema.of_string ~version:"3.0.0" "{\"Asset\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"assetId\":{\"type\":\"string\"},\"contentType\":{\"type\":\"string\"},\"fileName\":{\"type\":\"string\"},\"size\":{\"type\":\"number\"}},\"required\":[\"assetId\",\"contentType\",\"size\",\"fileName\"]},\"AssetId\":{\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"ieidlxygmwj87oxz5hxttoc8\"},\"BackupId\":{\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"ieidlxygmwj87oxz5hxttoc8\"},\"Bookmark\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"archived\":{\"type\":\"boolean\"},\"assets\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"},\"assetType\":{\"type\":\"string\",\"enum\":[\"linkHtmlContent\",\"screenshot\",\"pdf\",\"assetScreenshot\",\"bannerImage\",\"fullPageArchive\",\"video\",\"bookmarkAsset\",\"precrawledArchive\",\"userUploaded\",\"avatar\",\"unknown\"]},\"fileName\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"id\",\"assetType\"]}},\"content\":{\"oneOf\":[{\"type\":\"object\",\"properties\":{\"type\":{\"type\":\"string\",\"enum\":[\"link\"]},\"url\":{\"type\":\"string\"},\"title\":{\"type\":\"string\",\"nullable\":true},\"description\":{\"type\":\"string\",\"nullable\":true},\"imageUrl\":{\"type\":\"string\",\"nullable\":true},\"imageAssetId\":{\"type\":\"string\",\"nullable\":true},\"screenshotAssetId\":{\"type\":\"string\",\"nullable\":true},\"pdfAssetId\":{\"type\":\"string\",\"nullable\":true},\"fullPageArchiveAssetId\":{\"type\":\"string\",\"nullable\":true},\"precrawledArchiveAssetId\":{\"type\":\"string\",\"nullable\":true},\"videoAssetId\":{\"type\":\"string\",\"nullable\":true},\"favicon\":{\"type\":\"string\",\"nullable\":true},\"htmlContent\":{\"type\":\"string\",\"nullable\":true},\"contentAssetId\":{\"type\":\"string\",\"nullable\":true},\"crawledAt\":{\"type\":\"string\",\"nullable\":true},\"crawlStatus\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"success\",\"failure\",\"pending\"]},\"author\":{\"type\":\"string\",\"nullable\":true},\"publisher\":{\"type\":\"string\",\"nullable\":true},\"datePublished\":{\"type\":\"string\",\"nullable\":true},\"dateModified\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"type\",\"url\"]},{\"type\":\"object\",\"properties\":{\"type\":{\"type\":\"string\",\"enum\":[\"text\"]},\"text\":{\"type\":\"string\"},\"sourceUrl\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"type\",\"text\"]},{\"type\":\"object\",\"properties\":{\"type\":{\"type\":\"string\",\"enum\":[\"asset\"]},\"assetType\":{\"type\":\"string\",\"enum\":[\"image\",\"pdf\"]},\"assetId\":{\"type\":\"string\"},\"fileName\":{\"type\":\"string\",\"nullable\":true},\"sourceUrl\":{\"type\":\"string\",\"nullable\":true},\"size\":{\"type\":\"number\",\"nullable\":true},\"content\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"type\",\"assetType\",\"assetId\"]},{\"type\":\"object\",\"properties\":{\"type\":{\"type\":\"string\",\"enum\":[\"unknown\"]}},\"required\":[\"type\"]}]},\"createdAt\":{\"type\":\"string\"},\"favourited\":{\"type\":\"boolean\"},\"id\":{\"type\":\"string\"},\"modifiedAt\":{\"type\":\"string\",\"nullable\":true},\"note\":{\"type\":\"string\",\"nullable\":true},\"source\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"api\",\"web\",\"cli\",\"mobile\",\"extension\",\"singlefile\",\"rss\",\"import\",null]},\"summarizationStatus\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"success\",\"failure\",\"pending\",null]},\"summary\":{\"type\":\"string\",\"nullable\":true},\"taggingStatus\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"success\",\"failure\",\"pending\",null]},\"tags\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"},\"name\":{\"type\":\"string\"},\"attachedBy\":{\"type\":\"string\",\"enum\":[\"ai\",\"human\"]}},\"required\":[\"id\",\"name\",\"attachedBy\"]}},\"title\":{\"type\":\"string\",\"nullable\":true},\"userId\":{\"type\":\"string\"}},\"required\":[\"id\",\"createdAt\",\"modifiedAt\",\"archived\",\"favourited\",\"taggingStatus\",\"summarizationStatus\",\"userId\",\"tags\",\"content\",\"assets\"]},\"BookmarkId\":{\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"ieidlxygmwj87oxz5hxttoc8\"},\"Cursor\":{\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"File to be uploaded\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"Highlight\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"bookmarkId\":{\"type\":\"string\"},\"color\":{\"type\":\"string\",\"enum\":[\"yellow\",\"red\",\"green\",\"blue\"],\"default\":\"yellow\"},\"createdAt\":{\"type\":\"string\"},\"endOffset\":{\"type\":\"number\"},\"id\":{\"type\":\"string\"},\"note\":{\"type\":\"string\",\"nullable\":true},\"startOffset\":{\"type\":\"number\"},\"text\":{\"type\":\"string\",\"nullable\":true},\"userId\":{\"type\":\"string\"}},\"required\":[\"bookmarkId\",\"startOffset\",\"endOffset\",\"text\",\"note\",\"id\",\"userId\",\"createdAt\"]},\"HighlightId\":{\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"ieidlxygmwj87oxz5hxttoc8\"},\"List\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"description\":{\"type\":\"string\",\"nullable\":true},\"hasCollaborators\":{\"type\":\"boolean\"},\"icon\":{\"type\":\"string\"},\"id\":{\"type\":\"string\"},\"name\":{\"type\":\"string\"},\"parentId\":{\"type\":\"string\",\"nullable\":true},\"public\":{\"type\":\"boolean\"},\"query\":{\"type\":\"string\",\"nullable\":true},\"type\":{\"type\":\"string\",\"enum\":[\"manual\",\"smart\"],\"default\":\"manual\"},\"userRole\":{\"type\":\"string\",\"enum\":[\"owner\",\"editor\",\"viewer\",\"public\"]}},\"required\":[\"id\",\"name\",\"icon\",\"parentId\",\"public\",\"hasCollaborators\",\"userRole\"]},\"ListId\":{\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"ieidlxygmwj87oxz5hxttoc8\"},\"PaginatedBookmarks\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"bookmarks\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Bookmark\"}},\"nextCursor\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"bookmarks\",\"nextCursor\"]},\"PaginatedHighlights\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"highlights\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Highlight\"}},\"nextCursor\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"highlights\",\"nextCursor\"]},\"Tag\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"type\":\"string\"},\"name\":{\"type\":\"string\"},\"numBookmarks\":{\"type\":\"number\"},\"numBookmarksByAttachedType\":{\"type\":\"object\",\"properties\":{\"ai\":{\"type\":\"number\"},\"human\":{\"type\":\"number\"}}}},\"required\":[\"id\",\"name\",\"numBookmarks\",\"numBookmarksByAttachedType\"]},\"TagId\":{\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"ieidlxygmwj87oxz5hxttoc8\"}}"

type t = Openapi.Runtime.Client.t

let of_fetch ?max_response_bytes ~base_url session =
  Openapi.Runtime.Client.of_fetch ?max_response_bytes ~base_url session

let create ?session ?max_response_bytes ~sw env ~base_url =
  let session = match session with
    | Some s -> Fetch.restrict s
    | None -> Fetch_curl.std ~sw env
  in
  of_fetch ?max_response_bytes ~base_url session

let base_url = Openapi.Runtime.Client.base_url
let session = Openapi.Runtime.Client.session

module TagId = struct
  module Types = struct
    module T = struct
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "TagId" jsont
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
      |> Jsont.Object.mem "numBookmarks" Openapi.Runtime.number_jsont ~enc:(fun r -> r.num_bookmarks)
      |> Jsont.Object.mem "numBookmarksByAttachedType" Jsont.json ~enc:(fun r -> r.num_bookmarks_by_attached_type)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Tag" jsont
  end

  (** Get a single tag

      Get tag by its id *)
  let get_tags_by_tag_id ~tag_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("tagId", tag_id)] "/tags/{tagId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Tag\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Tag\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_tags_by_tag_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module ListId = struct
  module Types = struct
    module T = struct
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "ListId" jsont
  end
end

module List = struct
  module Types = struct
    module T = struct
      type t = {
        description : string option option;
        has_collaborators : bool;
        icon : string;
        id : string;
        name : string;
        parent_id : string option;
        public : bool;
        query : string option option;
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
      |> Jsont.Object.opt_mem "description" (Jsont.option Jsont.string) ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "hasCollaborators" Jsont.bool ~enc:(fun r -> r.has_collaborators)
      |> Jsont.Object.mem "icon" Jsont.string ~enc:(fun r -> r.icon)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "parentId" (Jsont.option Jsont.string) ~enc:(fun r -> r.parent_id)
      |> Jsont.Object.mem "public" Jsont.bool ~enc:(fun r -> r.public)
      |> Jsont.Object.opt_mem "query" (Jsont.option Jsont.string) ~enc:(fun r -> r.query)
      |> Jsont.Object.mem "type" Jsont.string ~dec_absent:(fun () -> "manual") ~enc:(fun r -> r.type_)
      |> Jsont.Object.mem "userRole" Jsont.string ~enc:(fun r -> r.user_role)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "List" jsont
  end

  (** Create a new list

      Create a new list *)
  let post_lists ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/lists" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"description\":{\"type\":\"string\",\"minLength\":0,\"maxLength\":500},\"icon\":{\"type\":\"string\"},\"name\":{\"type\":\"string\",\"minLength\":1,\"maxLength\":100},\"parentId\":{\"type\":\"string\",\"nullable\":true},\"query\":{\"type\":\"string\",\"minLength\":1},\"type\":{\"type\":\"string\",\"enum\":[\"manual\",\"smart\"],\"default\":\"manual\"}},\"required\":[\"name\",\"icon\"]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("201", "{\"$ref\":\"#/components/schemas/List\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/List\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None))]
      ~operation:"post_lists" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Get a single list

      Get list by its id *)
  let get_lists_by_list_id ~list_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("listId", list_id)] "/lists/{listId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/List\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/List\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_lists_by_list_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Update a list

      Update list by its id *)
  let patch_lists_by_list_id ~list_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("listId", list_id)] "/lists/{listId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"description\":{\"type\":\"string\",\"nullable\":true,\"minLength\":0,\"maxLength\":500},\"icon\":{\"type\":\"string\"},\"name\":{\"type\":\"string\",\"minLength\":1,\"maxLength\":100},\"parentId\":{\"type\":\"string\",\"nullable\":true},\"public\":{\"type\":\"boolean\"},\"query\":{\"type\":\"string\",\"minLength\":1}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/List\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/List\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"patch_lists_by_list_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PATCH
end

module HighlightId = struct
  module Types = struct
    module T = struct
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "HighlightId" jsont
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
      |> Jsont.Object.mem "color" Jsont.string ~dec_absent:(fun () -> "yellow") ~enc:(fun r -> r.color)
      |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "endOffset" Openapi.Runtime.number_jsont ~enc:(fun r -> r.end_offset)
      |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "note" (Jsont.option Jsont.string) ~enc:(fun r -> r.note)
      |> Jsont.Object.mem "startOffset" Openapi.Runtime.number_jsont ~enc:(fun r -> r.start_offset)
      |> Jsont.Object.mem "text" (Jsont.option Jsont.string) ~enc:(fun r -> r.text)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Highlight" jsont
  end

  (** Create a new highlight

      Create a new highlight *)
  let post_highlights ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/highlights" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"bookmarkId\":{\"type\":\"string\"},\"color\":{\"type\":\"string\",\"enum\":[\"yellow\",\"red\",\"green\",\"blue\"],\"default\":\"yellow\"},\"endOffset\":{\"type\":\"number\"},\"note\":{\"type\":\"string\",\"nullable\":true},\"startOffset\":{\"type\":\"number\"},\"text\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"bookmarkId\",\"startOffset\",\"endOffset\",\"text\",\"note\"]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("201", "{\"$ref\":\"#/components/schemas/Highlight\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Highlight\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"post_highlights" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Get a single highlight

      Get highlight by its id *)
  let get_highlights_by_highlight_id ~highlight_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("highlightId", highlight_id)] "/highlights/{highlightId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Highlight\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Highlight\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_highlights_by_highlight_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Delete a highlight

      Delete highlight by its id *)
  let delete_highlights_by_highlight_id ~highlight_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("highlightId", highlight_id)] "/highlights/{highlightId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Highlight\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Highlight\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_highlights_by_highlight_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Update a highlight

      Update highlight by its id *)
  let patch_highlights_by_highlight_id ~highlight_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("highlightId", highlight_id)] "/highlights/{highlightId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"color\":{\"type\":\"string\",\"enum\":[\"yellow\",\"red\",\"green\",\"blue\"]},\"note\":{\"type\":\"string\",\"nullable\":true}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Highlight\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Highlight\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"patch_highlights_by_highlight_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PATCH
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
      |> Jsont.Object.mem "nextCursor" (Jsont.option Jsont.string) ~enc:(fun r -> r.next_cursor)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PaginatedHighlights" jsont
  end

  (** Get all highlights

      Get all highlights *)
  let get_highlights ?limit ?cursor client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/highlights" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"limit" ~value:limit; Openapi.Runtime.Query.optional ~key:"cursor" ~value:cursor]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/PaginatedHighlights\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PaginatedHighlights\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_highlights" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
    let v () = Jsont.Object ([], Jsont.Meta.none)

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "File to be uploaded" jsont
  end
end

module Cursor = struct
  module Types = struct
    module T = struct
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Cursor" jsont
  end
end

module Client = struct
  (** Update user

      Update a user's role, bookmark quota, or storage quota. Admin access required. *)
  let put_admin_users_by_user_id ~user_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/admin/users/{userId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"description\":\"User update data\",\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"bookmarkQuota\":{\"type\":\"integer\",\"nullable\":true,\"minimum\":0},\"browserCrawlingEnabled\":{\"type\":\"boolean\",\"nullable\":true},\"role\":{\"type\":\"string\",\"enum\":[\"user\",\"admin\"]},\"storageQuota\":{\"type\":\"integer\",\"nullable\":true,\"minimum\":0}},\"required\":[],\"example\":{\"role\":\"admin\",\"bookmarkQuota\":1000,\"storageQuota\":5000000000}}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"success\":{\"type\":\"boolean\"}},\"required\":[\"success\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"success\":{\"type\":\"boolean\"}},\"required\":[\"success\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("401", (fun _ -> None)); ("403", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"put_admin_users_by_user_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Get a single asset

      Get asset by its id *)
  let get_assets_by_asset_id ~asset_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("assetId", asset_id)] "/assets/{assetId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_assets_by_asset_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get all backups

      Get all backups *)
  let get_backups client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/backups" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"backups\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"},\"userId\":{\"type\":\"string\"},\"assetId\":{\"type\":\"string\",\"nullable\":true},\"createdAt\":{\"type\":\"string\"},\"size\":{\"type\":\"number\"},\"bookmarkCount\":{\"type\":\"number\"},\"status\":{\"type\":\"string\",\"enum\":[\"pending\",\"success\",\"failure\"]},\"errorMessage\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"id\",\"userId\",\"assetId\",\"createdAt\",\"size\",\"bookmarkCount\",\"status\"]}}},\"required\":[\"backups\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"backups\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"},\"userId\":{\"type\":\"string\"},\"assetId\":{\"type\":\"string\",\"nullable\":true},\"createdAt\":{\"type\":\"string\"},\"size\":{\"type\":\"number\"},\"bookmarkCount\":{\"type\":\"number\"},\"status\":{\"type\":\"string\",\"enum\":[\"pending\",\"success\",\"failure\"]},\"errorMessage\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"id\",\"userId\",\"assetId\",\"createdAt\",\"size\",\"bookmarkCount\",\"status\"]}}},\"required\":[\"backups\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_backups" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Trigger a new backup

      Trigger a new backup *)
  let post_backups client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/backups" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("201", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"assetId\":{\"type\":\"string\",\"nullable\":true},\"bookmarkCount\":{\"type\":\"number\"},\"createdAt\":{\"type\":\"string\"},\"errorMessage\":{\"type\":\"string\",\"nullable\":true},\"id\":{\"type\":\"string\"},\"size\":{\"type\":\"number\"},\"status\":{\"type\":\"string\",\"enum\":[\"pending\",\"success\",\"failure\"]},\"userId\":{\"type\":\"string\"}},\"required\":[\"id\",\"userId\",\"assetId\",\"createdAt\",\"size\",\"bookmarkCount\",\"status\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"assetId\":{\"type\":\"string\",\"nullable\":true},\"bookmarkCount\":{\"type\":\"number\"},\"createdAt\":{\"type\":\"string\"},\"errorMessage\":{\"type\":\"string\",\"nullable\":true},\"id\":{\"type\":\"string\"},\"size\":{\"type\":\"number\"},\"status\":{\"type\":\"string\",\"enum\":[\"pending\",\"success\",\"failure\"]},\"userId\":{\"type\":\"string\"}},\"required\":[\"id\",\"userId\",\"assetId\",\"createdAt\",\"size\",\"bookmarkCount\",\"status\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_backups" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Get a single backup

      Get backup by its id *)
  let get_backups_by_backup_id ~backup_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("backupId", backup_id)] "/backups/{backupId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"assetId\":{\"type\":\"string\",\"nullable\":true},\"bookmarkCount\":{\"type\":\"number\"},\"createdAt\":{\"type\":\"string\"},\"errorMessage\":{\"type\":\"string\",\"nullable\":true},\"id\":{\"type\":\"string\"},\"size\":{\"type\":\"number\"},\"status\":{\"type\":\"string\",\"enum\":[\"pending\",\"success\",\"failure\"]},\"userId\":{\"type\":\"string\"}},\"required\":[\"id\",\"userId\",\"assetId\",\"createdAt\",\"size\",\"bookmarkCount\",\"status\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"assetId\":{\"type\":\"string\",\"nullable\":true},\"bookmarkCount\":{\"type\":\"number\"},\"createdAt\":{\"type\":\"string\"},\"errorMessage\":{\"type\":\"string\",\"nullable\":true},\"id\":{\"type\":\"string\"},\"size\":{\"type\":\"number\"},\"status\":{\"type\":\"string\",\"enum\":[\"pending\",\"success\",\"failure\"]},\"userId\":{\"type\":\"string\"}},\"required\":[\"id\",\"userId\",\"assetId\",\"createdAt\",\"size\",\"bookmarkCount\",\"status\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_backups_by_backup_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Delete a backup

      Delete backup by its id *)
  let delete_backups_by_backup_id ~backup_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("backupId", backup_id)] "/backups/{backupId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_backups_by_backup_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Download a backup

      Download backup file *)
  let get_backups_by_backup_id_download ~backup_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("backupId", backup_id)] "/backups/{backupId}/download" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/zip"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Media.of_strings ~accept:["application/zip"] "application/octet-stream" ~encode:(fun s -> s) ~decode:(fun s -> Ok s)) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_backups_by_backup_id_download" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Delete a bookmark

      Delete bookmark by its id *)
  let delete_bookmarks_by_bookmark_id ~bookmark_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_bookmarks_by_bookmark_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Update a bookmark

      Update bookmark by its id *)
  let patch_bookmarks_by_bookmark_id ~bookmark_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"archived\":{\"type\":\"boolean\"},\"assetContent\":{\"type\":\"string\",\"nullable\":true},\"author\":{\"type\":\"string\",\"nullable\":true},\"createdAt\":{\"type\":\"string\",\"nullable\":true},\"dateModified\":{\"type\":\"string\",\"nullable\":true},\"datePublished\":{\"type\":\"string\",\"nullable\":true},\"description\":{\"type\":\"string\",\"nullable\":true},\"favourited\":{\"type\":\"boolean\"},\"note\":{\"type\":\"string\"},\"publisher\":{\"type\":\"string\",\"nullable\":true},\"summary\":{\"type\":\"string\",\"nullable\":true},\"text\":{\"type\":\"string\",\"nullable\":true},\"title\":{\"type\":\"string\",\"nullable\":true,\"maxLength\":1000},\"url\":{\"type\":\"string\",\"format\":\"uri\"}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"archived\":{\"type\":\"boolean\"},\"createdAt\":{\"type\":\"string\"},\"favourited\":{\"type\":\"boolean\"},\"id\":{\"type\":\"string\"},\"modifiedAt\":{\"type\":\"string\",\"nullable\":true},\"note\":{\"type\":\"string\",\"nullable\":true},\"source\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"api\",\"web\",\"cli\",\"mobile\",\"extension\",\"singlefile\",\"rss\",\"import\"]},\"summarizationStatus\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"success\",\"failure\",\"pending\"]},\"summary\":{\"type\":\"string\",\"nullable\":true},\"taggingStatus\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"success\",\"failure\",\"pending\"]},\"title\":{\"type\":\"string\",\"nullable\":true},\"userId\":{\"type\":\"string\"}},\"required\":[\"id\",\"createdAt\",\"modifiedAt\",\"archived\",\"favourited\",\"taggingStatus\",\"summarizationStatus\",\"userId\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"archived\":{\"type\":\"boolean\"},\"createdAt\":{\"type\":\"string\"},\"favourited\":{\"type\":\"boolean\"},\"id\":{\"type\":\"string\"},\"modifiedAt\":{\"type\":\"string\",\"nullable\":true},\"note\":{\"type\":\"string\",\"nullable\":true},\"source\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"api\",\"web\",\"cli\",\"mobile\",\"extension\",\"singlefile\",\"rss\",\"import\"]},\"summarizationStatus\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"success\",\"failure\",\"pending\"]},\"summary\":{\"type\":\"string\",\"nullable\":true},\"taggingStatus\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"success\",\"failure\",\"pending\"]},\"title\":{\"type\":\"string\",\"nullable\":true},\"userId\":{\"type\":\"string\"}},\"required\":[\"id\",\"createdAt\",\"modifiedAt\",\"archived\",\"favourited\",\"taggingStatus\",\"summarizationStatus\",\"userId\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"patch_bookmarks_by_bookmark_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PATCH

  (** Attach asset

      Attach a new asset to a bookmark *)
  let post_bookmarks_by_bookmark_id_assets ~bookmark_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}/assets" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"assetType\":{\"type\":\"string\",\"enum\":[\"linkHtmlContent\",\"screenshot\",\"pdf\",\"assetScreenshot\",\"bannerImage\",\"fullPageArchive\",\"video\",\"bookmarkAsset\",\"precrawledArchive\",\"userUploaded\",\"avatar\",\"unknown\"]},\"id\":{\"type\":\"string\"}},\"required\":[\"id\",\"assetType\"]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("201", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"assetType\":{\"type\":\"string\",\"enum\":[\"linkHtmlContent\",\"screenshot\",\"pdf\",\"assetScreenshot\",\"bannerImage\",\"fullPageArchive\",\"video\",\"bookmarkAsset\",\"precrawledArchive\",\"userUploaded\",\"avatar\",\"unknown\"]},\"fileName\":{\"type\":\"string\",\"nullable\":true},\"id\":{\"type\":\"string\"}},\"required\":[\"id\",\"assetType\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"assetType\":{\"type\":\"string\",\"enum\":[\"linkHtmlContent\",\"screenshot\",\"pdf\",\"assetScreenshot\",\"bannerImage\",\"fullPageArchive\",\"video\",\"bookmarkAsset\",\"precrawledArchive\",\"userUploaded\",\"avatar\",\"unknown\"]},\"fileName\":{\"type\":\"string\",\"nullable\":true},\"id\":{\"type\":\"string\"}},\"required\":[\"id\",\"assetType\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"post_bookmarks_by_bookmark_id_assets" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Replace asset

      Replace an existing asset with a new one *)
  let put_bookmarks_by_bookmark_id_assets_by_asset_id ~bookmark_id ~asset_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id); ("assetId", asset_id)] "/bookmarks/{bookmarkId}/assets/{assetId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"assetId\":{\"type\":\"string\"}},\"required\":[\"assetId\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"put_bookmarks_by_bookmark_id_assets_by_asset_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Detach asset

      Detach an asset from a bookmark *)
  let delete_bookmarks_by_bookmark_id_assets_by_asset_id ~bookmark_id ~asset_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id); ("assetId", asset_id)] "/bookmarks/{bookmarkId}/assets/{assetId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_bookmarks_by_bookmark_id_assets_by_asset_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Get highlights of a bookmark

      Get highlights of a bookmark *)
  let get_bookmarks_by_bookmark_id_highlights ~bookmark_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}/highlights" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"highlights\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Highlight\"}}},\"required\":[\"highlights\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"highlights\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Highlight\"}}},\"required\":[\"highlights\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_bookmarks_by_bookmark_id_highlights" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get lists of a bookmark

      Get lists of a bookmark *)
  let get_bookmarks_by_bookmark_id_lists ~bookmark_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}/lists" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"lists\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/List\"}}},\"required\":[\"lists\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"lists\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/List\"}}},\"required\":[\"lists\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_bookmarks_by_bookmark_id_lists" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Summarize a bookmark

      Attaches a summary to the bookmark and returns the updated record. *)
  let post_bookmarks_by_bookmark_id_summarize ~bookmark_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}/summarize" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"archived\":{\"type\":\"boolean\"},\"createdAt\":{\"type\":\"string\"},\"favourited\":{\"type\":\"boolean\"},\"id\":{\"type\":\"string\"},\"modifiedAt\":{\"type\":\"string\",\"nullable\":true},\"note\":{\"type\":\"string\",\"nullable\":true},\"source\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"api\",\"web\",\"cli\",\"mobile\",\"extension\",\"singlefile\",\"rss\",\"import\"]},\"summarizationStatus\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"success\",\"failure\",\"pending\"]},\"summary\":{\"type\":\"string\",\"nullable\":true},\"taggingStatus\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"success\",\"failure\",\"pending\"]},\"title\":{\"type\":\"string\",\"nullable\":true},\"userId\":{\"type\":\"string\"}},\"required\":[\"id\",\"createdAt\",\"modifiedAt\",\"archived\",\"favourited\",\"taggingStatus\",\"summarizationStatus\",\"userId\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"archived\":{\"type\":\"boolean\"},\"createdAt\":{\"type\":\"string\"},\"favourited\":{\"type\":\"boolean\"},\"id\":{\"type\":\"string\"},\"modifiedAt\":{\"type\":\"string\",\"nullable\":true},\"note\":{\"type\":\"string\",\"nullable\":true},\"source\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"api\",\"web\",\"cli\",\"mobile\",\"extension\",\"singlefile\",\"rss\",\"import\"]},\"summarizationStatus\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"success\",\"failure\",\"pending\"]},\"summary\":{\"type\":\"string\",\"nullable\":true},\"taggingStatus\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"success\",\"failure\",\"pending\"]},\"title\":{\"type\":\"string\",\"nullable\":true},\"userId\":{\"type\":\"string\"}},\"required\":[\"id\",\"createdAt\",\"modifiedAt\",\"archived\",\"favourited\",\"taggingStatus\",\"summarizationStatus\",\"userId\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"post_bookmarks_by_bookmark_id_summarize" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Attach tags to a bookmark

      Attach tags to a bookmark *)
  let post_bookmarks_by_bookmark_id_tags ~bookmark_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}/tags" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"tags\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"tagId\":{\"type\":\"string\"},\"tagName\":{\"type\":\"string\"},\"attachedBy\":{\"type\":\"string\",\"enum\":[\"ai\",\"human\"],\"default\":\"human\"}}}}},\"required\":[\"tags\"]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"attached\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/TagId\"}}},\"required\":[\"attached\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"attached\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/TagId\"}}},\"required\":[\"attached\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"post_bookmarks_by_bookmark_id_tags" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Detach tags from a bookmark

      Detach tags from a bookmark *)
  let delete_bookmarks_by_bookmark_id_tags ~bookmark_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}/tags" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"tags\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"tagId\":{\"type\":\"string\"},\"tagName\":{\"type\":\"string\"},\"attachedBy\":{\"type\":\"string\",\"enum\":[\"ai\",\"human\"],\"default\":\"human\"}}}}},\"required\":[\"tags\"]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"detached\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/TagId\"}}},\"required\":[\"detached\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"detached\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/TagId\"}}},\"required\":[\"detached\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_bookmarks_by_bookmark_id_tags" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Get all lists

      Get all lists *)
  let get_lists client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/lists" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"lists\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/List\"}}},\"required\":[\"lists\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"lists\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/List\"}}},\"required\":[\"lists\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_lists" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Delete a list

      Delete list by its id *)
  let delete_lists_by_list_id ~list_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("listId", list_id)] "/lists/{listId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_lists_by_list_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Add a bookmark to a list

      Add the bookmarks to a list *)
  let put_lists_by_list_id_bookmarks_by_bookmark_id ~list_id ~bookmark_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("listId", list_id); ("bookmarkId", bookmark_id)] "/lists/{listId}/bookmarks/{bookmarkId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"put_lists_by_list_id_bookmarks_by_bookmark_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Remove a bookmark from a list

      Remove the bookmarks from a list *)
  let delete_lists_by_list_id_bookmarks_by_bookmark_id ~list_id ~bookmark_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("listId", list_id); ("bookmarkId", bookmark_id)] "/lists/{listId}/bookmarks/{bookmarkId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"delete_lists_by_list_id_bookmarks_by_bookmark_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Get all tags

      Get all tags *)
  let get_tags ?name_contains ?sort ?attached_by ?cursor ?limit client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/tags" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"nameContains" ~value:name_contains; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"attachedBy" ~value:attached_by; Openapi.Runtime.Query.optional ~key:"cursor" ~value:cursor; Openapi.Runtime.Query.optional ~key:"limit" ~value:limit]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"nextCursor\":{\"type\":\"string\",\"nullable\":true},\"tags\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Tag\"}}},\"required\":[\"tags\",\"nextCursor\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"nextCursor\":{\"type\":\"string\",\"nullable\":true},\"tags\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Tag\"}}},\"required\":[\"tags\",\"nextCursor\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_tags" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Create a new tag

      Create a new tag *)
  let post_tags ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/tags" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"name\":{\"type\":\"string\"}},\"required\":[\"name\"]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("201", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"type\":\"string\"},\"name\":{\"type\":\"string\"}},\"required\":[\"id\",\"name\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"type\":\"string\"},\"name\":{\"type\":\"string\"}},\"required\":[\"id\",\"name\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_tags" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete a tag

      Delete tag by its id *)
  let delete_tags_by_tag_id ~tag_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("tagId", tag_id)] "/tags/{tagId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_tags_by_tag_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Update a tag

      Update tag by its id *)
  let patch_tags_by_tag_id ~tag_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("tagId", tag_id)] "/tags/{tagId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"name\":{\"type\":\"string\"}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"type\":\"string\"},\"name\":{\"type\":\"string\"}},\"required\":[\"id\",\"name\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"type\":\"string\"},\"name\":{\"type\":\"string\"}},\"required\":[\"id\",\"name\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"patch_tags_by_tag_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PATCH

  (** Get current user info

      Returns info about the current user *)
  let get_users_me client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/users/me" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"email\":{\"type\":\"string\",\"nullable\":true},\"id\":{\"type\":\"string\"},\"image\":{\"type\":\"string\",\"nullable\":true},\"localUser\":{\"type\":\"boolean\"},\"name\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"id\",\"localUser\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"email\":{\"type\":\"string\",\"nullable\":true},\"id\":{\"type\":\"string\"},\"image\":{\"type\":\"string\",\"nullable\":true},\"localUser\":{\"type\":\"boolean\"},\"name\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"id\",\"localUser\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_users_me" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get current user stats

      Returns stats about the current user *)
  let get_users_me_stats client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/users/me/stats" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"assetsByType\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"type\":{\"type\":\"string\"},\"count\":{\"type\":\"number\"},\"totalSize\":{\"type\":\"number\"}},\"required\":[\"type\",\"count\",\"totalSize\"]}},\"bookmarkingActivity\":{\"type\":\"object\",\"properties\":{\"thisWeek\":{\"type\":\"number\"},\"thisMonth\":{\"type\":\"number\"},\"thisYear\":{\"type\":\"number\"},\"byHour\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"hour\":{\"type\":\"number\"},\"count\":{\"type\":\"number\"}},\"required\":[\"hour\",\"count\"]}},\"byDayOfWeek\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"day\":{\"type\":\"number\"},\"count\":{\"type\":\"number\"}},\"required\":[\"day\",\"count\"]}}},\"required\":[\"thisWeek\",\"thisMonth\",\"thisYear\",\"byHour\",\"byDayOfWeek\"]},\"bookmarksBySource\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"source\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"api\",\"web\",\"cli\",\"mobile\",\"extension\",\"singlefile\",\"rss\",\"import\"]},\"count\":{\"type\":\"number\"}},\"required\":[\"source\",\"count\"]}},\"bookmarksByType\":{\"type\":\"object\",\"properties\":{\"link\":{\"type\":\"number\"},\"text\":{\"type\":\"number\"},\"asset\":{\"type\":\"number\"}},\"required\":[\"link\",\"text\",\"asset\"]},\"numArchived\":{\"type\":\"number\"},\"numBookmarks\":{\"type\":\"number\"},\"numFavorites\":{\"type\":\"number\"},\"numHighlights\":{\"type\":\"number\"},\"numLists\":{\"type\":\"number\"},\"numTags\":{\"type\":\"number\"},\"tagUsage\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"},\"count\":{\"type\":\"number\"}},\"required\":[\"name\",\"count\"]},\"maxItems\":10},\"topDomains\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"domain\":{\"type\":\"string\"},\"count\":{\"type\":\"number\"}},\"required\":[\"domain\",\"count\"]},\"maxItems\":10},\"totalAssetSize\":{\"type\":\"number\"}},\"required\":[\"numBookmarks\",\"numFavorites\",\"numArchived\",\"numTags\",\"numLists\",\"numHighlights\",\"bookmarksByType\",\"topDomains\",\"totalAssetSize\",\"assetsByType\",\"bookmarkingActivity\",\"tagUsage\",\"bookmarksBySource\"]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"assetsByType\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"type\":{\"type\":\"string\"},\"count\":{\"type\":\"number\"},\"totalSize\":{\"type\":\"number\"}},\"required\":[\"type\",\"count\",\"totalSize\"]}},\"bookmarkingActivity\":{\"type\":\"object\",\"properties\":{\"thisWeek\":{\"type\":\"number\"},\"thisMonth\":{\"type\":\"number\"},\"thisYear\":{\"type\":\"number\"},\"byHour\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"hour\":{\"type\":\"number\"},\"count\":{\"type\":\"number\"}},\"required\":[\"hour\",\"count\"]}},\"byDayOfWeek\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"day\":{\"type\":\"number\"},\"count\":{\"type\":\"number\"}},\"required\":[\"day\",\"count\"]}}},\"required\":[\"thisWeek\",\"thisMonth\",\"thisYear\",\"byHour\",\"byDayOfWeek\"]},\"bookmarksBySource\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"source\":{\"type\":\"string\",\"nullable\":true,\"enum\":[\"api\",\"web\",\"cli\",\"mobile\",\"extension\",\"singlefile\",\"rss\",\"import\"]},\"count\":{\"type\":\"number\"}},\"required\":[\"source\",\"count\"]}},\"bookmarksByType\":{\"type\":\"object\",\"properties\":{\"link\":{\"type\":\"number\"},\"text\":{\"type\":\"number\"},\"asset\":{\"type\":\"number\"}},\"required\":[\"link\",\"text\",\"asset\"]},\"numArchived\":{\"type\":\"number\"},\"numBookmarks\":{\"type\":\"number\"},\"numFavorites\":{\"type\":\"number\"},\"numHighlights\":{\"type\":\"number\"},\"numLists\":{\"type\":\"number\"},\"numTags\":{\"type\":\"number\"},\"tagUsage\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"},\"count\":{\"type\":\"number\"}},\"required\":[\"name\",\"count\"]},\"maxItems\":10},\"topDomains\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"domain\":{\"type\":\"string\"},\"count\":{\"type\":\"number\"}},\"required\":[\"domain\",\"count\"]},\"maxItems\":10},\"totalAssetSize\":{\"type\":\"number\"}},\"required\":[\"numBookmarks\",\"numFavorites\",\"numArchived\",\"numTags\",\"numLists\",\"numHighlights\",\"bookmarksByType\",\"topDomains\",\"totalAssetSize\",\"assetsByType\",\"bookmarkingActivity\",\"tagUsage\",\"bookmarksBySource\"]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_users_me_stats" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module BookmarkId = struct
  module Types = struct
    module T = struct
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "BookmarkId" jsont
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
        note : string option option;
        source : string option option;
        summarization_status : string option;
        summary : string option option;
        tagging_status : string option;
        tags : Jsont.json list;
        title : string option option;
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
      |> Jsont.Object.mem "modifiedAt" (Jsont.option Jsont.string) ~enc:(fun r -> r.modified_at)
      |> Jsont.Object.opt_mem "note" (Jsont.option Jsont.string) ~enc:(fun r -> r.note)
      |> Jsont.Object.opt_mem "source" (Jsont.option Jsont.string) ~enc:(fun r -> r.source)
      |> Jsont.Object.mem "summarizationStatus" (Jsont.option Jsont.string) ~enc:(fun r -> r.summarization_status)
      |> Jsont.Object.opt_mem "summary" (Jsont.option Jsont.string) ~enc:(fun r -> r.summary)
      |> Jsont.Object.mem "taggingStatus" (Jsont.option Jsont.string) ~enc:(fun r -> r.tagging_status)
      |> Jsont.Object.mem "tags" (Jsont.list Jsont.json) ~enc:(fun r -> r.tags)
      |> Jsont.Object.opt_mem "title" (Jsont.option Jsont.string) ~enc:(fun r -> r.title)
      |> Jsont.Object.mem "userId" Jsont.string ~enc:(fun r -> r.user_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Bookmark" jsont
  end

  (** Create a new bookmark

      Create a new bookmark *)
  let post_bookmarks ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/bookmarks" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"type\":\"object\",\"properties\":{\"title\":{\"type\":\"string\",\"nullable\":true,\"maxLength\":1000},\"archived\":{\"type\":\"boolean\"},\"favourited\":{\"type\":\"boolean\"},\"note\":{\"type\":\"string\"},\"summary\":{\"type\":\"string\"},\"createdAt\":{\"type\":\"string\",\"nullable\":true},\"crawlPriority\":{\"type\":\"string\",\"enum\":[\"low\",\"normal\"]},\"importSessionId\":{\"type\":\"string\"},\"source\":{\"type\":\"string\",\"enum\":[\"api\",\"web\",\"cli\",\"mobile\",\"extension\",\"singlefile\",\"rss\",\"import\"]}}},{\"oneOf\":[{\"type\":\"object\",\"properties\":{\"type\":{\"type\":\"string\",\"enum\":[\"link\"]},\"url\":{\"type\":\"string\",\"format\":\"uri\"},\"precrawledArchiveId\":{\"type\":\"string\"}},\"required\":[\"type\",\"url\"]},{\"type\":\"object\",\"properties\":{\"type\":{\"type\":\"string\",\"enum\":[\"text\"]},\"text\":{\"type\":\"string\"},\"sourceUrl\":{\"type\":\"string\"}},\"required\":[\"type\",\"text\"]},{\"type\":\"object\",\"properties\":{\"type\":{\"type\":\"string\",\"enum\":[\"asset\"]},\"assetType\":{\"type\":\"string\",\"enum\":[\"image\",\"pdf\"]},\"assetId\":{\"type\":\"string\"},\"fileName\":{\"type\":\"string\"},\"sourceUrl\":{\"type\":\"string\"}},\"required\":[\"type\",\"assetType\",\"assetId\"]}]}],\"properties\":{},\"required\":[]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Bookmark\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}"); ("201", "{\"$ref\":\"#/components/schemas/Bookmark\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Bookmark\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None))]
      ~operation:"post_bookmarks" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Get a single bookmark

      Get bookmark by its id
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  let get_bookmarks_by_bookmark_id ~bookmark_id ?include_content client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("bookmarkId", bookmark_id)] "/bookmarks/{bookmarkId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"includeContent" ~value:include_content]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Bookmark\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Bookmark\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_bookmarks_by_bookmark_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
      |> Jsont.Object.mem "nextCursor" (Jsont.option Jsont.string) ~enc:(fun r -> r.next_cursor)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PaginatedBookmarks" jsont
  end

  (** Get all bookmarks

      Get all bookmarks
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  let get_bookmarks ?archived ?favourited ?sort_order ?limit ?cursor ?include_content client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/bookmarks" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"archived" ~value:archived; Openapi.Runtime.Query.optional ~key:"favourited" ~value:favourited; Openapi.Runtime.Query.optional ~key:"sortOrder" ~value:sort_order; Openapi.Runtime.Query.optional ~key:"limit" ~value:limit; Openapi.Runtime.Query.optional ~key:"cursor" ~value:cursor; Openapi.Runtime.Query.optional ~key:"includeContent" ~value:include_content]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/PaginatedBookmarks\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PaginatedBookmarks\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_bookmarks" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Search bookmarks

      Search bookmarks
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  let get_bookmarks_search ~q ?sort_order ?limit ?cursor ?include_content client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/bookmarks/search" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"q" ~value:q; Openapi.Runtime.Query.optional ~key:"sortOrder" ~value:sort_order; Openapi.Runtime.Query.optional ~key:"limit" ~value:limit; Openapi.Runtime.Query.optional ~key:"cursor" ~value:cursor; Openapi.Runtime.Query.optional ~key:"includeContent" ~value:include_content]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/PaginatedBookmarks\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PaginatedBookmarks\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_bookmarks_search" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get bookmarks in the list

      Get bookmarks in the list
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  let get_lists_by_list_id_bookmarks ~list_id ?sort_order ?limit ?cursor ?include_content client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("listId", list_id)] "/lists/{listId}/bookmarks" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"sortOrder" ~value:sort_order; Openapi.Runtime.Query.optional ~key:"limit" ~value:limit; Openapi.Runtime.Query.optional ~key:"cursor" ~value:cursor; Openapi.Runtime.Query.optional ~key:"includeContent" ~value:include_content]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/PaginatedBookmarks\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PaginatedBookmarks\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_lists_by_list_id_bookmarks" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get bookmarks with the tag

      Get bookmarks with the tag
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  let get_tags_by_tag_id_bookmarks ~tag_id ?sort_order ?limit ?cursor ?include_content client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("tagId", tag_id)] "/tags/{tagId}/bookmarks" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"sortOrder" ~value:sort_order; Openapi.Runtime.Query.optional ~key:"limit" ~value:limit; Openapi.Runtime.Query.optional ~key:"cursor" ~value:cursor; Openapi.Runtime.Query.optional ~key:"includeContent" ~value:include_content]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/PaginatedBookmarks\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PaginatedBookmarks\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_tags_by_tag_id_bookmarks" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module BackupId = struct
  module Types = struct
    module T = struct
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "BackupId" jsont
  end
end

module AssetId = struct
  module Types = struct
    module T = struct
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "AssetId" jsont
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
      |> Jsont.Object.mem "size" Openapi.Runtime.number_jsont ~enc:(fun r -> r.size)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Asset" jsont
  end

  (** Upload a new asset

      Upload a new asset *)
  let post_assets ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/assets" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Asset\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Asset\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_assets" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST
end
