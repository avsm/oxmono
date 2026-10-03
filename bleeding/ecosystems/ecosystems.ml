(** {1 Ecosystems}

    An open API service providing package, version and dependency metadata of many open source software ecosystems and registries.

    @version 1.1.0 *)

let __openapi_schemas = Openapi.Schema.of_string ~version:"3.0.1" "{\"Advisory\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"classification\":{\"type\":\"string\",\"nullable\":true},\"created_at\":{\"type\":\"string\"},\"cvss_score\":{\"type\":\"number\",\"nullable\":true},\"cvss_vector\":{\"type\":\"string\",\"nullable\":true},\"description\":{\"type\":\"string\",\"nullable\":true},\"identifiers\":{\"type\":\"array\",\"items\":{\"type\":\"string\",\"nullable\":true}},\"origin\":{\"type\":\"string\",\"nullable\":true},\"packages\":{\"type\":\"array\",\"items\":{\"type\":\"object\"}},\"published_at\":{\"type\":\"string\",\"nullable\":true},\"references\":{\"type\":\"array\",\"items\":{\"type\":\"string\",\"nullable\":true}},\"severity\":{\"type\":\"string\",\"nullable\":true},\"source_kind\":{\"type\":\"string\",\"nullable\":true},\"title\":{\"type\":\"string\",\"nullable\":true},\"updated_at\":{\"type\":\"string\"},\"url\":{\"type\":\"string\",\"nullable\":true},\"uuid\":{\"type\":\"string\"},\"withdrawn_at\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"uuid\",\"url\",\"title\",\"description\",\"origin\",\"severity\",\"published_at\",\"withdrawn_at\",\"classification\",\"cvss_score\",\"cvss_vector\",\"references\",\"source_kind\",\"identifiers\",\"packages\",\"created_at\",\"updated_at\"]},\"CodeMeta\":{\"description\":\"CodeMeta JSON-LD metadata format for software packages, compatible with Software Heritage\",\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"@context\":{\"type\":\"string\",\"description\":\"JSON-LD context URL\",\"example\":\"https://w3id.org/codemeta/3.0\"},\"@type\":{\"type\":\"string\",\"description\":\"Type of software artifact\",\"example\":\"SoftwareSourceCode\"},\"applicationCategory\":{\"type\":\"string\",\"description\":\"Package ecosystem/category\",\"nullable\":true},\"author\":{\"type\":\"array\",\"description\":\"Package authors\",\"nullable\":true,\"items\":{\"type\":\"object\",\"properties\":{\"@type\":{\"type\":\"string\",\"example\":\"Person\"},\"name\":{\"type\":\"string\"},\"url\":{\"type\":\"string\",\"nullable\":true}}}},\"codeRepository\":{\"type\":\"string\",\"description\":\"Source code repository URL\",\"nullable\":true},\"copyrightHolder\":{\"type\":\"array\",\"description\":\"Copyright holders\",\"nullable\":true,\"items\":{\"type\":\"object\",\"properties\":{\"@type\":{\"type\":\"string\",\"example\":\"Person\"},\"name\":{\"type\":\"string\"},\"url\":{\"type\":\"string\",\"nullable\":true}}}},\"copyrightYear\":{\"type\":\"integer\",\"description\":\"Copyright year\",\"nullable\":true},\"dateCreated\":{\"type\":\"string\",\"format\":\"date\",\"description\":\"Creation date (ISO 8601)\",\"nullable\":true},\"dateModified\":{\"type\":\"string\",\"format\":\"date\",\"description\":\"Last modification date (ISO 8601)\",\"nullable\":true},\"datePublished\":{\"type\":\"string\",\"format\":\"date\",\"description\":\"Publication date (ISO 8601)\",\"nullable\":true},\"description\":{\"type\":\"string\",\"description\":\"Package description\",\"nullable\":true},\"developmentStatus\":{\"type\":\"string\",\"description\":\"Development status\",\"nullable\":true},\"downloadUrl\":{\"type\":\"string\",\"description\":\"Package download URL\",\"nullable\":true},\"funder\":{\"type\":\"array\",\"description\":\"Funding sources\",\"nullable\":true,\"items\":{\"type\":\"object\",\"properties\":{\"@type\":{\"type\":\"string\",\"example\":\"Organization\"},\"url\":{\"type\":\"string\"}}}},\"https://forgefed.org/ns#forks\":{\"type\":\"integer\",\"description\":\"Fork count (ForgeFed)\",\"nullable\":true},\"https://www.w3.org/ns/activitystreams#likes\":{\"type\":\"integer\",\"description\":\"Star/like count (ActivityStreams)\",\"nullable\":true},\"identifier\":{\"type\":\"string\",\"description\":\"Package URL (purl) identifier\",\"example\":\"pkg:cargo/rand@0.8.5\"},\"issueTracker\":{\"type\":\"string\",\"description\":\"Issue tracker URL\",\"nullable\":true},\"keywords\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"description\":\"Keywords and tags\",\"nullable\":true},\"license\":{\"oneOf\":[{\"type\":\"string\"},{\"type\":\"array\",\"items\":{\"type\":\"string\"}}],\"description\":\"SPDX license URL(s)\",\"nullable\":true},\"maintainer\":{\"type\":\"array\",\"description\":\"Package maintainers\",\"nullable\":true,\"items\":{\"type\":\"object\",\"properties\":{\"@type\":{\"type\":\"string\",\"example\":\"Person\"},\"name\":{\"type\":\"string\"},\"url\":{\"type\":\"string\",\"nullable\":true}}}},\"name\":{\"type\":\"string\",\"description\":\"Package name\"},\"programmingLanguage\":{\"type\":\"object\",\"description\":\"Programming language information\",\"nullable\":true,\"properties\":{\"@type\":{\"type\":\"string\",\"example\":\"ComputerLanguage\"},\"name\":{\"type\":\"string\",\"example\":\"Rust\"}}},\"runtimePlatform\":{\"type\":\"string\",\"description\":\"Runtime platform/ecosystem\",\"nullable\":true},\"sameAs\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"description\":\"Alternative identifiers/URLs\",\"nullable\":true},\"softwareHelp\":{\"type\":\"object\",\"description\":\"Documentation/help resources\",\"nullable\":true,\"properties\":{\"@type\":{\"type\":\"string\",\"example\":\"WebSite\"},\"url\":{\"type\":\"string\"}}},\"softwareVersion\":{\"type\":\"string\",\"description\":\"Software version\",\"nullable\":true},\"url\":{\"type\":\"string\",\"description\":\"Homepage URL\",\"nullable\":true},\"version\":{\"type\":\"string\",\"description\":\"Version number\",\"nullable\":true}},\"required\":[\"@context\",\"@type\",\"identifier\",\"name\"]},\"Dependency\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"ecosystem\":{\"type\":\"string\"},\"id\":{\"type\":\"integer\"},\"kind\":{\"type\":\"string\",\"nullable\":true},\"optional\":{\"type\":\"boolean\",\"nullable\":true},\"package_name\":{\"type\":\"string\"},\"requirements\":{\"type\":\"string\",\"nullable\":true}},\"required\":[\"id\",\"ecosystem\",\"package_name\",\"requirements\",\"kind\",\"optional\"]},\"Keyword\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"name\":{\"type\":\"string\"},\"packages_count\":{\"type\":\"integer\"},\"packages_url\":{\"type\":\"string\"}},\"required\":[\"name\",\"packages_count\"]},\"KeywordWithPackages\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"name\":{\"type\":\"string\"},\"packages\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Package\"}},\"packages_count\":{\"type\":\"integer\",\"nullable\":true},\"packages_url\":{\"type\":\"string\"},\"related_keywords\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Keyword\"}}},\"required\":[\"name\",\"packages_count\",\"related_keywords\",\"packages\"]},\"Maintainer\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"created_at\":{\"type\":\"string\",\"format\":\"date-time\"},\"email\":{\"type\":\"string\",\"nullable\":true},\"html_url\":{\"type\":\"string\",\"nullable\":true},\"login\":{\"type\":\"string\",\"nullable\":true},\"name\":{\"type\":\"string\",\"nullable\":true},\"packages_count\":{\"type\":\"integer\"},\"packages_url\":{\"type\":\"string\"},\"role\":{\"type\":\"string\",\"nullable\":true},\"total_downloads\":{\"type\":\"integer\"},\"updated_at\":{\"type\":\"string\",\"format\":\"date-time\"},\"url\":{\"type\":\"string\",\"nullable\":true},\"uuid\":{\"type\":\"string\"}},\"required\":[\"uuid\",\"login\",\"name\",\"email\",\"url\",\"created_at\",\"updated_at\",\"packages_count\",\"packages_url\",\"html_url\"]},\"Namespace\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"name\":{\"type\":\"string\"},\"packages_count\":{\"type\":\"integer\"},\"packages_url\":{\"type\":\"string\"}},\"required\":[\"name\",\"packages_count\",\"packages_url\"]},\"Package\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"advisories\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Advisory\"}},\"codemeta_url\":{\"type\":\"string\"},\"created_at\":{\"type\":\"string\",\"format\":\"date-time\"},\"critical\":{\"type\":\"boolean\",\"nullable\":true},\"dependent_packages_count\":{\"type\":\"integer\"},\"dependent_packages_url\":{\"type\":\"string\"},\"dependent_repos_count\":{\"type\":\"integer\"},\"dependent_repositories_url\":{\"type\":\"string\"},\"description\":{\"type\":\"string\",\"nullable\":true},\"docker_dependents_count\":{\"type\":\"integer\",\"nullable\":true},\"docker_downloads_count\":{\"type\":\"integer\",\"nullable\":true},\"docker_usage_url\":{\"type\":\"string\"},\"documentation_url\":{\"type\":\"string\",\"nullable\":true},\"downloads\":{\"type\":\"integer\",\"nullable\":true},\"downloads_period\":{\"type\":\"string\",\"nullable\":true},\"ecosystem\":{\"type\":\"string\"},\"first_release_published_at\":{\"type\":\"string\",\"format\":\"date-time\",\"nullable\":true},\"funding_links\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},\"homepage\":{\"type\":\"string\",\"nullable\":true},\"id\":{\"type\":\"integer\"},\"install_command\":{\"type\":\"string\",\"nullable\":true},\"issue_metadata\":{\"type\":\"object\",\"nullable\":true},\"keywords_array\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},\"last_synced_at\":{\"type\":\"string\",\"format\":\"date-time\",\"nullable\":true},\"latest_release_number\":{\"type\":\"string\",\"nullable\":true},\"latest_release_published_at\":{\"type\":\"string\",\"format\":\"date-time\",\"nullable\":true},\"latest_version_url\":{\"type\":\"string\"},\"licenses\":{\"type\":\"string\",\"nullable\":true},\"maintainers\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Maintainer\"}},\"metadata\":{\"type\":\"object\",\"nullable\":true},\"name\":{\"type\":\"string\"},\"namespace\":{\"type\":\"string\",\"nullable\":true},\"normalized_licenses\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},\"purl\":{\"type\":\"string\"},\"rankings\":{\"type\":\"object\"},\"registry_url\":{\"type\":\"string\",\"nullable\":true},\"related_packages_url\":{\"type\":\"string\"},\"repo_metadata\":{\"type\":\"object\",\"nullable\":true},\"repo_metadata_updated_at\":{\"type\":\"string\",\"format\":\"date-time\",\"nullable\":true},\"repository_url\":{\"type\":\"string\",\"nullable\":true},\"status\":{\"type\":\"string\",\"nullable\":true},\"updated_at\":{\"type\":\"string\",\"format\":\"date-time\"},\"usage_url\":{\"type\":\"string\"},\"version_numbers_url\":{\"type\":\"string\"},\"versions_count\":{\"type\":\"integer\"},\"versions_url\":{\"type\":\"string\"}},\"required\":[\"id\",\"name\",\"ecosystem\",\"description\",\"homepage\",\"licenses\",\"normalized_licenses\",\"repository_url\",\"keywords_array\",\"namespace\",\"versions_count\",\"first_release_published_at\",\"latest_release_published_at\",\"latest_release_number\",\"last_synced_at\",\"created_at\",\"updated_at\",\"registry_url\",\"documentation_url\",\"install_command\",\"metadata\",\"repo_metadata\",\"repo_metadata_updated_at\",\"dependent_packages_count\",\"downloads\",\"downloads_period\",\"dependent_repos_count\",\"rankings\",\"purl\",\"advisories\",\"versions_url\",\"latest_version_url\",\"dependent_packages_url\",\"related_packages_url\",\"codemeta_url\",\"docker_usage_url\",\"docker_dependents_count\",\"docker_downloads_count\",\"maintainers\",\"usage_url\",\"dependent_repositories_url\",\"status\",\"funding_links\",\"critical\",\"issue_metadata\"]},\"PackageWithRegistry\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/Package\"},{\"type\":\"object\",\"properties\":{\"registry\":{\"$ref\":\"#/components/schemas/Registry\"}},\"required\":[\"registry\"]}],\"properties\":{},\"required\":[]},\"Registry\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"created_at\":{\"type\":\"string\",\"format\":\"date-time\"},\"default\":{\"type\":\"boolean\"},\"downloads\":{\"type\":\"integer\",\"format\":\"int64\"},\"ecosystem\":{\"type\":\"string\"},\"github\":{\"type\":\"string\",\"nullable\":true},\"icon_url\":{\"type\":\"string\"},\"keywords_count\":{\"type\":\"integer\",\"format\":\"int64\"},\"maintainers_count\":{\"type\":\"integer\",\"format\":\"int64\"},\"maintainers_url\":{\"type\":\"string\"},\"metadata\":{\"type\":\"object\",\"nullable\":true},\"name\":{\"type\":\"string\"},\"namespaces_count\":{\"type\":\"integer\",\"format\":\"int64\"},\"packages_count\":{\"type\":\"integer\",\"format\":\"int64\"},\"packages_url\":{\"type\":\"string\"},\"purl_type\":{\"type\":\"string\"},\"updated_at\":{\"type\":\"string\",\"format\":\"date-time\"},\"url\":{\"type\":\"string\"},\"versions_count\":{\"type\":\"integer\",\"format\":\"int64\"}},\"required\":[\"name\",\"url\",\"ecosystem\",\"default\",\"packages_count\",\"maintainers_count\",\"namespaces_count\",\"keywords_count\",\"github\",\"metadata\",\"created_at\",\"updated_at\",\"packages_url\",\"maintainers_url\",\"icon_url\"]},\"Version\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"codemeta_url\":{\"type\":\"string\"},\"created_at\":{\"type\":\"string\",\"format\":\"date-time\"},\"documentation_url\":{\"type\":\"string\",\"nullable\":true},\"download_url\":{\"type\":\"string\",\"nullable\":true},\"id\":{\"type\":\"integer\"},\"install_command\":{\"type\":\"string\",\"nullable\":true},\"integrity\":{\"type\":\"string\",\"nullable\":true},\"latest\":{\"type\":\"boolean\"},\"licenses\":{\"type\":\"string\",\"nullable\":true},\"metadata\":{\"type\":\"object\",\"nullable\":true},\"number\":{\"type\":\"string\"},\"published_at\":{\"type\":\"string\",\"nullable\":true},\"purl\":{\"type\":\"string\"},\"registry_url\":{\"type\":\"string\",\"nullable\":true},\"related_tag\":{\"type\":\"object\",\"nullable\":true},\"status\":{\"type\":\"string\",\"nullable\":true},\"updated_at\":{\"type\":\"string\",\"format\":\"date-time\"},\"version_url\":{\"type\":\"string\"}},\"required\":[\"id\",\"number\",\"published_at\",\"licenses\",\"integrity\",\"status\",\"download_url\",\"registry_url\",\"documentation_url\",\"install_command\",\"metadata\",\"created_at\",\"updated_at\",\"purl\",\"version_url\",\"related_tag\",\"latest\"]},\"VersionLookup\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/VersionWithDependencies\"},{\"type\":\"object\",\"required\":[\"package\"],\"properties\":{\"package\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}}],\"properties\":{},\"required\":[]},\"VersionWithDependencies\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"codemeta_url\":{\"type\":\"string\"},\"created_at\":{\"type\":\"string\",\"format\":\"date-time\"},\"dependencies\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Dependency\"}},\"documentation_url\":{\"type\":\"string\",\"nullable\":true},\"download_url\":{\"type\":\"string\",\"nullable\":true},\"id\":{\"type\":\"integer\"},\"install_command\":{\"type\":\"string\",\"nullable\":true},\"integrity\":{\"type\":\"string\",\"nullable\":true},\"latest\":{\"type\":\"boolean\"},\"licenses\":{\"type\":\"string\",\"nullable\":true},\"metadata\":{\"type\":\"object\",\"nullable\":true},\"number\":{\"type\":\"string\"},\"published_at\":{\"type\":\"string\",\"nullable\":true},\"purl\":{\"type\":\"string\"},\"registry_url\":{\"type\":\"string\",\"nullable\":true},\"related_tag\":{\"type\":\"object\",\"nullable\":true},\"status\":{\"type\":\"string\",\"nullable\":true},\"updated_at\":{\"type\":\"string\",\"format\":\"date-time\"},\"version_url\":{\"type\":\"string\"}},\"required\":[\"number\",\"published_at\",\"licenses\",\"integrity\",\"status\",\"download_url\",\"registry_url\",\"documentation_url\",\"install_command\",\"metadata\",\"created_at\",\"updated_at\",\"purl\",\"version_url\",\"codemeta_url\",\"related_tag\",\"latest\",\"dependencies\"]},\"VersionWithPackage\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"codemeta_url\":{\"type\":\"string\"},\"created_at\":{\"type\":\"string\",\"format\":\"date-time\"},\"documentation_url\":{\"type\":\"string\",\"nullable\":true},\"download_url\":{\"type\":\"string\",\"nullable\":true},\"id\":{\"type\":\"integer\"},\"install_command\":{\"type\":\"string\",\"nullable\":true},\"integrity\":{\"type\":\"string\",\"nullable\":true},\"latest\":{\"type\":\"boolean\"},\"licenses\":{\"type\":\"string\",\"nullable\":true},\"metadata\":{\"type\":\"object\",\"nullable\":true},\"number\":{\"type\":\"string\"},\"package_url\":{\"type\":\"string\"},\"published_at\":{\"type\":\"string\",\"nullable\":true},\"purl\":{\"type\":\"string\"},\"registry_url\":{\"type\":\"string\",\"nullable\":true},\"status\":{\"type\":\"string\",\"nullable\":true},\"updated_at\":{\"type\":\"string\",\"format\":\"date-time\"},\"version_url\":{\"type\":\"string\"}},\"required\":[\"id\",\"number\",\"published_at\",\"licenses\",\"integrity\",\"status\",\"download_url\",\"registry_url\",\"documentation_url\",\"install_command\",\"metadata\",\"created_at\",\"updated_at\",\"purl\",\"version_url\",\"codemeta_url\",\"related_tag\",\"latest\",\"package_url\"]}}"

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

module VersionWithPackage = struct
  module Types = struct
    module T = struct
      type t = {
        codemeta_url : string;
        created_at : Ptime.t;
        documentation_url : string option;
        download_url : string option;
        id : int;
        install_command : string option;
        integrity : string option;
        latest : bool;
        licenses : string option;
        metadata : Jsont.json option;
        number : string;
        package_url : string;
        published_at : string option;
        purl : string;
        registry_url : string option;
        status : string option;
        updated_at : Ptime.t;
        version_url : string;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~codemeta_url ~created_at ~id ~latest ~number ~package_url ~purl ~updated_at ~version_url ?documentation_url ?download_url ?install_command ?integrity ?licenses ?metadata ?published_at ?registry_url ?status () = { codemeta_url; created_at; documentation_url; download_url; id; install_command; integrity; latest; licenses; metadata; number; package_url; published_at; purl; registry_url; status; updated_at; version_url }

    let codemeta_url t = t.codemeta_url
    let created_at t = t.created_at
    let documentation_url t = t.documentation_url
    let download_url t = t.download_url
    let id t = t.id
    let install_command t = t.install_command
    let integrity t = t.integrity
    let latest t = t.latest
    let licenses t = t.licenses
    let metadata t = t.metadata
    let number t = t.number
    let package_url t = t.package_url
    let published_at t = t.published_at
    let purl t = t.purl
    let registry_url t = t.registry_url
    let status t = t.status
    let updated_at t = t.updated_at
    let version_url t = t.version_url

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VersionWithPackage"
        (fun codemeta_url created_at documentation_url download_url id install_command integrity latest licenses metadata number package_url published_at purl registry_url status updated_at version_url -> { codemeta_url; created_at; documentation_url; download_url; id; install_command; integrity; latest; licenses; metadata; number; package_url; published_at; purl; registry_url; status; updated_at; version_url })
      |> Jsont.Object.mem "codemeta_url" Jsont.string ~enc:(fun r -> r.codemeta_url)
      |> Jsont.Object.mem "created_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "documentation_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.documentation_url)
      |> Jsont.Object.mem "download_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.download_url)
      |> Jsont.Object.mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "install_command" (Jsont.option Jsont.string) ~enc:(fun r -> r.install_command)
      |> Jsont.Object.mem "integrity" (Jsont.option Jsont.string) ~enc:(fun r -> r.integrity)
      |> Jsont.Object.mem "latest" Jsont.bool ~enc:(fun r -> r.latest)
      |> Jsont.Object.mem "licenses" (Jsont.option Jsont.string) ~enc:(fun r -> r.licenses)
      |> Jsont.Object.mem "metadata" (Jsont.option Jsont.json) ~enc:(fun r -> r.metadata)
      |> Jsont.Object.mem "number" Jsont.string ~enc:(fun r -> r.number)
      |> Jsont.Object.mem "package_url" Jsont.string ~enc:(fun r -> r.package_url)
      |> Jsont.Object.mem "published_at" (Jsont.option Jsont.string) ~enc:(fun r -> r.published_at)
      |> Jsont.Object.mem "purl" Jsont.string ~enc:(fun r -> r.purl)
      |> Jsont.Object.mem "registry_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.registry_url)
      |> Jsont.Object.mem "status" (Jsont.option Jsont.string) ~enc:(fun r -> r.status)
      |> Jsont.Object.mem "updated_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "version_url" Jsont.string ~enc:(fun r -> r.version_url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VersionWithPackage" jsont
  end

  (** get a list of recently published versions from a registry
      @param registry_name name of registry
      @param page pagination page number
      @param per_page Number of records to return
      @param created_after filter by created_at after given time
      @param updated_after filter by updated_at after given time
      @param published_after filter by published_at after given time
      @param published_before filter by published_at before given time
      @param created_before filter by created_at before given time
      @param updated_before filter by updated_at before given time
      @param sort field to order results by
      @param order direction to order results by
  *)
  let get_registry_recent_versions ~registry_name ?page ?per_page ?created_after ?updated_after ?published_after ?published_before ?created_before ?updated_before ?sort ?order client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name)] "/registries/{registryName}/versions" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page; Openapi.Runtime.Query.optional ~key:"created_after" ~value:created_after; Openapi.Runtime.Query.optional ~key:"updated_after" ~value:updated_after; Openapi.Runtime.Query.optional ~key:"published_after" ~value:published_after; Openapi.Runtime.Query.optional ~key:"published_before" ~value:published_before; Openapi.Runtime.Query.optional ~key:"created_before" ~value:created_before; Openapi.Runtime.Query.optional ~key:"updated_before" ~value:updated_before; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/VersionWithPackage\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/VersionWithPackage\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_recent_versions" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module Version = struct
  module Types = struct
    module T = struct
      type t = {
        codemeta_url : string option;
        created_at : Ptime.t;
        documentation_url : string option;
        download_url : string option;
        id : int;
        install_command : string option;
        integrity : string option;
        latest : bool;
        licenses : string option;
        metadata : Jsont.json option;
        number : string;
        published_at : string option;
        purl : string;
        registry_url : string option;
        related_tag : Jsont.json option;
        status : string option;
        updated_at : Ptime.t;
        version_url : string;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~created_at ~id ~latest ~number ~purl ~updated_at ~version_url ?codemeta_url ?documentation_url ?download_url ?install_command ?integrity ?licenses ?metadata ?published_at ?registry_url ?related_tag ?status () = { codemeta_url; created_at; documentation_url; download_url; id; install_command; integrity; latest; licenses; metadata; number; published_at; purl; registry_url; related_tag; status; updated_at; version_url }

    let codemeta_url t = t.codemeta_url
    let created_at t = t.created_at
    let documentation_url t = t.documentation_url
    let download_url t = t.download_url
    let id t = t.id
    let install_command t = t.install_command
    let integrity t = t.integrity
    let latest t = t.latest
    let licenses t = t.licenses
    let metadata t = t.metadata
    let number t = t.number
    let published_at t = t.published_at
    let purl t = t.purl
    let registry_url t = t.registry_url
    let related_tag t = t.related_tag
    let status t = t.status
    let updated_at t = t.updated_at
    let version_url t = t.version_url

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Version"
        (fun codemeta_url created_at documentation_url download_url id install_command integrity latest licenses metadata number published_at purl registry_url related_tag status updated_at version_url -> { codemeta_url; created_at; documentation_url; download_url; id; install_command; integrity; latest; licenses; metadata; number; published_at; purl; registry_url; related_tag; status; updated_at; version_url })
      |> Jsont.Object.opt_mem "codemeta_url" Jsont.string ~enc:(fun r -> r.codemeta_url)
      |> Jsont.Object.mem "created_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "documentation_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.documentation_url)
      |> Jsont.Object.mem "download_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.download_url)
      |> Jsont.Object.mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "install_command" (Jsont.option Jsont.string) ~enc:(fun r -> r.install_command)
      |> Jsont.Object.mem "integrity" (Jsont.option Jsont.string) ~enc:(fun r -> r.integrity)
      |> Jsont.Object.mem "latest" Jsont.bool ~enc:(fun r -> r.latest)
      |> Jsont.Object.mem "licenses" (Jsont.option Jsont.string) ~enc:(fun r -> r.licenses)
      |> Jsont.Object.mem "metadata" (Jsont.option Jsont.json) ~enc:(fun r -> r.metadata)
      |> Jsont.Object.mem "number" Jsont.string ~enc:(fun r -> r.number)
      |> Jsont.Object.mem "published_at" (Jsont.option Jsont.string) ~enc:(fun r -> r.published_at)
      |> Jsont.Object.mem "purl" Jsont.string ~enc:(fun r -> r.purl)
      |> Jsont.Object.mem "registry_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.registry_url)
      |> Jsont.Object.mem "related_tag" (Jsont.option Jsont.json) ~enc:(fun r -> r.related_tag)
      |> Jsont.Object.mem "status" (Jsont.option Jsont.string) ~enc:(fun r -> r.status)
      |> Jsont.Object.mem "updated_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "version_url" Jsont.string ~enc:(fun r -> r.version_url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Version" jsont
  end

  (** get a list of versions for a package
      @param registry_name name of registry
      @param package_name name of package
      @param page pagination page number
      @param per_page Number of records to return
      @param created_after filter by created_at after given time
      @param updated_after filter by updated_at after given time
      @param published_after filter by published_at after given time
      @param published_before filter by published_at before given time
      @param created_before filter by created_at before given time
      @param updated_before filter by updated_at before given time
      @param sort field to order results by
      @param order direction to order results by
  *)
  let get_registry_package_versions ~registry_name ~package_name ?page ?per_page ?created_after ?updated_after ?published_after ?published_before ?created_before ?updated_before ?sort ?order client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("packageName", package_name)] "/registries/{registryName}/packages/{packageName}/versions" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page; Openapi.Runtime.Query.optional ~key:"created_after" ~value:created_after; Openapi.Runtime.Query.optional ~key:"updated_after" ~value:updated_after; Openapi.Runtime.Query.optional ~key:"published_after" ~value:published_after; Openapi.Runtime.Query.optional ~key:"published_before" ~value:published_before; Openapi.Runtime.Query.optional ~key:"created_before" ~value:created_before; Openapi.Runtime.Query.optional ~key:"updated_before" ~value:updated_before; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Version\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Version\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_package_versions" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module Registry = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t;
        default : bool;
        downloads : int64 option;
        ecosystem : string;
        github : string option;
        icon_url : string;
        keywords_count : int64;
        maintainers_count : int64;
        maintainers_url : string;
        metadata : Jsont.json option;
        name : string;
        namespaces_count : int64;
        packages_count : int64;
        packages_url : string;
        purl_type : string option;
        updated_at : Ptime.t;
        url : string;
        versions_count : int64 option;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~created_at ~default ~ecosystem ~icon_url ~keywords_count ~maintainers_count ~maintainers_url ~name ~namespaces_count ~packages_count ~packages_url ~updated_at ~url ?downloads ?github ?metadata ?purl_type ?versions_count () = { created_at; default; downloads; ecosystem; github; icon_url; keywords_count; maintainers_count; maintainers_url; metadata; name; namespaces_count; packages_count; packages_url; purl_type; updated_at; url; versions_count }

    let created_at t = t.created_at
    let default t = t.default
    let downloads t = t.downloads
    let ecosystem t = t.ecosystem
    let github t = t.github
    let icon_url t = t.icon_url
    let keywords_count t = t.keywords_count
    let maintainers_count t = t.maintainers_count
    let maintainers_url t = t.maintainers_url
    let metadata t = t.metadata
    let name t = t.name
    let namespaces_count t = t.namespaces_count
    let packages_count t = t.packages_count
    let packages_url t = t.packages_url
    let purl_type t = t.purl_type
    let updated_at t = t.updated_at
    let url t = t.url
    let versions_count t = t.versions_count

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Registry"
        (fun created_at default downloads ecosystem github icon_url keywords_count maintainers_count maintainers_url metadata name namespaces_count packages_count packages_url purl_type updated_at url versions_count -> { created_at; default; downloads; ecosystem; github; icon_url; keywords_count; maintainers_count; maintainers_url; metadata; name; namespaces_count; packages_count; packages_url; purl_type; updated_at; url; versions_count })
      |> Jsont.Object.mem "created_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "default" Jsont.bool ~enc:(fun r -> r.default)
      |> Jsont.Object.opt_mem "downloads" Openapi.Runtime.int64_jsont ~enc:(fun r -> r.downloads)
      |> Jsont.Object.mem "ecosystem" Jsont.string ~enc:(fun r -> r.ecosystem)
      |> Jsont.Object.mem "github" (Jsont.option Jsont.string) ~enc:(fun r -> r.github)
      |> Jsont.Object.mem "icon_url" Jsont.string ~enc:(fun r -> r.icon_url)
      |> Jsont.Object.mem "keywords_count" Openapi.Runtime.int64_jsont ~enc:(fun r -> r.keywords_count)
      |> Jsont.Object.mem "maintainers_count" Openapi.Runtime.int64_jsont ~enc:(fun r -> r.maintainers_count)
      |> Jsont.Object.mem "maintainers_url" Jsont.string ~enc:(fun r -> r.maintainers_url)
      |> Jsont.Object.mem "metadata" (Jsont.option Jsont.json) ~enc:(fun r -> r.metadata)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "namespaces_count" Openapi.Runtime.int64_jsont ~enc:(fun r -> r.namespaces_count)
      |> Jsont.Object.mem "packages_count" Openapi.Runtime.int64_jsont ~enc:(fun r -> r.packages_count)
      |> Jsont.Object.mem "packages_url" Jsont.string ~enc:(fun r -> r.packages_url)
      |> Jsont.Object.opt_mem "purl_type" Jsont.string ~enc:(fun r -> r.purl_type)
      |> Jsont.Object.mem "updated_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.opt_mem "versions_count" Openapi.Runtime.int64_jsont ~enc:(fun r -> r.versions_count)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Registry" jsont
  end

  (** list registries
      @param ecosystem filter by ecosystem name
      @param page pagination page number
      @param per_page Number of records to return
  *)
  let get_registries ?ecosystem ?page ?per_page client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/registries" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"ecosystem" ~value:ecosystem; Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Registry\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Registry\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registries" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get a registry by name
      @param registry_name name of registry
      @param page pagination page number
      @param per_page Number of records to return
  *)
  let get_registry ~registry_name ?page ?per_page client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name)] "/registries/{registryName}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Registry\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Registry\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module Namespace = struct
  module Types = struct
    module T = struct
      type t = {
        name : string;
        packages_count : int;
        packages_url : string;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~name ~packages_count ~packages_url () = { name; packages_count; packages_url }

    let name t = t.name
    let packages_count t = t.packages_count
    let packages_url t = t.packages_url

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Namespace"
        (fun name packages_count packages_url -> { name; packages_count; packages_url })
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "packages_count" Openapi.Runtime.int_jsont ~enc:(fun r -> r.packages_count)
      |> Jsont.Object.mem "packages_url" Jsont.string ~enc:(fun r -> r.packages_url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Namespace" jsont
  end

  (** get a list of namespaces from a registry
      @param registry_name name of registry
      @param page pagination page number
      @param per_page Number of records to return
  *)
  let get_registry_namespaces ~registry_name ?page ?per_page client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name)] "/registries/{registryName}/namespaces" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Namespace\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Namespace\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_namespaces" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get a namespace by name
      @param registry_name name of registry
      @param namespace_name name of namespace
  *)
  let get_registry_namespace ~registry_name ~namespace_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("namespaceName", namespace_name)] "/registries/{registryName}/namespaces/{namespaceName}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Namespace\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Namespace\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_namespace" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module Maintainer = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t;
        email : string option;
        html_url : string option;
        login : string option;
        name : string option;
        packages_count : int;
        packages_url : string;
        role : string option option;
        total_downloads : int option;
        updated_at : Ptime.t;
        url : string option;
        uuid : string;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~created_at ~packages_count ~packages_url ~updated_at ~uuid ?email ?html_url ?login ?name ?role ?total_downloads ?url () = { created_at; email; html_url; login; name; packages_count; packages_url; role; total_downloads; updated_at; url; uuid }

    let created_at t = t.created_at
    let email t = t.email
    let html_url t = t.html_url
    let login t = t.login
    let name t = t.name
    let packages_count t = t.packages_count
    let packages_url t = t.packages_url
    let role t = t.role
    let total_downloads t = t.total_downloads
    let updated_at t = t.updated_at
    let url t = t.url
    let uuid t = t.uuid

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Maintainer"
        (fun created_at email html_url login name packages_count packages_url role total_downloads updated_at url uuid -> { created_at; email; html_url; login; name; packages_count; packages_url; role; total_downloads; updated_at; url; uuid })
      |> Jsont.Object.mem "created_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "email" (Jsont.option Jsont.string) ~enc:(fun r -> r.email)
      |> Jsont.Object.mem "html_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.html_url)
      |> Jsont.Object.mem "login" (Jsont.option Jsont.string) ~enc:(fun r -> r.login)
      |> Jsont.Object.mem "name" (Jsont.option Jsont.string) ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "packages_count" Openapi.Runtime.int_jsont ~enc:(fun r -> r.packages_count)
      |> Jsont.Object.mem "packages_url" Jsont.string ~enc:(fun r -> r.packages_url)
      |> Jsont.Object.opt_mem "role" (Jsont.option Jsont.string) ~enc:(fun r -> r.role)
      |> Jsont.Object.opt_mem "total_downloads" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total_downloads)
      |> Jsont.Object.mem "updated_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "url" (Jsont.option Jsont.string) ~enc:(fun r -> r.url)
      |> Jsont.Object.mem "uuid" Jsont.string ~enc:(fun r -> r.uuid)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Maintainer" jsont
  end

  (** get a list of maintainers from a registry
      @param registry_name name of registry
      @param page pagination page number
      @param per_page Number of records to return
      @param created_after filter by created_at after given time
      @param updated_after filter by updated_at after given time
      @param sort field to order results by
      @param order direction to order results by
  *)
  let get_registry_maintainers ~registry_name ?page ?per_page ?created_after ?updated_after ?sort ?order client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name)] "/registries/{registryName}/maintainers" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page; Openapi.Runtime.Query.optional ~key:"created_after" ~value:created_after; Openapi.Runtime.Query.optional ~key:"updated_after" ~value:updated_after; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Maintainer\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Maintainer\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_maintainers" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get a maintainer by login or UUID
      @param registry_name name of registry
      @param maintainer_login_or_uuid login or uuid of maintainer
  *)
  let get_registry_maintainer ~registry_name ~maintainer_login_or_uuid client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("MaintainerLoginOrUUID", maintainer_login_or_uuid)] "/registries/{registryName}/maintainers/{MaintainerLoginOrUUID}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Maintainer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Maintainer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_maintainer" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module Keyword = struct
  module Types = struct
    module T = struct
      type t = {
        name : string;
        packages_count : int;
        packages_url : string option;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~name ~packages_count ?packages_url () = { name; packages_count; packages_url }

    let name t = t.name
    let packages_count t = t.packages_count
    let packages_url t = t.packages_url

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Keyword"
        (fun name packages_count packages_url -> { name; packages_count; packages_url })
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "packages_count" Openapi.Runtime.int_jsont ~enc:(fun r -> r.packages_count)
      |> Jsont.Object.opt_mem "packages_url" Jsont.string ~enc:(fun r -> r.packages_url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Keyword" jsont
  end

  (** list keywords
      @param page pagination page number
      @param per_page Number of records to return
  *)
  let get_keywords ?page ?per_page client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/keywords" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Keyword\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Keyword\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_keywords" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module Dependency = struct
  module Types = struct
    module T = struct
      type t = {
        ecosystem : string;
        id : int;
        kind : string option;
        optional : bool option;
        package_name : string;
        requirements : string option;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~ecosystem ~id ~package_name ?kind ?optional ?requirements () = { ecosystem; id; kind; optional; package_name; requirements }

    let ecosystem t = t.ecosystem
    let id t = t.id
    let kind t = t.kind
    let optional t = t.optional
    let package_name t = t.package_name
    let requirements t = t.requirements

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Dependency"
        (fun ecosystem id kind optional package_name requirements -> { ecosystem; id; kind; optional; package_name; requirements })
      |> Jsont.Object.mem "ecosystem" Jsont.string ~enc:(fun r -> r.ecosystem)
      |> Jsont.Object.mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "kind" (Jsont.option Jsont.string) ~enc:(fun r -> r.kind)
      |> Jsont.Object.mem "optional" (Jsont.option Jsont.bool) ~enc:(fun r -> r.optional)
      |> Jsont.Object.mem "package_name" Jsont.string ~enc:(fun r -> r.package_name)
      |> Jsont.Object.mem "requirements" (Jsont.option Jsont.string) ~enc:(fun r -> r.requirements)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Dependency" jsont
  end

  (** list dependencies
      @param page pagination page number
      @param per_page Number of records to return
      @param ecosystem ecosystem name
      @param version_id id of the version that declares the dependencies
      @param package_name package name
      @param package_id package id
      @param requirements requirements
      @param kind kind
      @param optional optional
      @param after filter by id after given id
      @param sort field to order results by
      @param order direction to order results by
  *)
  let get_dependencies ?page ?per_page ?ecosystem ?version_id ?package_name ?package_id ?requirements ?kind ?optional ?after ?sort ?order client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/dependencies" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page; Openapi.Runtime.Query.optional ~key:"ecosystem" ~value:ecosystem; Openapi.Runtime.Query.optional ~key:"version_id" ~value:version_id; Openapi.Runtime.Query.optional ~key:"package_name" ~value:package_name; Openapi.Runtime.Query.optional ~key:"package_id" ~value:package_id; Openapi.Runtime.Query.optional ~key:"requirements" ~value:requirements; Openapi.Runtime.Query.optional ~key:"kind" ~value:kind; Openapi.Runtime.Query.optional ~key:"optional" ~value:optional; Openapi.Runtime.Query.optional ~key:"after" ~value:after; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Dependency\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Dependency\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_dependencies" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module VersionWithDependencies = struct
  module Types = struct
    module T = struct
      type t = {
        codemeta_url : string;
        created_at : Ptime.t;
        dependencies : Dependency.T.t list;
        documentation_url : string option;
        download_url : string option;
        id : int option;
        install_command : string option;
        integrity : string option;
        latest : bool;
        licenses : string option;
        metadata : Jsont.json option;
        number : string;
        published_at : string option;
        purl : string;
        registry_url : string option;
        related_tag : Jsont.json option;
        status : string option;
        updated_at : Ptime.t;
        version_url : string;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~codemeta_url ~created_at ~dependencies ~latest ~number ~purl ~updated_at ~version_url ?documentation_url ?download_url ?id ?install_command ?integrity ?licenses ?metadata ?published_at ?registry_url ?related_tag ?status () = { codemeta_url; created_at; dependencies; documentation_url; download_url; id; install_command; integrity; latest; licenses; metadata; number; published_at; purl; registry_url; related_tag; status; updated_at; version_url }

    let codemeta_url t = t.codemeta_url
    let created_at t = t.created_at
    let dependencies t = t.dependencies
    let documentation_url t = t.documentation_url
    let download_url t = t.download_url
    let id t = t.id
    let install_command t = t.install_command
    let integrity t = t.integrity
    let latest t = t.latest
    let licenses t = t.licenses
    let metadata t = t.metadata
    let number t = t.number
    let published_at t = t.published_at
    let purl t = t.purl
    let registry_url t = t.registry_url
    let related_tag t = t.related_tag
    let status t = t.status
    let updated_at t = t.updated_at
    let version_url t = t.version_url

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VersionWithDependencies"
        (fun codemeta_url created_at dependencies documentation_url download_url id install_command integrity latest licenses metadata number published_at purl registry_url related_tag status updated_at version_url -> { codemeta_url; created_at; dependencies; documentation_url; download_url; id; install_command; integrity; latest; licenses; metadata; number; published_at; purl; registry_url; related_tag; status; updated_at; version_url })
      |> Jsont.Object.mem "codemeta_url" Jsont.string ~enc:(fun r -> r.codemeta_url)
      |> Jsont.Object.mem "created_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "dependencies" (Jsont.list Dependency.T.jsont) ~enc:(fun r -> r.dependencies)
      |> Jsont.Object.mem "documentation_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.documentation_url)
      |> Jsont.Object.mem "download_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.download_url)
      |> Jsont.Object.opt_mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "install_command" (Jsont.option Jsont.string) ~enc:(fun r -> r.install_command)
      |> Jsont.Object.mem "integrity" (Jsont.option Jsont.string) ~enc:(fun r -> r.integrity)
      |> Jsont.Object.mem "latest" Jsont.bool ~enc:(fun r -> r.latest)
      |> Jsont.Object.mem "licenses" (Jsont.option Jsont.string) ~enc:(fun r -> r.licenses)
      |> Jsont.Object.mem "metadata" (Jsont.option Jsont.json) ~enc:(fun r -> r.metadata)
      |> Jsont.Object.mem "number" Jsont.string ~enc:(fun r -> r.number)
      |> Jsont.Object.mem "published_at" (Jsont.option Jsont.string) ~enc:(fun r -> r.published_at)
      |> Jsont.Object.mem "purl" Jsont.string ~enc:(fun r -> r.purl)
      |> Jsont.Object.mem "registry_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.registry_url)
      |> Jsont.Object.mem "related_tag" (Jsont.option Jsont.json) ~enc:(fun r -> r.related_tag)
      |> Jsont.Object.mem "status" (Jsont.option Jsont.string) ~enc:(fun r -> r.status)
      |> Jsont.Object.mem "updated_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "version_url" Jsont.string ~enc:(fun r -> r.version_url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VersionWithDependencies" jsont
  end

  (** get the latest version of a package
      @param registry_name name of registry
      @param package_name name of package
  *)
  let get_registry_package_latest_version ~registry_name ~package_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("packageName", package_name)] "/registries/{registryName}/packages/{packageName}/latest_version" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VersionWithDependencies\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VersionWithDependencies\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_registry_package_latest_version" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get a version of a package
      @param registry_name name of registry
      @param package_name name of package
      @param version_number number of version
  *)
  let get_registry_package_version ~registry_name ~package_name ~version_number client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("packageName", package_name); ("versionNumber", version_number)] "/registries/{registryName}/packages/{packageName}/versions/{versionNumber}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VersionWithDependencies\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VersionWithDependencies\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_package_version" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module CodeMeta = struct
  module Types = struct
    module T = struct
      (** CodeMeta JSON-LD metadata format for software packages, compatible with Software Heritage *)
      type t = {
        context : string;  (** JSON-LD context URL *)
        type_ : string;  (** Type of software artifact *)
        application_category : string option option;  (** Package ecosystem/category *)
        author : Jsont.json list option option;  (** Package authors *)
        code_repository : string option option;  (** Source code repository URL *)
        copyright_holder : Jsont.json list option option;  (** Copyright holders *)
        copyright_year : int option option;  (** Copyright year *)
        date_created : string option option;  (** Creation date (ISO 8601) *)
        date_modified : string option option;  (** Last modification date (ISO 8601) *)
        date_published : string option option;  (** Publication date (ISO 8601) *)
        description : string option option;  (** Package description *)
        development_status : string option option;  (** Development status *)
        download_url : string option option;  (** Package download URL *)
        funder : Jsont.json list option option;  (** Funding sources *)
        https__forgefed_org_nsforks : int option option;  (** Fork count (ForgeFed) *)
        https__www_w3_org_ns_activitystreamslikes : int option option;  (** Star/like count (ActivityStreams) *)
        identifier : string;  (** Package URL (purl) identifier *)
        issue_tracker : string option option;  (** Issue tracker URL *)
        keywords : string list option option;  (** Keywords and tags *)
        license : Jsont.json option option;  (** SPDX license URL(s) *)
        maintainer : Jsont.json list option option;  (** Package maintainers *)
        name : string;  (** Package name *)
        programming_language : Jsont.json option option;  (** Programming language information *)
        runtime_platform : string option option;  (** Runtime platform/ecosystem *)
        same_as : string list option option;  (** Alternative identifiers/URLs *)
        software_help : Jsont.json option option;  (** Documentation/help resources *)
        software_version : string option option;  (** Software version *)
        url : string option option;  (** Homepage URL *)
        version : string option option;  (** Version number *)
      }
    end
  end

  module T = struct
    include Types.T

    let v ~context ~type_ ~identifier ~name ?application_category ?author ?code_repository ?copyright_holder ?copyright_year ?date_created ?date_modified ?date_published ?description ?development_status ?download_url ?funder ?https__forgefed_org_nsforks ?https__www_w3_org_ns_activitystreamslikes ?issue_tracker ?keywords ?license ?maintainer ?programming_language ?runtime_platform ?same_as ?software_help ?software_version ?url ?version () = { context; type_; application_category; author; code_repository; copyright_holder; copyright_year; date_created; date_modified; date_published; description; development_status; download_url; funder; https__forgefed_org_nsforks; https__www_w3_org_ns_activitystreamslikes; identifier; issue_tracker; keywords; license; maintainer; name; programming_language; runtime_platform; same_as; software_help; software_version; url; version }

    let context t = t.context
    let type_ t = t.type_
    let application_category t = t.application_category
    let author t = t.author
    let code_repository t = t.code_repository
    let copyright_holder t = t.copyright_holder
    let copyright_year t = t.copyright_year
    let date_created t = t.date_created
    let date_modified t = t.date_modified
    let date_published t = t.date_published
    let description t = t.description
    let development_status t = t.development_status
    let download_url t = t.download_url
    let funder t = t.funder
    let https__forgefed_org_nsforks t = t.https__forgefed_org_nsforks
    let https__www_w3_org_ns_activitystreamslikes t = t.https__www_w3_org_ns_activitystreamslikes
    let identifier t = t.identifier
    let issue_tracker t = t.issue_tracker
    let keywords t = t.keywords
    let license t = t.license
    let maintainer t = t.maintainer
    let name t = t.name
    let programming_language t = t.programming_language
    let runtime_platform t = t.runtime_platform
    let same_as t = t.same_as
    let software_help t = t.software_help
    let software_version t = t.software_version
    let url t = t.url
    let version t = t.version

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CodeMeta"
        (fun context type_ application_category author code_repository copyright_holder copyright_year date_created date_modified date_published description development_status download_url funder https__forgefed_org_nsforks https__www_w3_org_ns_activitystreamslikes identifier issue_tracker keywords license maintainer name programming_language runtime_platform same_as software_help software_version url version -> { context; type_; application_category; author; code_repository; copyright_holder; copyright_year; date_created; date_modified; date_published; description; development_status; download_url; funder; https__forgefed_org_nsforks; https__www_w3_org_ns_activitystreamslikes; identifier; issue_tracker; keywords; license; maintainer; name; programming_language; runtime_platform; same_as; software_help; software_version; url; version })
      |> Jsont.Object.mem "@context" Jsont.string ~enc:(fun r -> r.context)
      |> Jsont.Object.mem "@type" Jsont.string ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "applicationCategory" (Jsont.option Jsont.string) ~enc:(fun r -> r.application_category)
      |> Jsont.Object.opt_mem "author" (Jsont.option (Jsont.list Jsont.json)) ~enc:(fun r -> r.author)
      |> Jsont.Object.opt_mem "codeRepository" (Jsont.option Jsont.string) ~enc:(fun r -> r.code_repository)
      |> Jsont.Object.opt_mem "copyrightHolder" (Jsont.option (Jsont.list Jsont.json)) ~enc:(fun r -> r.copyright_holder)
      |> Jsont.Object.opt_mem "copyrightYear" (Jsont.option Openapi.Runtime.int_jsont) ~enc:(fun r -> r.copyright_year)
      |> Jsont.Object.opt_mem "dateCreated" (Jsont.option Jsont.string) ~enc:(fun r -> r.date_created)
      |> Jsont.Object.opt_mem "dateModified" (Jsont.option Jsont.string) ~enc:(fun r -> r.date_modified)
      |> Jsont.Object.opt_mem "datePublished" (Jsont.option Jsont.string) ~enc:(fun r -> r.date_published)
      |> Jsont.Object.opt_mem "description" (Jsont.option Jsont.string) ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "developmentStatus" (Jsont.option Jsont.string) ~enc:(fun r -> r.development_status)
      |> Jsont.Object.opt_mem "downloadUrl" (Jsont.option Jsont.string) ~enc:(fun r -> r.download_url)
      |> Jsont.Object.opt_mem "funder" (Jsont.option (Jsont.list Jsont.json)) ~enc:(fun r -> r.funder)
      |> Jsont.Object.opt_mem "https://forgefed.org/ns#forks" (Jsont.option Openapi.Runtime.int_jsont) ~enc:(fun r -> r.https__forgefed_org_nsforks)
      |> Jsont.Object.opt_mem "https://www.w3.org/ns/activitystreams#likes" (Jsont.option Openapi.Runtime.int_jsont) ~enc:(fun r -> r.https__www_w3_org_ns_activitystreamslikes)
      |> Jsont.Object.mem "identifier" Jsont.string ~enc:(fun r -> r.identifier)
      |> Jsont.Object.opt_mem "issueTracker" (Jsont.option Jsont.string) ~enc:(fun r -> r.issue_tracker)
      |> Jsont.Object.opt_mem "keywords" (Jsont.option (Jsont.list Jsont.string)) ~enc:(fun r -> r.keywords)
      |> Jsont.Object.opt_mem "license" (Jsont.option Jsont.json) ~enc:(fun r -> r.license)
      |> Jsont.Object.opt_mem "maintainer" (Jsont.option (Jsont.list Jsont.json)) ~enc:(fun r -> r.maintainer)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "programmingLanguage" (Jsont.option Jsont.json) ~enc:(fun r -> r.programming_language)
      |> Jsont.Object.opt_mem "runtimePlatform" (Jsont.option Jsont.string) ~enc:(fun r -> r.runtime_platform)
      |> Jsont.Object.opt_mem "sameAs" (Jsont.option (Jsont.list Jsont.string)) ~enc:(fun r -> r.same_as)
      |> Jsont.Object.opt_mem "softwareHelp" (Jsont.option Jsont.json) ~enc:(fun r -> r.software_help)
      |> Jsont.Object.opt_mem "softwareVersion" (Jsont.option Jsont.string) ~enc:(fun r -> r.software_version)
      |> Jsont.Object.opt_mem "url" (Jsont.option Jsont.string) ~enc:(fun r -> r.url)
      |> Jsont.Object.opt_mem "version" (Jsont.option Jsont.string) ~enc:(fun r -> r.version)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "CodeMeta" jsont
  end

  (** get CodeMeta metadata for a package
      @param registry_name name of registry
      @param package_name name of package
  *)
  let get_registry_package_code_meta ~registry_name ~package_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("packageName", package_name)] "/registries/{registryName}/packages/{packageName}/codemeta" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/CodeMeta\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/CodeMeta\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_package_code_meta" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get CodeMeta metadata for a version
      @param registry_name name of registry
      @param package_name name of package
      @param version_number number of version
  *)
  let get_registry_package_version_code_meta ~registry_name ~package_name ~version_number client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("packageName", package_name); ("versionNumber", version_number)] "/registries/{registryName}/packages/{packageName}/versions/{versionNumber}/codemeta" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/CodeMeta\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/CodeMeta\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_package_version_code_meta" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module Client = struct
  (** list unique maintainers of critical packages
      @param registry filter by registry name
      @param page pagination page number
      @param per_page Number of records to return
      @param sort field to sort results by (login or packages_count)
      @param order direction to sort results by (asc or desc)
  *)
  let get_critical_maintainers ?registry ?page ?per_page ?sort ?order client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/critical/maintainers" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"registry" ~value:registry; Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"object\",\"properties\":{\"login\":{\"type\":\"string\"},\"name\":{\"type\":\"string\",\"nullable\":true},\"registry_name\":{\"type\":\"string\"},\"packages_count\":{\"type\":\"integer\"},\"packages\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}}}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"object\",\"properties\":{\"login\":{\"type\":\"string\"},\"name\":{\"type\":\"string\",\"nullable\":true},\"registry_name\":{\"type\":\"string\"},\"packages_count\":{\"type\":\"integer\"},\"packages\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}}}}" (Jsont.list Jsont.json)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_critical_maintainers" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get a list of package names from a registry
      @param registry_name name of registry
      @param page pagination page number
      @param per_page Number of records to return
      @param created_after filter by created_at after given time
      @param updated_after filter by updated_at after given time
      @param created_before filter by created_at before given time
      @param updated_before filter by updated_at before given time
      @param sort field to order results by
      @param order direction to order results by
      @param critical filter by critical packages
      @param funding filter by packages with funding information
      @param prefix filter by package names starting with this string (case insensitive)
      @param postfix filter by package names ending with this string (case insensitive)
  *)
  let get_registry_package_names ~registry_name ?page ?per_page ?created_after ?updated_after ?created_before ?updated_before ?sort ?order ?critical ?funding ?prefix ?postfix client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name)] "/registries/{registryName}/package_names" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page; Openapi.Runtime.Query.optional ~key:"created_after" ~value:created_after; Openapi.Runtime.Query.optional ~key:"updated_after" ~value:updated_after; Openapi.Runtime.Query.optional ~key:"created_before" ~value:created_before; Openapi.Runtime.Query.optional ~key:"updated_before" ~value:updated_before; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order; Openapi.Runtime.Query.optional ~key:"critical" ~value:critical; Openapi.Runtime.Query.optional ~key:"funding" ~value:funding; Openapi.Runtime.Query.optional ~key:"prefix" ~value:prefix; Openapi.Runtime.Query.optional ~key:"postfix" ~value:postfix]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}" (Jsont.list Jsont.string)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_package_names" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get a list of dependency kinds for a package
      @param registry_name name of registry
      @param package_name name of package
      @param latest only count packages whose latest version depends on this package (default true). Set to false to include historical dependents.
  *)
  let get_registry_package_dependent_package_kinds ~registry_name ~package_name ?latest client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("packageName", package_name)] "/registries/{registryName}/packages/{packageName}/dependent_package_kinds" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"latest" ~value:latest]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}" (Jsont.list Jsont.string)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_package_dependent_package_kinds" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get a list of version numbers for a package from a registry
      @param registry_name name of registry
      @param package_name name of package
  *)
  let get_registry_package_version_numbers ~registry_name ~package_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("packageName", package_name)] "/registries/{registryName}/packages/{packageName}/version_numbers" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}" (Jsont.list Jsont.string)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_package_version_numbers" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module Advisory = struct
  module Types = struct
    module T = struct
      type t = {
        classification : string option;
        created_at : string;
        cvss_score : float option;
        cvss_vector : string option;
        description : string option;
        identifiers : string option list;
        origin : string option;
        packages : Jsont.json list;
        published_at : string option;
        references : string option list;
        severity : string option;
        source_kind : string option;
        title : string option;
        updated_at : string;
        url : string option;
        uuid : string;
        withdrawn_at : string option;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~created_at ~identifiers ~packages ~references ~updated_at ~uuid ?classification ?cvss_score ?cvss_vector ?description ?origin ?published_at ?severity ?source_kind ?title ?url ?withdrawn_at () = { classification; created_at; cvss_score; cvss_vector; description; identifiers; origin; packages; published_at; references; severity; source_kind; title; updated_at; url; uuid; withdrawn_at }

    let classification t = t.classification
    let created_at t = t.created_at
    let cvss_score t = t.cvss_score
    let cvss_vector t = t.cvss_vector
    let description t = t.description
    let identifiers t = t.identifiers
    let origin t = t.origin
    let packages t = t.packages
    let published_at t = t.published_at
    let references t = t.references
    let severity t = t.severity
    let source_kind t = t.source_kind
    let title t = t.title
    let updated_at t = t.updated_at
    let url t = t.url
    let uuid t = t.uuid
    let withdrawn_at t = t.withdrawn_at

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Advisory"
        (fun classification created_at cvss_score cvss_vector description identifiers origin packages published_at references severity source_kind title updated_at url uuid withdrawn_at -> { classification; created_at; cvss_score; cvss_vector; description; identifiers; origin; packages; published_at; references; severity; source_kind; title; updated_at; url; uuid; withdrawn_at })
      |> Jsont.Object.mem "classification" (Jsont.option Jsont.string) ~enc:(fun r -> r.classification)
      |> Jsont.Object.mem "created_at" Jsont.string ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "cvss_score" (Jsont.option Openapi.Runtime.number_jsont) ~enc:(fun r -> r.cvss_score)
      |> Jsont.Object.mem "cvss_vector" (Jsont.option Jsont.string) ~enc:(fun r -> r.cvss_vector)
      |> Jsont.Object.mem "description" (Jsont.option Jsont.string) ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "identifiers" (Jsont.list (Jsont.option Jsont.string)) ~enc:(fun r -> r.identifiers)
      |> Jsont.Object.mem "origin" (Jsont.option Jsont.string) ~enc:(fun r -> r.origin)
      |> Jsont.Object.mem "packages" (Jsont.list Jsont.json) ~enc:(fun r -> r.packages)
      |> Jsont.Object.mem "published_at" (Jsont.option Jsont.string) ~enc:(fun r -> r.published_at)
      |> Jsont.Object.mem "references" (Jsont.list (Jsont.option Jsont.string)) ~enc:(fun r -> r.references)
      |> Jsont.Object.mem "severity" (Jsont.option Jsont.string) ~enc:(fun r -> r.severity)
      |> Jsont.Object.mem "source_kind" (Jsont.option Jsont.string) ~enc:(fun r -> r.source_kind)
      |> Jsont.Object.mem "title" (Jsont.option Jsont.string) ~enc:(fun r -> r.title)
      |> Jsont.Object.mem "updated_at" Jsont.string ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "url" (Jsont.option Jsont.string) ~enc:(fun r -> r.url)
      |> Jsont.Object.mem "uuid" Jsont.string ~enc:(fun r -> r.uuid)
      |> Jsont.Object.mem "withdrawn_at" (Jsont.option Jsont.string) ~enc:(fun r -> r.withdrawn_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Advisory" jsont
  end
end

module PackageWithRegistry = struct
  module Types = struct
    module T = struct
      type t = {
        advisories : Advisory.T.t list;
        codemeta_url : string;
        created_at : Ptime.t;
        critical : bool option;
        dependent_packages_count : int;
        dependent_packages_url : string;
        dependent_repos_count : int;
        dependent_repositories_url : string;
        description : string option;
        docker_dependents_count : int option;
        docker_downloads_count : int option;
        docker_usage_url : string;
        documentation_url : string option;
        downloads : int option;
        downloads_period : string option;
        ecosystem : string;
        first_release_published_at : Ptime.t option;
        funding_links : string list;
        homepage : string option;
        id : int;
        install_command : string option;
        issue_metadata : Jsont.json option;
        keywords_array : string list;
        last_synced_at : Ptime.t option;
        latest_release_number : string option;
        latest_release_published_at : Ptime.t option;
        latest_version_url : string;
        licenses : string option;
        maintainers : Maintainer.T.t list;
        metadata : Jsont.json option;
        name : string;
        namespace : string option;
        normalized_licenses : string list;
        purl : string;
        rankings : Jsont.json;
        registry_url : string option;
        related_packages_url : string;
        repo_metadata : Jsont.json option;
        repo_metadata_updated_at : Ptime.t option;
        repository_url : string option;
        status : string option;
        updated_at : Ptime.t;
        usage_url : string;
        version_numbers_url : string option;
        versions_count : int;
        versions_url : string;
        registry : Registry.T.t;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~advisories ~codemeta_url ~created_at ~dependent_packages_count ~dependent_packages_url ~dependent_repos_count ~dependent_repositories_url ~docker_usage_url ~ecosystem ~funding_links ~id ~keywords_array ~latest_version_url ~maintainers ~name ~normalized_licenses ~purl ~rankings ~related_packages_url ~updated_at ~usage_url ~versions_count ~versions_url ~registry ?critical ?description ?docker_dependents_count ?docker_downloads_count ?documentation_url ?downloads ?downloads_period ?first_release_published_at ?homepage ?install_command ?issue_metadata ?last_synced_at ?latest_release_number ?latest_release_published_at ?licenses ?metadata ?namespace ?registry_url ?repo_metadata ?repo_metadata_updated_at ?repository_url ?status ?version_numbers_url () = { advisories; codemeta_url; created_at; critical; dependent_packages_count; dependent_packages_url; dependent_repos_count; dependent_repositories_url; description; docker_dependents_count; docker_downloads_count; docker_usage_url; documentation_url; downloads; downloads_period; ecosystem; first_release_published_at; funding_links; homepage; id; install_command; issue_metadata; keywords_array; last_synced_at; latest_release_number; latest_release_published_at; latest_version_url; licenses; maintainers; metadata; name; namespace; normalized_licenses; purl; rankings; registry_url; related_packages_url; repo_metadata; repo_metadata_updated_at; repository_url; status; updated_at; usage_url; version_numbers_url; versions_count; versions_url; registry }

    let advisories t = t.advisories
    let codemeta_url t = t.codemeta_url
    let created_at t = t.created_at
    let critical t = t.critical
    let dependent_packages_count t = t.dependent_packages_count
    let dependent_packages_url t = t.dependent_packages_url
    let dependent_repos_count t = t.dependent_repos_count
    let dependent_repositories_url t = t.dependent_repositories_url
    let description t = t.description
    let docker_dependents_count t = t.docker_dependents_count
    let docker_downloads_count t = t.docker_downloads_count
    let docker_usage_url t = t.docker_usage_url
    let documentation_url t = t.documentation_url
    let downloads t = t.downloads
    let downloads_period t = t.downloads_period
    let ecosystem t = t.ecosystem
    let first_release_published_at t = t.first_release_published_at
    let funding_links t = t.funding_links
    let homepage t = t.homepage
    let id t = t.id
    let install_command t = t.install_command
    let issue_metadata t = t.issue_metadata
    let keywords_array t = t.keywords_array
    let last_synced_at t = t.last_synced_at
    let latest_release_number t = t.latest_release_number
    let latest_release_published_at t = t.latest_release_published_at
    let latest_version_url t = t.latest_version_url
    let licenses t = t.licenses
    let maintainers t = t.maintainers
    let metadata t = t.metadata
    let name t = t.name
    let namespace t = t.namespace
    let normalized_licenses t = t.normalized_licenses
    let purl t = t.purl
    let rankings t = t.rankings
    let registry_url t = t.registry_url
    let related_packages_url t = t.related_packages_url
    let repo_metadata t = t.repo_metadata
    let repo_metadata_updated_at t = t.repo_metadata_updated_at
    let repository_url t = t.repository_url
    let status t = t.status
    let updated_at t = t.updated_at
    let usage_url t = t.usage_url
    let version_numbers_url t = t.version_numbers_url
    let versions_count t = t.versions_count
    let versions_url t = t.versions_url
    let registry t = t.registry

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PackageWithRegistry"
        (fun advisories codemeta_url created_at critical dependent_packages_count dependent_packages_url dependent_repos_count dependent_repositories_url description docker_dependents_count docker_downloads_count docker_usage_url documentation_url downloads downloads_period ecosystem first_release_published_at funding_links homepage id install_command issue_metadata keywords_array last_synced_at latest_release_number latest_release_published_at latest_version_url licenses maintainers metadata name namespace normalized_licenses purl rankings registry_url related_packages_url repo_metadata repo_metadata_updated_at repository_url status updated_at usage_url version_numbers_url versions_count versions_url registry -> { advisories; codemeta_url; created_at; critical; dependent_packages_count; dependent_packages_url; dependent_repos_count; dependent_repositories_url; description; docker_dependents_count; docker_downloads_count; docker_usage_url; documentation_url; downloads; downloads_period; ecosystem; first_release_published_at; funding_links; homepage; id; install_command; issue_metadata; keywords_array; last_synced_at; latest_release_number; latest_release_published_at; latest_version_url; licenses; maintainers; metadata; name; namespace; normalized_licenses; purl; rankings; registry_url; related_packages_url; repo_metadata; repo_metadata_updated_at; repository_url; status; updated_at; usage_url; version_numbers_url; versions_count; versions_url; registry })
      |> Jsont.Object.mem "advisories" (Jsont.list Advisory.T.jsont) ~enc:(fun r -> r.advisories)
      |> Jsont.Object.mem "codemeta_url" Jsont.string ~enc:(fun r -> r.codemeta_url)
      |> Jsont.Object.mem "created_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "critical" (Jsont.option Jsont.bool) ~enc:(fun r -> r.critical)
      |> Jsont.Object.mem "dependent_packages_count" Openapi.Runtime.int_jsont ~enc:(fun r -> r.dependent_packages_count)
      |> Jsont.Object.mem "dependent_packages_url" Jsont.string ~enc:(fun r -> r.dependent_packages_url)
      |> Jsont.Object.mem "dependent_repos_count" Openapi.Runtime.int_jsont ~enc:(fun r -> r.dependent_repos_count)
      |> Jsont.Object.mem "dependent_repositories_url" Jsont.string ~enc:(fun r -> r.dependent_repositories_url)
      |> Jsont.Object.mem "description" (Jsont.option Jsont.string) ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "docker_dependents_count" (Jsont.option Openapi.Runtime.int_jsont) ~enc:(fun r -> r.docker_dependents_count)
      |> Jsont.Object.mem "docker_downloads_count" (Jsont.option Openapi.Runtime.int_jsont) ~enc:(fun r -> r.docker_downloads_count)
      |> Jsont.Object.mem "docker_usage_url" Jsont.string ~enc:(fun r -> r.docker_usage_url)
      |> Jsont.Object.mem "documentation_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.documentation_url)
      |> Jsont.Object.mem "downloads" (Jsont.option Openapi.Runtime.int_jsont) ~enc:(fun r -> r.downloads)
      |> Jsont.Object.mem "downloads_period" (Jsont.option Jsont.string) ~enc:(fun r -> r.downloads_period)
      |> Jsont.Object.mem "ecosystem" Jsont.string ~enc:(fun r -> r.ecosystem)
      |> Jsont.Object.mem "first_release_published_at" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.first_release_published_at)
      |> Jsont.Object.mem "funding_links" (Jsont.list Jsont.string) ~enc:(fun r -> r.funding_links)
      |> Jsont.Object.mem "homepage" (Jsont.option Jsont.string) ~enc:(fun r -> r.homepage)
      |> Jsont.Object.mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "install_command" (Jsont.option Jsont.string) ~enc:(fun r -> r.install_command)
      |> Jsont.Object.mem "issue_metadata" (Jsont.option Jsont.json) ~enc:(fun r -> r.issue_metadata)
      |> Jsont.Object.mem "keywords_array" (Jsont.list Jsont.string) ~enc:(fun r -> r.keywords_array)
      |> Jsont.Object.mem "last_synced_at" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.last_synced_at)
      |> Jsont.Object.mem "latest_release_number" (Jsont.option Jsont.string) ~enc:(fun r -> r.latest_release_number)
      |> Jsont.Object.mem "latest_release_published_at" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.latest_release_published_at)
      |> Jsont.Object.mem "latest_version_url" Jsont.string ~enc:(fun r -> r.latest_version_url)
      |> Jsont.Object.mem "licenses" (Jsont.option Jsont.string) ~enc:(fun r -> r.licenses)
      |> Jsont.Object.mem "maintainers" (Jsont.list Maintainer.T.jsont) ~enc:(fun r -> r.maintainers)
      |> Jsont.Object.mem "metadata" (Jsont.option Jsont.json) ~enc:(fun r -> r.metadata)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "namespace" (Jsont.option Jsont.string) ~enc:(fun r -> r.namespace)
      |> Jsont.Object.mem "normalized_licenses" (Jsont.list Jsont.string) ~enc:(fun r -> r.normalized_licenses)
      |> Jsont.Object.mem "purl" Jsont.string ~enc:(fun r -> r.purl)
      |> Jsont.Object.mem "rankings" Jsont.json ~enc:(fun r -> r.rankings)
      |> Jsont.Object.mem "registry_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.registry_url)
      |> Jsont.Object.mem "related_packages_url" Jsont.string ~enc:(fun r -> r.related_packages_url)
      |> Jsont.Object.mem "repo_metadata" (Jsont.option Jsont.json) ~enc:(fun r -> r.repo_metadata)
      |> Jsont.Object.mem "repo_metadata_updated_at" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.repo_metadata_updated_at)
      |> Jsont.Object.mem "repository_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.repository_url)
      |> Jsont.Object.mem "status" (Jsont.option Jsont.string) ~enc:(fun r -> r.status)
      |> Jsont.Object.mem "updated_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "usage_url" Jsont.string ~enc:(fun r -> r.usage_url)
      |> Jsont.Object.opt_mem "version_numbers_url" Jsont.string ~enc:(fun r -> r.version_numbers_url)
      |> Jsont.Object.mem "versions_count" Openapi.Runtime.int_jsont ~enc:(fun r -> r.versions_count)
      |> Jsont.Object.mem "versions_url" Jsont.string ~enc:(fun r -> r.versions_url)
      |> Jsont.Object.mem "registry" Registry.T.jsont ~enc:(fun r -> r.registry)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PackageWithRegistry" jsont
  end

  (** list critical packages
      @param registry filter by registry name
      @param page pagination page number
      @param per_page Number of records to return
      @param sort field to sort results by
      @param order direction to sort results by (asc or desc)
  *)
  let get_critical_packages ?registry ?page ?per_page ?sort ?order client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/critical" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"registry" ~value:registry; Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_critical_packages" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** list critical packages with sole maintainers
      @param registry filter by registry name
      @param page pagination page number
      @param per_page Number of records to return
      @param sort field to sort results by
      @param order direction to sort results by (asc or desc)
  *)
  let get_critical_sole_maintainers ?registry ?page ?per_page ?sort ?order client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/critical/sole_maintainers" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"registry" ~value:registry; Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_critical_sole_maintainers" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** lookup multiple packages by repository URLs, PURLs, or names *)
  let bulk_lookup_packages ~body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/packages/bulk_lookup" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"ecosystem\":{\"type\":\"string\",\"description\":\"filter results by ecosystem name\"},\"names\":{\"type\":\"array\",\"description\":\"array of package names to lookup\",\"items\":{\"type\":\"string\"}},\"purls\":{\"type\":\"array\",\"description\":\"array of package URLs to lookup (maximum 100)\",\"maxItems\":100,\"items\":{\"type\":\"string\"}},\"repository_urls\":{\"type\":\"array\",\"description\":\"array of repository URLs to lookup\",\"items\":{\"type\":\"string\"}}},\"required\":[]}" Jsont.json)) body in
    let __openapi_body = Some __openapi_body in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None))]
      ~operation:"bulk_lookup_packages" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** list all critical packages
      @param page pagination page number
      @param per_page Number of records to return
      @param created_after filter by created_at after given time
      @param updated_after filter by updated_at after given time
      @param created_before filter by created_at before given time
      @param updated_before filter by updated_at before given time
      @param funding filter by packages with funding information
      @param sort field to sort results by
      @param order direction to sort results by (asc or desc)
  *)
  let get_critical_packages_list ?page ?per_page ?created_after ?updated_after ?created_before ?updated_before ?funding ?sort ?order client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/packages/critical" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page; Openapi.Runtime.Query.optional ~key:"created_after" ~value:created_after; Openapi.Runtime.Query.optional ~key:"updated_after" ~value:updated_after; Openapi.Runtime.Query.optional ~key:"created_before" ~value:created_before; Openapi.Runtime.Query.optional ~key:"updated_before" ~value:updated_before; Openapi.Runtime.Query.optional ~key:"funding" ~value:funding; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_critical_packages_list" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** lookup a single package by repository URL, purl or ecosystem+name. For multiple packages use POST /packages/bulk_lookup.
      @param repository_url repository URL
      @param purl single package URL. For multiple purls use POST /packages/bulk_lookup.
      @param ecosystem ecosystem name
      @param name package name
      @param sort field to sort results by
      @param order direction to sort results by
  *)
  let lookup_package ?repository_url ?purl ?ecosystem ?name ?sort ?order client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/packages/lookup" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"repository_url" ~value:repository_url; Openapi.Runtime.Query.optional ~key:"purl" ~value:purl; Openapi.Runtime.Query.optional ~key:"ecosystem" ~value:ecosystem; Openapi.Runtime.Query.optional ~key:"name" ~value:name; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None))]
      ~operation:"lookup_package" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** lookup a package within a registry by repository URL, purl or ecosystem+name
      @param registry_name name of registry
      @param repository_url repository URL
      @param purl single package URL. For multiple purls use POST /packages/bulk_lookup.
      @param ecosystem ecosystem name
      @param name package name
      @param sort field to sort results by
      @param order direction to sort results by
  *)
  let lookup_registry_package ~registry_name ?repository_url ?purl ?ecosystem ?name ?sort ?order client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name)] "/registries/{registryName}/lookup" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"repository_url" ~value:repository_url; Openapi.Runtime.Query.optional ~key:"purl" ~value:purl; Openapi.Runtime.Query.optional ~key:"ecosystem" ~value:ecosystem; Openapi.Runtime.Query.optional ~key:"name" ~value:name; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/PackageWithRegistry\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"lookup_registry_package" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module VersionLookup = struct
  module Types = struct
    module T = struct
      type t = {
        codemeta_url : string;
        created_at : Ptime.t;
        dependencies : Dependency.T.t list;
        documentation_url : string option;
        download_url : string option;
        id : int option;
        install_command : string option;
        integrity : string option;
        latest : bool;
        licenses : string option;
        metadata : Jsont.json option;
        number : string;
        published_at : string option;
        purl : string;
        registry_url : string option;
        related_tag : Jsont.json option;
        status : string option;
        updated_at : Ptime.t;
        version_url : string;
        package : PackageWithRegistry.T.t;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~codemeta_url ~created_at ~dependencies ~latest ~number ~purl ~updated_at ~version_url ~package ?documentation_url ?download_url ?id ?install_command ?integrity ?licenses ?metadata ?published_at ?registry_url ?related_tag ?status () = { codemeta_url; created_at; dependencies; documentation_url; download_url; id; install_command; integrity; latest; licenses; metadata; number; published_at; purl; registry_url; related_tag; status; updated_at; version_url; package }

    let codemeta_url t = t.codemeta_url
    let created_at t = t.created_at
    let dependencies t = t.dependencies
    let documentation_url t = t.documentation_url
    let download_url t = t.download_url
    let id t = t.id
    let install_command t = t.install_command
    let integrity t = t.integrity
    let latest t = t.latest
    let licenses t = t.licenses
    let metadata t = t.metadata
    let number t = t.number
    let published_at t = t.published_at
    let purl t = t.purl
    let registry_url t = t.registry_url
    let related_tag t = t.related_tag
    let status t = t.status
    let updated_at t = t.updated_at
    let version_url t = t.version_url
    let package t = t.package

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VersionLookup"
        (fun codemeta_url created_at dependencies documentation_url download_url id install_command integrity latest licenses metadata number published_at purl registry_url related_tag status updated_at version_url package -> { codemeta_url; created_at; dependencies; documentation_url; download_url; id; install_command; integrity; latest; licenses; metadata; number; published_at; purl; registry_url; related_tag; status; updated_at; version_url; package })
      |> Jsont.Object.mem "codemeta_url" Jsont.string ~enc:(fun r -> r.codemeta_url)
      |> Jsont.Object.mem "created_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "dependencies" (Jsont.list Dependency.T.jsont) ~enc:(fun r -> r.dependencies)
      |> Jsont.Object.mem "documentation_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.documentation_url)
      |> Jsont.Object.mem "download_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.download_url)
      |> Jsont.Object.opt_mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "install_command" (Jsont.option Jsont.string) ~enc:(fun r -> r.install_command)
      |> Jsont.Object.mem "integrity" (Jsont.option Jsont.string) ~enc:(fun r -> r.integrity)
      |> Jsont.Object.mem "latest" Jsont.bool ~enc:(fun r -> r.latest)
      |> Jsont.Object.mem "licenses" (Jsont.option Jsont.string) ~enc:(fun r -> r.licenses)
      |> Jsont.Object.mem "metadata" (Jsont.option Jsont.json) ~enc:(fun r -> r.metadata)
      |> Jsont.Object.mem "number" Jsont.string ~enc:(fun r -> r.number)
      |> Jsont.Object.mem "published_at" (Jsont.option Jsont.string) ~enc:(fun r -> r.published_at)
      |> Jsont.Object.mem "purl" Jsont.string ~enc:(fun r -> r.purl)
      |> Jsont.Object.mem "registry_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.registry_url)
      |> Jsont.Object.mem "related_tag" (Jsont.option Jsont.json) ~enc:(fun r -> r.related_tag)
      |> Jsont.Object.mem "status" (Jsont.option Jsont.string) ~enc:(fun r -> r.status)
      |> Jsont.Object.mem "updated_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "version_url" Jsont.string ~enc:(fun r -> r.version_url)
      |> Jsont.Object.mem "package" PackageWithRegistry.T.jsont ~enc:(fun r -> r.package)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VersionLookup" jsont
  end

  (** lookup versions by integrity hash
      @param integrity integrity hash (SRI format)
      @param sha256 sha256 hash (hex)
      @param sha1 sha1 hash (hex)
      @param sha512 sha512 hash (hex)
      @param page page number
      @param per_page Number of records to return
  *)
  let lookup_versions ?integrity ?sha256 ?sha1 ?sha512 ?page ?per_page client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/versions/lookup" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"integrity" ~value:integrity; Openapi.Runtime.Query.optional ~key:"sha256" ~value:sha256; Openapi.Runtime.Query.optional ~key:"sha1" ~value:sha1; Openapi.Runtime.Query.optional ~key:"sha512" ~value:sha512; Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/VersionLookup\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/VersionLookup\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None))]
      ~operation:"lookup_versions" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module Package = struct
  module Types = struct
    module T = struct
      type t = {
        advisories : Advisory.T.t list;
        codemeta_url : string;
        created_at : Ptime.t;
        critical : bool option;
        dependent_packages_count : int;
        dependent_packages_url : string;
        dependent_repos_count : int;
        dependent_repositories_url : string;
        description : string option;
        docker_dependents_count : int option;
        docker_downloads_count : int option;
        docker_usage_url : string;
        documentation_url : string option;
        downloads : int option;
        downloads_period : string option;
        ecosystem : string;
        first_release_published_at : Ptime.t option;
        funding_links : string list;
        homepage : string option;
        id : int;
        install_command : string option;
        issue_metadata : Jsont.json option;
        keywords_array : string list;
        last_synced_at : Ptime.t option;
        latest_release_number : string option;
        latest_release_published_at : Ptime.t option;
        latest_version_url : string;
        licenses : string option;
        maintainers : Maintainer.T.t list;
        metadata : Jsont.json option;
        name : string;
        namespace : string option;
        normalized_licenses : string list;
        purl : string;
        rankings : Jsont.json;
        registry_url : string option;
        related_packages_url : string;
        repo_metadata : Jsont.json option;
        repo_metadata_updated_at : Ptime.t option;
        repository_url : string option;
        status : string option;
        updated_at : Ptime.t;
        usage_url : string;
        version_numbers_url : string option;
        versions_count : int;
        versions_url : string;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~advisories ~codemeta_url ~created_at ~dependent_packages_count ~dependent_packages_url ~dependent_repos_count ~dependent_repositories_url ~docker_usage_url ~ecosystem ~funding_links ~id ~keywords_array ~latest_version_url ~maintainers ~name ~normalized_licenses ~purl ~rankings ~related_packages_url ~updated_at ~usage_url ~versions_count ~versions_url ?critical ?description ?docker_dependents_count ?docker_downloads_count ?documentation_url ?downloads ?downloads_period ?first_release_published_at ?homepage ?install_command ?issue_metadata ?last_synced_at ?latest_release_number ?latest_release_published_at ?licenses ?metadata ?namespace ?registry_url ?repo_metadata ?repo_metadata_updated_at ?repository_url ?status ?version_numbers_url () = { advisories; codemeta_url; created_at; critical; dependent_packages_count; dependent_packages_url; dependent_repos_count; dependent_repositories_url; description; docker_dependents_count; docker_downloads_count; docker_usage_url; documentation_url; downloads; downloads_period; ecosystem; first_release_published_at; funding_links; homepage; id; install_command; issue_metadata; keywords_array; last_synced_at; latest_release_number; latest_release_published_at; latest_version_url; licenses; maintainers; metadata; name; namespace; normalized_licenses; purl; rankings; registry_url; related_packages_url; repo_metadata; repo_metadata_updated_at; repository_url; status; updated_at; usage_url; version_numbers_url; versions_count; versions_url }

    let advisories t = t.advisories
    let codemeta_url t = t.codemeta_url
    let created_at t = t.created_at
    let critical t = t.critical
    let dependent_packages_count t = t.dependent_packages_count
    let dependent_packages_url t = t.dependent_packages_url
    let dependent_repos_count t = t.dependent_repos_count
    let dependent_repositories_url t = t.dependent_repositories_url
    let description t = t.description
    let docker_dependents_count t = t.docker_dependents_count
    let docker_downloads_count t = t.docker_downloads_count
    let docker_usage_url t = t.docker_usage_url
    let documentation_url t = t.documentation_url
    let downloads t = t.downloads
    let downloads_period t = t.downloads_period
    let ecosystem t = t.ecosystem
    let first_release_published_at t = t.first_release_published_at
    let funding_links t = t.funding_links
    let homepage t = t.homepage
    let id t = t.id
    let install_command t = t.install_command
    let issue_metadata t = t.issue_metadata
    let keywords_array t = t.keywords_array
    let last_synced_at t = t.last_synced_at
    let latest_release_number t = t.latest_release_number
    let latest_release_published_at t = t.latest_release_published_at
    let latest_version_url t = t.latest_version_url
    let licenses t = t.licenses
    let maintainers t = t.maintainers
    let metadata t = t.metadata
    let name t = t.name
    let namespace t = t.namespace
    let normalized_licenses t = t.normalized_licenses
    let purl t = t.purl
    let rankings t = t.rankings
    let registry_url t = t.registry_url
    let related_packages_url t = t.related_packages_url
    let repo_metadata t = t.repo_metadata
    let repo_metadata_updated_at t = t.repo_metadata_updated_at
    let repository_url t = t.repository_url
    let status t = t.status
    let updated_at t = t.updated_at
    let usage_url t = t.usage_url
    let version_numbers_url t = t.version_numbers_url
    let versions_count t = t.versions_count
    let versions_url t = t.versions_url

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Package"
        (fun advisories codemeta_url created_at critical dependent_packages_count dependent_packages_url dependent_repos_count dependent_repositories_url description docker_dependents_count docker_downloads_count docker_usage_url documentation_url downloads downloads_period ecosystem first_release_published_at funding_links homepage id install_command issue_metadata keywords_array last_synced_at latest_release_number latest_release_published_at latest_version_url licenses maintainers metadata name namespace normalized_licenses purl rankings registry_url related_packages_url repo_metadata repo_metadata_updated_at repository_url status updated_at usage_url version_numbers_url versions_count versions_url -> { advisories; codemeta_url; created_at; critical; dependent_packages_count; dependent_packages_url; dependent_repos_count; dependent_repositories_url; description; docker_dependents_count; docker_downloads_count; docker_usage_url; documentation_url; downloads; downloads_period; ecosystem; first_release_published_at; funding_links; homepage; id; install_command; issue_metadata; keywords_array; last_synced_at; latest_release_number; latest_release_published_at; latest_version_url; licenses; maintainers; metadata; name; namespace; normalized_licenses; purl; rankings; registry_url; related_packages_url; repo_metadata; repo_metadata_updated_at; repository_url; status; updated_at; usage_url; version_numbers_url; versions_count; versions_url })
      |> Jsont.Object.mem "advisories" (Jsont.list Advisory.T.jsont) ~enc:(fun r -> r.advisories)
      |> Jsont.Object.mem "codemeta_url" Jsont.string ~enc:(fun r -> r.codemeta_url)
      |> Jsont.Object.mem "created_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "critical" (Jsont.option Jsont.bool) ~enc:(fun r -> r.critical)
      |> Jsont.Object.mem "dependent_packages_count" Openapi.Runtime.int_jsont ~enc:(fun r -> r.dependent_packages_count)
      |> Jsont.Object.mem "dependent_packages_url" Jsont.string ~enc:(fun r -> r.dependent_packages_url)
      |> Jsont.Object.mem "dependent_repos_count" Openapi.Runtime.int_jsont ~enc:(fun r -> r.dependent_repos_count)
      |> Jsont.Object.mem "dependent_repositories_url" Jsont.string ~enc:(fun r -> r.dependent_repositories_url)
      |> Jsont.Object.mem "description" (Jsont.option Jsont.string) ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "docker_dependents_count" (Jsont.option Openapi.Runtime.int_jsont) ~enc:(fun r -> r.docker_dependents_count)
      |> Jsont.Object.mem "docker_downloads_count" (Jsont.option Openapi.Runtime.int_jsont) ~enc:(fun r -> r.docker_downloads_count)
      |> Jsont.Object.mem "docker_usage_url" Jsont.string ~enc:(fun r -> r.docker_usage_url)
      |> Jsont.Object.mem "documentation_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.documentation_url)
      |> Jsont.Object.mem "downloads" (Jsont.option Openapi.Runtime.int_jsont) ~enc:(fun r -> r.downloads)
      |> Jsont.Object.mem "downloads_period" (Jsont.option Jsont.string) ~enc:(fun r -> r.downloads_period)
      |> Jsont.Object.mem "ecosystem" Jsont.string ~enc:(fun r -> r.ecosystem)
      |> Jsont.Object.mem "first_release_published_at" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.first_release_published_at)
      |> Jsont.Object.mem "funding_links" (Jsont.list Jsont.string) ~enc:(fun r -> r.funding_links)
      |> Jsont.Object.mem "homepage" (Jsont.option Jsont.string) ~enc:(fun r -> r.homepage)
      |> Jsont.Object.mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "install_command" (Jsont.option Jsont.string) ~enc:(fun r -> r.install_command)
      |> Jsont.Object.mem "issue_metadata" (Jsont.option Jsont.json) ~enc:(fun r -> r.issue_metadata)
      |> Jsont.Object.mem "keywords_array" (Jsont.list Jsont.string) ~enc:(fun r -> r.keywords_array)
      |> Jsont.Object.mem "last_synced_at" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.last_synced_at)
      |> Jsont.Object.mem "latest_release_number" (Jsont.option Jsont.string) ~enc:(fun r -> r.latest_release_number)
      |> Jsont.Object.mem "latest_release_published_at" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.latest_release_published_at)
      |> Jsont.Object.mem "latest_version_url" Jsont.string ~enc:(fun r -> r.latest_version_url)
      |> Jsont.Object.mem "licenses" (Jsont.option Jsont.string) ~enc:(fun r -> r.licenses)
      |> Jsont.Object.mem "maintainers" (Jsont.list Maintainer.T.jsont) ~enc:(fun r -> r.maintainers)
      |> Jsont.Object.mem "metadata" (Jsont.option Jsont.json) ~enc:(fun r -> r.metadata)
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "namespace" (Jsont.option Jsont.string) ~enc:(fun r -> r.namespace)
      |> Jsont.Object.mem "normalized_licenses" (Jsont.list Jsont.string) ~enc:(fun r -> r.normalized_licenses)
      |> Jsont.Object.mem "purl" Jsont.string ~enc:(fun r -> r.purl)
      |> Jsont.Object.mem "rankings" Jsont.json ~enc:(fun r -> r.rankings)
      |> Jsont.Object.mem "registry_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.registry_url)
      |> Jsont.Object.mem "related_packages_url" Jsont.string ~enc:(fun r -> r.related_packages_url)
      |> Jsont.Object.mem "repo_metadata" (Jsont.option Jsont.json) ~enc:(fun r -> r.repo_metadata)
      |> Jsont.Object.mem "repo_metadata_updated_at" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.repo_metadata_updated_at)
      |> Jsont.Object.mem "repository_url" (Jsont.option Jsont.string) ~enc:(fun r -> r.repository_url)
      |> Jsont.Object.mem "status" (Jsont.option Jsont.string) ~enc:(fun r -> r.status)
      |> Jsont.Object.mem "updated_at" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "usage_url" Jsont.string ~enc:(fun r -> r.usage_url)
      |> Jsont.Object.opt_mem "version_numbers_url" Jsont.string ~enc:(fun r -> r.version_numbers_url)
      |> Jsont.Object.mem "versions_count" Openapi.Runtime.int_jsont ~enc:(fun r -> r.versions_count)
      |> Jsont.Object.mem "versions_url" Jsont.string ~enc:(fun r -> r.versions_url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Package" jsont
  end

  (** get packages for a maintainer by login or UUID
      @param registry_name name of registry
      @param maintainer_login_or_uuid login or uuid of maintainer
      @param page pagination page number
      @param per_page Number of records to return
  *)
  let get_registry_maintainer_packages ~registry_name ~maintainer_login_or_uuid ?page ?per_page client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("MaintainerLoginOrUUID", maintainer_login_or_uuid)] "/registries/{registryName}/maintainers/{MaintainerLoginOrUUID}/packages" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Package\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Package\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_maintainer_packages" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get packages for a namespace by login or UUID
      @param registry_name name of registry
      @param namespace_name lname of namespace
      @param page pagination page number
      @param per_page Number of records to return
  *)
  let get_registry_namespace_packages ~registry_name ~namespace_name ?page ?per_page client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("namespaceName", namespace_name)] "/registries/{registryName}/namespaces/{namespaceName}/packages" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Package\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Package\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_namespace_packages" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get a list of packages from a registry
      @param registry_name name of registry
      @param page pagination page number
      @param per_page Number of records to return
      @param created_after filter by created_at after given time
      @param updated_after filter by updated_at after given time
      @param created_before filter by created_at before given time
      @param updated_before filter by updated_at before given time
      @param critical filter by critical packages
      @param sort field to order results by
      @param order direction to order results by
  *)
  let get_registry_packages ~registry_name ?page ?per_page ?created_after ?updated_after ?created_before ?updated_before ?critical ?sort ?order client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name)] "/registries/{registryName}/packages" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page; Openapi.Runtime.Query.optional ~key:"created_after" ~value:created_after; Openapi.Runtime.Query.optional ~key:"updated_after" ~value:updated_after; Openapi.Runtime.Query.optional ~key:"created_before" ~value:created_before; Openapi.Runtime.Query.optional ~key:"updated_before" ~value:updated_before; Openapi.Runtime.Query.optional ~key:"critical" ~value:critical; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Package\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Package\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_packages" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get a package by name
      @param registry_name name of registry
      @param package_name name of package
  *)
  let get_registry_package ~registry_name ~package_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("packageName", package_name)] "/registries/{registryName}/packages/{packageName}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Package\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Package\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_package" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get a list of packages that depend on a package
      @param registry_name name of registry
      @param package_name name of package
      @param page pagination page number
      @param per_page Number of records to return
      @param created_after filter by created_at after given time
      @param updated_after filter by updated_at after given time
      @param sort field to order results by
      @param order direction to order results by
      @param latest only include packages whose latest version depends on this package (default true). Set to false to include historical dependents.
      @param kind filter by dependency kind
  *)
  let get_registry_package_dependent_packages ~registry_name ~package_name ?page ?per_page ?created_after ?updated_after ?sort ?order ?latest ?kind client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("packageName", package_name)] "/registries/{registryName}/packages/{packageName}/dependent_packages" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page; Openapi.Runtime.Query.optional ~key:"created_after" ~value:created_after; Openapi.Runtime.Query.optional ~key:"updated_after" ~value:updated_after; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order; Openapi.Runtime.Query.optional ~key:"latest" ~value:latest; Openapi.Runtime.Query.optional ~key:"kind" ~value:kind]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Package\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Package\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_package_dependent_packages" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** get a list of packages that are related to a package
      @param registry_name name of registry
      @param package_name name of package
      @param page pagination page number
      @param per_page Number of records to return
      @param created_after filter by created_at after given time
      @param updated_after filter by updated_at after given time
      @param sort field to order results by
      @param order direction to order results by
  *)
  let get_registry_package_related_packages ~registry_name ~package_name ?page ?per_page ?created_after ?updated_after ?sort ?order client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registryName", registry_name); ("packageName", package_name)] "/registries/{registryName}/packages/{packageName}/related_packages" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page; Openapi.Runtime.Query.optional ~key:"created_after" ~value:created_after; Openapi.Runtime.Query.optional ~key:"updated_after" ~value:updated_after; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"order" ~value:order]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Package\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/Package\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_registry_package_related_packages" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module KeywordWithPackages = struct
  module Types = struct
    module T = struct
      type t = {
        name : string;
        packages : Package.T.t list;
        packages_count : int option;
        packages_url : string option;
        related_keywords : Keyword.T.t list;
      }
    end
  end

  module T = struct
    include Types.T

    let v ~name ~packages ~related_keywords ?packages_count ?packages_url () = { name; packages; packages_count; packages_url; related_keywords }

    let name t = t.name
    let packages t = t.packages
    let packages_count t = t.packages_count
    let packages_url t = t.packages_url
    let related_keywords t = t.related_keywords

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"KeywordWithPackages"
        (fun name packages packages_count packages_url related_keywords -> { name; packages; packages_count; packages_url; related_keywords })
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.mem "packages" (Jsont.list Package.T.jsont) ~enc:(fun r -> r.packages)
      |> Jsont.Object.mem "packages_count" (Jsont.option Openapi.Runtime.int_jsont) ~enc:(fun r -> r.packages_count)
      |> Jsont.Object.opt_mem "packages_url" Jsont.string ~enc:(fun r -> r.packages_url)
      |> Jsont.Object.mem "related_keywords" (Jsont.list Keyword.T.jsont) ~enc:(fun r -> r.related_keywords)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "KeywordWithPackages" jsont
  end

  (** get a keyword by name
      @param keyword_name name of keyword
      @param page pagination page number
      @param per_page Number of records to return
  *)
  let get_keyword ~keyword_name ?page ?per_page client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("keywordName", keyword_name)] "/keywords/{keywordName}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"page" ~value:page; Openapi.Runtime.Query.optional ~key:"per_page" ~value:per_page]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/KeywordWithPackages\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/KeywordWithPackages\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_keyword" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end
