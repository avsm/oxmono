(** {1 Ecosystems}

    An open API service providing package, version and dependency metadata of many open source software ecosystems and registries.

    @version 1.1.0 *)

type t

val of_fetch : ?max_response_bytes:int -> base_url:string -> _ Fetch.t -> t
(** Use an existing Fetch stack, including scoped credentials, retries and limits.
    [max_response_bytes] defaults to 16 MiB. JSON requires a declared Content-Type.
    Response bodies are closed before returning. Writes do not follow redirects. *)

val create :
  ?session:_ Fetch.t ->
  ?max_response_bytes:int ->
  sw:Eio.Switch.t ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  base_url:string ->
  t
(** [create ?session ~sw env ~base_url] is a client rooted at [base_url].
    [session] is the HTTP client to issue requests through, already carrying
    whatever credentials and policy the caller wants; when it is omitted a
    default {!Fetch_curl.std} stack is created under [sw]. *)

val base_url : t -> string
val session : t -> Fetch.plain

module VersionWithPackage : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : codemeta_url:string -> created_at:Ptime.t -> id:int -> latest:bool -> number:string -> package_url:string -> purl:string -> updated_at:Ptime.t -> version_url:string -> ?documentation_url:string -> ?download_url:string -> ?install_command:string -> ?integrity:string -> ?licenses:string -> ?metadata:Jsont.json -> ?published_at:string -> ?registry_url:string -> ?status:string -> unit -> t

    val codemeta_url : t -> string

    val created_at : t -> Ptime.t

    val documentation_url : t -> string option

    val download_url : t -> string option

    val id : t -> int

    val install_command : t -> string option

    val integrity : t -> string option

    val latest : t -> bool

    val licenses : t -> string option

    val metadata : t -> Jsont.json option

    val number : t -> string

    val package_url : t -> string

    val published_at : t -> string option

    val purl : t -> string

    val registry_url : t -> string option

    val status : t -> string option

    val updated_at : t -> Ptime.t

    val version_url : t -> string

    val jsont : t Jsont.t
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
  val get_registry_recent_versions : registry_name:string -> ?page:string -> ?per_page:string -> ?created_after:string -> ?updated_after:string -> ?published_after:string -> ?published_before:string -> ?created_before:string -> ?updated_before:string -> ?sort:string -> ?order:string -> t -> unit -> T.t list
end

module Version : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : created_at:Ptime.t -> id:int -> number:string -> purl:string -> related_tag:Jsont.json -> updated_at:Ptime.t -> version_url:string -> ?codemeta_url:string -> ?documentation_url:string -> ?download_url:string -> ?install_command:string -> ?integrity:string -> ?licenses:string -> ?metadata:Jsont.json -> ?published_at:string -> ?registry_url:string -> ?status:string -> unit -> t

    val codemeta_url : t -> string option

    val created_at : t -> Ptime.t

    val documentation_url : t -> string option

    val download_url : t -> string option

    val id : t -> int

    val install_command : t -> string option

    val integrity : t -> string option

    val licenses : t -> string option

    val metadata : t -> Jsont.json option

    val number : t -> string

    val published_at : t -> string option

    val purl : t -> string

    val registry_url : t -> string option

    val related_tag : t -> Jsont.json

    val status : t -> string option

    val updated_at : t -> Ptime.t

    val version_url : t -> string

    val jsont : t Jsont.t
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
  val get_registry_package_versions : registry_name:string -> package_name:string -> ?page:string -> ?per_page:string -> ?created_after:string -> ?updated_after:string -> ?published_after:string -> ?published_before:string -> ?created_before:string -> ?updated_before:string -> ?sort:string -> ?order:string -> t -> unit -> T.t list
end

module Registry : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : created_at:Ptime.t -> default:bool -> downloads:int64 -> ecosystem:string -> icon_url:string -> keywords_count:int64 -> maintainers_count:int64 -> maintainers_url:string -> name:string -> namespaces_count:int64 -> packages_count:int64 -> packages_url:string -> purl_type:string -> updated_at:Ptime.t -> url:string -> ?github:string -> ?metadata:Jsont.json -> ?versions_count:int64 -> unit -> t

    val created_at : t -> Ptime.t

    val default : t -> bool

    val downloads : t -> int64

    val ecosystem : t -> string

    val github : t -> string option

    val icon_url : t -> string

    val keywords_count : t -> int64

    val maintainers_count : t -> int64

    val maintainers_url : t -> string

    val metadata : t -> Jsont.json option

    val name : t -> string

    val namespaces_count : t -> int64

    val packages_count : t -> int64

    val packages_url : t -> string

    val purl_type : t -> string

    val updated_at : t -> Ptime.t

    val url : t -> string

    val versions_count : t -> int64 option

    val jsont : t Jsont.t
  end

  (** list registries
      @param ecosystem filter by ecosystem name
      @param page pagination page number
      @param per_page Number of records to return
  *)
  val get_registries : ?ecosystem:string -> ?page:string -> ?per_page:string -> t -> unit -> T.t list

  (** get a registry by name
      @param registry_name name of registry
      @param page pagination page number
      @param per_page Number of records to return
  *)
  val get_registry : registry_name:string -> ?page:string -> ?per_page:string -> t -> unit -> T.t
end

module Namespace : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : name:string -> packages_count:int -> packages_url:string -> unit -> t

    val name : t -> string

    val packages_count : t -> int

    val packages_url : t -> string

    val jsont : t Jsont.t
  end

  (** get a list of namespaces from a registry
      @param registry_name name of registry
      @param page pagination page number
      @param per_page Number of records to return
  *)
  val get_registry_namespaces : registry_name:string -> ?page:string -> ?per_page:string -> t -> unit -> T.t list

  (** get a namespace by name
      @param registry_name name of registry
      @param namespace_name name of namespace
  *)
  val get_registry_namespace : registry_name:string -> namespace_name:string -> t -> unit -> T.t
end

module Maintainer : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : created_at:Ptime.t -> packages_count:int -> packages_url:string -> updated_at:Ptime.t -> uuid:string -> ?email:string -> ?html_url:string -> ?login:string -> ?name:string -> ?role:string option -> ?total_downloads:int -> ?url:string -> unit -> t

    val created_at : t -> Ptime.t

    val email : t -> string option

    val html_url : t -> string option

    val login : t -> string option

    val name : t -> string option

    val packages_count : t -> int

    val packages_url : t -> string

    val role : t -> string option option

    val total_downloads : t -> int option

    val updated_at : t -> Ptime.t

    val url : t -> string option

    val uuid : t -> string

    val jsont : t Jsont.t
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
  val get_registry_maintainers : registry_name:string -> ?page:string -> ?per_page:string -> ?created_after:string -> ?updated_after:string -> ?sort:string -> ?order:string -> t -> unit -> T.t list

  (** get a maintainer by login or UUID
      @param registry_name name of registry
      @param maintainer_login_or_uuid login or uuid of maintainer
  *)
  val get_registry_maintainer : registry_name:string -> maintainer_login_or_uuid:string -> t -> unit -> T.t
end

module Keyword : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : name:string -> packages_count:int -> ?packages_url:string -> unit -> t

    val name : t -> string

    val packages_count : t -> int

    val packages_url : t -> string option

    val jsont : t Jsont.t
  end

  (** list keywords
      @param page pagination page number
      @param per_page Number of records to return
  *)
  val get_keywords : ?page:string -> ?per_page:string -> t -> unit -> T.t list
end

module Dependency : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : ecosystem:string -> id:int -> package_name:string -> ?kind:string -> ?optional:bool -> ?requirements:string -> unit -> t

    val ecosystem : t -> string

    val id : t -> int

    val kind : t -> string option

    val optional : t -> bool option

    val package_name : t -> string

    val requirements : t -> string option

    val jsont : t Jsont.t
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
  val get_dependencies : ?page:string -> ?per_page:string -> ?ecosystem:string -> ?version_id:string -> ?package_name:string -> ?package_id:string -> ?requirements:string -> ?kind:string -> ?optional:string -> ?after:string -> ?sort:string -> ?order:string -> t -> unit -> T.t list
end

module VersionWithDependencies : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : codemeta_url:string -> created_at:Ptime.t -> dependencies:Dependency.T.t list -> latest:bool -> number:string -> purl:string -> related_tag:Jsont.json -> updated_at:Ptime.t -> version_url:string -> ?documentation_url:string -> ?download_url:string -> ?id:int -> ?install_command:string -> ?integrity:string -> ?licenses:string -> ?metadata:Jsont.json -> ?published_at:string -> ?registry_url:string -> ?status:string -> unit -> t

    val codemeta_url : t -> string

    val created_at : t -> Ptime.t

    val dependencies : t -> Dependency.T.t list

    val documentation_url : t -> string option

    val download_url : t -> string option

    val id : t -> int option

    val install_command : t -> string option

    val integrity : t -> string option

    val latest : t -> bool

    val licenses : t -> string option

    val metadata : t -> Jsont.json option

    val number : t -> string

    val published_at : t -> string option

    val purl : t -> string

    val registry_url : t -> string option

    val related_tag : t -> Jsont.json

    val status : t -> string option

    val updated_at : t -> Ptime.t

    val version_url : t -> string

    val jsont : t Jsont.t
  end

  (** get the latest version of a package
      @param registry_name name of registry
      @param package_name name of package
  *)
  val get_registry_package_latest_version : registry_name:string -> package_name:string -> t -> unit -> T.t

  (** get a version of a package
      @param registry_name name of registry
      @param package_name name of package
      @param version_number number of version
  *)
  val get_registry_package_version : registry_name:string -> package_name:string -> version_number:string -> t -> unit -> T.t
end

module CodeMeta : sig
  module T : sig
    (** CodeMeta JSON-LD metadata format for software packages, compatible with Software Heritage *)
    type t

    (** Construct a value
        @param context JSON-LD context URL
        @param type_ Type of software artifact
        @param identifier Package URL (purl) identifier
        @param name Package name
        @param application_category Package ecosystem/category
        @param author Package authors
        @param code_repository Source code repository URL
        @param copyright_holder Copyright holders
        @param copyright_year Copyright year
        @param date_created Creation date (ISO 8601)
        @param date_modified Last modification date (ISO 8601)
        @param date_published Publication date (ISO 8601)
        @param description Package description
        @param development_status Development status
        @param download_url Package download URL
        @param funder Funding sources
        @param https__forgefed_org_nsforks Fork count (ForgeFed)
        @param https__www_w3_org_ns_activitystreamslikes Star/like count (ActivityStreams)
        @param issue_tracker Issue tracker URL
        @param keywords Keywords and tags
        @param license SPDX license URL(s)
        @param maintainer Package maintainers
        @param programming_language Programming language information
        @param runtime_platform Runtime platform/ecosystem
        @param same_as Alternative identifiers/URLs
        @param software_help Documentation/help resources
        @param software_version Software version
        @param url Homepage URL
        @param version Version number
    *)
    val v : context:string -> type_:string -> identifier:string -> name:string -> ?application_category:string option -> ?author:Jsont.json list option -> ?code_repository:string option -> ?copyright_holder:Jsont.json list option -> ?copyright_year:int option -> ?date_created:string option -> ?date_modified:string option -> ?date_published:string option -> ?description:string option -> ?development_status:string option -> ?download_url:string option -> ?funder:Jsont.json list option -> ?https__forgefed_org_nsforks:int option -> ?https__www_w3_org_ns_activitystreamslikes:int option -> ?issue_tracker:string option -> ?keywords:string list option -> ?license:Jsont.json option -> ?maintainer:Jsont.json list option -> ?programming_language:Jsont.json option -> ?runtime_platform:string option -> ?same_as:string list option -> ?software_help:Jsont.json option -> ?software_version:string option -> ?url:string option -> ?version:string option -> unit -> t

    (** JSON-LD context URL *)
    val context : t -> string

    (** Type of software artifact *)
    val type_ : t -> string

    (** Package ecosystem/category *)
    val application_category : t -> string option option

    (** Package authors *)
    val author : t -> Jsont.json list option option

    (** Source code repository URL *)
    val code_repository : t -> string option option

    (** Copyright holders *)
    val copyright_holder : t -> Jsont.json list option option

    (** Copyright year *)
    val copyright_year : t -> int option option

    (** Creation date (ISO 8601) *)
    val date_created : t -> string option option

    (** Last modification date (ISO 8601) *)
    val date_modified : t -> string option option

    (** Publication date (ISO 8601) *)
    val date_published : t -> string option option

    (** Package description *)
    val description : t -> string option option

    (** Development status *)
    val development_status : t -> string option option

    (** Package download URL *)
    val download_url : t -> string option option

    (** Funding sources *)
    val funder : t -> Jsont.json list option option

    (** Fork count (ForgeFed) *)
    val https__forgefed_org_nsforks : t -> int option option

    (** Star/like count (ActivityStreams) *)
    val https__www_w3_org_ns_activitystreamslikes : t -> int option option

    (** Package URL (purl) identifier *)
    val identifier : t -> string

    (** Issue tracker URL *)
    val issue_tracker : t -> string option option

    (** Keywords and tags *)
    val keywords : t -> string list option option

    (** SPDX license URL(s) *)
    val license : t -> Jsont.json option option

    (** Package maintainers *)
    val maintainer : t -> Jsont.json list option option

    (** Package name *)
    val name : t -> string

    (** Programming language information *)
    val programming_language : t -> Jsont.json option option

    (** Runtime platform/ecosystem *)
    val runtime_platform : t -> string option option

    (** Alternative identifiers/URLs *)
    val same_as : t -> string list option option

    (** Documentation/help resources *)
    val software_help : t -> Jsont.json option option

    (** Software version *)
    val software_version : t -> string option option

    (** Homepage URL *)
    val url : t -> string option option

    (** Version number *)
    val version : t -> string option option

    val jsont : t Jsont.t
  end

  (** get CodeMeta metadata for a package
      @param registry_name name of registry
      @param package_name name of package
  *)
  val get_registry_package_code_meta : registry_name:string -> package_name:string -> t -> unit -> T.t

  (** get CodeMeta metadata for a version
      @param registry_name name of registry
      @param package_name name of package
      @param version_number number of version
  *)
  val get_registry_package_version_code_meta : registry_name:string -> package_name:string -> version_number:string -> t -> unit -> T.t
end

module Client : sig
  (** list unique maintainers of critical packages
      @param registry filter by registry name
      @param page pagination page number
      @param per_page Number of records to return
      @param sort field to sort results by (login or packages_count)
      @param order direction to sort results by (asc or desc)
  *)
  val get_critical_maintainers : ?registry:string -> ?page:string -> ?per_page:string -> ?sort:string -> ?order:string -> t -> unit -> Jsont.json list

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
  val get_registry_package_names : registry_name:string -> ?page:string -> ?per_page:string -> ?created_after:string -> ?updated_after:string -> ?created_before:string -> ?updated_before:string -> ?sort:string -> ?order:string -> ?critical:string -> ?funding:string -> ?prefix:string -> ?postfix:string -> t -> unit -> string list

  (** get a list of dependency kinds for a package
      @param registry_name name of registry
      @param package_name name of package
      @param latest only count packages whose latest version depends on this package (default true). Set to false to include historical dependents.
  *)
  val get_registry_package_dependent_package_kinds : registry_name:string -> package_name:string -> ?latest:string -> t -> unit -> string list

  (** get a list of version numbers for a package from a registry
      @param registry_name name of registry
      @param package_name name of package
  *)
  val get_registry_package_version_numbers : registry_name:string -> package_name:string -> t -> unit -> string list
end

module Advisory : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : created_at:string -> identifiers:string option list -> packages:Jsont.json list -> references:string option list -> updated_at:string -> uuid:string -> ?classification:string -> ?cvss_score:float -> ?cvss_vector:string -> ?description:string -> ?origin:string -> ?published_at:string -> ?severity:string -> ?source_kind:string -> ?title:string -> ?url:string -> ?withdrawn_at:string -> unit -> t

    val classification : t -> string option

    val created_at : t -> string

    val cvss_score : t -> float option

    val cvss_vector : t -> string option

    val description : t -> string option

    val identifiers : t -> string option list

    val origin : t -> string option

    val packages : t -> Jsont.json list

    val published_at : t -> string option

    val references : t -> string option list

    val severity : t -> string option

    val source_kind : t -> string option

    val title : t -> string option

    val updated_at : t -> string

    val url : t -> string option

    val uuid : t -> string

    val withdrawn_at : t -> string option

    val jsont : t Jsont.t
  end
end

module PackageWithRegistry : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : advisories:Advisory.T.t list -> codemeta_url:string -> created_at:Ptime.t -> dependent_packages_count:int -> dependent_packages_url:string -> dependent_repos_count:int -> dependent_repositories_url:string -> docker_usage_url:string -> downloads:int -> ecosystem:string -> funding_links:string list -> id:int -> issue_metadata:Jsont.json -> keywords_array:string list -> latest_version_url:string -> maintainers:Maintainer.T.t list -> name:string -> normalized_licenses:string list -> purl:string -> rankings:Jsont.json -> related_packages_url:string -> updated_at:Ptime.t -> usage_url:string -> versions_count:int -> versions_url:string -> registry:Registry.T.t -> ?critical:bool -> ?description:string -> ?docker_dependents_count:int -> ?docker_downloads_count:int -> ?documentation_url:string -> ?downloads_period:string -> ?first_release_published_at:Ptime.t -> ?homepage:string -> ?install_command:string -> ?last_synced_at:Ptime.t -> ?latest_release_number:string -> ?latest_release_published_at:Ptime.t -> ?licenses:string -> ?metadata:Jsont.json -> ?namespace:string -> ?registry_url:string -> ?repo_metadata:Jsont.json -> ?repo_metadata_updated_at:Ptime.t -> ?repository_url:string -> ?status:string -> ?version_numbers_url:string -> unit -> t

    val advisories : t -> Advisory.T.t list

    val codemeta_url : t -> string

    val created_at : t -> Ptime.t

    val critical : t -> bool option

    val dependent_packages_count : t -> int

    val dependent_packages_url : t -> string

    val dependent_repos_count : t -> int

    val dependent_repositories_url : t -> string

    val description : t -> string option

    val docker_dependents_count : t -> int option

    val docker_downloads_count : t -> int option

    val docker_usage_url : t -> string

    val documentation_url : t -> string option

    val downloads : t -> int

    val downloads_period : t -> string option

    val ecosystem : t -> string

    val first_release_published_at : t -> Ptime.t option

    val funding_links : t -> string list

    val homepage : t -> string option

    val id : t -> int

    val install_command : t -> string option

    val issue_metadata : t -> Jsont.json

    val keywords_array : t -> string list

    val last_synced_at : t -> Ptime.t option

    val latest_release_number : t -> string option

    val latest_release_published_at : t -> Ptime.t option

    val latest_version_url : t -> string

    val licenses : t -> string option

    val maintainers : t -> Maintainer.T.t list

    val metadata : t -> Jsont.json option

    val name : t -> string

    val namespace : t -> string option

    val normalized_licenses : t -> string list

    val purl : t -> string

    val rankings : t -> Jsont.json

    val registry_url : t -> string option

    val related_packages_url : t -> string

    val repo_metadata : t -> Jsont.json option

    val repo_metadata_updated_at : t -> Ptime.t option

    val repository_url : t -> string option

    val status : t -> string option

    val updated_at : t -> Ptime.t

    val usage_url : t -> string

    val version_numbers_url : t -> string option

    val versions_count : t -> int

    val versions_url : t -> string

    val registry : t -> Registry.T.t

    val jsont : t Jsont.t
  end

  (** list critical packages
      @param registry filter by registry name
      @param page pagination page number
      @param per_page Number of records to return
      @param sort field to sort results by
      @param order direction to sort results by (asc or desc)
  *)
  val get_critical_packages : ?registry:string -> ?page:string -> ?per_page:string -> ?sort:string -> ?order:string -> t -> unit -> T.t list

  (** list critical packages with sole maintainers
      @param registry filter by registry name
      @param page pagination page number
      @param per_page Number of records to return
      @param sort field to sort results by
      @param order direction to sort results by (asc or desc)
  *)
  val get_critical_sole_maintainers : ?registry:string -> ?page:string -> ?per_page:string -> ?sort:string -> ?order:string -> t -> unit -> T.t list

  (** lookup multiple packages by repository URLs, PURLs, or names *)
  val bulk_lookup_packages : body:Jsont.json -> t -> unit -> T.t list

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
  val get_critical_packages_list : ?page:string -> ?per_page:string -> ?created_after:string -> ?updated_after:string -> ?created_before:string -> ?updated_before:string -> ?funding:string -> ?sort:string -> ?order:string -> t -> unit -> T.t list

  (** lookup a single package by repository URL, purl or ecosystem+name. For multiple packages use POST /packages/bulk_lookup.
      @param repository_url repository URL
      @param purl single package URL. For multiple purls use POST /packages/bulk_lookup.
      @param ecosystem ecosystem name
      @param name package name
      @param sort field to sort results by
      @param order direction to sort results by
  *)
  val lookup_package : ?repository_url:string -> ?purl:string -> ?ecosystem:string -> ?name:string -> ?sort:string -> ?order:string -> t -> unit -> T.t list

  (** lookup a package within a registry by repository URL, purl or ecosystem+name
      @param registry_name name of registry
      @param repository_url repository URL
      @param purl single package URL. For multiple purls use POST /packages/bulk_lookup.
      @param ecosystem ecosystem name
      @param name package name
      @param sort field to sort results by
      @param order direction to sort results by
  *)
  val lookup_registry_package : registry_name:string -> ?repository_url:string -> ?purl:string -> ?ecosystem:string -> ?name:string -> ?sort:string -> ?order:string -> t -> unit -> T.t list
end

module VersionLookup : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : codemeta_url:string -> created_at:Ptime.t -> dependencies:Dependency.T.t list -> latest:bool -> number:string -> purl:string -> related_tag:Jsont.json -> updated_at:Ptime.t -> version_url:string -> package:PackageWithRegistry.T.t -> ?documentation_url:string -> ?download_url:string -> ?id:int -> ?install_command:string -> ?integrity:string -> ?licenses:string -> ?metadata:Jsont.json -> ?published_at:string -> ?registry_url:string -> ?status:string -> unit -> t

    val codemeta_url : t -> string

    val created_at : t -> Ptime.t

    val dependencies : t -> Dependency.T.t list

    val documentation_url : t -> string option

    val download_url : t -> string option

    val id : t -> int option

    val install_command : t -> string option

    val integrity : t -> string option

    val latest : t -> bool

    val licenses : t -> string option

    val metadata : t -> Jsont.json option

    val number : t -> string

    val published_at : t -> string option

    val purl : t -> string

    val registry_url : t -> string option

    val related_tag : t -> Jsont.json

    val status : t -> string option

    val updated_at : t -> Ptime.t

    val version_url : t -> string

    val package : t -> PackageWithRegistry.T.t

    val jsont : t Jsont.t
  end

  (** lookup versions by integrity hash
      @param integrity integrity hash (SRI format)
      @param sha256 sha256 hash (hex)
      @param sha1 sha1 hash (hex)
      @param sha512 sha512 hash (hex)
      @param page page number
      @param per_page Number of records to return
  *)
  val lookup_versions : ?integrity:string -> ?sha256:string -> ?sha1:string -> ?sha512:string -> ?page:string -> ?per_page:string -> t -> unit -> T.t list
end

module Package : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : advisories:Advisory.T.t list -> codemeta_url:string -> created_at:Ptime.t -> dependent_packages_count:int -> dependent_packages_url:string -> dependent_repos_count:int -> dependent_repositories_url:string -> docker_usage_url:string -> downloads:int -> ecosystem:string -> funding_links:string list -> id:int -> issue_metadata:Jsont.json -> keywords_array:string list -> latest_version_url:string -> maintainers:Maintainer.T.t list -> name:string -> normalized_licenses:string list -> purl:string -> rankings:Jsont.json -> related_packages_url:string -> updated_at:Ptime.t -> usage_url:string -> versions_count:int -> versions_url:string -> ?critical:bool -> ?description:string -> ?docker_dependents_count:int -> ?docker_downloads_count:int -> ?documentation_url:string -> ?downloads_period:string -> ?first_release_published_at:Ptime.t -> ?homepage:string -> ?install_command:string -> ?last_synced_at:Ptime.t -> ?latest_release_number:string -> ?latest_release_published_at:Ptime.t -> ?licenses:string -> ?metadata:Jsont.json -> ?namespace:string -> ?registry_url:string -> ?repo_metadata:Jsont.json -> ?repo_metadata_updated_at:Ptime.t -> ?repository_url:string -> ?status:string -> ?version_numbers_url:string -> unit -> t

    val advisories : t -> Advisory.T.t list

    val codemeta_url : t -> string

    val created_at : t -> Ptime.t

    val critical : t -> bool option

    val dependent_packages_count : t -> int

    val dependent_packages_url : t -> string

    val dependent_repos_count : t -> int

    val dependent_repositories_url : t -> string

    val description : t -> string option

    val docker_dependents_count : t -> int option

    val docker_downloads_count : t -> int option

    val docker_usage_url : t -> string

    val documentation_url : t -> string option

    val downloads : t -> int

    val downloads_period : t -> string option

    val ecosystem : t -> string

    val first_release_published_at : t -> Ptime.t option

    val funding_links : t -> string list

    val homepage : t -> string option

    val id : t -> int

    val install_command : t -> string option

    val issue_metadata : t -> Jsont.json

    val keywords_array : t -> string list

    val last_synced_at : t -> Ptime.t option

    val latest_release_number : t -> string option

    val latest_release_published_at : t -> Ptime.t option

    val latest_version_url : t -> string

    val licenses : t -> string option

    val maintainers : t -> Maintainer.T.t list

    val metadata : t -> Jsont.json option

    val name : t -> string

    val namespace : t -> string option

    val normalized_licenses : t -> string list

    val purl : t -> string

    val rankings : t -> Jsont.json

    val registry_url : t -> string option

    val related_packages_url : t -> string

    val repo_metadata : t -> Jsont.json option

    val repo_metadata_updated_at : t -> Ptime.t option

    val repository_url : t -> string option

    val status : t -> string option

    val updated_at : t -> Ptime.t

    val usage_url : t -> string

    val version_numbers_url : t -> string option

    val versions_count : t -> int

    val versions_url : t -> string

    val jsont : t Jsont.t
  end

  (** get packages for a maintainer by login or UUID
      @param registry_name name of registry
      @param maintainer_login_or_uuid login or uuid of maintainer
      @param page pagination page number
      @param per_page Number of records to return
  *)
  val get_registry_maintainer_packages : registry_name:string -> maintainer_login_or_uuid:string -> ?page:string -> ?per_page:string -> t -> unit -> T.t list

  (** get packages for a namespace by login or UUID
      @param registry_name name of registry
      @param namespace_name lname of namespace
      @param page pagination page number
      @param per_page Number of records to return
  *)
  val get_registry_namespace_packages : registry_name:string -> namespace_name:string -> ?page:string -> ?per_page:string -> t -> unit -> T.t list

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
  val get_registry_packages : registry_name:string -> ?page:string -> ?per_page:string -> ?created_after:string -> ?updated_after:string -> ?created_before:string -> ?updated_before:string -> ?critical:string -> ?sort:string -> ?order:string -> t -> unit -> T.t list

  (** get a package by name
      @param registry_name name of registry
      @param package_name name of package
  *)
  val get_registry_package : registry_name:string -> package_name:string -> t -> unit -> T.t

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
  val get_registry_package_dependent_packages : registry_name:string -> package_name:string -> ?page:string -> ?per_page:string -> ?created_after:string -> ?updated_after:string -> ?sort:string -> ?order:string -> ?latest:string -> ?kind:string -> t -> unit -> T.t list

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
  val get_registry_package_related_packages : registry_name:string -> package_name:string -> ?page:string -> ?per_page:string -> ?created_after:string -> ?updated_after:string -> ?sort:string -> ?order:string -> t -> unit -> T.t list
end

module KeywordWithPackages : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : name:string -> packages:Package.T.t list -> related_keywords:Keyword.T.t list -> ?packages_count:int -> ?packages_url:string -> unit -> t

    val name : t -> string

    val packages : t -> Package.T.t list

    val packages_count : t -> int option

    val packages_url : t -> string option

    val related_keywords : t -> Keyword.T.t list

    val jsont : t Jsont.t
  end

  (** get a keyword by name
      @param keyword_name name of keyword
      @param page pagination page number
      @param per_page Number of records to return
  *)
  val get_keyword : keyword_name:string -> ?page:string -> ?per_page:string -> t -> unit -> T.t
end
