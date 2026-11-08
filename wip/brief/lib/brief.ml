(* SPDX-License-Identifier: ISC *)

type confidence = High | Medium | Low | Other of string
type source = Project_script | Knowledge_base | Config_file | Other of string

type command = {
  run : string;
  alternatives : string list;
  source : source;
  inferred_tool : string option;
}

type taxonomy = {
  role : string list;
  function_ : string list;
  layer : string list;
  domain : string list;
  audience : string list;
  technology : string list;
}

type detection = {
  name : string;
  category : string;
  confidence : confidence;
  command : command option;
  config_files : string list;
  lockfile : string option;
  homepage : string option;
  docs : string option;
  repo : string option;
  description : string option;
  taxonomy : taxonomy option;
}

type script = { name : string; run : string; source : string }
type manifest = { ecosystem : string; path : string; kind : string }

type dependency_source = {
  kind : string;
  value : string option;
  branch : string option;
  tag : string option;
  ref_ : string option;
  rel : string option;
}

type dependency = {
  manifest : string;
  name : string;
  version : string option;
  purl : string;
  scope : string option;
  direct : bool;
  source : dependency_source option;
}

type style = {
  indentation : string option;
  indent_source : string option;
  line_ending : string option;
  trailing_newline : bool option;
}

type layout = {
  source_dirs : string list;
  test_dirs : string list;
  mirrors_source : bool;
}

type platforms = {
  runtime_version_files : (string * string) list;
  ci_matrix_versions : (string * string list) list;
  ci_matrix_os : string list;
}

type resources = {
  readme : string option;
  changelog : string option;
  roadmap : string option;
  license : string option;
  license_type : string option;
  legal : (string * string) list;
  community : (string * string) list;
  security : (string * string) list;
  metadata : (string * string) list;
  agents : (string * string) list;
}

type skill = {
  name : string;
  description : string option;
  path : string;
  format : string;
}

type git = {
  branch : string option;
  default_branch : string option;
  remotes : (string * string) list;
  commit_count : int;
}

type lines = {
  total_files : int;
  total_lines : int;
  by_language : (string * int) list;
  source : string;
}

type stats = {
  duration_ms : float;
  files_checked : int;
  tools_matched : int;
  tools_checked : int;
  scan_entries : int;
  scan_truncated : bool;
}

type t = {
  version : string;
  path : string;
  diff_ref : string option;
  diff_commits : string list;
  changed_files : string list;
  languages : detection list;
  package_managers : detection list;
  scripts : script list;
  tools : (string * detection list) list;
  style : style option;
  layout : layout option;
  platforms : platforms option;
  resources : resources option;
  skills : skill list;
  git : git option;
  lines : lines option;
  manifests : manifest list;
  dependencies : dependency list;
  stats : stats;
}

module O = Jsont.Object

let empty_list () = []
let is_empty l = l = []
let is_false b = not b

(* A list member that brief leaves out when it is empty. *)
let list_mem name t ~(enc @ portable) map =
  O.mem name (Jsont.list t) ~dec_absent:empty_list ~enc ~enc_omit:is_empty map

let of_bindings l =
  List.fold_left
    (fun m (k, v) -> Jsont.String_map.add k v m)
    (Jsont.String_map.create ())
    l

let bindings m =
  List.rev (Jsont.String_map.fold (fun k v acc -> (k, v) :: acc) m [])

(* An object whose members are all of one type, as an association list sorted
   by member name. *)
let assoc t = Jsont.map ~dec:bindings ~enc:of_bindings (O.as_string_map t)

let assoc_mem name t ~(enc @ portable) map =
  O.mem name (assoc t) ~dec_absent:empty_list ~enc ~enc_omit:is_empty map

let confidence_jsont =
  Jsont.map ~kind:"confidence" Jsont.string
    ~dec:(function
      | "high" -> High | "medium" -> Medium | "low" -> Low | s -> Other s)
    ~enc:(function
      | High -> "high" | Medium -> "medium" | Low -> "low" | Other s -> s)

let source_jsont =
  Jsont.map ~kind:"source" Jsont.string
    ~dec:(function
      | "project_script" -> Project_script
      | "knowledge_base" -> Knowledge_base
      | "config_file" -> Config_file
      | s -> Other s)
    ~enc:(function
      | Project_script -> "project_script"
      | Knowledge_base -> "knowledge_base"
      | Config_file -> "config_file"
      | Other s -> s)

let command_jsont =
  O.map ~kind:"command" (fun run alternatives source inferred_tool : command ->
      { run; alternatives; source; inferred_tool })
  |> O.mem "run" Jsont.string ~enc:(fun (c : command) -> c.run)
  |> list_mem "alternatives" Jsont.string ~enc:(fun (c : command) ->
         c.alternatives)
  |> O.mem "source" source_jsont ~enc:(fun (c : command) -> c.source)
  |> O.opt_mem "inferred_tool" Jsont.string ~enc:(fun (c : command) ->
         c.inferred_tool)
  |> O.finish

let taxonomy_jsont =
  O.map ~kind:"taxonomy" (fun role function_ layer domain audience technology ->
      { role; function_; layer; domain; audience; technology })
  |> list_mem "role" Jsont.string ~enc:(fun (t : taxonomy) -> t.role)
  |> list_mem "function" Jsont.string ~enc:(fun (t : taxonomy) -> t.function_)
  |> list_mem "layer" Jsont.string ~enc:(fun (t : taxonomy) -> t.layer)
  |> list_mem "domain" Jsont.string ~enc:(fun (t : taxonomy) -> t.domain)
  |> list_mem "audience" Jsont.string ~enc:(fun (t : taxonomy) -> t.audience)
  |> list_mem "technology" Jsont.string ~enc:(fun (t : taxonomy) ->
         t.technology)
  |> O.finish

let detection_jsont =
  O.map ~kind:"detection"
    (fun
      name
      category
      confidence
      command
      config_files
      lockfile
      homepage
      docs
      repo
      description
      taxonomy
    ->
      {
        name;
        category;
        confidence;
        command;
        config_files;
        lockfile;
        homepage;
        docs;
        repo;
        description;
        taxonomy;
      })
  |> O.mem "name" Jsont.string ~enc:(fun (d : detection) -> d.name)
  |> O.mem "category" Jsont.string ~enc:(fun (d : detection) -> d.category)
  |> O.mem "confidence" confidence_jsont ~enc:(fun (d : detection) ->
         d.confidence)
  |> O.opt_mem "command" command_jsont ~enc:(fun (d : detection) -> d.command)
  |> list_mem "config_files" Jsont.string ~enc:(fun (d : detection) ->
         d.config_files)
  |> O.opt_mem "lockfile" Jsont.string ~enc:(fun (d : detection) -> d.lockfile)
  |> O.opt_mem "homepage" Jsont.string ~enc:(fun (d : detection) -> d.homepage)
  |> O.opt_mem "docs" Jsont.string ~enc:(fun (d : detection) -> d.docs)
  |> O.opt_mem "repo" Jsont.string ~enc:(fun (d : detection) -> d.repo)
  |> O.opt_mem "description" Jsont.string ~enc:(fun (d : detection) ->
         d.description)
  |> O.opt_mem "taxonomy" taxonomy_jsont ~enc:(fun (d : detection) ->
         d.taxonomy)
  |> O.finish

let script_jsont =
  O.map ~kind:"script" (fun name run source : script -> { name; run; source })
  |> O.mem "name" Jsont.string ~enc:(fun (s : script) -> s.name)
  |> O.mem "run" Jsont.string ~enc:(fun (s : script) -> s.run)
  |> O.mem "source" Jsont.string ~enc:(fun (s : script) -> s.source)
  |> O.finish

let manifest_jsont =
  O.map ~kind:"manifest" (fun ecosystem path kind : manifest ->
      { ecosystem; path; kind })
  |> O.mem "ecosystem" Jsont.string ~enc:(fun (m : manifest) -> m.ecosystem)
  |> O.mem "path" Jsont.string ~enc:(fun (m : manifest) -> m.path)
  |> O.mem "kind" Jsont.string ~enc:(fun (m : manifest) -> m.kind)
  |> O.finish

let dependency_source_jsont =
  O.map ~kind:"dependency source"
    (fun kind value branch tag ref_ rel : dependency_source ->
      { kind; value; branch; tag; ref_; rel })
  |> O.mem "kind" Jsont.string ~enc:(fun (s : dependency_source) -> s.kind)
  |> O.opt_mem "value" Jsont.string ~enc:(fun (s : dependency_source) ->
         s.value)
  |> O.opt_mem "branch" Jsont.string ~enc:(fun (s : dependency_source) ->
         s.branch)
  |> O.opt_mem "tag" Jsont.string ~enc:(fun (s : dependency_source) -> s.tag)
  |> O.opt_mem "ref" Jsont.string ~enc:(fun (s : dependency_source) -> s.ref_)
  |> O.opt_mem "rel" Jsont.string ~enc:(fun (s : dependency_source) -> s.rel)
  |> O.finish

let dependency_jsont =
  O.map ~kind:"dependency"
    (fun manifest name version purl scope direct source : dependency ->
      { manifest; name; version; purl; scope; direct; source })
  |> O.mem "manifest" Jsont.string ~enc:(fun (d : dependency) -> d.manifest)
  |> O.mem "name" Jsont.string ~enc:(fun (d : dependency) -> d.name)
  |> O.opt_mem "version" Jsont.string ~enc:(fun (d : dependency) -> d.version)
  |> O.mem "purl" Jsont.string ~enc:(fun (d : dependency) -> d.purl)
  |> O.opt_mem "scope" Jsont.string ~enc:(fun (d : dependency) -> d.scope)
  |> O.mem "direct" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun (d : dependency) -> d.direct)
  |> O.opt_mem "source" dependency_source_jsont ~enc:(fun (d : dependency) ->
         d.source)
  |> O.finish

let style_jsont =
  O.map ~kind:"style"
    (fun indentation indent_source line_ending trailing_newline ->
      { indentation; indent_source; line_ending; trailing_newline })
  |> O.opt_mem "indentation" Jsont.string ~enc:(fun (s : style) ->
         s.indentation)
  |> O.opt_mem "indent_source" Jsont.string ~enc:(fun (s : style) ->
         s.indent_source)
  |> O.opt_mem "line_ending" Jsont.string ~enc:(fun (s : style) ->
         s.line_ending)
  |> O.opt_mem "trailing_newline" Jsont.bool ~enc:(fun (s : style) ->
         s.trailing_newline)
  |> O.finish

let layout_jsont =
  O.map ~kind:"layout" (fun source_dirs test_dirs mirrors_source : layout ->
      { source_dirs; test_dirs; mirrors_source })
  |> list_mem "source_dirs" Jsont.string ~enc:(fun (l : layout) ->
         l.source_dirs)
  |> list_mem "test_dirs" Jsont.string ~enc:(fun (l : layout) -> l.test_dirs)
  |> O.mem "mirrors_source" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun (l : layout) -> l.mirrors_source)
       ~enc_omit:is_false
  |> O.finish

let platforms_jsont =
  O.map ~kind:"platforms"
    (fun runtime_version_files ci_matrix_versions ci_matrix_os ->
      { runtime_version_files; ci_matrix_versions; ci_matrix_os })
  |> assoc_mem "runtime_version_files" Jsont.string ~enc:(fun (p : platforms) ->
         p.runtime_version_files)
  |> assoc_mem "ci_matrix_versions" (Jsont.list Jsont.string)
       ~enc:(fun (p : platforms) -> p.ci_matrix_versions)
  |> list_mem "ci_matrix_os" Jsont.string ~enc:(fun (p : platforms) ->
         p.ci_matrix_os)
  |> O.finish

let resources_jsont =
  O.map ~kind:"resources"
    (fun
      readme
      changelog
      roadmap
      license
      license_type
      legal
      community
      security
      metadata
      agents
    ->
      {
        readme;
        changelog;
        roadmap;
        license;
        license_type;
        legal;
        community;
        security;
        metadata;
        agents;
      })
  |> O.opt_mem "readme" Jsont.string ~enc:(fun (r : resources) -> r.readme)
  |> O.opt_mem "changelog" Jsont.string ~enc:(fun (r : resources) ->
         r.changelog)
  |> O.opt_mem "roadmap" Jsont.string ~enc:(fun (r : resources) -> r.roadmap)
  |> O.opt_mem "license" Jsont.string ~enc:(fun (r : resources) -> r.license)
  |> O.opt_mem "license_type" Jsont.string ~enc:(fun (r : resources) ->
         r.license_type)
  |> assoc_mem "legal" Jsont.string ~enc:(fun (r : resources) -> r.legal)
  |> assoc_mem "community" Jsont.string ~enc:(fun (r : resources) ->
         r.community)
  |> assoc_mem "security" Jsont.string ~enc:(fun (r : resources) -> r.security)
  |> assoc_mem "metadata" Jsont.string ~enc:(fun (r : resources) -> r.metadata)
  |> assoc_mem "agents" Jsont.string ~enc:(fun (r : resources) -> r.agents)
  |> O.finish

let skill_jsont =
  O.map ~kind:"skill" (fun name description path format : skill ->
      { name; description; path; format })
  |> O.mem "name" Jsont.string ~enc:(fun (s : skill) -> s.name)
  |> O.opt_mem "description" Jsont.string ~enc:(fun (s : skill) ->
         s.description)
  |> O.mem "path" Jsont.string ~enc:(fun (s : skill) -> s.path)
  |> O.mem "format" Jsont.string ~enc:(fun (s : skill) -> s.format)
  |> O.finish

let git_jsont =
  O.map ~kind:"git" (fun branch default_branch remotes commit_count : git ->
      { branch; default_branch; remotes; commit_count })
  |> O.opt_mem "branch" Jsont.string ~enc:(fun (g : git) -> g.branch)
  |> O.opt_mem "default_branch" Jsont.string ~enc:(fun (g : git) ->
         g.default_branch)
  |> assoc_mem "remotes" Jsont.string ~enc:(fun (g : git) -> g.remotes)
  |> O.mem "commit_count" Jsont.int
       ~dec_absent:(fun () -> 0)
       ~enc:(fun (g : git) -> g.commit_count)
  |> O.finish

let lines_jsont =
  O.map ~kind:"lines" (fun total_files total_lines by_language source : lines ->
      { total_files; total_lines; by_language; source })
  |> O.mem "total_files" Jsont.int ~enc:(fun (l : lines) -> l.total_files)
  |> O.mem "total_lines" Jsont.int ~enc:(fun (l : lines) -> l.total_lines)
  |> assoc_mem "by_language" Jsont.int ~enc:(fun (l : lines) -> l.by_language)
  |> O.mem "source" Jsont.string ~enc:(fun (l : lines) -> l.source)
  |> O.finish

let stats_jsont =
  O.map ~kind:"stats"
    (fun
      duration_ms
      files_checked
      tools_matched
      tools_checked
      scan_entries
      scan_truncated
    ->
      {
        duration_ms;
        files_checked;
        tools_matched;
        tools_checked;
        scan_entries;
        scan_truncated;
      })
  |> O.mem "duration_ms" Jsont.number
       ~dec_absent:(fun () -> 0.)
       ~enc:(fun (s : stats) -> s.duration_ms)
  |> O.mem "files_checked" Jsont.int
       ~dec_absent:(fun () -> 0)
       ~enc:(fun (s : stats) -> s.files_checked)
  |> O.mem "tools_matched" Jsont.int
       ~dec_absent:(fun () -> 0)
       ~enc:(fun (s : stats) -> s.tools_matched)
  |> O.mem "tools_checked" Jsont.int
       ~dec_absent:(fun () -> 0)
       ~enc:(fun (s : stats) -> s.tools_checked)
  |> O.mem "scan_entries" Jsont.int
       ~dec_absent:(fun () -> 0)
       ~enc:(fun (s : stats) -> s.scan_entries)
  |> O.mem "scan_truncated" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun (s : stats) -> s.scan_truncated)
       ~enc_omit:is_false
  |> O.finish

let jsont =
  O.map ~kind:"brief report"
    (fun
      version
      path
      diff_ref
      diff_commits
      changed_files
      languages
      package_managers
      scripts
      tools
      style
      layout
      platforms
      resources
      skills
      git
      lines
      manifests
      dependencies
      stats
    ->
      {
        version;
        path;
        diff_ref;
        diff_commits;
        changed_files;
        languages;
        package_managers;
        scripts;
        tools;
        style;
        layout;
        platforms;
        resources;
        skills;
        git;
        lines;
        manifests;
        dependencies;
        stats;
      })
  |> O.mem "version" Jsont.string ~enc:(fun (t : t) -> t.version)
  |> O.mem "path" Jsont.string ~enc:(fun (t : t) -> t.path)
  |> O.opt_mem "diff_ref" Jsont.string ~enc:(fun (t : t) -> t.diff_ref)
  |> list_mem "diff_commits" Jsont.string ~enc:(fun (t : t) -> t.diff_commits)
  |> list_mem "changed_files" Jsont.string ~enc:(fun (t : t) -> t.changed_files)
  |> list_mem "languages" detection_jsont ~enc:(fun (t : t) -> t.languages)
  |> list_mem "package_managers" detection_jsont ~enc:(fun (t : t) ->
         t.package_managers)
  |> list_mem "scripts" script_jsont ~enc:(fun (t : t) -> t.scripts)
  |> assoc_mem "tools" (Jsont.list detection_jsont) ~enc:(fun (t : t) ->
         t.tools)
  |> O.opt_mem "style" style_jsont ~enc:(fun (t : t) -> t.style)
  |> O.opt_mem "layout" layout_jsont ~enc:(fun (t : t) -> t.layout)
  |> O.opt_mem "platforms" platforms_jsont ~enc:(fun (t : t) -> t.platforms)
  |> O.opt_mem "resources" resources_jsont ~enc:(fun (t : t) -> t.resources)
  |> list_mem "skills" skill_jsont ~enc:(fun (t : t) -> t.skills)
  |> O.opt_mem "git" git_jsont ~enc:(fun (t : t) -> t.git)
  |> O.opt_mem "lines" lines_jsont ~enc:(fun (t : t) -> t.lines)
  |> list_mem "manifests" manifest_jsont ~enc:(fun (t : t) -> t.manifests)
  |> list_mem "dependencies" dependency_jsont ~enc:(fun (t : t) ->
         t.dependencies)
  |> O.mem "stats" stats_jsont ~enc:(fun (t : t) -> t.stats)
  |> O.finish

let of_string s = Jsont_bytesrw.decode_string jsont s

let to_string t =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont t with
  | Ok s -> s
  | Error e -> invalid_arg e

let named name l =
  let name = String.lowercase_ascii name in
  List.find_opt (fun (d : detection) -> String.lowercase_ascii d.name = name) l

let language t name = named name t.languages
let package_manager t name = named name t.package_managers

let tool t ~category name =
  Option.bind (List.assoc_opt category t.tools) (named name)

let ecosystems t =
  List.fold_left
    (fun acc (m : manifest) ->
      if List.mem m.ecosystem acc then acc else acc @ [ m.ecosystem ])
    [] t.manifests

let summary t =
  let b = Buffer.create 256 in
  let names l =
    String.concat ", " (List.map (fun (d : detection) -> d.name) l)
  in
  let line label value =
    if value <> "" then Printf.bprintf b "%-18s%s\n" (label ^ ":") value
  in
  line "Languages" (names t.languages);
  line "Package managers" (names t.package_managers);
  List.iter (fun (category, tools) -> line category (names tools)) t.tools;
  line "Manifests" (string_of_int (List.length t.manifests));
  if t.stats.scan_truncated then
    Buffer.add_string b "The scan stopped at brief's limit on entries.\n";
  Buffer.contents b

let max_output = 16 * 1024 * 1024

let bounded limit r =
  let b = Buffer.create 4096 in
  let rec loop () =
    match Eio.Buf_read.ensure r 1 with
    | () ->
        let n = Eio.Buf_read.buffered_bytes r in
        if Buffer.length b + n > limit then failwith "brief wrote too much";
        Buffer.add_string b (Eio.Buf_read.take n r);
        loop ()
    | exception End_of_file -> Buffer.contents b
  in
  loop ()

let run ?(brief = "brief") ?(timeout = 60.) ~process ~clock dir =
  let stderr = Buffer.create 256 in
  let failed message =
    let err = String.trim (Buffer.contents stderr) in
    Error (Printf.sprintf "%s: %s" brief (if err <> "" then err else message))
  in
  try
    Ok
      (Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock timeout)
         (fun () ->
           Eio.Process.parse_out process (bounded max_output)
             ~stderr:(Eio.Flow.buffer_sink stderr)
             [ brief; "-json"; dir ]))
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | Eio.Time.Timeout ->
      failed (Printf.sprintf "no report within %g seconds" timeout)
  | Eio.Io (Eio.Process.E (Eio.Process.Executable_not_found _), _) ->
      failed "not found on the path"
  | Eio.Io _ as e -> failed (Printexc.to_string e)
  | Failure m -> failed m

let scan ?brief ?timeout ~process ~clock dir =
  Result.bind (run ?brief ?timeout ~process ~clock dir) of_string
