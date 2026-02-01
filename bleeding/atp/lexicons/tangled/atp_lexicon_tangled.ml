(* Atp_lexicon_tangled - generated from atproto lexicons *)

(** Utility functions for resilient parsing. *)
module Filter = struct
  (** [filter_list jsont json_list] parses each element with [jsont],
      returning only successfully parsed elements. Non-compliant records
      are silently skipped. *)
  let filter_list (type a) (jsont : a Jsont.t) (json_list : Jsont.json list) : a list =
    List.filter_map (fun json ->
      match Jsont.Json.decode jsont json with
      | Ok v -> Some v
      | Error _ -> None
    ) json_list
end

module Sh = struct
  module Tangled = struct
    module String = struct
type main = {
  contents : string;
  created_at : string;
  description : string;
  filename : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ contents created_at description filename -> { contents; created_at; description; filename })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.string" ~enc:(fun _ -> "sh.tangled.string")
  |> Jsont.Object.mem "contents" Jsont.string ~enc:(fun r -> r.contents)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "filename" Jsont.string ~enc:(fun r -> r.filename)
  |> Jsont.Object.finish

    end
    module Spindle = struct
type main = {
  created_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at -> { created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.spindle" ~enc:(fun _ -> "sh.tangled.spindle")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.finish

      module Member = struct
type main = {
  created_at : string;
  instance : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at instance subject -> { created_at; instance; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.spindle.member" ~enc:(fun _ -> "sh.tangled.spindle.member")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "instance" Jsont.string ~enc:(fun r -> r.instance)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
    end
    module Repo = struct
type main = {
  created_at : string;
  description : string option;
  knot : string;
  labels : string list option;
  name : string;
  source : string option;
  spindle : string option;
  topics : string list option;
  website : string option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at description knot labels name source spindle topics website -> { created_at; description; knot; labels; name; source; spindle; topics; website })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo" ~enc:(fun _ -> "sh.tangled.repo")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "knot" Jsont.string ~enc:(fun r -> r.knot)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Jsont.string) ~enc:(fun r -> r.labels)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "source" Jsont.string ~enc:(fun r -> r.source)
  |> Jsont.Object.opt_mem "spindle" Jsont.string ~enc:(fun r -> r.spindle)
  |> Jsont.Object.opt_mem "topics" (Jsont.list Jsont.string) ~enc:(fun r -> r.topics)
  |> Jsont.Object.opt_mem "website" Jsont.string ~enc:(fun r -> r.website)
  |> Jsont.Object.finish

      module Tree = struct
type last_commit = {
  hash : string;
  message : string;
  when_ : string;
}

let last_commit_jsont =
  Jsont.Object.map ~kind:"Last_commit"
    (fun _typ hash message when_ -> { hash; message; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.tree#lastCommit" ~enc:(fun _ -> "sh.tangled.repo.tree#lastCommit")
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type readme = {
  contents : string;
  filename : string;
}

let readme_jsont =
  Jsont.Object.map ~kind:"Readme"
    (fun _typ contents filename -> { contents; filename })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.tree#readme" ~enc:(fun _ -> "sh.tangled.repo.tree#readme")
  |> Jsont.Object.mem "contents" Jsont.string ~enc:(fun r -> r.contents)
  |> Jsont.Object.mem "filename" Jsont.string ~enc:(fun r -> r.filename)
  |> Jsont.Object.finish

type tree_entry = {
  last_commit : last_commit option;
  mode : string;
  name : string;
  size : int;
}

let tree_entry_jsont =
  Jsont.Object.map ~kind:"Tree_entry"
    (fun _typ last_commit mode name size -> { last_commit; mode; name; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.tree#treeEntry" ~enc:(fun _ -> "sh.tangled.repo.tree#treeEntry")
  |> Jsont.Object.opt_mem "last_commit" last_commit_jsont ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.mem "mode" Jsont.string ~enc:(fun r -> r.mode)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type params = {
  path : string option;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path ref_ repo -> {
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  dotdot : string option;
  files : tree_entry list;
  parent : string option;
  readme : readme option;
  ref_ : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ dotdot files parent readme ref_ -> { dotdot; files; parent; readme; ref_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.tree#output" ~enc:(fun _ -> "sh.tangled.repo.tree#output")
  |> Jsont.Object.opt_mem "dotdot" Jsont.string ~enc:(fun r -> r.dotdot)
  |> Jsont.Object.mem "files" (Jsont.list tree_entry_jsont) ~enc:(fun r -> r.files)
  |> Jsont.Object.opt_mem "parent" Jsont.string ~enc:(fun r -> r.parent)
  |> Jsont.Object.opt_mem "readme" readme_jsont ~enc:(fun r -> r.readme)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.finish

      end
      module Tags = struct
type params = {
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit repo -> {
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module SetDefaultBranch = struct
type input = {
  default_branch : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ default_branch repo -> { default_branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.setDefaultBranch#input" ~enc:(fun _ -> "sh.tangled.repo.setDefaultBranch#input")
  |> Jsont.Object.mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module RemoveSecret = struct
type input = {
  key : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ key repo -> { key; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.removeSecret#input" ~enc:(fun _ -> "sh.tangled.repo.removeSecret#input")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module Pull = struct
type source = {
  branch : string;
  repo : string option;
  sha : string;
}

let source_jsont =
  Jsont.Object.map ~kind:"Source"
    (fun _typ branch repo sha -> { branch; repo; sha })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.pull#source" ~enc:(fun _ -> "sh.tangled.repo.pull#source")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.opt_mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "sha" Jsont.string ~enc:(fun r -> r.sha)
  |> Jsont.Object.finish

type target = {
  branch : string;
  repo : string;
}

let target_jsont =
  Jsont.Object.map ~kind:"Target"
    (fun _typ branch repo -> { branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.pull#target" ~enc:(fun _ -> "sh.tangled.repo.pull#target")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type main = {
  body : string option;
  created_at : string;
  mentions : string list option;
  patch : string option;
  patch_blob : Atp.Blob_ref.t;
  references : string list option;
  source : source option;
  target : target;
  title : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ body created_at mentions patch patch_blob references source target title -> { body; created_at; mentions; patch; patch_blob; references; source; target; title })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.pull" ~enc:(fun _ -> "sh.tangled.repo.pull")
  |> Jsont.Object.opt_mem "body" Jsont.string ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "mentions" (Jsont.list Jsont.string) ~enc:(fun r -> r.mentions)
  |> Jsont.Object.opt_mem "patch" Jsont.string ~enc:(fun r -> r.patch)
  |> Jsont.Object.mem "patchBlob" Atp.Blob_ref.jsont ~enc:(fun r -> r.patch_blob)
  |> Jsont.Object.opt_mem "references" (Jsont.list Jsont.string) ~enc:(fun r -> r.references)
  |> Jsont.Object.opt_mem "source" source_jsont ~enc:(fun r -> r.source)
  |> Jsont.Object.mem "target" target_jsont ~enc:(fun r -> r.target)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.finish

        module Status = struct
type main = {
  pull : string;
  status : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ pull status -> { pull; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.pull.status" ~enc:(fun _ -> "sh.tangled.repo.pull.status")
  |> Jsont.Object.mem "pull" Jsont.string ~enc:(fun r -> r.pull)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

          module Open = struct
type main = string
let main_jsont = Jsont.string

          end
          module Merged = struct
type main = string
let main_jsont = Jsont.string

          end
          module Closed = struct
type main = string
let main_jsont = Jsont.string

          end
        end
        module Comment = struct
type main = {
  body : string;
  created_at : string;
  mentions : string list option;
  pull : string;
  references : string list option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ body created_at mentions pull references -> { body; created_at; mentions; pull; references })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.pull.comment" ~enc:(fun _ -> "sh.tangled.repo.pull.comment")
  |> Jsont.Object.mem "body" Jsont.string ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "mentions" (Jsont.list Jsont.string) ~enc:(fun r -> r.mentions)
  |> Jsont.Object.mem "pull" Jsont.string ~enc:(fun r -> r.pull)
  |> Jsont.Object.opt_mem "references" (Jsont.list Jsont.string) ~enc:(fun r -> r.references)
  |> Jsont.Object.finish

        end
      end
      module MergeCheck = struct
type conflict_info = {
  filename : string;
  reason : string;
}

let conflict_info_jsont =
  Jsont.Object.map ~kind:"Conflict_info"
    (fun _typ filename reason -> { filename; reason })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.mergeCheck#conflictInfo" ~enc:(fun _ -> "sh.tangled.repo.mergeCheck#conflictInfo")
  |> Jsont.Object.mem "filename" Jsont.string ~enc:(fun r -> r.filename)
  |> Jsont.Object.mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.finish

type input = {
  branch : string;
  did : string;
  name : string;
  patch : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch did name patch -> { branch; did; name; patch })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.mergeCheck#input" ~enc:(fun _ -> "sh.tangled.repo.mergeCheck#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "patch" Jsont.string ~enc:(fun r -> r.patch)
  |> Jsont.Object.finish

type output = {
  conflicts : conflict_info list option;
  error : string option;
  is_conflicted : bool;
  message : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ conflicts error is_conflicted message -> { conflicts; error; is_conflicted; message })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.mergeCheck#output" ~enc:(fun _ -> "sh.tangled.repo.mergeCheck#output")
  |> Jsont.Object.opt_mem "conflicts" (Jsont.list conflict_info_jsont) ~enc:(fun r -> r.conflicts)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.mem "is_conflicted" Jsont.bool ~enc:(fun r -> r.is_conflicted)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.finish

      end
      module Merge = struct
type input = {
  author_email : string option;
  author_name : string option;
  branch : string;
  commit_body : string option;
  commit_message : string option;
  did : string;
  name : string;
  patch : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ author_email author_name branch commit_body commit_message did name patch -> { author_email; author_name; branch; commit_body; commit_message; did; name; patch })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.merge#input" ~enc:(fun _ -> "sh.tangled.repo.merge#input")
  |> Jsont.Object.opt_mem "authorEmail" Jsont.string ~enc:(fun r -> r.author_email)
  |> Jsont.Object.opt_mem "authorName" Jsont.string ~enc:(fun r -> r.author_name)
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.opt_mem "commitBody" Jsont.string ~enc:(fun r -> r.commit_body)
  |> Jsont.Object.opt_mem "commitMessage" Jsont.string ~enc:(fun r -> r.commit_message)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "patch" Jsont.string ~enc:(fun r -> r.patch)
  |> Jsont.Object.finish

      end
      module Log = struct
type params = {
  cursor : string option;
  limit : int option;
  path : string option;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit path ref_ repo -> {
      cursor;
      limit;
      path;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module ListSecrets = struct
type secret = {
  created_at : string;
  created_by : string;
  key : string;
  repo : string;
}

let secret_jsont =
  Jsont.Object.map ~kind:"Secret"
    (fun _typ created_at created_by key repo -> { created_at; created_by; key; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.listSecrets#secret" ~enc:(fun _ -> "sh.tangled.repo.listSecrets#secret")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "createdBy" Jsont.string ~enc:(fun r -> r.created_by)
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type params = {
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo -> {
      repo;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  secrets : secret list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ secrets -> { secrets })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.listSecrets#output" ~enc:(fun _ -> "sh.tangled.repo.listSecrets#output")
  |> Jsont.Object.mem "secrets" (Jsont.list secret_jsont) ~enc:(fun r -> r.secrets)
  |> Jsont.Object.finish

      end
      module Languages = struct
type language = {
  color : string option;
  extensions : string list option;
  file_count : int option;
  name : string;
  percentage : int;
  size : int;
}

let language_jsont =
  Jsont.Object.map ~kind:"Language"
    (fun _typ color extensions file_count name percentage size -> { color; extensions; file_count; name; percentage; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.languages#language" ~enc:(fun _ -> "sh.tangled.repo.languages#language")
  |> Jsont.Object.opt_mem "color" Jsont.string ~enc:(fun r -> r.color)
  |> Jsont.Object.opt_mem "extensions" (Jsont.list Jsont.string) ~enc:(fun r -> r.extensions)
  |> Jsont.Object.opt_mem "fileCount" Jsont.int ~enc:(fun r -> r.file_count)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "percentage" Jsont.int ~enc:(fun r -> r.percentage)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type params = {
  ref_ : string option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun ref_ repo -> {
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  languages : language list;
  ref_ : string;
  total_files : int option;
  total_size : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ languages ref_ total_files total_size -> { languages; ref_; total_files; total_size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.languages#output" ~enc:(fun _ -> "sh.tangled.repo.languages#output")
  |> Jsont.Object.mem "languages" (Jsont.list language_jsont) ~enc:(fun r -> r.languages)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.opt_mem "totalFiles" Jsont.int ~enc:(fun r -> r.total_files)
  |> Jsont.Object.opt_mem "totalSize" Jsont.int ~enc:(fun r -> r.total_size)
  |> Jsont.Object.finish

      end
      module Issue = struct
type main = {
  body : string option;
  created_at : string;
  mentions : string list option;
  references : string list option;
  repo : string;
  title : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ body created_at mentions references repo title -> { body; created_at; mentions; references; repo; title })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.issue" ~enc:(fun _ -> "sh.tangled.repo.issue")
  |> Jsont.Object.opt_mem "body" Jsont.string ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "mentions" (Jsont.list Jsont.string) ~enc:(fun r -> r.mentions)
  |> Jsont.Object.opt_mem "references" (Jsont.list Jsont.string) ~enc:(fun r -> r.references)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.finish

        module State = struct
type main = {
  issue : string;
  state : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ issue state -> { issue; state })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.issue.state" ~enc:(fun _ -> "sh.tangled.repo.issue.state")
  |> Jsont.Object.mem "issue" Jsont.string ~enc:(fun r -> r.issue)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.finish

          module Open = struct
type main = string
let main_jsont = Jsont.string

          end
          module Closed = struct
type main = string
let main_jsont = Jsont.string

          end
        end
        module Comment = struct
type main = {
  body : string;
  created_at : string;
  issue : string;
  mentions : string list option;
  references : string list option;
  reply_to : string option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ body created_at issue mentions references reply_to -> { body; created_at; issue; mentions; references; reply_to })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.issue.comment" ~enc:(fun _ -> "sh.tangled.repo.issue.comment")
  |> Jsont.Object.mem "body" Jsont.string ~enc:(fun r -> r.body)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "issue" Jsont.string ~enc:(fun r -> r.issue)
  |> Jsont.Object.opt_mem "mentions" (Jsont.list Jsont.string) ~enc:(fun r -> r.mentions)
  |> Jsont.Object.opt_mem "references" (Jsont.list Jsont.string) ~enc:(fun r -> r.references)
  |> Jsont.Object.opt_mem "replyTo" Jsont.string ~enc:(fun r -> r.reply_to)
  |> Jsont.Object.finish

        end
      end
      module HiddenRef = struct
type input = {
  fork_ref : string;
  remote_ref : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ fork_ref remote_ref repo -> { fork_ref; remote_ref; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.hiddenRef#input" ~enc:(fun _ -> "sh.tangled.repo.hiddenRef#input")
  |> Jsont.Object.mem "forkRef" Jsont.string ~enc:(fun r -> r.fork_ref)
  |> Jsont.Object.mem "remoteRef" Jsont.string ~enc:(fun r -> r.remote_ref)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  error : string option;
  ref_ : string option;
  success : bool;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ error ref_ success -> { error; ref_; success })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.hiddenRef#output" ~enc:(fun _ -> "sh.tangled.repo.hiddenRef#output")
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.opt_mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "success" Jsont.bool ~enc:(fun r -> r.success)
  |> Jsont.Object.finish

      end
      module GetDefaultBranch = struct
type signature = {
  email : string;
  name : string;
  when_ : string;
}

let signature_jsont =
  Jsont.Object.map ~kind:"Signature"
    (fun _typ email name when_ -> { email; name; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.getDefaultBranch#signature" ~enc:(fun _ -> "sh.tangled.repo.getDefaultBranch#signature")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type params = {
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo -> {
      repo;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  author : signature option;
  hash : string;
  message : string option;
  name : string;
  short_hash : string option;
  when_ : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ author hash message name short_hash when_ -> { author; hash; message; name; short_hash; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.getDefaultBranch#output" ~enc:(fun _ -> "sh.tangled.repo.getDefaultBranch#output")
  |> Jsont.Object.opt_mem "author" signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "shortHash" Jsont.string ~enc:(fun r -> r.short_hash)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

      end
      module ForkSync = struct
type input = {
  branch : string;
  did : string;
  name : string;
  source : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch did name source -> { branch; did; name; source })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.forkSync#input" ~enc:(fun _ -> "sh.tangled.repo.forkSync#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "source" Jsont.string ~enc:(fun r -> r.source)
  |> Jsont.Object.finish

      end
      module ForkStatus = struct
type input = {
  branch : string;
  did : string;
  hidden_ref : string;
  name : string;
  source : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch did hidden_ref name source -> { branch; did; hidden_ref; name; source })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.forkStatus#input" ~enc:(fun _ -> "sh.tangled.repo.forkStatus#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "hiddenRef" Jsont.string ~enc:(fun r -> r.hidden_ref)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "source" Jsont.string ~enc:(fun r -> r.source)
  |> Jsont.Object.finish

type output = {
  status : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ status -> { status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.forkStatus#output" ~enc:(fun _ -> "sh.tangled.repo.forkStatus#output")
  |> Jsont.Object.mem "status" Jsont.int ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

      end
      module Diff = struct
type params = {
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun ref_ repo -> {
      ref_;
      repo;
    })
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module DeleteBranch = struct
type input = {
  branch : string;
  repo : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ branch repo -> { branch; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.deleteBranch#input" ~enc:(fun _ -> "sh.tangled.repo.deleteBranch#input")
  |> Jsont.Object.mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

      end
      module Delete = struct
type input = {
  did : string;
  name : string;
  rkey : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ did name rkey -> { did; name; rkey })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.delete#input" ~enc:(fun _ -> "sh.tangled.repo.delete#input")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.finish

      end
      module Create = struct
type input = {
  default_branch : string option;
  rkey : string;
  source : string option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ default_branch rkey source -> { default_branch; rkey; source })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.create#input" ~enc:(fun _ -> "sh.tangled.repo.create#input")
  |> Jsont.Object.opt_mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "rkey" Jsont.string ~enc:(fun r -> r.rkey)
  |> Jsont.Object.opt_mem "source" Jsont.string ~enc:(fun r -> r.source)
  |> Jsont.Object.finish

      end
      module Compare = struct
type params = {
  repo : string;
  rev1 : string;
  rev2 : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun repo rev1 rev2 -> {
      repo;
      rev1;
      rev2;
    })
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "rev1" Jsont.string
       ~enc:(fun r -> r.rev1)
  |> Jsont.Object.mem "rev2" Jsont.string
       ~enc:(fun r -> r.rev2)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module Collaborator = struct
type main = {
  created_at : string;
  repo : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at repo subject -> { created_at; repo; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.collaborator" ~enc:(fun _ -> "sh.tangled.repo.collaborator")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module Branches = struct
type params = {
  cursor : string option;
  limit : int option;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit repo -> {
      cursor;
      limit;
      repo;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module Branch = struct
type signature = {
  email : string;
  name : string;
  when_ : string;
}

let signature_jsont =
  Jsont.Object.map ~kind:"Signature"
    (fun _typ email name when_ -> { email; name; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.branch#signature" ~enc:(fun _ -> "sh.tangled.repo.branch#signature")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type params = {
  name : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun name repo -> {
      name;
      repo;
    })
  |> Jsont.Object.mem "name" Jsont.string
       ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  author : signature option;
  hash : string;
  is_default : bool option;
  message : string option;
  name : string;
  short_hash : string option;
  when_ : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ author hash is_default message name short_hash when_ -> { author; hash; is_default; message; name; short_hash; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.branch#output" ~enc:(fun _ -> "sh.tangled.repo.branch#output")
  |> Jsont.Object.opt_mem "author" signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.opt_mem "isDefault" Jsont.bool ~enc:(fun r -> r.is_default)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.opt_mem "shortHash" Jsont.string ~enc:(fun r -> r.short_hash)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

      end
      module Blob = struct
type signature = {
  email : string;
  name : string;
  when_ : string;
}

let signature_jsont =
  Jsont.Object.map ~kind:"Signature"
    (fun _typ email name when_ -> { email; name; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.blob#signature" ~enc:(fun _ -> "sh.tangled.repo.blob#signature")
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type submodule = {
  branch : string option;
  name : string;
  url : string;
}

let submodule_jsont =
  Jsont.Object.map ~kind:"Submodule"
    (fun _typ branch name url -> { branch; name; url })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.blob#submodule" ~enc:(fun _ -> "sh.tangled.repo.blob#submodule")
  |> Jsont.Object.opt_mem "branch" Jsont.string ~enc:(fun r -> r.branch)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

type last_commit = {
  author : signature option;
  hash : string;
  message : string;
  short_hash : string option;
  when_ : string;
}

let last_commit_jsont =
  Jsont.Object.map ~kind:"Last_commit"
    (fun _typ author hash message short_hash when_ -> { author; hash; message; short_hash; when_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.blob#lastCommit" ~enc:(fun _ -> "sh.tangled.repo.blob#lastCommit")
  |> Jsont.Object.opt_mem "author" signature_jsont ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "hash" Jsont.string ~enc:(fun r -> r.hash)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.opt_mem "shortHash" Jsont.string ~enc:(fun r -> r.short_hash)
  |> Jsont.Object.mem "when" Jsont.string ~enc:(fun r -> r.when_)
  |> Jsont.Object.finish

type params = {
  path : string;
  raw : bool option;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun path raw ref_ repo -> {
      path;
      raw;
      ref_;
      repo;
    })
  |> Jsont.Object.mem "path" Jsont.string
       ~enc:(fun r -> r.path)
  |> Jsont.Object.opt_mem "raw" Jsont.bool
       ~enc:(fun r -> r.raw)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = {
  content : string option;
  encoding : string option;
  is_binary : bool option;
  last_commit : last_commit option;
  mime_type : string option;
  path : string;
  ref_ : string;
  size : int option;
  submodule : submodule option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ content encoding is_binary last_commit mime_type path ref_ size submodule -> { content; encoding; is_binary; last_commit; mime_type; path; ref_; size; submodule })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.blob#output" ~enc:(fun _ -> "sh.tangled.repo.blob#output")
  |> Jsont.Object.opt_mem "content" Jsont.string ~enc:(fun r -> r.content)
  |> Jsont.Object.opt_mem "encoding" Jsont.string ~enc:(fun r -> r.encoding)
  |> Jsont.Object.opt_mem "isBinary" Jsont.bool ~enc:(fun r -> r.is_binary)
  |> Jsont.Object.opt_mem "lastCommit" last_commit_jsont ~enc:(fun r -> r.last_commit)
  |> Jsont.Object.opt_mem "mimeType" Jsont.string ~enc:(fun r -> r.mime_type)
  |> Jsont.Object.mem "path" Jsont.string ~enc:(fun r -> r.path)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.opt_mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.opt_mem "submodule" submodule_jsont ~enc:(fun r -> r.submodule)
  |> Jsont.Object.finish

      end
      module Artifact = struct
type main = {
  artifact : Atp.Blob_ref.t;
  created_at : string;
  name : string;
  repo : string;
  tag : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ artifact created_at name repo tag -> { artifact; created_at; name; repo; tag })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.artifact" ~enc:(fun _ -> "sh.tangled.repo.artifact")
  |> Jsont.Object.mem "artifact" Atp.Blob_ref.jsont ~enc:(fun r -> r.artifact)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "tag" Jsont.binary_string ~enc:(fun r -> r.tag)
  |> Jsont.Object.finish

      end
      module Archive = struct
type params = {
  format : string option;
  prefix : string option;
  ref_ : string;
  repo : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun format prefix ref_ repo -> {
      format;
      prefix;
      ref_;
      repo;
    })
  |> Jsont.Object.opt_mem "format" Jsont.string
       ~enc:(fun r -> r.format)
  |> Jsont.Object.opt_mem "prefix" Jsont.string
       ~enc:(fun r -> r.prefix)
  |> Jsont.Object.mem "ref" Jsont.string
       ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repo" Jsont.string
       ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type output = unit
let output_jsont = Jsont.ignore

      end
      module AddSecret = struct
type input = {
  key : string;
  repo : string;
  value : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ key repo value -> { key; repo; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.repo.addSecret#input" ~enc:(fun _ -> "sh.tangled.repo.addSecret#input")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
    end
    module PublicKey = struct
type main = {
  created_at : string;
  key : string;
  name : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at key name -> { created_at; key; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.publicKey" ~enc:(fun _ -> "sh.tangled.publicKey")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

    end
    module Pipeline = struct
type clone_opts = {
  depth : int;
  skip : bool;
  submodules : bool;
}

let clone_opts_jsont =
  Jsont.Object.map ~kind:"Clone_opts"
    (fun _typ depth skip submodules -> { depth; skip; submodules })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.pipeline#cloneOpts" ~enc:(fun _ -> "sh.tangled.pipeline#cloneOpts")
  |> Jsont.Object.mem "depth" Jsont.int ~enc:(fun r -> r.depth)
  |> Jsont.Object.mem "skip" Jsont.bool ~enc:(fun r -> r.skip)
  |> Jsont.Object.mem "submodules" Jsont.bool ~enc:(fun r -> r.submodules)
  |> Jsont.Object.finish

type pair = {
  key : string;
  value : string;
}

let pair_jsont =
  Jsont.Object.map ~kind:"Pair"
    (fun _typ key value -> { key; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.pipeline#pair" ~enc:(fun _ -> "sh.tangled.pipeline#pair")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type pull_request_trigger_data = {
  action : string;
  source_branch : string;
  source_sha : string;
  target_branch : string;
}

let pull_request_trigger_data_jsont =
  Jsont.Object.map ~kind:"Pull_request_trigger_data"
    (fun _typ action source_branch source_sha target_branch -> { action; source_branch; source_sha; target_branch })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.pipeline#pullRequestTriggerData" ~enc:(fun _ -> "sh.tangled.pipeline#pullRequestTriggerData")
  |> Jsont.Object.mem "action" Jsont.string ~enc:(fun r -> r.action)
  |> Jsont.Object.mem "sourceBranch" Jsont.string ~enc:(fun r -> r.source_branch)
  |> Jsont.Object.mem "sourceSha" Jsont.string ~enc:(fun r -> r.source_sha)
  |> Jsont.Object.mem "targetBranch" Jsont.string ~enc:(fun r -> r.target_branch)
  |> Jsont.Object.finish

type push_trigger_data = {
  new_sha : string;
  old_sha : string;
  ref_ : string;
}

let push_trigger_data_jsont =
  Jsont.Object.map ~kind:"Push_trigger_data"
    (fun _typ new_sha old_sha ref_ -> { new_sha; old_sha; ref_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.pipeline#pushTriggerData" ~enc:(fun _ -> "sh.tangled.pipeline#pushTriggerData")
  |> Jsont.Object.mem "newSha" Jsont.string ~enc:(fun r -> r.new_sha)
  |> Jsont.Object.mem "oldSha" Jsont.string ~enc:(fun r -> r.old_sha)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.finish

type trigger_repo = {
  default_branch : string;
  did : string;
  knot : string;
  repo : string;
}

let trigger_repo_jsont =
  Jsont.Object.map ~kind:"Trigger_repo"
    (fun _typ default_branch did knot repo -> { default_branch; did; knot; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.pipeline#triggerRepo" ~enc:(fun _ -> "sh.tangled.pipeline#triggerRepo")
  |> Jsont.Object.mem "defaultBranch" Jsont.string ~enc:(fun r -> r.default_branch)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "knot" Jsont.string ~enc:(fun r -> r.knot)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type manual_trigger_data = {
  inputs : pair list option;
}

let manual_trigger_data_jsont =
  Jsont.Object.map ~kind:"Manual_trigger_data"
    (fun _typ inputs -> { inputs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.pipeline#manualTriggerData" ~enc:(fun _ -> "sh.tangled.pipeline#manualTriggerData")
  |> Jsont.Object.opt_mem "inputs" (Jsont.list pair_jsont) ~enc:(fun r -> r.inputs)
  |> Jsont.Object.finish

type workflow = {
  clone : clone_opts;
  engine : string;
  name : string;
  raw : string;
}

let workflow_jsont =
  Jsont.Object.map ~kind:"Workflow"
    (fun _typ clone engine name raw -> { clone; engine; name; raw })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.pipeline#workflow" ~enc:(fun _ -> "sh.tangled.pipeline#workflow")
  |> Jsont.Object.mem "clone" clone_opts_jsont ~enc:(fun r -> r.clone)
  |> Jsont.Object.mem "engine" Jsont.string ~enc:(fun r -> r.engine)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "raw" Jsont.string ~enc:(fun r -> r.raw)
  |> Jsont.Object.finish

type trigger_metadata = {
  kind : string;
  manual : manual_trigger_data option;
  pull_request : pull_request_trigger_data option;
  push : push_trigger_data option;
  repo : trigger_repo;
}

let trigger_metadata_jsont =
  Jsont.Object.map ~kind:"Trigger_metadata"
    (fun _typ kind manual pull_request push repo -> { kind; manual; pull_request; push; repo })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.pipeline#triggerMetadata" ~enc:(fun _ -> "sh.tangled.pipeline#triggerMetadata")
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun r -> r.kind)
  |> Jsont.Object.opt_mem "manual" manual_trigger_data_jsont ~enc:(fun r -> r.manual)
  |> Jsont.Object.opt_mem "pullRequest" pull_request_trigger_data_jsont ~enc:(fun r -> r.pull_request)
  |> Jsont.Object.opt_mem "push" push_trigger_data_jsont ~enc:(fun r -> r.push)
  |> Jsont.Object.mem "repo" trigger_repo_jsont ~enc:(fun r -> r.repo)
  |> Jsont.Object.finish

type main = {
  trigger_metadata : trigger_metadata;
  workflows : workflow list;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ trigger_metadata workflows -> { trigger_metadata; workflows })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.pipeline" ~enc:(fun _ -> "sh.tangled.pipeline")
  |> Jsont.Object.mem "triggerMetadata" trigger_metadata_jsont ~enc:(fun r -> r.trigger_metadata)
  |> Jsont.Object.mem "workflows" (Jsont.list workflow_jsont) ~enc:(fun r -> r.workflows)
  |> Jsont.Object.finish

      module Status = struct
type main = {
  created_at : string;
  error : string option;
  exit_code : int option;
  pipeline : string;
  status : string;
  workflow : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at error exit_code pipeline status workflow -> { created_at; error; exit_code; pipeline; status; workflow })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.pipeline.status" ~enc:(fun _ -> "sh.tangled.pipeline.status")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.opt_mem "exitCode" Jsont.int ~enc:(fun r -> r.exit_code)
  |> Jsont.Object.mem "pipeline" Jsont.string ~enc:(fun r -> r.pipeline)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "workflow" Jsont.string ~enc:(fun r -> r.workflow)
  |> Jsont.Object.finish

      end
    end
    module Owner = struct
type output = {
  owner : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ owner -> { owner })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.owner#output" ~enc:(fun _ -> "sh.tangled.owner#output")
  |> Jsont.Object.mem "owner" Jsont.string ~enc:(fun r -> r.owner)
  |> Jsont.Object.finish

    end
    module Label = struct
      module Op = struct
type operand = {
  key : string;
  value : string;
}

let operand_jsont =
  Jsont.Object.map ~kind:"Operand"
    (fun _typ key value -> { key; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.label.op#operand" ~enc:(fun _ -> "sh.tangled.label.op#operand")
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type main = {
  add : operand list;
  delete : operand list;
  performed_at : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ add delete performed_at subject -> { add; delete; performed_at; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.label.op" ~enc:(fun _ -> "sh.tangled.label.op")
  |> Jsont.Object.mem "add" (Jsont.list operand_jsont) ~enc:(fun r -> r.add)
  |> Jsont.Object.mem "delete" (Jsont.list operand_jsont) ~enc:(fun r -> r.delete)
  |> Jsont.Object.mem "performedAt" Jsont.string ~enc:(fun r -> r.performed_at)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module Definition = struct
type value_type = {
  enum : string list option;
  format : string;
  type_ : string;
}

let value_type_jsont =
  Jsont.Object.map ~kind:"Value_type"
    (fun _typ enum format type_ -> { enum; format; type_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.label.definition#valueType" ~enc:(fun _ -> "sh.tangled.label.definition#valueType")
  |> Jsont.Object.opt_mem "enum" (Jsont.list Jsont.string) ~enc:(fun r -> r.enum)
  |> Jsont.Object.mem "format" Jsont.string ~enc:(fun r -> r.format)
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun r -> r.type_)
  |> Jsont.Object.finish

type main = {
  color : string option;
  created_at : string;
  multiple : bool option;
  name : string;
  scope : string list;
  value_type : value_type;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ color created_at multiple name scope value_type -> { color; created_at; multiple; name; scope; value_type })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.label.definition" ~enc:(fun _ -> "sh.tangled.label.definition")
  |> Jsont.Object.opt_mem "color" Jsont.string ~enc:(fun r -> r.color)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "multiple" Jsont.bool ~enc:(fun r -> r.multiple)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "scope" (Jsont.list Jsont.string) ~enc:(fun r -> r.scope)
  |> Jsont.Object.mem "valueType" value_type_jsont ~enc:(fun r -> r.value_type)
  |> Jsont.Object.finish

      end
    end
    module Knot = struct
type main = {
  created_at : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at -> { created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.knot" ~enc:(fun _ -> "sh.tangled.knot")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.finish

      module Version = struct
type output = {
  version : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ version -> { version })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.knot.version#output" ~enc:(fun _ -> "sh.tangled.knot.version#output")
  |> Jsont.Object.mem "version" Jsont.string ~enc:(fun r -> r.version)
  |> Jsont.Object.finish

      end
      module Member = struct
type main = {
  created_at : string;
  domain : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at domain subject -> { created_at; domain; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.knot.member" ~enc:(fun _ -> "sh.tangled.knot.member")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "domain" Jsont.string ~enc:(fun r -> r.domain)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module ListKeys = struct
type public_key = {
  created_at : string;
  did : string;
  key : string;
}

let public_key_jsont =
  Jsont.Object.map ~kind:"Public_key"
    (fun _typ created_at did key -> { created_at; did; key })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.knot.listKeys#publicKey" ~enc:(fun _ -> "sh.tangled.knot.listKeys#publicKey")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun r -> r.key)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit -> {
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  keys : public_key list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor keys -> { cursor; keys })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.knot.listKeys#output" ~enc:(fun _ -> "sh.tangled.knot.listKeys#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "keys" (Jsont.list public_key_jsont) ~enc:(fun r -> r.keys)
  |> Jsont.Object.finish

      end
    end
    module Graph = struct
      module Follow = struct
type main = {
  created_at : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at subject -> { created_at; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.graph.follow" ~enc:(fun _ -> "sh.tangled.graph.follow")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
    end
    module Git = struct
      module RefUpdate = struct
type individual_email_commit_count = {
  count : int;
  email : string;
}

let individual_email_commit_count_jsont =
  Jsont.Object.map ~kind:"Individual_email_commit_count"
    (fun _typ count email -> { count; email })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.git.refUpdate#individualEmailCommitCount" ~enc:(fun _ -> "sh.tangled.git.refUpdate#individualEmailCommitCount")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.finish

type individual_language_size = {
  lang : string;
  size : int;
}

let individual_language_size_jsont =
  Jsont.Object.map ~kind:"Individual_language_size"
    (fun _typ lang size -> { lang; size })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.git.refUpdate#individualLanguageSize" ~enc:(fun _ -> "sh.tangled.git.refUpdate#individualLanguageSize")
  |> Jsont.Object.mem "lang" Jsont.string ~enc:(fun r -> r.lang)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type commit_count_breakdown = {
  by_email : individual_email_commit_count list option;
}

let commit_count_breakdown_jsont =
  Jsont.Object.map ~kind:"Commit_count_breakdown"
    (fun _typ by_email -> { by_email })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.git.refUpdate#commitCountBreakdown" ~enc:(fun _ -> "sh.tangled.git.refUpdate#commitCountBreakdown")
  |> Jsont.Object.opt_mem "byEmail" (Jsont.list individual_email_commit_count_jsont) ~enc:(fun r -> r.by_email)
  |> Jsont.Object.finish

type lang_breakdown = {
  inputs : individual_language_size list option;
}

let lang_breakdown_jsont =
  Jsont.Object.map ~kind:"Lang_breakdown"
    (fun _typ inputs -> { inputs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.git.refUpdate#langBreakdown" ~enc:(fun _ -> "sh.tangled.git.refUpdate#langBreakdown")
  |> Jsont.Object.opt_mem "inputs" (Jsont.list individual_language_size_jsont) ~enc:(fun r -> r.inputs)
  |> Jsont.Object.finish

type meta = {
  commit_count : commit_count_breakdown;
  is_default_ref : bool;
  lang_breakdown : lang_breakdown option;
}

let meta_jsont =
  Jsont.Object.map ~kind:"Meta"
    (fun _typ commit_count is_default_ref lang_breakdown -> { commit_count; is_default_ref; lang_breakdown })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.git.refUpdate#meta" ~enc:(fun _ -> "sh.tangled.git.refUpdate#meta")
  |> Jsont.Object.mem "commitCount" commit_count_breakdown_jsont ~enc:(fun r -> r.commit_count)
  |> Jsont.Object.mem "isDefaultRef" Jsont.bool ~enc:(fun r -> r.is_default_ref)
  |> Jsont.Object.opt_mem "langBreakdown" lang_breakdown_jsont ~enc:(fun r -> r.lang_breakdown)
  |> Jsont.Object.finish

type main = {
  committer_did : string;
  meta : meta;
  new_sha : string;
  old_sha : string;
  ref_ : string;
  repo_did : string;
  repo_name : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ committer_did meta new_sha old_sha ref_ repo_did repo_name -> { committer_did; meta; new_sha; old_sha; ref_; repo_did; repo_name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.git.refUpdate" ~enc:(fun _ -> "sh.tangled.git.refUpdate")
  |> Jsont.Object.mem "committerDid" Jsont.string ~enc:(fun r -> r.committer_did)
  |> Jsont.Object.mem "meta" meta_jsont ~enc:(fun r -> r.meta)
  |> Jsont.Object.mem "newSha" Jsont.string ~enc:(fun r -> r.new_sha)
  |> Jsont.Object.mem "oldSha" Jsont.string ~enc:(fun r -> r.old_sha)
  |> Jsont.Object.mem "ref" Jsont.string ~enc:(fun r -> r.ref_)
  |> Jsont.Object.mem "repoDid" Jsont.string ~enc:(fun r -> r.repo_did)
  |> Jsont.Object.mem "repoName" Jsont.string ~enc:(fun r -> r.repo_name)
  |> Jsont.Object.finish

      end
    end
    module Feed = struct
      module Star = struct
type main = {
  created_at : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at subject -> { created_at; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.feed.star" ~enc:(fun _ -> "sh.tangled.feed.star")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module Reaction = struct
type main = {
  created_at : string;
  reaction : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at reaction subject -> { created_at; reaction; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.feed.reaction" ~enc:(fun _ -> "sh.tangled.feed.reaction")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "reaction" Jsont.string ~enc:(fun r -> r.reaction)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
    end
    module Actor = struct
      module Profile = struct
type main = {
  bluesky : bool;
  description : string option;
  links : string list option;
  location : string option;
  pinned_repositories : string list option;
  pronouns : string option;
  stats : string list option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ bluesky description links location pinned_repositories pronouns stats -> { bluesky; description; links; location; pinned_repositories; pronouns; stats })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"sh.tangled.actor.profile" ~enc:(fun _ -> "sh.tangled.actor.profile")
  |> Jsont.Object.mem "bluesky" Jsont.bool ~enc:(fun r -> r.bluesky)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "links" (Jsont.list Jsont.string) ~enc:(fun r -> r.links)
  |> Jsont.Object.opt_mem "location" Jsont.string ~enc:(fun r -> r.location)
  |> Jsont.Object.opt_mem "pinnedRepositories" (Jsont.list Jsont.string) ~enc:(fun r -> r.pinned_repositories)
  |> Jsont.Object.opt_mem "pronouns" Jsont.string ~enc:(fun r -> r.pronouns)
  |> Jsont.Object.opt_mem "stats" (Jsont.list Jsont.string) ~enc:(fun r -> r.stats)
  |> Jsont.Object.finish

      end
    end
  end
end
