(* Atp_lexicon_tangled - generated from atproto lexicons *)

(** AT Protocol lexicon types and Jsont codecs for Atp_lexicon_tangled. *)

(** Utility functions for resilient parsing. *)
module Filter : sig
  val filter_list : 'a Jsont.t -> Jsont.json list -> 'a list
  (** [filter_list jsont json_list] parses each element with [jsont],
      returning only successfully parsed elements. Non-compliant records
      are silently skipped. *)
end

module Sh : sig
  module Tangled : sig
    module String : sig

type main = {
  contents : string;
  created_at : string;
  description : string;
  filename : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

    end
    module Spindle : sig

type main = {
  created_at : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      module Member : sig

type main = {
  created_at : string;
  instance : string;  (** spindle instance that the subject is now a member of *)
  subject : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Repo : sig

type main = {
  created_at : string;
  description : string option;
  knot : string;  (** knot where the repo was created *)
  labels : string list option;  (** List of labels that this repo subscribes to *)
  name : string;  (** name of the repo *)
  source : string option;  (** source of the repo *)
  spindle : string option;  (** CI runner to send jobs to and receive results from *)
  topics : string list option;  (** Topics related to the repo *)
  website : string option;  (** Any URI related to the repo *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      module Tree : sig

type last_commit = {
  hash : string;  (** Commit hash *)
  message : string;  (** Commit message *)
  when_ : string;  (** Commit timestamp *)
}

(** Jsont codec for {!type:last_commit}. *)
val last_commit_jsont : last_commit Jsont.t


type readme = {
  contents : string;  (** Contents of the readme file *)
  filename : string;  (** Name of the readme file *)
}

(** Jsont codec for {!type:readme}. *)
val readme_jsont : readme Jsont.t


type tree_entry = {
  last_commit : last_commit option;
  mode : string;  (** File mode *)
  name : string;  (** Relative file or directory name *)
  size : int;  (** File size in bytes *)
}

(** Jsont codec for {!type:tree_entry}. *)
val tree_entry_jsont : tree_entry Jsont.t


(** Query/procedure parameters. *)
type params = {
  path : string option;  (** Path within the repository tree *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  dotdot : string option;  (** Parent directory path *)
  files : tree_entry list;
  parent : string option;  (** The parent path in the tree *)
  readme : readme option;  (** Readme for this file tree *)
  ref_ : string;  (** The git reference used *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Tags : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of tags to return *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

      end
      module SetDefaultBranch : sig
(** Set the default branch for a repository *)


type input = {
  default_branch : string;
  repo : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module RemoveSecret : sig
(** Remove a CI secret *)


type input = {
  key : string;
  repo : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Pull : sig

type source = {
  branch : string;
  repo : string option;
  sha : string;
}

(** Jsont codec for {!type:source}. *)
val source_jsont : source Jsont.t


type target = {
  branch : string;
  repo : string;
}

(** Jsont codec for {!type:target}. *)
val target_jsont : target Jsont.t


type main = {
  body : string option;
  created_at : string;
  mentions : string list option;
  patch : string option;  (** (deprecated) use patchBlob instead *)
  patch_blob : Atp.Blob_ref.t;  (** patch content *)
  references : string list option;
  source : source option;
  target : target;
  title : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        module Status : sig

type main = {
  pull : string;
  status : string;  (** status of the pull request *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

          module Open : sig
(** open pull request *)

type main = string
val main_jsont : main Jsont.t

          end
          module Merged : sig
(** merged pull request *)

type main = string
val main_jsont : main Jsont.t

          end
          module Closed : sig
(** closed pull request *)

type main = string
val main_jsont : main Jsont.t

          end
        end
        module Comment : sig

type main = {
  body : string;
  created_at : string;
  mentions : string list option;
  pull : string;
  references : string list option;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        end
      end
      module MergeCheck : sig

type conflict_info = {
  filename : string;  (** Name of the conflicted file *)
  reason : string;  (** Reason for the conflict *)
}

(** Jsont codec for {!type:conflict_info}. *)
val conflict_info_jsont : conflict_info Jsont.t

(** Check if a merge is possible between two branches *)


type input = {
  branch : string;  (** Target branch to merge into *)
  did : string;  (** DID of the repository owner *)
  name : string;  (** Name of the repository *)
  patch : string;  (** Patch or pull request to check for merge conflicts *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  conflicts : conflict_info list option;  (** List of files with merge conflicts *)
  error : string option;  (** Error message if check failed *)
  is_conflicted : bool;  (** Whether the merge has conflicts *)
  message : string option;  (** Additional message about the merge check *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Merge : sig
(** Merge a patch into a repository branch *)


type input = {
  author_email : string option;  (** Author email for the merge commit *)
  author_name : string option;  (** Author name for the merge commit *)
  branch : string;  (** Target branch to merge into *)
  commit_body : string option;  (** Additional commit message body *)
  commit_message : string option;  (** Merge commit message *)
  did : string;  (** DID of the repository owner *)
  name : string;  (** Name of the repository *)
  patch : string;  (** Patch content to merge *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Log : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor (commit SHA) *)
  limit : int option;  (** Maximum number of commits to return *)
  path : string option;  (** Path to filter commits by *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

      end
      module ListSecrets : sig

type secret = {
  created_at : string;
  created_by : string;
  key : string;
  repo : string;
}

(** Jsont codec for {!type:secret}. *)
val secret_jsont : secret Jsont.t


(** Query/procedure parameters. *)
type params = {
  repo : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  secrets : secret list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Languages : sig

type language = {
  color : string option;  (** Hex color code for this language *)
  extensions : string list option;  (** File extensions associated with this language *)
  file_count : int option;  (** Number of files in this language *)
  name : string;  (** Programming language name *)
  percentage : int;  (** Percentage of total codebase (0-100) *)
  size : int;  (** Total size of files in this language (bytes) *)
}

(** Jsont codec for {!type:language}. *)
val language_jsont : language Jsont.t


(** Query/procedure parameters. *)
type params = {
  ref_ : string option;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  languages : language list;
  ref_ : string;  (** The git reference used *)
  total_files : int option;  (** Total number of files analyzed *)
  total_size : int option;  (** Total size of all analyzed files in bytes *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Issue : sig

type main = {
  body : string option;
  created_at : string;
  mentions : string list option;
  references : string list option;
  repo : string;
  title : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        module State : sig

type main = {
  issue : string;
  state : string;  (** state of the issue *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

          module Open : sig
(** open issue *)

type main = string
val main_jsont : main Jsont.t

          end
          module Closed : sig
(** closed issue *)

type main = string
val main_jsont : main Jsont.t

          end
        end
        module Comment : sig

type main = {
  body : string;
  created_at : string;
  issue : string;
  mentions : string list option;
  references : string list option;
  reply_to : string option;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

        end
      end
      module HiddenRef : sig
(** Create a hidden ref in a repository *)


type input = {
  fork_ref : string;  (** Fork reference name *)
  remote_ref : string;  (** Remote reference name *)
  repo : string;  (** AT-URI of the repository *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  error : string option;  (** Error message if creation failed *)
  ref_ : string option;  (** The created hidden ref name *)
  success : bool;  (** Whether the hidden ref was created successfully *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetDefaultBranch : sig

type signature = {
  email : string;  (** Author email *)
  name : string;  (** Author name *)
  when_ : string;  (** Author timestamp *)
}

(** Jsont codec for {!type:signature}. *)
val signature_jsont : signature Jsont.t


(** Query/procedure parameters. *)
type params = {
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  author : signature option;
  hash : string;  (** Latest commit hash on default branch *)
  message : string option;  (** Latest commit message *)
  name : string;  (** Default branch name *)
  short_hash : string option;  (** Short commit hash *)
  when_ : string;  (** Timestamp of latest commit *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ForkSync : sig
(** Sync a forked repository with its upstream source *)


type input = {
  branch : string;  (** Branch to sync *)
  did : string;  (** DID of the fork owner *)
  name : string;  (** Name of the forked repository *)
  source : string;  (** AT-URI of the source repository *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module ForkStatus : sig
(** Check fork status relative to upstream source *)


type input = {
  branch : string;  (** Branch to check status for *)
  did : string;  (** DID of the fork owner *)
  hidden_ref : string;  (** Hidden ref to use for comparison *)
  name : string;  (** Name of the forked repository *)
  source : string;  (** Source repository URL *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  status : int;  (** Fork status: 0=UpToDate, 1=FastForwardable, 2=Conflict, 3=MissingBranch *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Diff : sig

(** Query/procedure parameters. *)
type params = {
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

      end
      module DeleteBranch : sig
(** Delete a branch on this repository *)


type input = {
  branch : string;
  repo : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Delete : sig
(** Delete a repository *)


type input = {
  did : string;  (** DID of the repository owner *)
  name : string;  (** Name of the repository to delete *)
  rkey : string;  (** Rkey of the repository record *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Create : sig
(** Create a new repository *)


type input = {
  default_branch : string option;  (** Default branch to push to *)
  rkey : string;  (** Rkey of the repository record *)
  source : string option;  (** A source URL to clone from, populate this when forking or importing a repository. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Compare : sig

(** Query/procedure parameters. *)
type params = {
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
  rev1 : string;  (** First revision (commit, branch, or tag) *)
  rev2 : string;  (** Second revision (commit, branch, or tag) *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** Compare output in application/json *)

type output = unit
val output_jsont : output Jsont.t

      end
      module Collaborator : sig

type main = {
  created_at : string;
  repo : string;  (** repo to add this user to *)
  subject : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Branches : sig

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of branches to return *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = unit
val output_jsont : output Jsont.t

      end
      module Branch : sig

type signature = {
  email : string;  (** Author email *)
  name : string;  (** Author name *)
  when_ : string;  (** Author timestamp *)
}

(** Jsont codec for {!type:signature}. *)
val signature_jsont : signature Jsont.t


(** Query/procedure parameters. *)
type params = {
  name : string;  (** Branch name to get information for *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  author : signature option;
  hash : string;  (** Latest commit hash on this branch *)
  is_default : bool option;  (** Whether this is the default branch *)
  message : string option;  (** Latest commit message *)
  name : string;  (** Branch name *)
  short_hash : string option;  (** Short commit hash *)
  when_ : string;  (** Timestamp of latest commit *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Blob : sig

type signature = {
  email : string;  (** Author email *)
  name : string;  (** Author name *)
  when_ : string;  (** Author timestamp *)
}

(** Jsont codec for {!type:signature}. *)
val signature_jsont : signature Jsont.t


type submodule = {
  branch : string option;  (** Branch to track in the submodule *)
  name : string;  (** Submodule name *)
  url : string;  (** Submodule repository URL *)
}

(** Jsont codec for {!type:submodule}. *)
val submodule_jsont : submodule Jsont.t


type last_commit = {
  author : signature option;
  hash : string;  (** Commit hash *)
  message : string;  (** Commit message *)
  short_hash : string option;  (** Short commit hash *)
  when_ : string;  (** Commit timestamp *)
}

(** Jsont codec for {!type:last_commit}. *)
val last_commit_jsont : last_commit Jsont.t


(** Query/procedure parameters. *)
type params = {
  path : string;  (** Path to the file within the repository *)
  raw : bool option;  (** Return raw file content instead of JSON response *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  content : string option;  (** File content (base64 encoded for binary files) *)
  encoding : string option;  (** Content encoding *)
  is_binary : bool option;  (** Whether the file is binary *)
  last_commit : last_commit option;
  mime_type : string option;  (** MIME type of the file *)
  path : string;  (** The file path *)
  ref_ : string;  (** The git reference used *)
  size : int option;  (** File size in bytes *)
  submodule : submodule option;  (** Submodule information if path is a submodule *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Artifact : sig

type main = {
  artifact : Atp.Blob_ref.t;  (** the artifact *)
  created_at : string;  (** time of creation of this artifact *)
  name : string;  (** name of the artifact *)
  repo : string;  (** repo that this artifact is being uploaded to *)
  tag : string;  (** hash of the tag object that this artifact is attached to (only annotated tags are supported) *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Archive : sig

(** Query/procedure parameters. *)
type params = {
  format : string option;  (** Archive format *)
  prefix : string option;  (** Prefix for files in the archive *)
  ref_ : string;  (** Git reference (branch, tag, or commit SHA) *)
  repo : string;  (** Repository identifier in format 'did:plc:.../repoName' *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t

(** Binary archive data *)

type output = unit
val output_jsont : output Jsont.t

      end
      module AddSecret : sig
(** Add a CI secret *)


type input = {
  key : string;
  repo : string;
  value : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
    end
    module PublicKey : sig

type main = {
  created_at : string;  (** key upload timestamp *)
  key : string;  (** public key contents *)
  name : string;  (** human-readable name for this key *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

    end
    module Pipeline : sig

type clone_opts = {
  depth : int;
  skip : bool;
  submodules : bool;
}

(** Jsont codec for {!type:clone_opts}. *)
val clone_opts_jsont : clone_opts Jsont.t


type pair = {
  key : string;
  value : string;
}

(** Jsont codec for {!type:pair}. *)
val pair_jsont : pair Jsont.t


type pull_request_trigger_data = {
  action : string;
  source_branch : string;
  source_sha : string;
  target_branch : string;
}

(** Jsont codec for {!type:pull_request_trigger_data}. *)
val pull_request_trigger_data_jsont : pull_request_trigger_data Jsont.t


type push_trigger_data = {
  new_sha : string;
  old_sha : string;
  ref_ : string;
}

(** Jsont codec for {!type:push_trigger_data}. *)
val push_trigger_data_jsont : push_trigger_data Jsont.t


type trigger_repo = {
  default_branch : string;
  did : string;
  knot : string;
  repo : string;
}

(** Jsont codec for {!type:trigger_repo}. *)
val trigger_repo_jsont : trigger_repo Jsont.t


type manual_trigger_data = {
  inputs : pair list option;
}

(** Jsont codec for {!type:manual_trigger_data}. *)
val manual_trigger_data_jsont : manual_trigger_data Jsont.t


type workflow = {
  clone : clone_opts;
  engine : string;
  name : string;
  raw : string;
}

(** Jsont codec for {!type:workflow}. *)
val workflow_jsont : workflow Jsont.t


type trigger_metadata = {
  kind : string;
  manual : manual_trigger_data option;
  pull_request : pull_request_trigger_data option;
  push : push_trigger_data option;
  repo : trigger_repo;
}

(** Jsont codec for {!type:trigger_metadata}. *)
val trigger_metadata_jsont : trigger_metadata Jsont.t


type main = {
  trigger_metadata : trigger_metadata;
  workflows : workflow list;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      module Status : sig

type main = {
  created_at : string;  (** time of creation of this status update *)
  error : string option;  (** error message if failed *)
  exit_code : int option;  (** exit code if failed *)
  pipeline : string;  (** ATURI of the pipeline *)
  status : string;  (** status of the workflow *)
  workflow : string;  (** name of the workflow within this pipeline *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Owner : sig
(** Get the owner of a service *)


type output = {
  owner : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

    end
    module Label : sig
      module Op : sig

type operand = {
  key : string;  (** ATURI to the label definition *)
  value : string;  (** Stringified value of the label. This is first unstringed by appviews and then interpreted as a concrete value. *)
}

(** Jsont codec for {!type:operand}. *)
val operand_jsont : operand Jsont.t


type main = {
  add : operand list;
  delete : operand list;
  performed_at : string;
  subject : string;  (** The subject (task, pull or discussion) of this label. Appviews may apply a `scope` check and refuse this op. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Definition : sig

type value_type = {
  enum : string list option;  (** Closed set of values that this label can take. *)
  format : string;  (** An optional constraint that can be applied on string concrete types. *)
  type_ : string;  (** The concrete type of this label's value. *)
}

(** Jsont codec for {!type:value_type}. *)
val value_type_jsont : value_type Jsont.t


type main = {
  color : string option;  (** The hex value for the background color for the label. Appviews may choose to respect this. *)
  created_at : string;
  multiple : bool option;  (** Whether this label can be repeated for a given entity, eg.: \[reviewer:foo, reviewer:bar\] *)
  name : string;  (** The display name of this label. *)
  scope : string list;  (** The areas of the repo this label may apply to, eg.: sh.tangled.repo.issue. Appviews may choose to respect this. *)
  value_type : value_type;  (** The type definition of this label. Appviews may allow sorting for certain types. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Knot : sig

type main = {
  created_at : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      module Version : sig
(** Get the version of a knot *)


type output = {
  version : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Member : sig

type main = {
  created_at : string;
  domain : string;  (** domain that this member now belongs to *)
  subject : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module ListKeys : sig

type public_key = {
  created_at : string;  (** Key upload timestamp *)
  did : string;  (** DID associated with the public key *)
  key : string;  (** Public key contents *)
}

(** Jsont codec for {!type:public_key}. *)
val public_key_jsont : public_key Jsont.t

(** List all public keys stored in the knot server *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Pagination cursor *)
  limit : int option;  (** Maximum number of keys to return *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;  (** Pagination cursor for next page *)
  keys : public_key list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Graph : sig
      module Follow : sig

type main = {
  created_at : string;
  subject : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Git : sig
      module RefUpdate : sig

type individual_email_commit_count = {
  count : int;
  email : string;
}

(** Jsont codec for {!type:individual_email_commit_count}. *)
val individual_email_commit_count_jsont : individual_email_commit_count Jsont.t


type individual_language_size = {
  lang : string;
  size : int;
}

(** Jsont codec for {!type:individual_language_size}. *)
val individual_language_size_jsont : individual_language_size Jsont.t


type commit_count_breakdown = {
  by_email : individual_email_commit_count list option;
}

(** Jsont codec for {!type:commit_count_breakdown}. *)
val commit_count_breakdown_jsont : commit_count_breakdown Jsont.t


type lang_breakdown = {
  inputs : individual_language_size list option;
}

(** Jsont codec for {!type:lang_breakdown}. *)
val lang_breakdown_jsont : lang_breakdown Jsont.t


type meta = {
  commit_count : commit_count_breakdown;
  is_default_ref : bool;
  lang_breakdown : lang_breakdown option;
}

(** Jsont codec for {!type:meta}. *)
val meta_jsont : meta Jsont.t

(** An update to a git repository, emitted by knots. *)

type main = {
  committer_did : string;  (** did of the user that pushed this ref *)
  meta : meta;
  new_sha : string;  (** new SHA of this ref *)
  old_sha : string;  (** old SHA of this ref *)
  ref_ : string;  (** Ref being updated *)
  repo_did : string;  (** did of the owner of the repo *)
  repo_name : string;  (** name of the repo *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Feed : sig
      module Star : sig

type main = {
  created_at : string;
  subject : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Reaction : sig

type main = {
  created_at : string;
  reaction : string;
  subject : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Actor : sig
      module Profile : sig
(** A declaration of a Tangled account profile. *)

type main = {
  bluesky : bool;  (** Include link to this account on Bluesky. *)
  description : string option;  (** Free-form profile description text. *)
  links : string list option;
  location : string option;  (** Free-form location text. *)
  pinned_repositories : string list option;  (** Any ATURI, it is up to appviews to validate these fields. *)
  pronouns : string option;  (** Preferred gender pronouns. *)
  stats : string list option;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
  end
end
