(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

(** OCaml name generation from AT Protocol lexicons.

    This module provides functions for converting AT Protocol identifiers
    (NSIDs, definition names, field names) into valid OCaml identifiers
    following OCaml naming conventions.

    {2 Naming Conventions}

    - Type names use [snake_case]
    - Module names use [PascalCase]
    - Field names use [snake_case]
    - OCaml keywords are escaped with trailing underscore

    {2 NSID Structure}

    NSIDs (Namespaced Identifiers) follow reverse-domain notation:
    [com.example.namespace.lexiconName]

    These are converted to:
    - File paths: [com_example_namespace_lexiconName.ml]
    - Module names: [Com_example_namespace_lexiconName] *)

(** {1 Keyword Handling} *)

val escape_keyword : string -> string
(** [escape_keyword s] returns [s] with a trailing underscore if [s] is an OCaml
    reserved keyword, otherwise returns [s] unchanged.

    Example: [escape_keyword "type"] returns ["type_"]. *)

(** {1 Case Conversion} *)

val camel_to_snake : string -> string
(** [camel_to_snake s] converts [camelCase] or [PascalCase] to [snake_case].

    Examples:
    - ["camelCase"] -> ["camel_case"]
    - ["PascalCase"] -> ["pascal_case"]
    - ["XMLParser"] -> ["xml_parser"] *)

(** {1 Field Names} *)

val field_name : string -> string
(** [field_name s] converts a JSON field name to an OCaml record field name.

    Applies snake_case conversion and keyword escaping. *)

val needs_key_annotation : string -> string -> bool
(** [needs_key_annotation json_name ocaml_name] returns [true] if the OCaml name
    differs from the JSON name and requires a [[@key "..."]] annotation. *)

val key_annotation : string -> string -> string
(** [key_annotation json_name ocaml_name] returns the jsont key annotation
    string if needed, or empty string if names match. *)

(** {1 Type Names} *)

val type_name : string -> string
(** [type_name s] converts a definition name to an OCaml type name.

    Applies snake_case conversion and keyword escaping.

    Example: ["postView"] -> ["post_view"]. *)

val def_module_name : string -> string
(** [def_module_name s] converts a definition name to a module name.

    Example: ["postView"] -> ["PostView"]. *)

(** {1 Reference Handling} *)

type ref_parts = {
  nsid : string option;  (** External NSID if present. *)
  def : string option;  (** Definition name if present. *)
}
(** Parsed components of a lexicon reference string. *)

val parse_ref : string -> ref_parts
(** [parse_ref ref_str] parses a reference string into components.

    Reference formats:
    - ["#localDef"] -> [{nsid=None; def=Some "localDef"}]
    - ["com.example#someDef"] -> [{nsid=Some "com.example"; def=Some "someDef"}]
    - ["com.example"] -> [{nsid=Some "com.example"; def=None}] *)

val variant_name_of_ref : string -> string
(** [variant_name_of_ref ref_str] extracts a variant constructor name from a
    reference string.

    Examples:
    - ["#localDef"] -> ["LocalDef"]
    - ["com.example.defs#someDef"] -> ["SomeDef"] *)

val qualified_variant_name_of_ref : string -> string
(** [qualified_variant_name_of_ref ref_str] generates a qualified variant name
    including the last NSID segment to avoid conflicts.

    Examples:
    - ["app.bsky.embed.images#view"] -> ["ImagesView"]
    - ["app.bsky.embed.images"] -> ["Images"]
    - ["#localDef"] -> ["LocalDef"] *)

(** {1 Union Types} *)

val union_type_name : string list -> string
(** [union_type_name refs] generates a type name for a union of references.

    For single refs, uses the ref name. For multiple refs, combines the first
    two sorted names with ["_or_"]. *)

(** {1 NSID Conversion} *)

val flat_name_of_nsid : string -> string
(** [flat_name_of_nsid nsid] converts NSID to flat underscore-separated name.

    Example: ["app.bsky.feed.post"] -> ["app_bsky_feed_post"]. *)

val file_path_of_nsid : string -> string
(** [file_path_of_nsid nsid] converts NSID to OCaml source file path.

    Example: ["app.bsky.feed.post"] -> ["app_bsky_feed_post.ml"]. *)

val flat_module_name_of_nsid : string -> string
(** [flat_module_name_of_nsid nsid] converts NSID to module name.

    Example: ["app.bsky.feed.post"] -> ["App_bsky_feed_post"]. *)

val module_path_of_nsid : string -> string list
(** [module_path_of_nsid nsid] splits NSID into module path segments.

    Example: ["app.bsky.feed.post"] -> ["App"; "Bsky"; "Feed"; "Post"]. *)

val module_path_string : string -> string
(** [module_path_string nsid] converts NSID to hierarchical module path string.

    Example: ["app.bsky.feed.post"] -> ["App.Bsky.Feed.Post"]. *)

val relative_module_path : from_nsid:string -> to_nsid:string -> string
(** [relative_module_path ~from_nsid ~to_nsid] computes the relative module path
    from one NSID location to another.

    Finds the common prefix between the NSIDs and returns the path from the
    common ancestor to the target.

    Examples:
    - [from "app.bsky.actor.defs" to "app.bsky.notification.defs"] ->
      ["Notification.Defs"] (common prefix is ["app.bsky"])
    - [from "app.bsky.actor.defs" to "com.atproto.label.defs"] ->
      ["Com.Atproto.Label.Defs"] (no common prefix) *)

val type_name_of_nsid : string -> string
(** [type_name_of_nsid nsid] extracts the type name from the last segment of an
    NSID.

    Example: ["app.bsky.feed.post"] -> ["post"]. *)

(** {1 Shared Module Generation}

    Functions for generating shared module names when handling cyclic
    dependencies between lexicon files. *)

val common_prefix_of_nsids : string list -> string list
(** [common_prefix_of_nsids nsids] finds the common prefix segments from a list
    of NSIDs.

    Example: [["app.bsky.actor.defs"; "app.bsky.feed.defs"]] ->
    [["app"; "bsky"]]. *)

val shared_file_name : string list -> int -> string
(** [shared_file_name nsids index] generates a shared module file name from
    NSIDs.

    Example: [["app.bsky.actor.defs"; "app.bsky.feed.defs"]] with index 1 ->
    ["app_bsky_shared_1.ml"]. *)

val shared_module_name : string list -> int -> string
(** [shared_module_name nsids index] generates a shared module name from NSIDs.

    Example: [["app.bsky.actor.defs"; "app.bsky.feed.defs"]] with index 1 ->
    ["App_bsky_shared_1"]. *)

val shared_type_name : string -> string -> string
(** [shared_type_name nsid def_name] generates a short type name for use in
    shared modules.

    Uses the last segment of the NSID as context (or second-last if last is
    "defs").

    Example: [shared_type_name "app.bsky.actor.defs" "viewerState"] ->
    ["actor_viewer_state"]. *)

(** {1 Module Hierarchy} *)

(** Trie structure for organizing NSIDs into nested modules.

    Supports the case where an NSID exists at an intermediate level that also
    has children, which is common in some lexicon hierarchies. *)
type trie =
  | Node of (string * trie) list
      (** Intermediate node with no NSID at this level. *)
  | Module of string  (** Leaf node with an NSID and no children. *)
  | ModuleWithChildren of string * (string * trie) list
      (** Node with both an NSID at this level AND child modules. This handles
          cases like [sh.tangled.pipeline] (parent NSID) and
          [sh.tangled.pipeline.status] (child NSID). *)

val group_nsids_by_prefix : string list -> (string * trie) list
(** [group_nsids_by_prefix nsids] organizes NSIDs into a trie structure suitable
    for generating nested OCaml module hierarchies.

    Example: [["app.bsky.feed.post"; "app.bsky.feed.like"]] produces a trie with
    [App > Bsky > Feed > {Post, Like}].

    Supports parent+child NSIDs like [sh.tangled.pipeline] and
    [sh.tangled.pipeline.status] using [ModuleWithChildren]. *)
