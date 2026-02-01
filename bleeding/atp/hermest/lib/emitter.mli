(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

(** Code emitter for lexicon code generation.

    This module provides a buffered code emitter with state tracking for:
    - Module imports
    - Generated union types (to avoid duplicates)
    - Union name registration (for reuse across definitions)
    - Cyclic dependency detection

    {2 Usage}

    {[
      let out = Emitter.make () in
      Emitter.emitln out "type t = {";
      Emitter.emitln out "  name : string;";
      Emitter.emitln out "}";
      let code = Emitter.contents out
    ]} *)

(** {1 Emitter State} *)

type t
(** Mutable emitter state including output buffer and tracking information. *)

val make : ?cyclic_nsids:string list -> unit -> t
(** [make ?cyclic_nsids ()] creates a new emitter.

    @param cyclic_nsids
      List of NSIDs that are part of dependency cycles. References to these
      NSIDs will use [Jsont.json] to break cycles. *)

(** {1 Cyclic Dependency Handling} *)

val is_cyclic_nsid : t -> string -> bool
(** [is_cyclic_nsid t nsid] returns [true] if [nsid] is part of a dependency
    cycle and should use [Jsont.json] instead of a direct type reference. *)

(** {1 Import Management} *)

val add_import : t -> string -> unit
(** [add_import t module_name] records that [module_name] should be imported.
    Duplicates are ignored. *)

val get_imports : t -> string list
(** [get_imports t] returns the list of recorded module imports. *)

(** {1 Union Type Tracking} *)

val mark_union_generated : t -> string -> unit
(** [mark_union_generated t name] marks union type [name] as generated to
    prevent duplicate definitions. *)

val is_union_generated : t -> string -> bool
(** [is_union_generated t name] returns [true] if union type [name] has already
    been generated. *)

val register_union_name : t -> string list -> string -> unit
(** [register_union_name t refs name] registers a context-based name for a union
    defined by [refs]. This allows inline unions with the same set of refs to
    reuse the same type name.

    @param refs List of type references in the union
    @param name The generated type name for this union *)

val lookup_union_name : t -> string list -> string option
(** [lookup_union_name t refs] looks up the registered name for a union with the
    given [refs]. Returns [None] if no name is registered. *)

(** {1 Code Emission} *)

val emit : t -> string -> unit
(** [emit t s] appends string [s] to the output buffer. *)

val emitln : t -> string -> unit
(** [emitln t s] appends string [s] followed by a newline to the output buffer.
*)

val emit_newline : t -> unit
(** [emit_newline t] appends a newline to the output buffer. *)

val contents : t -> string
(** [contents t] returns the accumulated output as a string. *)
