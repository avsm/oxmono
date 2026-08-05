(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

(** Strongly Connected Components for lexicon dependency analysis.

    This module implements Tarjan's algorithm to find strongly connected
    components (SCCs) in the dependency graph of lexicon definitions and files.

    SCCs are used to:
    - Detect circular dependencies between lexicons
    - Generate code in correct dependency order
    - Handle mutually recursive type definitions

    {2 Algorithm}

    Uses Tarjan's strongly connected components algorithm which runs in O(V+E)
    time where V is the number of nodes and E is the number of edges.

    {2 Example}

    {[
      let sccs = Scc.find_file_sccs lexicons in
      List.iter
        (fun scc ->
          match scc with
          | [ single ] ->
              (* non-cyclic *)
              generate_module single
          | multiple ->
              (* cyclic group *)
              generate_cyclic_modules multiple)
        sccs
    ]} *)

(** {1 Generic SCC Finding} *)

val find_sccs :
  'node list ->
  get_id:('node -> string) ->
  get_deps:('node -> string list) ->
  'node list list
(** [find_sccs nodes ~get_id ~get_deps] finds strongly connected components in a
    directed graph.

    @param nodes List of graph nodes
    @param get_id Function to get unique identifier for a node
    @param get_deps Function to get list of dependency IDs for a node

    Returns SCCs in reverse topological order (dependencies first). Each SCC is
    a list of nodes; single-element lists indicate non-cyclic nodes. *)

(** {1 Lexicon-Specific Functions} *)

val find_def_sccs :
  string -> Lexicon_types.def_entry list -> Lexicon_types.def_entry list list
(** [find_def_sccs nsid defs] finds SCCs among definitions within a single
    lexicon document.

    @param nsid The lexicon's NSID (used to identify self-references)
    @param defs List of definitions in the lexicon

    Returns SCCs in reverse topological order. Used to detect mutually recursive
    type definitions within a single file. *)

val get_external_nsids : Lexicon_types.lexicon_doc -> string list
(** [get_external_nsids doc] returns the list of external NSIDs that [doc]
    depends on.

    Scans all type definitions for external references (refs and unions) and
    collects the unique NSIDs. Does not include self-references. *)

val find_file_sccs :
  Lexicon_types.lexicon_doc list -> Lexicon_types.lexicon_doc list list
(** [find_file_sccs lexicons] finds SCCs among lexicon files based on their
    cross-file dependencies.

    @param lexicons List of parsed lexicon documents

    Returns SCCs in reverse topological order (dependencies first). Cyclic SCCs
    (multiple files) require special handling during code generation to break
    the dependency cycle. *)
