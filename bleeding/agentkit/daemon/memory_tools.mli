(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The tools an agent changes its memory through.

    A wake-up's context does not survive it and memory does, so these are how
    anything the agent wants to know next time is written down. Each entry
    mutation mints one version and appends one {!Agentkit.Journal.Memory_write}
    record, so a version corresponds to one tool call and the audit trace lines
    up with the model's actions one to one. Derived summary writes do not mint
    memory versions. Their tool calls and results remain in the journal.

    The journal record is appended between the fsynced snapshot and the move of
    [current], which is the order {!Agentkit.Memory} imposes. A journal that
    cannot be written raises there, and the run stops rather than going on
    unlogged. *)

val list : memory:Agentkit.Memory.t -> Ds4.Tool.t
(** [list ~memory] is [memory_list], which reports the id, kind, title and tags
    of every entry at the version in force. *)

val read : memory:Agentkit.Memory.t -> Ds4.Tool.t
(** [read ~memory] is [memory_read], which reports one entry in full. *)

val write : memory:Agentkit.Memory.t -> journal:Agentkit.Journal.t -> Ds4.Tool.t
(** [write ~memory ~journal] is [memory_write], which adds an entry or replaces
    the one of that id and mints a version. It takes a [why], since the version
    and the journal record both carry what the write was for. *)

val forget :
  memory:Agentkit.Memory.t -> journal:Agentkit.Journal.t -> Ds4.Tool.t
(** [forget ~memory ~journal] is [memory_forget], which mints a version with an
    entry absent. The version that held it is not touched, so the entry is still
    readable at every version it was in. *)

val all :
  memory:Agentkit.Memory.t -> journal:Agentkit.Journal.t -> Ds4.Tool.t list
(** [all ~memory ~journal] are the entry and episode tools, in the order a model
    meets them. *)

val overview : memory:Agentkit.Memory.t -> Ds4.Tool.t
(** [overview ~memory] is a bounded map of episodic memory. *)

val expand : memory:Agentkit.Memory.t -> Ds4.Tool.t
(** [expand ~memory] drills into a current episode range. *)

val summarize : memory:Agentkit.Memory.t -> Ds4.Tool.t
(** [summarize ~memory] accepts a bounded agent-written derived summary after
    the agent has read the range's sources. Obsolete keys are refused. *)
