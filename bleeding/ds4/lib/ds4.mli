(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Run language models locally on the DS4 engine, and build agents on top.

    This library wraps the DS4 inference engine, which runs the DeepSeek V4,
    DeepSeek V4.1 Flash, GLM 5.3 Flash and Qwen3.8 Flash Next models, and adds a
    tool-using agent over it. The backend is chosen at link time by depending on
    an implementation, either [ds4.metal] or [ds4.cpu]. The API is the same for
    both.

    Prompt encoding and reply parsing live in the companion {!Dsml} library,
    which speaks every model's markup. The agent picks the one the loaded model
    expects. *)

module V4 = V4
(** The inference engine. Open a model with {!V4.create}, then use
    {!V4.generate} for a single reply or {!V4.Session} for a conversation. *)

module Agent = Agent
(** A tool-using agent. {!Agent.create} starts a conversation with a set of
    tools, and {!Agent.send} runs one user turn to completion. *)

module Tool = Tool
(** A typed tool, built from an argument codec and an OCaml handler. *)

module Toolbox = Toolbox
(** Ready-made tools for reading files, searching a tree, resolving hostnames
    and running commands. Each reaches only what it is given. *)

module Camel = Camel
(** A camel that says things, for presenting a model's replies. *)
