(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type scope = Task_core.scope
type level = Task_core.level

let scope ~current ~within ~cancelled = { Task_core.current; within; cancelled }
let root ?label ~scope ~now () = Task_core.root ?label ~scope ~now ()
let report = Task_core.report
let current (root : Task.t) = Task_core.current (Task_core.work root)
let ending = Task_core.on_ending
let id (t : Task.t) = Task_core.id t
