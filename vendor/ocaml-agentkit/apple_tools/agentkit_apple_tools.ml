(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Codec = Apple_fm.Codec
module Tool = Agentkit_apple_fm.Tool
open Codec

let field_description = function
  | "cap" ->
      "Capability returned by open_dir. Use an empty string for the starting \
       directory."
  | "path" -> "Path relative to the selected workspace or capability."
  | "kind" -> "Memory kind: fact, open_item, reference, or procedure."
  | "tags" -> "Tags for grouping memory entries. Use [] when there are none."
  | "why" -> "One line explaining why this memory change is needed."
  | "unused" -> "Pass an empty string."
  | "args" -> "Program arguments as separate strings, without shell quoting."
  | "render" -> "Use text for page content or raw for response bytes."
  | "max_bytes" -> "Maximum response size in bytes. Use 0 for the default."
  | "targets" -> "Dune build target. Use . for the whole workspace."
  | "module" -> "OCaml module name. Use an empty string for the project."
  | "id" -> "Stable memory entry identifier."
  | "body" -> "Text to preserve in memory for a later wake-up."
  | "title" -> "One-line title for a memory entry."
  | "query" -> "Type or value to search for."
  | "line" -> "Line number, starting at 1."
  | "col" -> "Column number, starting at 0."
  | "start" -> "First line to return, starting at 1."
  | "count" -> "Number of lines to return. Use 0 for the rest of the file."
  | "old" -> "Exact passage to replace. It must occur once."
  | "new" -> "Replacement passage."
  | "content" -> "New file contents."
  | field -> field

let bind native codec =
  let name = Ds4.Tool.name native in
  Tool.v ~description:(Ds4.Tool.description native) codec (fun args ->
      let arguments =
        match Codec.encode_arguments codec args with
        | Ok arguments -> arguments
        | Error message ->
            invalid_arg ("cannot encode " ^ name ^ " arguments: " ^ message)
      in
      Ds4.Tool.invoke native { Dsml.name; id = None; arguments })

let empty name = Codec.Invoke.(map name () |> seal)

let one name field value =
  Codec.Invoke.(
    map name Fun.id
    |> param ~enc:Fun.id ~description:(field_description field) field value
    |> seal)

let one_default name field value default =
  Codec.Invoke.(
    map name Fun.id
    |> param ~enc:Fun.id ~default ~description:(field_description field) field
         value
    |> seal)

let two name first second =
  Codec.Invoke.(
    map name (fun a b -> (a, b))
    |> param ~enc:fst ~description:(field_description first) first string
    |> param ~enc:snd ~description:(field_description second) second string
    |> seal)

let three name first second third =
  Codec.Invoke.(
    map name (fun a b c -> (a, b, c))
    |> param
         ~enc:(fun (a, _, _) -> a)
         ~description:(field_description first) first string
    |> param
         ~enc:(fun (_, b, _) -> b)
         ~description:(field_description second) second string
    |> param
         ~enc:(fun (_, _, c) -> c)
         ~description:(field_description third) third string
    |> seal)

let four name first second third fourth =
  Codec.Invoke.(
    map name (fun a b c d -> (a, b, c, d))
    |> param ~enc:(fun (a, _, _, _) -> a) first string
    |> param ~enc:(fun (_, b, _, _) -> b) second string
    |> param ~enc:(fun (_, _, c, _) -> c) third string
    |> param ~enc:(fun (_, _, _, d) -> d) fourth string
    |> seal)

let two_strings_one_int name first second third =
  Codec.Invoke.(
    map name (fun a b c -> (a, b, c))
    |> param ~enc:(fun (a, _, _) -> a) first string
    |> param ~enc:(fun (_, b, _) -> b) second string
    |> param ~enc:(fun (_, _, c) -> c) third int
    |> seal)

let two_strings_two_ints name first second third fourth =
  Codec.Invoke.(
    map name (fun a b c d -> (a, b, c, d))
    |> param ~enc:(fun (a, _, _, _) -> a) first string
    |> param ~enc:(fun (_, b, _, _) -> b) second string
    |> param ~enc:(fun (_, _, c, _) -> c) third int
    |> param ~enc:(fun (_, _, _, d) -> d) fourth int
    |> seal)

let complete name =
  Codec.Invoke.(
    map name (fun cap path line col prefix -> (cap, path, line, col, prefix))
    |> param ~enc:(fun (cap, _, _, _, _) -> cap) "cap" string
    |> param ~enc:(fun (_, path, _, _, _) -> path) "path" string
    |> param ~enc:(fun (_, _, line, _, _) -> line) "line" int
    |> param ~enc:(fun (_, _, _, col, _) -> col) "col" int
    |> param ~enc:(fun (_, _, _, _, prefix) -> prefix) "prefix" string
    |> seal)

let fetch name =
  Codec.Invoke.(
    map name (fun url render max_bytes -> (url, render, max_bytes))
    |> param ~enc:(fun (url, _, _) -> url) "url" string
    |> param ~enc:(fun (_, render, _) -> render) "render" string
    |> param ~enc:(fun (_, _, max_bytes) -> max_bytes) "max_bytes" int
    |> seal)

let run name =
  Codec.Invoke.(
    map name (fun program args -> (program, args))
    |> param ~enc:fst "program" string
    |> param ~enc:snd "args" (array string)
    |> seal)

let memory_write name =
  Codec.Invoke.(
    map name (fun id kind title body tags why ->
        (id, kind, title, body, tags, why))
    |> param
         ~enc:(fun (id, _, _, _, _, _) -> id)
         ~description:(field_description "id") "id" string
    |> param
         ~enc:(fun (_, kind, _, _, _, _) -> kind)
         ~description:(field_description "kind") "kind" string
    |> param
         ~enc:(fun (_, _, title, _, _, _) -> title)
         ~description:(field_description "title")
         "title" string
    |> param
         ~enc:(fun (_, _, _, body, _, _) -> body)
         ~description:(field_description "body") "body" string
    |> param
         ~enc:(fun (_, _, _, _, tags, _) -> tags)
         ~description:(field_description "tags") "tags" (array string)
    |> param
         ~enc:(fun (_, _, _, _, _, why) -> why)
         ~description:(field_description "why") "why" string
    |> seal)

let names =
  [
    "open_dir";
    "caps";
    "list";
    "tree";
    "read";
    "read_lines";
    "find";
    "grep";
    "stat";
    "write";
    "append";
    "edit";
    "dns";
    "build";
    "test";
    "promote";
    "project";
    "outline";
    "type_at";
    "locate";
    "errors";
    "occurrences";
    "search";
    "complete";
    "fetch";
    "head";
    "run";
    "memory_list";
    "memory_read";
    "memory_write";
    "memory_forget";
  ]

let supported name = List.mem name names

let of_ds4 native =
  let name = Ds4.Tool.name native in
  let wrap codec = bind native codec in
  match name with
  | "open_dir" | "list" | "read" | "stat" | "outline" | "errors" ->
      wrap (two name "cap" "path")
  | "caps" -> wrap (one name "unused" Codec.string)
  | "tree" -> wrap (two_strings_one_int name "cap" "path" "depth")
  | "read_lines" ->
      wrap (two_strings_two_ints name "cap" "path" "start" "count")
  | "find" | "grep" -> wrap (three name "cap" "path" "substring")
  | "write" | "append" -> wrap (three name "cap" "path" "content")
  | "edit" -> wrap (four name "cap" "path" "old" "new")
  | "dns" -> wrap (one name "host" Codec.string)
  | "build" -> wrap (one_default name "targets" Codec.string ".")
  | "test" -> wrap (empty name)
  | "promote" -> wrap (one name "path" Codec.string)
  | "project" -> wrap (one_default name "module" Codec.string "")
  | "type_at" | "locate" | "occurrences" ->
      wrap (two_strings_two_ints name "cap" "path" "line" "col")
  | "search" -> wrap (three name "cap" "path" "query")
  | "complete" -> wrap (complete name)
  | "fetch" -> wrap (fetch name)
  | "head" -> wrap (one name "url" Codec.string)
  | "run" -> wrap (run name)
  | "memory_list" -> wrap (two name "kind" "tag")
  | "memory_read" -> wrap (one name "id" Codec.string)
  | "memory_write" -> wrap (memory_write name)
  | "memory_forget" -> wrap (two name "id" "why")
  | _ -> invalid_arg ("no Apple tool schema for " ^ name)

let context_size model =
  if model <> "default" then invalid_arg "Apple model must be apple/default";
  let native = Apple_fm.Model.default in
  (Apple_fm.Model.info ~model:native ()).context_size

let create ~sw ~model ~system tools =
  let context_size = context_size model in
  let native = Apple_fm.Model.default in
  let tools = List.map of_ds4 tools in
  let system =
    system
    ^ "\n\n\
       Use a tool only when the request needs it. Do not write to the \
       workspace unless the request asks for a change."
  in
  let agent =
    Agentkit_apple_fm.Agent.create ~sw ~model:native ~instructions:system
      ~compact_at:75 tools
  in
  (Agentkit.Driver.session (module Agentkit_apple_fm.Agent) agent, context_size)
