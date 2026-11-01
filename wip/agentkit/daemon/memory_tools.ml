(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Journal = Agentkit.Journal
module Memory = Agentkit.Memory
module Tool = Ds4.Tool

(* What a kind is for, said to the model rather than left for it to guess. A
   brief sorts on these, so an entry filed under the wrong one is either read
   back in full every wake-up or not read back at all. *)
let kinds_description =
  "one of \"fact\", \"open_item\", \"reference\", \"procedure\" or \
   \"episode\". A fact is something durable you learned and would have to find \
   out again. An open_item is work you have started and not finished, and it \
   is the kind that makes a restart survivable, so file anything you mean to \
   carry on with under it. A reference is a pointer outward, a URL or an \
   identifier, kept so that you do not have to search for it twice. A \
   procedure is how to do something you have had to work out once already. An \
   episode records completed activity or an observation, compressed in future \
   briefs."

let kind_of s =
  match Memory.kind_of_name s with
  | Some k -> Ok k
  | None ->
      Error
        (Printf.sprintf "%S is not a memory kind. Write %s" s kinds_description)

let tags_of = function
  | Jsont.Array (elts, _) ->
      List.fold_right
        (fun elt acc ->
          match (elt, acc) with
          | Jsont.String (v, _), Ok vs -> Ok (v :: vs)
          | Jsont.String _, e -> e
          | _ -> Error "every element of tags must be a string.")
        elts (Ok [])
  | _ ->
      Error
        "tags must be a JSON array of strings, such as [\"ds4\", \"upstream\"]."

let tags_description =
  "the tags, as a JSON array of strings, such as [\"ds4\"]. Left out, none. \
   They are for grouping entries you will want together, and memory_list \
   filters on them."

let why_description =
  "why you are writing this, in one line. It is kept on the version and in the \
   journal, so a person reading the trace afterwards sees what each change to \
   memory was for."

let entry_line (e : Memory.entry) =
  Printf.sprintf "  %-10s %-24s %s%s"
    (Memory.kind_name e.Memory.kind)
    e.Memory.id e.Memory.title
    (match e.Memory.tags with
    | [] -> ""
    | tags -> "  [" ^ String.concat " " tags ^ "]")

let ids entries =
  match entries with
  | [] -> "memory holds no entries"
  | entries ->
      "the entries are "
      ^ String.concat ", "
          (List.map (fun (e : Memory.entry) -> e.Memory.id) entries)

let list ~memory =
  let codec =
    let open Dsml.Codec in
    Invoke.map "memory_list" (fun kind tag -> (kind, tag))
    |> Invoke.param ~enc:fst ~default:"" "kind" string
         ~description:
           ("show only entries of this kind, " ^ kinds_description
          ^ " Left out, every kind.")
    |> Invoke.param ~enc:snd ~default:"" "tag" string
         ~description:
           "show only entries carrying this tag. Left out, every entry."
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "List what is in memory: the id, kind, title and tags of each entry at \
       the version in force, without the bodies. Call it to see what you \
       already know before you go and find it out again, and to find the id of \
       an entry you mean to read, update or forget." codec (fun (kind, tag) ->
      match
        if kind = "" then Ok None else Result.map Option.some (kind_of kind)
      with
      | Error e -> e
      | Ok wanted ->
          let entries = Memory.entries memory in
          let entries =
            List.filter
              (fun (e : Memory.entry) ->
                (match wanted with None -> true | Some k -> e.Memory.kind = k)
                && (tag = "" || List.mem tag e.Memory.tags))
              entries
          in
          let version = Memory.version memory in
          if entries = [] then
            Printf.sprintf "memory version %d holds no entry that matches.\n"
              version
          else
            Printf.sprintf "memory version %d, %d %s\n%s\n" version
              (List.length entries)
              (if List.length entries = 1 then "entry" else "entries")
              (String.concat "\n" (List.map entry_line entries)))

let read ~memory =
  let codec =
    let open Dsml.Codec in
    Invoke.map "memory_read" (fun id -> id)
    |> Invoke.param ~enc:Fun.id "id" string
         ~description:"the id of the entry to read, as memory_list reports it"
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Read one entry of memory in full: its kind, its tags, the versions it \
       first appeared in and last changed in, and its whole body." codec
    (fun id ->
      let entries = Memory.entries memory in
      match
        List.find_opt (fun (e : Memory.entry) -> e.Memory.id = id) entries
      with
      | None ->
          Printf.sprintf "no entry %S at memory version %d. %s." id
            (Memory.version memory) (ids entries)
      | Some e ->
          Printf.sprintf
            "%s (%s), created at version %d, updated at version %d%s\n\
             %s\n\n\
             %s\n"
            e.Memory.id
            (Memory.kind_name e.Memory.kind)
            e.Memory.created e.Memory.updated
            (match e.Memory.tags with
            | [] -> ""
            | tags -> "\ntags: " ^ String.concat ", " tags)
            e.Memory.title e.Memory.body)

let write ~memory ~journal =
  let codec =
    let open Dsml.Codec in
    Invoke.map "memory_write" (fun id kind title body tags why ->
        (id, kind, title, body, tags, why))
    |> Invoke.param
         ~enc:(fun (i, _, _, _, _, _) -> i)
         "id" string
         ~description:
           "the id of the entry, a short stable name such as \"ds4-upstream\". \
            Writing an id that is already there replaces that entry, which is \
            how an entry is updated."
    |> Invoke.param
         ~enc:(fun (_, k, _, _, _, _) -> k)
         "kind" string ~description:kinds_description
    |> Invoke.param
         ~enc:(fun (_, _, t, _, _, _) -> t)
         "title" string
         ~description:
           "one line saying what the entry is. It is what a listing shows and \
            what the next wake-up reads first."
    |> Invoke.param
         ~enc:(fun (_, _, _, b, _, _) -> b)
         "body" string
         ~description:
           "the text of the entry. Write what a reader who has none of your \
            context would need, since that reader is you at the next wake-up."
    |> Invoke.param
         ~enc:(fun (_, _, _, _, t, _) -> t)
         ~default:(Jsont.Json.list []) "tags" json ~description:tags_description
    |> Invoke.param
         ~enc:(fun (_, _, _, _, _, w) -> w)
         "why" string ~description:why_description
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Write an entry into memory, which is the only thing that survives this \
       wake-up. Everything in this conversation is discarded when it ends, so \
       anything you will want to know next time has to go through here. Each \
       call mints a new version holding the whole of memory, and the version \
       before it is left as it was." codec
    (fun (id, kind, title, body, tags, why) ->
      match (kind_of kind, tags_of tags) with
      | Error e, _ | _, Error e -> e
      | Ok kind, Ok tags ->
          if String.trim why = "" then
            "why must say what this write is for, in one line. It is kept on \
             the version and in the journal."
          else
            let to_ =
              Memory.write memory ~seq:(Journal.next_seq journal) ~cause:why
                ~journal:(fun mw ->
                  ignore (Journal.append journal (Journal.Memory_write mw)))
                ~id ~kind ~title ~body ~tags
            in
            Printf.sprintf "memory version %d: wrote %S as a %s.\n" to_ id
              (Memory.kind_name kind))

let forget ~memory ~journal =
  let codec =
    let open Dsml.Codec in
    Invoke.map "memory_forget" (fun id why -> (id, why))
    |> Invoke.param ~enc:fst "id" string
         ~description:"the id of the entry to drop, as memory_list reports it"
    |> Invoke.param ~enc:snd "why" string ~description:why_description
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Drop an entry from memory. It mints a version with the entry absent and \
       does not touch the versions that held it, so the entry is still there \
       in the history for anyone auditing it. Forget an open_item you have \
       finished and a fact that has turned out to be wrong. Nothing else \
       prunes memory, so this is the only way it gets smaller." codec
    (fun (id, why) ->
      let entries = Memory.entries memory in
      if not (List.exists (fun (e : Memory.entry) -> e.Memory.id = id) entries)
      then
        Printf.sprintf "no entry %S at memory version %d. %s." id
          (Memory.version memory) (ids entries)
      else if String.trim why = "" then
        "why must say what this write is for, in one line. It is kept on the \
         version and in the journal."
      else
        let to_ =
          Memory.forget memory ~seq:(Journal.next_seq journal) ~cause:why
            ~journal:(fun mw ->
              ignore (Journal.append journal (Journal.Memory_write mw)))
            id
        in
        Printf.sprintf "memory version %d: forgot %S.\n" to_ id)

let overview ~memory =
  let codec =
    Dsml.Codec.Invoke.map "memory_overview" () |> Dsml.Codec.Invoke.seal
  in
  Tool.v
    ~description:
      "Read a bounded overview of episodic memory. Facts, procedures and open \
       items are read separately with memory_list/read." codec (fun () ->
      let summaries = Memory.summaries memory in
      Agentkit.Memo.overview
        (Memory.episode_tree (Memory.entries memory))
        ~budget:8
        ~lookup:(fun key -> List.assoc_opt key summaries)
      |> Agentkit.Memo.render ~limit:3500)

let expand ~memory =
  let codec =
    let open Dsml.Codec in
    Invoke.map "memory_expand" Fun.id
    |> Invoke.param ~enc:Fun.id "key" string
         ~description:"the range key returned by memory_overview"
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Expand an episode range into two children or its original source. Use \
       memory_read for an exact source entry." codec (fun key ->
      try
        let summaries = Memory.summaries memory in
        Agentkit.Memo.expand
          (Memory.episode_tree (Memory.entries memory))
          ~key
          ~lookup:(fun key -> List.assoc_opt key summaries)
        |> Agentkit.Memo.render ~limit:3500
      with Invalid_argument message -> message)

let summarize ~memory =
  let codec =
    let open Dsml.Codec in
    Invoke.map "memory_summarize" (fun key summary -> (key, summary))
    |> Invoke.param ~enc:fst "key" string
         ~description:"a current episode range key, after expanding its sources"
    |> Invoke.param ~enc:snd "summary" string
         ~description:
           "at most 512 UTF-8 bytes preserving source IDs, attribution, \
            uncertainty and corrections. Never invent facts."
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Cache a lossy summary of an episode range whose sources you have read. \
       This does not replace or erase the originals." codec (fun (key, text) ->
      try
        Memory.save_summary memory ~key ~text;
        "Episode summary cached."
      with Invalid_argument message -> message)

let all ~memory ~journal =
  [
    list ~memory;
    read ~memory;
    write ~memory ~journal;
    forget ~memory ~journal;
    overview ~memory;
    expand ~memory;
    summarize ~memory;
  ]
