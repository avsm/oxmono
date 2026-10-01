(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Tool = Ds4.Tool
module Plain = Ds4.Toolbox

(* The capability rule has to be stated, not implied. Left to itself a model
   reaches for the shell, which needs no capability and so never asks for one,
   and the whole discipline is bypassed without either side noticing. *)
let default_system =
  "You are a concise coding agent.\n\n\
   Every file tool takes a cap argument naming a capability. Before you read, \
   search, list or write anywhere, call open_dir on the directory you want and \
   pass the name it returns, such as \"<lib>\", as cap in every later call. \
   Mint the capability first and work through it, rather than passing an empty \
   cap. A directory outside the one you started in must be requested with \
   open_dir before any other tool can reach it. Write \"~/x\" for a path under \
   the home directory rather than guessing where home is.\n\n\
   Use the file tools rather than bash for anything on disk. Start with tree \
   to get your bearings, then grep or read_lines rather than reading whole \
   files. Change a file with edit, which replaces one passage of it, and keep \
   write for creating a file or replacing the whole of one. A file longer than \
   one reply can hold is written in parts: write the first part, then append \
   the rest, since a call that runs past the end of a reply is discarded \
   whole."

(* What the agent is told about the dune tools, added to the system prompt only
   when they are there to use. A model that hears about a tool it does not have
   asks for it and then falls back to the shell. *)
let dune_system =
  "This workspace is built by dune, and you have tools for it. Call project \
   rather than tree to learn what it is made of, since it names each library \
   and executable with its directory, its modules and what it requires, and \
   naming one module reports only the component that holds it. Change code \
   with edit, which replaces one passage and leaves the rest of the file \
   alone, and keep write for creating a file or replacing the whole of one. \
   After either, read the diagnostics the call itself returns rather than \
   running a build through bash. Run test to check your work, and promote to \
   accept the file a test offers when its recorded output has changed. A \
   target for build is a path, such as \".\" for the whole workspace or \
   \"lib\" for a directory, and never an alias such as \"@check\"."

(* The merlin tools are named apart from the rest, since a workspace dune
   serves may still have no ocamlmerlin to answer for it.

   Every rule here is stated as a decision rule rather than a preference, and
   the table names the question rather than the tool, because that is the form
   the model has in front of it when it chooses. A preference loses to the
   habit of opening a file, which is what every other repository the model has
   seen rewarded.

   The last paragraph is what makes the difference. The base prompt sends the
   model to tree, grep and read_lines, and unless those are given their new and
   smaller jobs here the model reads them as still standing. Interfaces first,
   because an mli is the shape of a module stated once and the ml is that shape
   restated among everything else, and the tool results a model fills its
   context with are what makes it run out of context. *)
let merlin_system =
  "These tools answer from the compiler's own view of this code, so they are \
   exact where reading the text is a guess. Do not open an OCaml file to find \
   something out. Ask the tool that knows:\n\
   - what a module offers: outline, on its .mli.\n\
   - the type of anything: type_at.\n\
   - where a name comes from: locate, which answers with the definition itself.\n\
   - where a name is used: occurrences.\n\
   - whether a file still compiles: errors.\n\
   - what an installed library offers: complete.\n\
   - a value whose type you know and whose name you do not: search.\n\n\
   Call occurrences, never grep, for a name that OCaml defines. It knows the \
   identifier rather than the text, so it finds every use and no comment or \
   unrelated name, and it is what to call again before changing anything a \
   caller can see. grep is for text that is not an OCaml identifier, such as a \
   message, a comment, or a name in a dune file.\n\n\
   read and read_lines are for a passage one of these tools has already \
   pointed you at, and for a file that is not OCaml. Reading a .ml to learn \
   what its module offers costs several times the context that an outline of \
   its .mli does, and running out of context is what stops a task before it is \
   finished."

type status =
  | Serving of { hello : Proto.hello; refused : string option }
  | Unstarted of string

type agent = { agent : Ds4.Agent.t; status : status; instructions : bool }

let system_prompt ~base ~okit root =
  let add acc = function None -> acc | Some text -> acc ^ "\n\n" ^ text in
  add (add base okit) (Agentkit.Instructions.load root)

let assemble ~vision ~sw ~proc ~net ~clock ~caps ~trace ~argv ~root =
  let okit =
    match Client.start ~sw ~proc ~clock ~trace ~argv with
    | Ok client ->
        (* Stopped before the release of [sw] kills it, release handlers being
           run in the reverse of the order they were added and the spawn's
           having been added first. A killed okitd leaves the dune server it
           started running, where a stopped one takes it with it. *)
        Eio.Switch.on_release sw (fun () -> Client.stop client);
        Ok client
    | Error _ as e -> e
  in
  (* okit's write and edit build what they changed, so each stands in for the
     plain one rather than joining it. Two writes would leave the model
     choosing. *)
  let writers, dune_tools, okit_system =
    match okit with
    | Error _ -> ([ Plain.write ~caps; Plain.edit ~caps ], [], None)
    | Ok client -> (
        let hello = Client.hello client in
        match hello.Proto.dune with
        | false -> ([ Plain.write ~caps; Plain.edit ~caps ], [], None)
        | true ->
            let merlin, merlin_prompt =
              if hello.merlin then
                ( [
                    Toolbox.outline ~client ~caps;
                    Toolbox.type_at ~client ~caps;
                    Toolbox.locate ~client ~caps;
                    Toolbox.occurrences ~client ~caps;
                    Toolbox.errors ~client ~caps;
                    Toolbox.search ~client ~caps;
                    Toolbox.complete ~client ~caps;
                  ],
                  "\n\n" ^ merlin_system )
              else ([], "")
            in
            ( [ Toolbox.write ~client ~caps; Toolbox.edit ~client ~caps ],
              [
                Toolbox.build ~client;
                Toolbox.test ~client;
                Toolbox.promote ~client;
                Toolbox.project ~client;
              ]
              @ merlin,
              Some (dune_system ^ merlin_prompt) ))
  in
  let tools =
    [
      Plain.open_dir ~caps;
      Plain.caps ~caps;
      Plain.tree ~caps;
      Plain.list ~caps;
      Plain.read ~caps;
      Plain.read_lines ~caps;
      Plain.find ~caps;
      Plain.grep ~caps;
      Plain.stat ~caps;
      (* Not okit's, whichever writers are: a file arriving in parts does not
         compile until the last of them, so a build after each part would
         report errors that are only the file being unfinished. The model
         builds when it is done. *)
      Plain.append ~caps;
    ]
    @ (if vision then [ Plain.view_image ~caps ] else [])
    @ writers
    @ [ Plain.dns ~net ]
    @ dune_tools
  in
  (* Nothing here is logged. A caller shows what became of okit in whatever it
     shows a person, an interface note or a journal record, and a warning
     written from underneath it would say the same thing where that person is
     not looking. *)
  let status =
    match okit with
    | Ok client ->
        let hello = Client.hello client in
        (* A workspace with no dune-project asked for no session, so the dune
           tools are absent rather than refused. Only a workspace that has one
           and got no tools was refused, which is the case worth reporting. *)
        let refused =
          if
            (not hello.Proto.dune)
            && Eio.Path.is_file Eio.Path.(root / "dune-project")
          then Some hello.Proto.status
          else None
        in
        Serving { hello; refused }
    | Error e -> Unstarted e
  in
  (tools, okit_system, status)

let create_agent ~vision ~sw ~proc ~net ~clock ~domain_mgr ~fs ~cache ~model
    ~root ~approve ~trace ~argv ~system ~thinking ~ctx_size ~max_ctx_size ~seed
    =
  let caps = Ds4.Toolbox.Caps.create ~sw ~fs ~approve root in
  let tools, okit, status =
    assemble ~vision:(Option.is_some vision) ~sw ~proc ~net ~clock ~caps ~trace
      ~argv ~root
  in
  let instructions = Option.is_some (Agentkit.Instructions.load root) in
  let system = system_prompt ~base:system ~okit root in
  let engine = Ds4.V4.create ~sw ~domain_mgr ?vision ~cache ~model () in
  let agent =
    Ds4.Agent.create engine ~system ~thinking ~ctx_size ~max_ctx_size ~seed
      ~now:(fun () -> Eio.Time.now clock)
      ~tools
  in
  { agent; status; instructions }
