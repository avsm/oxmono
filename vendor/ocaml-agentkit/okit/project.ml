(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

(* Reading a record out of [dune describe --format csexp] output. *)
let atom = function Csexp.Atom a -> Some a | Csexp.List _ -> None
let to_list = function Csexp.List l -> Some l | Csexp.Atom _ -> None

let field t name =
  match t with
  | Csexp.List entries ->
      List.find_map
        (function
          | Csexp.List [ Csexp.Atom n; v ] when n = name -> Some v | _ -> None)
        entries
  | Csexp.Atom _ -> None

type module_ = { name : string; impl : string option; intf : string option }

type component = {
  kind : [ `Library | `Executable ];
  name : string;
  local : bool;
  requires : string list;
  source_dir : string;
  modules : module_ list;
}

type t = { root : string; components : component list }

(* Dune writes every path under the build context, which with a build directory
   of ours is an absolute path outside the workspace, and writes it under
   _build/default when the build directory is the workspace's own. A reader
   wants the path as the source tree writes it, so both are stripped. *)
let relative ~context ~root path =
  let under prefix =
    if prefix = "" then None
    else if path = prefix then Some ""
    else
      let prefix = prefix ^ "/" in
      let n = String.length prefix in
      if String.starts_with ~prefix path then
        Some (String.sub path n (String.length path - n))
      else None
  in
  match List.find_map under [ context; root; "_build/default" ] with
  | Some p -> p
  | None -> path

let atom_field record name = Option.bind (field record name) atom

let list_field record name =
  match Option.bind (field record name) to_list with Some l -> l | None -> []

let atoms record name = List.filter_map atom (list_field record name)

(* A field dune states as a list of at most one path, such as the interface of
   a module that may not have one. *)
let optional_atom record name =
  match atoms record name with p :: _ -> Some p | [] -> None

let missing ~what ~field =
  Error
    (Printf.sprintf
       "dune describe workspace reported a %s with no %s. okit reads the shape \
        dune 3.24 and later write."
       what field)

let rec collect f = function
  | [] -> Ok []
  | x :: xs ->
      let* y = f x in
      let* ys = collect f xs in
      Ok (y :: ys)

let module_of ~context ~root record =
  match atom_field record "name" with
  | None -> missing ~what:"module" ~field:"name"
  | Some name ->
      let path field =
        Option.map (relative ~context ~root) (optional_atom record field)
      in
      Ok { name; impl = path "impl"; intf = path "intf" }

(* The alias module dune writes for a library, and the one it writes for the
   modules of an executable, are files it generates in the build directory.
   Neither is in the source tree, so a reader cannot open either. *)
let generated m =
  match (m.impl, m.intf) with
  | Some p, None -> Filename.check_suffix p ".ml-gen"
  | _ -> false

(* Only a local component's modules are of any use to a reader working here,
   and an installed library reports none of them anyway. *)
let modules_of ~context ~root ~local record =
  if not local then Ok []
  else
    let* modules =
      collect (module_of ~context ~root) (list_field record "modules")
    in
    Ok (List.filter (fun m -> not (generated m)) modules)

let requires_of ~names record =
  List.filter_map
    (fun uid -> Hashtbl.find_opt names uid)
    (atoms record "requires")

let library ~names ~context ~root record =
  match atom_field record "name" with
  | None -> missing ~what:"library" ~field:"name"
  | Some name ->
      let local = atom_field record "local" = Some "true" in
      let* modules = modules_of ~context ~root ~local record in
      let source_dir =
        match atom_field record "source_dir" with
        | Some d -> relative ~context ~root d
        | None -> ""
      in
      Ok
        {
          kind = `Library;
          name;
          local;
          requires = requires_of ~names record;
          source_dir;
          modules;
        }

(* An executables stanza names its programs together and states no directory of
   its own, so the directory is the one its modules are in. *)
let executables ~names ~context ~root record =
  match atoms record "names" with
  | [] -> missing ~what:"executables stanza" ~field:"names"
  | programs ->
      let* modules = modules_of ~context ~root ~local:true record in
      let source_dir =
        match List.find_map (fun m -> m.impl) modules with
        | None -> ""
        | Some p -> ( match Filename.dirname p with "." -> "" | d -> d)
      in
      Ok
        {
          kind = `Executable;
          name = String.concat " " programs;
          local = true;
          requires = requires_of ~names record;
          source_dir;
          modules;
        }

(* Each item is a two element list of its kind and its record, so a kind okit
   does not know is skipped rather than read wrongly. Dune adds kinds between
   versions. *)
let item ~names ~context ~root = function
  | Csexp.List [ Csexp.Atom "library"; record ] ->
      let* c = library ~names ~context ~root record in
      Ok (Some c)
  | Csexp.List [ Csexp.Atom "executables"; record ] ->
      let* c = executables ~names ~context ~root record in
      Ok (Some c)
  | _ -> Ok None

(* Dune names a library dependency by the uid of the library, so every library
   in the report is read for its uid before any dependency is resolved. *)
let library_names items =
  let names = Hashtbl.create 64 in
  List.iter
    (function
      | Csexp.List [ Csexp.Atom "library"; record ] -> (
          match (atom_field record "uid", atom_field record "name") with
          | Some uid, Some name -> Hashtbl.replace names uid name
          | _ -> ())
      | _ -> ())
    items;
  names

let of_sexp sexp =
  match to_list sexp with
  | None ->
      Error
        "dune describe workspace answered with an atom where okit expected the \
         list of what the workspace holds."
  | Some items ->
      let* root =
        match Option.bind (field sexp "root") atom with
        | Some r -> Ok r
        | None -> missing ~what:"workspace" ~field:"root"
      in
      let context =
        match Option.bind (field sexp "build_context") atom with
        | Some c -> c
        | None -> ""
      in
      let names = library_names items in
      let* components = collect (item ~names ~context ~root) items in
      Ok { root; components = List.filter_map Fun.id components }

(* The build directory is ours and holds nothing anyone else may want, so it
   goes as a whole. A link is unlinked rather than followed, since dune links
   into the source tree and into the shared cache. *)
let rec remove_tree path =
  match Unix.lstat path with
  | exception Unix.Unix_error _ -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (try Sys.readdir path with Sys_error _ -> [||]);
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

(* DUNE_BUILD_DIR is what keeps this off the lock a watching server holds, so
   an inherited setting must not survive into the child. INSIDE_DUNE goes too:
   dune sets it for the commands a build runs, and a dune that sees it takes
   the directory it starts in for the workspace root rather than looking for
   one, which would map an agent started from inside a build to nothing. *)
let environment ~build_dir =
  let dropped = [ "DUNE_BUILD_DIR="; "INSIDE_DUNE=" ] in
  let keep v =
    not (List.exists (fun prefix -> String.starts_with ~prefix v) dropped)
  in
  Array.append
    (Array.of_list (List.filter keep (Array.to_list (Unix.environment ()))))
    [| "DUNE_BUILD_DIR=" ^ build_dir |]

let first_line s =
  match String.index_opt s '\n' with None -> s | Some i -> String.sub s 0 i

(* The callback is the caller's, and a map must not fail because it raised.
   Cancellation is a fiber ending rather than a fault of the callback's, so it
   is passed on. *)
let guarded trace s =
  try trace s with Eio.Cancel.Cancelled _ as e -> raise e | _ -> ()

let describe ?(trace = ignore) ~proc ~root () =
  let trace = guarded trace in
  let dir = Eio.Path.native_exn root in
  trace "describe: running";
  let done_ r =
    (match r with
    | Ok t ->
        trace
          (let n = List.length t.components in
           Printf.sprintf "describe: %d component%s" n
             (if n = 1 then "" else "s"))
    | Error e -> trace ("describe: error " ^ first_line e));
    r
  in
  done_
  @@
  match Filename.temp_dir "okit" "describe" with
  | exception Sys_error m ->
      Error
        (Printf.sprintf
           "okit needs a build directory of its own to describe %s, and could \
            not make one: %s. Set TMPDIR to a directory it may write to."
           dir m)
  | build_dir ->
      Fun.protect
        ~finally:(fun () -> remove_tree build_dir)
        (fun () ->
          let errors = Buffer.create 256 in
          match
            (* An empty standard input rather than the caller's own, which for
               an okitd is the pipe its protocol arrives on. *)
            Eio.Process.parse_out proc Eio.Buf_read.take_all ~cwd:root
              ~stdin:(Eio.Flow.string_source "")
              ~stderr:(Eio.Flow.buffer_sink errors)
              ~env:(environment ~build_dir)
              [ "dune"; "describe"; "workspace"; "--format"; "csexp" ]
          with
          | exception (Eio.Exn.Io _ as e) ->
              Error
                (Printf.sprintf "%s could not be described: %s%s" dir
                   (Printexc.to_string e)
                   (match String.trim (Buffer.contents errors) with
                   | "" -> ""
                   | out -> "\ndune said:\n" ^ out))
          | out -> (
              match Csexp.parse_string out with
              | Error (_, m) ->
                  Error
                    (Printf.sprintf
                       "okit could not read what dune describe workspace wrote \
                        in %s: %s"
                       dir m)
              | Ok sexp -> of_sexp sexp))

let max_text = 4000
let max_externals = 60

(* The wording tools use where output is cut, so that a reader meets one form
   of it wherever it appears. *)
let note_truncated buf ~shown ~what =
  Buffer.add_string buf
    (Printf.sprintf "\n… truncated at %d %s. Narrow the query for more.\n" shown
       what)

let dir_text = function "" -> "." | d -> d

(* [only] is the one module to list, for a query that asked about a module
   rather than about the workspace. The component's own line is written either
   way, since what a module belongs to is most of the answer. *)
let block ?only c =
  let buf = Buffer.create 256 in
  Buffer.add_string buf
    (Printf.sprintf "%s %s (%s)"
       (match c.kind with `Library -> "library" | `Executable -> "executable")
       c.name (dir_text c.source_dir));
  if c.requires <> [] then
    Buffer.add_string buf (" requires " ^ String.concat " " c.requires);
  Buffer.add_char buf '\n';
  List.iter
    (fun m ->
      let files = List.filter_map Fun.id [ m.impl; m.intf ] in
      Buffer.add_string buf
        (Printf.sprintf "  %s%s\n" m.name
           (match files with [] -> "" | fs -> " " ^ String.concat " " fs)))
    (match only with None -> c.modules | Some m -> [ m ]);
  Buffer.contents buf

(* A module is named [Report] in OCaml and [report.ml] in the tree, and a
   reader coming from a file name has the second. Both reduce to the first. *)
let module_text t ~name =
  let wanted =
    String.capitalize_ascii (Filename.remove_extension (Filename.basename name))
  in
  let owner c =
    Option.map
      (fun m -> (c, m))
      (List.find_opt (fun (m : module_) -> m.name = wanted) c.modules)
  in
  match List.filter_map owner t.components with
  | [] ->
      Printf.sprintf
        "no module named %s is built in this workspace. Call project with an \
         empty module for the whole map."
        wanted
  | owners -> String.concat "" (List.map (fun (c, m) -> block ~only:m c) owners)

let to_text t =
  let locals = List.filter (fun c -> c.local) t.components in
  let externals =
    List.sort_uniq compare
      (List.filter_map
         (fun c -> if c.local then None else Some c.name)
         t.components)
  in
  let shown_externals = List.filteri (fun i _ -> i < max_externals) externals in
  let tail = Buffer.create 256 in
  if shown_externals <> [] then begin
    Buffer.add_string tail ("external: " ^ String.concat " " shown_externals);
    Buffer.add_char tail '\n';
    if List.length externals > max_externals then
      note_truncated tail ~shown:max_externals ~what:"external libraries"
  end;
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (Printf.sprintf "root: %s\n" t.root);
  (* What is installed elsewhere is the shorter half and is kept whole, so the
     components are what gives way. *)
  let budget = max_text - Buffer.length tail in
  let rec add shown = function
    | [] -> ()
    | c :: cs ->
        let text = block c in
        if Buffer.length buf + String.length text > budget then
          note_truncated buf ~shown ~what:"components"
        else begin
          Buffer.add_string buf text;
          add (shown + 1) cs
        end
  in
  add 0 locals;
  Buffer.add_buffer buf tail;
  Buffer.contents buf
