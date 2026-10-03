let ( / ) = Filename.concat
let fail fmt = Printf.ksprintf failwith fmt
let read p = In_channel.with_open_bin p In_channel.input_all
let exists p = Sys.file_exists p

let rec mkdir p =
  if not (exists p) then (
    mkdir (Filename.dirname p);
    try Unix.mkdir p 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write p s =
  mkdir (Filename.dirname p);
  Out_channel.with_open_bin p (fun oc -> Out_channel.output_string oc s)

let atomic_write p s =
  let tmp = p ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
  write tmp s;
  Unix.rename tmp p

let lines s = String.split_on_char '\n' s |> List.filter (( <> ) "")
let nul_lines s = String.split_on_char '\000' s |> List.filter (( <> ) "")
let sorted_dir p = Sys.readdir p |> Array.to_list |> List.sort String.compare
let hash s = OpamHash.compute_from_string ~kind:`SHA256 s |> OpamHash.contents
let hash_file p = OpamHash.compute ~kind:`SHA256 p |> OpamHash.contents

let hash_fields xs =
  xs
  |> List.map (fun s -> string_of_int (String.length s) ^ ":" ^ s)
  |> String.concat "" |> hash

let getenv name fallback =
  match Sys.getenv_opt name with Some s when s <> "" -> s | _ -> fallback

let home () = getenv "HOME" (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir
let data_dir () = getenv "XDG_DATA_HOME" (home () / ".local/share") / "ox"
let cache_dir () = getenv "XDG_CACHE_HOME" (home () / ".cache") / "ox"
let opam_file p = OpamFile.make (OpamFilename.raw p)
let read_opam p = OpamFile.OPAM.read (opam_file p)
let write_opam p opam = write p (OpamFile.OPAM.write_to_string opam)

let env_bindings env =
  Array.to_list env |> List.filter_map (fun s -> OpamStd.String.cut_at s '=')

let replace_env env overrides =
  let key s =
    match String.index_opt s '=' with Some i -> String.sub s 0 i | None -> s
  in
  let names = List.map fst overrides in
  Array.to_list env |> List.filter (fun s -> not (List.mem (key s) names))
  |> fun rest ->
  Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) overrides @ rest)

let clean_env () =
  Unix.environment () |> Array.to_list
  |> List.filter (fun s ->
         not
           (List.exists
              (fun p -> String.starts_with ~prefix:p s)
              [
                "OPAM";
                "GIT_DIR=";
                "GIT_WORK_TREE=";
                "GIT_INDEX_FILE=";
                "GIT_COMMON_DIR=";
                "GIT_OBJECT_DIRECTORY=";
                "OCAMLPATH=";
                "OCAMLLIB=";
                "CAML_LD_LIBRARY_PATH=";
                "DUNE_CONFIGURATOR=";
                "DUNE_WORKSPACE=";
                "INSIDE_DUNE=";
                "OCAMLFIND_CONF=";
                "OCAMLFIND_DESTDIR=";
                "OCAMLFIND_LDCONF=";
              ]))
  |> Array.of_list
  |> fun env -> replace_env env [ ("GIT_TERMINAL_PROMPT", "0") ]

type proc = Eio_unix.Process.mgr_ty Eio.Resource.t

let capture ?(env = clean_env ()) proc args =
  Eio.Process.parse_out ~env proc Eio.Buf_read.take_all args

let command ?(env = clean_env ()) proc args =
  Eio.Process.run ~env proc
    ([ "/bin/sh"; "-c"; "exec \"$@\" 1>&2"; "ox-command" ] @ args)

let git proc repo args =
  capture proc ([ "git"; "-C"; repo ] @ args) |> String.trim

let log fmt = Printf.ksprintf (fun s -> Printf.eprintf "ox: %s\n%!" s) fmt

(* Hash metadata and auxiliary files, without following repository symlinks. *)
let tree_hash root =
  let rec walk rel =
    let path = root / rel in
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        sorted_dir path
        |> List.filter (( <> ) ".git")
        |> List.concat_map (fun name ->
               walk (if rel = "" then name else rel / name))
    | Unix.S_REG ->
        [ rel; string_of_int (Unix.stat path).Unix.st_perm; hash_file path ]
    | Unix.S_LNK ->
        if Sys.is_directory path then
          fail "Directory symlink is unsupported: %s" path;
        [ rel; Unix.readlink path; hash_file path ]
    | _ -> fail "Unsupported repository entry: %s" path
  in
  hash_fields (walk "")

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | Unix.S_DIR ->
      List.iter (fun n -> remove_tree (path / n)) (sorted_dir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let publish_dir path build =
  let tmp = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
  mkdir (Filename.dirname path);
  remove_tree tmp;
  Fun.protect
    ~finally:(fun () -> remove_tree tmp)
    (fun () ->
      build tmp;
      Unix.rename tmp path)

let git_url source =
  if String.starts_with ~prefix:"git+" source then
    String.sub source 4 (String.length source - 4)
  else source

let refresh_checkout proc path =
  if git proc path [ "status"; "--porcelain" ] <> "" then
    fail "Cached checkout has local changes: %s" path;
  command proc [ "git"; "-C"; path; "fetch"; "--prune"; "origin" ];
  command proc [ "git"; "-C"; path; "reset"; "--hard"; "origin/HEAD" ]

let rec copy_files ~src ~dst =
  match (Unix.lstat src).Unix.st_kind with
  | Unix.S_DIR ->
      mkdir dst;
      List.iter
        (fun name -> copy_files ~src:(src / name) ~dst:(dst / name))
        (sorted_dir src)
  | Unix.S_REG ->
      write dst (read src);
      Unix.chmod dst ((Unix.stat src).Unix.st_perm land 0o777)
  | _ -> fail "Unsupported metadata entry: %s" src
