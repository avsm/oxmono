type t = {
  package : string;
  version : string;
  epoch : int option;
  maintainer : string;
  homepage : string;
  license : string;
  prefix : string;
  synopsis : string;
  description : string;
  binaries : string list;
  depexts : (string * string list) list;
}

(* Per-target depexts serialise as an object [{ "ubuntu-26.04": ["a","b"], … }].
   Internally we carry an assoc list (lookup by tag, stable order) and
   convert to/from a stdlib StringMap at the jsont boundary. *)
module String_map = Jsont.String_map

let depexts_codec : (string * string list) list Jsont.t =
  let underlying = Jsont.Object.as_string_map (Jsont.list Jsont.string) in
  Jsont.map ~kind:"osdist.depexts.v1"
    ~dec:(fun m ->
      List.rev (String_map.fold (fun k v acc -> (k, v) :: acc) m []))
    ~enc:(fun pairs ->
      List.fold_left
        (fun m (k, v) -> String_map.add k v m)
        (String_map.create ()) pairs)
    underlying

let codec : t Jsont.t =
  let open Jsont in
  Object.map ~kind:"osdist.spec.v1"
    (fun
      package
      version
      epoch
      maintainer
      homepage
      license
      prefix
      synopsis
      description
      binaries
      depexts
    ->
      {
        package;
        version;
        epoch;
        maintainer;
        homepage;
        license;
        prefix;
        synopsis;
        description;
        binaries;
        depexts;
      })
  |> Object.mem "package" string ~enc:(fun r -> r.package)
  |> Object.mem "version" string ~enc:(fun r -> r.version)
  |> Object.opt_mem "epoch" int ~enc:(fun r -> r.epoch)
  |> Object.mem "maintainer" string ~enc:(fun r -> r.maintainer)
  |> Object.mem "homepage" string ~enc:(fun r -> r.homepage)
  |> Object.mem "license" string ~enc:(fun r -> r.license)
  |> Object.mem "prefix" string ~enc:(fun r -> r.prefix)
  |> Object.mem "synopsis" string ~enc:(fun r -> r.synopsis)
  |> Object.mem "description" string ~enc:(fun r -> r.description)
  |> Object.mem "binaries" (list string)
       ~dec_absent:(fun () -> [])
       ~enc:(fun r -> r.binaries)
       ~enc_omit:(fun xs -> xs = [])
  |> Object.mem "depexts" depexts_codec
       ~dec_absent:(fun () -> [])
       ~enc:(fun r -> r.depexts)
       ~enc_omit:(fun xs -> xs = [])
  |> Object.finish

let sidecar_path ~bundle_path =
  if Filename.check_suffix bundle_path ".tar.gz" then
    Filename.chop_suffix bundle_path ".tar.gz" ^ ".osdist.json"
  else bundle_path ^ ".osdist.json"

(* Atomic write: stage to [<dst>.tmp.<pid>] then rename. Same pattern as
   [Manifest_layer.write_payload] — keeps a partial write from being
   observed by a concurrent reader. *)
let write_sidecar ~path t =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec t with
  | Error e -> Fmt.failwith "osdist: encode sidecar %s: %s" path e
  | Ok s ->
      let tmp = Fmt.str "%s.tmp.%d" path (Unix.getpid ()) in
      Out_channel.with_open_bin tmp (fun oc -> Out_channel.output_string oc s);
      Sys.rename tmp path

let err_msgf fmt = Fmt.kstr (fun s -> Error s) fmt

let read_sidecar ~path =
  if not (Sys.file_exists path) then err_msgf "no such file: %s" path
  else
    try
      let s = In_channel.with_open_text path In_channel.input_all in
      match Jsont_bytesrw.decode_string ~locs:true ~file:path codec s with
      | Ok r -> Ok r
      | Error e -> Error e
    with exn -> Error (Printexc.to_string exn)

(* -- Defaults / construction -------------------------------------------- *)

let default_maintainer () =
  try
    Sys.getenv "DEBEMAIL" |> fun email ->
    let name = try Sys.getenv "DEBFULLNAME" with Not_found -> "Maintainer" in
    Fmt.str "%s <%s>" name email
  with Not_found -> "Maintainer <maintainer@example.org>"

let of_target_name ~name ~version =
  {
    package = name;
    version;
    epoch = None;
    maintainer = default_maintainer ();
    homepage = "";
    license = "ISC";
    prefix = "/usr";
    synopsis = Fmt.str "%s — packaged by osdist" name;
    description = Fmt.str "%s, built and packaged by osdist." name;
    binaries = [];
    depexts = [];
  }

let override ?package ?epoch ?maintainer ?homepage ?license ?prefix t =
  let opt v old = match v with None -> old | Some x -> x in
  {
    t with
    package = opt package t.package;
    epoch = (match epoch with None -> t.epoch | Some _ -> epoch);
    maintainer = opt maintainer t.maintainer;
    homepage = opt homepage t.homepage;
    license = opt license t.license;
    prefix = opt prefix t.prefix;
  }

(* -- Project-mode derivation from *.opam DAG ---------------------------- *)

type derive_error =
  | No_opam_files
  | Multiple_roots of string list
  | Cycle of string list

let pp_derive_error ppf = function
  | No_opam_files -> Fmt.pf ppf "no *.opam files in cwd"
  | Multiple_roots xs ->
      Fmt.pf ppf "ambiguous root package: %s (pass --pkg-name)"
        (String.concat ", " xs)
  | Cycle xs ->
      Fmt.pf ppf "dependency cycle among local opam files: %s"
        (String.concat " → " xs)

(* List <cwd>/*.opam, sorted. Skips dune-emitted .opam.template. *)
let list_opam_files cwd =
  if not (Sys.file_exists cwd) then []
  else
    Sys.readdir cwd |> Array.to_list |> List.sort String.compare
    |> List.filter_map (fun name ->
           if
             Filename.check_suffix name ".opam"
             && not (Filename.check_suffix name ".opam.template")
           then
             let base = Filename.chop_suffix name ".opam" in
             Some (base, Filename.concat cwd name)
           else None)

(* Read an OpamFile.OPAM.t from a path. *)
let read_opam path =
  let raw = In_channel.with_open_text path In_channel.input_all in
  let filename = OpamFile.make (OpamFilename.of_string path) in
  OpamFile.OPAM.read_from_string ~filename raw

(* Extract every package name that appears in the [depends:] field
   (regardless of build/test/with-doc filters — we want the topology, not
   the per-context active set). *)
let dep_names (opam : OpamFile.OPAM.t) : string list =
  let f = OpamFile.OPAM.depends opam in
  let acc = ref [] in
  let formula : (OpamPackage.Name.t * 'a) OpamFormula.formula = f in
  OpamFormula.iter
    (fun (name, _) -> acc := OpamPackage.Name.to_string name :: !acc)
    formula;
  List.sort_uniq String.compare !acc

(* Find the unique root: a local package whose name appears in no other
   local package's depends list. *)
let root_package local =
  let names = List.map fst local in
  let name_set = List.fold_left (fun s n -> n :: s) [] names in
  let depended_on =
    List.fold_left
      (fun acc (_name, opam) ->
        let here = dep_names opam in
        List.fold_left
          (fun acc d -> if List.mem d name_set then d :: acc else acc)
          acc here)
      [] local
    |> List.sort_uniq String.compare
  in
  let roots = List.filter (fun n -> not (List.mem n depended_on)) names in
  match roots with
  | [ r ] -> Ok r
  | [] -> Error (Cycle names)
  | xs -> Error (Multiple_roots xs)

(* Small helpers for the dune-project version extractor below. *)

let is_ws c = c = ' ' || c = '\t' || c = '\n'

(* [needle_at s i needle] is true iff [s] has [needle] at offset [i]
   followed by whitespace (so [(version] doesn't match inside [(versions]).
*)
let needle_at s i needle =
  let nlen = String.length needle in
  i + nlen <= String.length s
  && String.sub s i nlen = needle
  && i + nlen < String.length s
  && is_ws s.[i + nlen]

(* Skip whitespace in [s] starting at [i]; return the new offset. *)
let skip_ws s i =
  let len = String.length s in
  let j = ref i in
  while !j < len && is_ws s.[!j] do
    incr j
  done;
  !j

(* Read a single dune-style atom (optionally double-quoted) from [s]
   starting at [i]. Returns [(value, next-offset)]. *)
let read_atom s i =
  let len = String.length s in
  if i < len && s.[i] = '"' then (
    let start = i + 1 in
    let j = ref start in
    while !j < len && s.[!j] <> '"' do
      incr j
    done;
    (String.sub s start (!j - start), !j))
  else
    let j = ref i in
    while
      !j < len
      &&
      let c = s.[!j] in
      c <> ')' && not (is_ws c)
    do
      incr j
    done;
    (String.sub s i (!j - i), !j)

(* Find the value of the [(version <atom>)] stanza in [s], if any. *)
let rec find_version_stanza s i =
  if i + 9 > String.length s then None
  else if needle_at s i "(version" then
    let v, _ = read_atom s (skip_ws s (i + 8)) in
    if v = "" then None else Some v
  else find_version_stanza s (i + 1)

(* Read [(version "X")] from dune-project. Best-effort: returns [None] on
   any IO failure, leaving the opam [version:] field as fallback. *)
let dune_project_version cwd =
  let p = Filename.concat cwd "dune-project" in
  if not (Sys.file_exists p) then None
  else
    try
      let s = In_channel.with_open_text p In_channel.input_all in
      find_version_stanza s 0
    with Sys_error _ | End_of_file -> None

(* Best-effort accessor for opam string-list fields ([homepage:],
   [bug-reports:], etc.). [Invalid_argument] / [Failure] come out of the
   opam-format AST when the field is malformed; we'd rather show the
   placeholder than fail the whole bundle. *)
let string_of_opam_string_field f opam =
  try match f opam with [] -> "" | s :: _ -> s
  with Invalid_argument _ | Failure _ -> ""

let synopsis_of opam =
  match OpamFile.OPAM.synopsis opam with Some s -> s | None -> ""

let description_of opam =
  match OpamFile.OPAM.descr_body opam with Some s -> s | None -> ""

let homepage_of opam = string_of_opam_string_field OpamFile.OPAM.homepage opam

let license_of opam =
  match OpamFile.OPAM.license opam with
  | [] -> "ISC"
  | ls -> String.concat " AND " ls

let maintainer_of opam =
  match OpamFile.OPAM.maintainer opam with
  | [] -> default_maintainer ()
  | m :: _ -> m

let opam_version_of opam =
  try Some (OpamPackage.Version.to_string (OpamFile.OPAM.version opam))
  with Invalid_argument _ | Failure _ -> None

(* Build a fresh {!t} from a parsed opam file. All fields are populated
   from the opam metadata where present; missing fields fall back to the
   same defaults as {!of_target_name}. *)
let of_opam ~name ~version ~opam =
  {
    package = name;
    version;
    epoch = None;
    maintainer = maintainer_of opam;
    homepage = homepage_of opam;
    license = license_of opam;
    prefix = "/usr";
    synopsis = synopsis_of opam;
    description =
      (let s = description_of opam in
       if s = "" then synopsis_of opam else s);
    binaries = [];
    depexts = [];
  }

let of_opam_file ~name ~version ~path =
  try Ok (of_opam ~name ~version ~opam:(read_opam path))
  with exn -> Error (Printexc.to_string exn)

let pp ppf t = Fmt.pf ppf "%s %s" t.package t.version

(* Pick a version for the project, in order of preference: dune-project's
   [(version)] stanza, then the opam [version:] field, then ["0.0.0"]. *)
let project_version cwd opam =
  match dune_project_version cwd with
  | Some v -> v
  | None -> ( match opam_version_of opam with Some v -> v | None -> "0.0.0")

let of_local_project ~cwd =
  match list_opam_files cwd with
  | [] -> Error No_opam_files
  | files ->
      let local = List.map (fun (n, p) -> (n, read_opam p)) files in
      Result.bind (root_package local) (fun name ->
          let opam = List.assoc name local in
          Ok (of_opam ~name ~version:(project_version cwd opam) ~opam))
