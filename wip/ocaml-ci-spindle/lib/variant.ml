(* SPDX-License-Identifier: ISC *)

type arch = X86_64 | Arm64 | Ppc64le | Riscv64
type platform = Debian of arch | Windows
type os = Linux | Windows

type t = {
  name : string;
  image : string;
  pool : string;
  os : os;
  vars : Solve.platform;
  compiler : string * string;
  requires : string list;
  oxcaml : bool;
}

let arch_name = function
  | X86_64 -> "x86_64"
  | Arm64 -> "arm64"
  | Ppc64le -> "ppc64le"
  | Riscv64 -> "riscv64"

(* opam calls ppc64le [ppc64], as OCluster's pool does. *)
let opam_arch = function
  | X86_64 -> "x86_64"
  | Arm64 -> "arm64"
  | Ppc64le -> "ppc64"
  | Riscv64 -> "riscv64"

let platforms =
  [
    ("x86_64", Debian X86_64);
    ("arm64", Debian Arm64);
    ("ppc64le", Debian Ppc64le);
    ("riscv64", Debian Riscv64);
    ("windows", (Windows : platform));
  ]

let opam_version = "2.6.0"

let debian arch =
  {
    Solve.os = "linux";
    os_family = "debian";
    os_distribution = "debian";
    os_version = "13";
    arch = opam_arch arch;
    opam_version;
  }

let windows_vars =
  {
    Solve.os = "win32";
    os_family = "windows";
    os_distribution = "cygwin";
    os_version = "10.0.26100";
    arch = "x86_64";
    opam_version;
  }

let minor_of v =
  match String.split_on_char '.' v with
  | major :: minor :: _ -> Some (major ^ "." ^ minor)
  | _ -> None

let v ?(platform = Debian X86_64) version =
  let minor = Option.value ~default:version (minor_of version) in
  match platform with
  | Debian arch ->
      {
        name = Printf.sprintf "%s:%s:Debian-13" version (arch_name arch);
        image = "ocaml/opam:debian-13-ocaml-" ^ minor;
        pool = "linux-" ^ opam_arch arch;
        os = Linux;
        vars = debian arch;
        compiler = ("ocaml", version);
        requires = [];
        oxcaml = false;
      }
  | Windows ->
      {
        name = version ^ ":x86_64:Windows-mingw";
        image = "ocaml/opam:windows-server-mingw-ltsc2025-ocaml-" ^ minor;
        pool = "windows-x86_64";
        os = Windows;
        vars = windows_vars;
        compiler = ("ocaml", version);
        requires = [ "host-system-mingw" ];
        oxcaml = false;
      }

let oxcaml =
  {
    name = "5.2.0+ox:x86_64:Debian-13";
    image = "ocaml/opam:debian-13-ocaml-5.2";
    pool = "linux-x86_64";
    os = Linux;
    vars = debian X86_64;
    compiler = ("oxcaml-compiler", "5.2.0minus39");
    requires = [];
    oxcaml = true;
  }

let version t = snd t.compiler

let choose ~packages minors =
  (* Newest first, one release per minor version, from 4.14 on. *)
  let releases =
    Solve.versions packages "ocaml-base-compiler"
    |> List.fold_left
         (fun acc v ->
           match minor_of v with
           | Some m when not (List.mem_assoc m acc) -> (m, v) :: acc
           | _ -> acc)
         []
    |> List.rev
    |> List.filter (fun (m, _) ->
        match String.split_on_char '.' m with
        | [ a; b ] -> (
            match (int_of_string_opt a, int_of_string_opt b) with
            | Some a, Some b -> (a, b) >= (4, 14)
            | _ -> false)
        | _ -> false)
  in
  let wanted =
    match minors with
    | [ "auto" ] -> List.filteri (fun i (m, _) -> i < 2 || m = "4.14") releases
    | minors -> List.filter (fun (m, _) -> List.mem m minors) releases
  in
  List.map (fun (_, version) -> v version) wanted
