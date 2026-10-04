let cmd ~proc_mgr argv =
  try
    Eio.Process.parse_out proc_mgr Eio.Buf_read.take_all argv
    |> String.trim |> Option.some
  with Eio.Exn.Io _ -> None

(* Spawn with both streams into a throwaway buffer: Nix's [which] writes
   "no NAME in PATH" to stderr on miss, and [parse_out] only captures
   stdout, so the failing probe would otherwise leak a line to the
   caller's stderr. *)
let has_cmd ~proc_mgr name =
  Eio.Switch.run @@ fun sw ->
  let buf = Buffer.create 64 in
  let sink = Eio.Flow.buffer_sink buf in
  let child =
    Eio.Process.spawn ~sw proc_mgr ~stdout:sink ~stderr:sink [ "which"; name ]
  in
  match Eio.Process.await child with
  | `Exited 0 -> true
  | `Exited _ | `Signaled _ -> false

let exists ~fs path =
  try
    ignore (Eio.Path.stat ~follow:true Eio.Path.(fs / path));
    true
  with Eio.Exn.Io _ -> false

module Arch = struct
  type t =
    [ `X86_64
    | `X86_32
    | `Arm64
    | `Arm32
    | `Ppc64
    | `Ppc32
    | `S390x
    | `Riscv64
    | `Unknown of string ]

  let of_string raw =
    match String.lowercase_ascii raw with
    | "x86_64" | "amd64" -> `X86_64
    | "x86" | "i386" | "i486" | "i586" | "i686" -> `X86_32
    | "aarch64" | "arm64" -> `Arm64
    | "ppc64" | "ppc64le" -> `Ppc64
    | "ppc" | "powerpc" | "ppcle" -> `Ppc32
    | "s390x" -> `S390x
    | "riscv64" -> `Riscv64
    | s
      when String.starts_with ~prefix:"armv" s
           || String.starts_with ~prefix:"earm" s ->
        `Arm32
    | s -> `Unknown s

  let to_string = function
    | `X86_64 -> "x86_64"
    | `X86_32 -> "x86_32"
    | `Arm64 -> "arm64"
    | `Arm32 -> "arm32"
    | `Ppc64 -> "ppc64"
    | `Ppc32 -> "ppc32"
    | `S390x -> "s390x"
    | `Riscv64 -> "riscv64"
    | `Unknown s -> s

  let pp fmt t = Fmt.string fmt (to_string t)

  let detect ~proc_mgr =
    cmd ~proc_mgr [ "uname"; "-m" ]
    |> Option.map of_string
    |> Option.value ~default:(`Unknown "unknown")
end

module OS = struct
  type linux =
    [ `Alpine
    | `Android
    | `Arch
    | `CentOS
    | `Debian
    | `Fedora
    | `Gentoo
    | `NixOS
    | `OpenSUSE
    | `RHEL
    | `Ubuntu
    | `Other of string ]

  type macos = [ `Homebrew | `MacPorts | `None ]

  type kind =
    [ `Linux of linux
    | `MacOS of macos
    | `FreeBSD
    | `OpenBSD
    | `NetBSD
    | `DragonFly
    | `Win32
    | `Cygwin
    | `Unknown of string ]

  type t = { kind : kind; version : string; family : string }

  let linux_to_string = function
    | `Alpine -> "alpine"
    | `Android -> "android"
    | `Arch -> "arch"
    | `CentOS -> "centos"
    | `Debian -> "debian"
    | `Fedora -> "fedora"
    | `Gentoo -> "gentoo"
    | `NixOS -> "nixos"
    | `OpenSUSE -> "opensuse"
    | `RHEL -> "rhel"
    | `Ubuntu -> "ubuntu"
    | `Other s -> s

  let macos_to_string = function
    | `Homebrew -> "homebrew"
    | `MacPorts -> "macports"
    | `None -> "macos"

  let kind_to_string = function
    | `Linux l -> linux_to_string l
    | `MacOS m -> macos_to_string m
    | `FreeBSD -> "freebsd"
    | `OpenBSD -> "openbsd"
    | `NetBSD -> "netbsd"
    | `DragonFly -> "dragonfly"
    | `Win32 -> "win32"
    | `Cygwin -> "cygwin"
    | `Unknown s -> s

  let os_to_string = function
    | `Linux _ -> "linux"
    | `MacOS _ -> "macos"
    | `FreeBSD -> "freebsd"
    | `OpenBSD -> "openbsd"
    | `NetBSD -> "netbsd"
    | `DragonFly -> "dragonfly"
    | `Win32 -> "win32"
    | `Cygwin -> "cygwin"
    | `Unknown s -> s

  let to_string t = os_to_string t.kind
  let pp fmt t = Fmt.string fmt (to_string t)
  let pp_kind fmt k = Fmt.string fmt (kind_to_string k)

  let detect_linux ~fs ~os_release =
    if exists ~fs "/system/build.prop" then `Android
    else
      match List.assoc_opt "ID" os_release with
      | Some s -> (
          match String.lowercase_ascii s with
          | "alpine" -> `Alpine
          | "arch" | "archlinux" -> `Arch
          | "centos" -> `CentOS
          | "debian" -> `Debian
          | "fedora" -> `Fedora
          | "gentoo" -> `Gentoo
          | "nixos" -> `NixOS
          | "rhel" | "redhat" -> `RHEL
          | "ubuntu" -> `Ubuntu
          | "android" -> `Android
          | s when String.length s >= 8 && String.sub s 0 8 = "opensuse" ->
              `OpenSUSE
          | s -> `Other s)
      | None -> `Other "linux"

  (* On Apple Silicon, Homebrew installs to [/opt/homebrew/bin/brew],
     which is NOT on the default PATH — users get it on PATH by running
     [eval "$(/opt/homebrew/bin/brew shellenv)"] in their shell init.
     Anywhere oi is launched without that init (GUI launcher, cron, IDE
     plugin, /usr/bin/env), [which brew] fails. Probe the canonical
     install paths directly first, then fall back to PATH lookup. *)
  let probe_paths ~fs paths = List.exists (exists ~fs) paths

  let detect_macos ~fs ~proc_mgr =
    let brew_paths =
      [
        "/opt/homebrew/bin/brew";
        (* Apple Silicon default *)
        "/usr/local/bin/brew";
        (* Intel default *)
      ]
    in
    let port_paths = [ "/opt/local/bin/port" ] in
    if probe_paths ~fs brew_paths || has_cmd ~proc_mgr "brew" then `Homebrew
    else if probe_paths ~fs port_paths || has_cmd ~proc_mgr "port" then
      `MacPorts
    else `None

  let detect_version ~proc_mgr ~os_release kind =
    (match kind with
      | `MacOS _ -> cmd ~proc_mgr [ "sw_vers"; "-productVersion" ]
      | `FreeBSD -> cmd ~proc_mgr [ "uname"; "-U" ]
      | `Linux _ -> List.assoc_opt "VERSION_ID" os_release
      | _ -> None)
    |> Option.value ~default:"unknown"

  let detect_family ~os_release kind =
    match kind with
    | `Linux _ ->
        List.assoc_opt "ID_LIKE" os_release
        |> Option.map (fun v ->
            String.lowercase_ascii (List.hd (String.split_on_char ' ' v)))
        |> Option.value ~default:(kind_to_string kind)
    | _ -> kind_to_string kind

  let detect ~proc_mgr ~fs ~os_release =
    let uname =
      match Sys.os_type with
      | "Win32" -> "win32"
      | _ ->
          cmd ~proc_mgr [ "uname"; "-s" ]
          |> Option.map String.lowercase_ascii
          |> Option.value ~default:"unknown"
    in
    let kind =
      match uname with
      | "linux" -> `Linux (detect_linux ~fs ~os_release)
      | "darwin" | "osx" | "macos" -> `MacOS (detect_macos ~fs ~proc_mgr)
      | "freebsd" -> `FreeBSD
      | "openbsd" -> `OpenBSD
      | "netbsd" -> `NetBSD
      | "dragonfly" -> `DragonFly
      | "win32" -> `Win32
      | s -> `Unknown s
    in
    let version = detect_version ~proc_mgr ~os_release kind in
    let family = detect_family ~os_release kind in
    { kind; version; family }
end

type t = { arch : Arch.t; os : OS.t; jobs : int }

let unquote s =
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2) else s

let parse_kv content =
  String.split_on_char '\n' content
  |> List.filter_map (fun line ->
      match String.index_opt line '=' with
      | None -> None
      | Some i ->
          let k = String.sub line 0 i in
          let v = String.sub line (i + 1) (String.length line - i - 1) in
          Some (k, unquote (String.trim v)))

let load_os_release ~fs =
  [ "/etc/os-release"; "/usr/lib/os-release" ]
  |> List.find_map (fun path ->
      try Some (Eio.Path.load Eio.Path.(fs / path)) with Eio.Exn.Io _ -> None)
  |> Option.map parse_kv |> Option.value ~default:[]

let detect ~proc_mgr ~fs =
  let arch = Arch.detect ~proc_mgr in
  let os_release = load_os_release ~fs in
  let os = OS.detect ~proc_mgr ~fs ~os_release in
  let jobs =
    cmd ~proc_mgr [ "getconf"; "_NPROCESSORS_ONLN" ]
    |> Fun.flip Option.bind int_of_string_opt
    |> Option.value ~default:4
  in
  { arch; os; jobs }

let pp fmt t =
  Fmt.pf fmt
    "@[<v>arch:         %a@,\
     os:           %a@,\
     os-version:   %s@,\
     os-family:    %s@,\
     jobs:         %d@]"
    Arch.pp t.arch OS.pp t.os t.os.version t.os.family t.jobs
