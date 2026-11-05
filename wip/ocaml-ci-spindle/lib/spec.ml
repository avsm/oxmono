(* SPDX-License-Identifier: ISC *)

(* An S-expression atom, quoted when it is not a plain word. *)
let atom s =
  let plain =
    s <> ""
    && String.for_all
         (function
           | 'a' .. 'z'
           | 'A' .. 'Z'
           | '0' .. '9'
           | '-' | '_' | '.' | '/' | ':' | '@' | '~' | '+' | '=' ->
               true
           | _ -> false)
         s
  in
  if plain then s
  else
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b '"';
    String.iter
      (function
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | '\n' -> Buffer.add_string b "\\n"
        | c -> Buffer.add_char b c)
      s;
    Buffer.add_char b '"';
    Buffer.contents b

let download_cache (variant : Variant.t) =
  Printf.sprintf "(cache (opam-archives (target %s)))"
    (atom
       (match variant.os with
       | Linux -> "/home/opam/.opam/download-cache"
       | Windows -> "c:\\opam\\.opam\\download-cache"))

let run ?(network = false) ?cache shell =
  Printf.sprintf "(run%s%s (shell %s))"
    (match cache with Some c -> " " ^ c | None -> "")
    (if network then " (network host)" else "")
    (atom shell)

let env k v = Printf.sprintf "(env %s %s)" k (atom v)

(* A project package is pinned at [dev] from its opam file in the root. *)
let package_of_file f = Filename.chop_suffix (Filename.basename f) ".opam"

let v ~(variant : Variant.t) ~opam_repository ?oxcaml_repository
    ?(remotes = []) ~opam_files ~packages () =
  let cache = download_cache variant in
  let roots = List.map package_of_file opam_files in
  let root_versions = List.map (fun p -> p ^ ".dev") roots in
  let deps = List.filter (fun p -> not (List.mem p root_versions)) packages in
  let opam = "opam-" ^ String.sub Variant.(variant.vars.opam_version) 0 3 in
  (* On Windows, OBuilder copies to native paths while the shell is Cygwin's,
     where the same directory is [/home/opam/src]. *)
  let link, home, pin_dir, build =
    match variant.os with
    | Linux ->
        ( Printf.sprintf "sudo ln -f /usr/bin/%s /usr/bin/opam" opam,
          [ "(workdir /src)" ],
          "./",
          "opam exec -- dune build @install @check @runtest && rm -rf _build" )
    | Windows ->
        ( Printf.sprintf "ln -f /usr/local/bin/%s /usr/local/bin/opam" opam,
          [],
          "/cygwin64/home/opam/src/",
          "cd /home/opam/src && opam exec -- dune build @install @check \
           @runtest && rm -rf _build" )
  in
  let src = match variant.os with Linux -> "/src/" | Windows -> pin_dir in
  let oxcaml =
    match (variant.oxcaml, oxcaml_repository) with
    | true, Some (url, commit) ->
        let compiler, version = variant.compiler in
        [
          run ~network:true ~cache
            (Printf.sprintf
               "opam repository add ox %s --dont-select && opam switch create \
                ox %s.%s --repositories=ox,default"
               (Filename.quote ("git+" ^ url ^ "#" ^ commit))
               compiler version);
          run "opam exec -- ocaml -version";
        ]
    | _ -> []
  in
  (* A project's own remotes go above every other repository, as its solve
     read them, each added at the top in reverse, at the commit solved
     against. *)
  let remotes =
    List.rev
      (List.mapi
         (fun i (url, commit) ->
           run ~network:true ~cache
             (Printf.sprintf "opam repository add x%d %s --rank=1" (i + 1)
                (Filename.quote ("git+" ^ url ^ "#" ^ commit))))
         remotes)
  in
  let ops =
    [
      Printf.sprintf "(comment %s)" (atom variant.name);
      "(user (uid 1000) (gid 1000))";
      env "CLICOLOR_FORCE" "1";
      env "OPAMCOLOR" "always";
    ]
    @ home
    @ [
        run link;
        run ~network:true ~cache
          (Printf.sprintf
             "cd ~/opam-repository && (git cat-file -e %s || git fetch origin \
              master) && git reset -q --hard %s && git log --no-decorate -n1 \
              --oneline"
             opam_repository opam_repository);
        run "opam init --reinit -ni";
      ]
    @ oxcaml
    @ remotes
    @ [ run "uname -rs && opam exec -- ocaml -version && opam --version" ]
    @ (match variant.os with
      | Linux -> [ run "sudo chown opam /src" ]
      | Windows -> [])
    @ [
        run ~network:true ~cache "opam update -u";
        Printf.sprintf "(copy (src %s) (dst %s))"
          (String.concat " " (List.map atom opam_files))
          (atom pin_dir);
        run ~network:true
          (String.concat " && "
             (List.map
                (fun p ->
                  Printf.sprintf "opam pin add -yn %s.dev %s" p
                    (Filename.quote pin_dir))
                roots));
        env "DEPS" (String.concat " " deps);
        env "CI" "true";
        env "OCAMLCI" "true";
        (* opam 2.6 rejects [--cli=2.6] here, as ocaml/opam#7229 reports. *)
        run ~network:true ~cache
          (Printf.sprintf
             "opam update --depexts && opam install --depext-only -y %s $DEPS"
             (String.concat " " root_versions));
        run ~network:true ~cache "opam install $DEPS";
        Printf.sprintf "(copy (src .) (dst %s))" (atom src);
        run build;
      ]
  in
  Printf.sprintf "((from %s)\n %s)\n" (atom variant.image)
    (String.concat "\n " ops)

(* A day10 build solves and builds the project itself, against the same
   opam-repository commit and for the same platform as the OBuilder one. *)
let day10 ~(variant : Variant.t) ~opam_repository:(url, commit) ~opam_files :
    Ocluster_client.day10 =
  {
    opam_repository = url;
    opam_repository_commit = commit;
    ocaml_version = fst variant.compiler ^ "." ^ snd variant.compiler;
    with_test = true;
    dune_args = [ "@install"; "@check"; "@runtest" ];
    only_packages = List.map Filename.remove_extension opam_files;
    arch = variant.vars.arch;
    os = variant.vars.os;
    os_family = variant.vars.os_family;
    os_distribution = variant.vars.os_distribution;
    os_version = variant.vars.os_version;
  }

let day10_jsont =
  let open Jsont.Object in
  map ~kind:"day10"
    (fun
      opam_repository
      opam_repository_commit
      ocaml_version
      with_test
      dune_args
      only_packages
      arch
      os
      os_family
      os_distribution
      os_version
      :
      Ocluster_client.day10
    ->
      {
        opam_repository;
        opam_repository_commit;
        ocaml_version;
        with_test;
        dune_args;
        only_packages;
        arch;
        os;
        os_family;
        os_distribution;
        os_version;
      })
  |> mem "opamRepository" Jsont.string ~enc:(fun (d : Ocluster_client.day10) ->
      d.opam_repository)
  |> mem "opamRepositoryCommit" Jsont.string
       ~enc:(fun (d : Ocluster_client.day10) -> d.opam_repository_commit)
  |> mem "ocamlVersion" Jsont.string ~enc:(fun (d : Ocluster_client.day10) ->
      d.ocaml_version)
  |> mem "withTest" Jsont.bool ~enc:(fun (d : Ocluster_client.day10) ->
      d.with_test)
  |> mem "duneArgs" (Jsont.list Jsont.string)
       ~enc:(fun (d : Ocluster_client.day10) -> d.dune_args)
  |> mem "onlyPackages" (Jsont.list Jsont.string)
       ~enc:(fun (d : Ocluster_client.day10) -> d.only_packages)
  |> mem "arch" Jsont.string ~enc:(fun (d : Ocluster_client.day10) -> d.arch)
  |> mem "os" Jsont.string ~enc:(fun (d : Ocluster_client.day10) -> d.os)
  |> mem "osFamily" Jsont.string ~enc:(fun (d : Ocluster_client.day10) ->
      d.os_family)
  |> mem "osDistribution" Jsont.string ~enc:(fun (d : Ocluster_client.day10) ->
      d.os_distribution)
  |> mem "osVersion" Jsont.string ~enc:(fun (d : Ocluster_client.day10) ->
      d.os_version)
  |> finish
