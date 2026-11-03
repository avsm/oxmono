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

let download_cache =
  "(cache (opam-archives (target /home/opam/.opam/download-cache)))"

let run ?(network = false) ?(cache = false) shell =
  Printf.sprintf "(run%s%s (shell %s))"
    (if cache then " " ^ download_cache else "")
    (if network then " (network host)" else "")
    (atom shell)

let env k v = Printf.sprintf "(env %s %s)" k (atom v)

(* A project package is pinned at [dev] from its opam file in the root. *)
let package_of_file f = Filename.chop_suffix (Filename.basename f) ".opam"

let v ~variant ~opam_repository ?oxcaml_repository ~opam_files ~packages () =
  let roots = List.map package_of_file opam_files in
  let root_versions = List.map (fun p -> p ^ ".dev") roots in
  let deps = List.filter (fun p -> not (List.mem p root_versions)) packages in
  let oxcaml =
    match (variant.Variant.oxcaml, oxcaml_repository) with
    | true, Some (url, commit) ->
        let compiler, version = variant.compiler in
        [
          run ~network:true ~cache:true
            (Printf.sprintf
               "opam repository add ox %s --dont-select && opam switch create \
                ox %s.%s --repositories=ox,default"
               (Filename.quote ("git+" ^ url ^ "#" ^ commit))
               compiler version);
          run "opam exec -- ocaml -version";
        ]
    | _ -> []
  in
  let ops =
    [
      Printf.sprintf "(comment %s)" (atom variant.name);
      "(user (uid 1000) (gid 1000))";
      env "CLICOLOR_FORCE" "1";
      env "OPAMCOLOR" "always";
      "(workdir /src)";
      run "sudo ln -f /usr/bin/opam-2.3 /usr/bin/opam";
      run ~network:true ~cache:true
        (Printf.sprintf
           "cd ~/opam-repository && (git cat-file -e %s || git fetch origin \
            master) && git reset -q --hard %s && git log --no-decorate -n1 \
            --oneline"
           opam_repository opam_repository);
      run "opam init --reinit -ni";
    ]
    @ oxcaml
    @ [
        run "uname -rs && opam exec -- ocaml -version && opam --version";
        run "sudo chown opam /src";
        run ~network:true ~cache:true "opam update -u";
        Printf.sprintf "(copy (src %s) (dst ./))"
          (String.concat " " (List.map atom opam_files));
        run ~network:true
          (String.concat " && "
             (List.map
                (fun p -> Printf.sprintf "opam pin add -yn %s.dev ./" p)
                roots));
        env "DEPS" (String.concat " " deps);
        env "CI" "true";
        env "OCAMLCI" "true";
        run ~network:true ~cache:true
          (Printf.sprintf
             "opam update --depexts && opam install --cli=2.3 --depext-only -y \
              %s $DEPS"
             (String.concat " " root_versions));
        run ~network:true ~cache:true "opam install $DEPS";
        "(copy (src .) (dst /src/))";
        run "opam exec -- dune build @install @check @runtest && rm -rf _build";
      ]
  in
  Printf.sprintf "((from %s)\n %s)\n" (atom variant.image)
    (String.concat "\n " ops)
