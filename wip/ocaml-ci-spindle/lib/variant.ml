(* SPDX-License-Identifier: ISC *)

type t = {
  name : string;
  image : string;
  compiler : string * string;
  oxcaml : bool;
}

let ocaml minor version =
  {
    name = "debian-12-ocaml-" ^ minor;
    image = "ocaml/opam:debian-12-ocaml-" ^ minor;
    compiler = ("ocaml", version);
    oxcaml = false;
  }

let oxcaml =
  {
    name = "debian-12-oxcaml-5.2minus39";
    image = "ocaml/opam:debian-12-ocaml-5.2";
    compiler = ("oxcaml-compiler", "5.2.0minus39");
    oxcaml = true;
  }

let minor_of v =
  match String.split_on_char '.' v with
  | major :: minor :: _ -> Some (major ^ "." ^ minor)
  | _ -> None

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
        | [ a; b ] -> (int_of_string a, int_of_string b) >= (4, 14)
        | _ -> false)
  in
  let wanted =
    match minors with
    | [ "auto" ] ->
        let newest = List.filteri (fun i _ -> i < 2) releases in
        let lts = List.filter (fun (m, _) -> m = "4.14") releases in
        List.sort_uniq compare (lts @ newest)
    | minors -> List.filter (fun (m, _) -> List.mem m minors) releases
  in
  List.map (fun (m, v) -> ocaml m v) wanted
