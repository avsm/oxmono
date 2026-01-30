open! Core

module Insertion = struct
  type t =
    { placement_cnum : int
    ; value : string
    }
  [@@deriving sexp]

  let apply_all ~original insertions =
    let verify_insertions_range () =
      let all_insertions_in_range =
        List.for_all insertions ~f:(fun { placement_cnum; _ } ->
          0 <= placement_cnum && placement_cnum <= String.length original)
      in
      match all_insertions_in_range with
      | true -> ()
      | false ->
        raise_s
          [%message
            {|"ppx_quick_test" bug! Attempted to insert correction at at out of bounds location.|}
              (insertions : t list)
              ~original_legnth:(String.length original : int)
              (original : string)]
    in
    let get_and_verify_insertion_map insertions =
      let insertion_map =
        List.map insertions ~f:(fun { placement_cnum; value } -> placement_cnum, value)
        |> Map.of_alist_or_error (module Int)
      in
      match insertion_map with
      | Ok map -> map
      | Error error ->
        raise_s
          [%message
            {|"ppx_quick_test" bug! Attempt to insert multiple corrections at the same location.|}
              (error : Error.t)
              (insertions : t list)
              (original : string)]
    in
    verify_insertions_range ();
    let insertions_map = get_and_verify_insertion_map insertions in
    let total_output_length =
      String.length original
      + List.sum (module Int) insertions ~f:(fun t -> String.length t.value)
    in
    let buffer = Buffer.create total_output_length in
    let insert_if_found ~index =
      match Map.find insertions_map index with
      | None -> ()
      | Some str_to_insert -> Buffer.add_string buffer str_to_insert
    in
    String.iteri original ~f:(fun i c ->
      insert_if_found ~index:i;
      Buffer.add_char buffer c);
    insert_if_found ~index:(String.length original);
    Buffer.contents buffer
  ;;
end

type t =
  { mutable insertions : Insertion.t list
  ; filename_rel_to_project_root : string
  }

let global_registry = ref String.Map.empty

let create ~filename_rel_to_project_root =
  let new_ = { insertions = []; filename_rel_to_project_root } in
  global_registry
  := match Map.add !global_registry ~key:filename_rel_to_project_root ~data:new_ with
     | `Ok map -> map
     | `Duplicate ->
       raise_s
         [%message
           "ppx_quick_test bug: tried to create a registry for the same file twice"
             ~duplicate:(filename_rel_to_project_root : string)]
;;

let make_corrected_file ~filename_rel_to_project_root =
  let t =
    Map.find !global_registry filename_rel_to_project_root
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s
        [%message
          "ppx_quick_test bug: tried to create a corrected file for a file that was not \
           registered"
            ~unregistered_file:(filename_rel_to_project_root : string)])
  in
  let filename_rel_to_cwd = Stdlib.Filename.basename filename_rel_to_project_root in
  let expect_test_failures_present =
    let module Ppx_expect_runtime =
      Ppx_expect_runtime.For_quick_test
      [@alert "-ppx_expect_runtime"]
    in
    let filename_absolute = Ppx_expect_runtime.absolute_path ~filename_rel_to_cwd in
    Ppx_expect_runtime.file_has_expect_test_failures ~filename_absolute
  in
  match expect_test_failures_present, t.insertions with
  | true, _ | _, [] -> ()
  | false, insertions ->
    let prev_contents = In_channel.read_all filename_rel_to_cwd in
    let next_contents = Insertion.apply_all ~original:prev_contents insertions in
    (match Make_corrected_file.f ~path:filename_rel_to_cwd ~next_contents () with
     | Ok _ -> ()
     | Error _ ->
       let message =
         {|"ppx_quick_test" found failing tests and suggests adding the inputs to your examples for regression testing.|}
       in
       Stdio.eprintf "%s\n%!" message;
       exit 1)
;;

let add_insertion ~filename_rel_to_project_root insertion =
  match Map.find !global_registry filename_rel_to_project_root with
  | None ->
    raise_s
      [%message
        "ppx_quick_test bug: tried to add an insertion for a file that was not registered"
          ~unregistered_file:(filename_rel_to_project_root : string)]
  | Some t -> t.insertions <- insertion :: t.insertions
;;
