open Ctypes
module Types = C.Functions.T
module Funs = C.Functions

type ctx = Types.context structure ptr

let null_ctx = Ctypes.coerce (ptr void) (ptr Types.pj_ctx) null
let ctx = Funs.proj_context_create

let reset_errno t =
  let _ : int = Funs.proj_errno_reset t in
  ()

let check_and_raise_error_with_context ctx =
  (* We need to check that there was actually an error, as calling errno_string with
  errno 0 I have observed crashes in PROJ, which means we get no sensible error state in
  the OCaml side of things, just termination. *)
  match Funs.proj_context_errno ctx with
  | 0 -> failwith "This is not an error - NULL was probably allowed"
  | err ->
      let s = Funs.proj_context_errno_string ctx err in
      failwith s

let finalize_proj_objects o =
  Gc.finalise (fun obj -> ignore (Funs.proj_destroy obj)) o;
  o

module CRS = struct
  type t = Types.obj structure ptr

  let of_string ?(ctx = null_ctx) definition =
    let crs = Funs.proj_create ctx definition in
    if Ctypes.is_null crs then check_and_raise_error_with_context ctx
    else finalize_proj_objects crs

  let of_wkt ?(options = []) ?(ctx = null_ctx) wkt =
    let array = CArray.of_list string options in
    let options_ptr = CArray.start array in
    let cast_options_ptr =
      Ctypes.coerce (ptr string) (ptr (const (ptr (const char)))) options_ptr
    in
    let extras_null = Ctypes.coerce (ptr void) (ptr (ptr (ptr char))) null in
    let crs =
      Funs.proj_create_from_wkt ctx wkt cast_options_ptr extras_null extras_null
    in
    if Ctypes.is_null crs then check_and_raise_error_with_context ctx
    else finalize_proj_objects crs

  let name t =
    reset_errno t;
    match Funs.proj_get_name t with
    | None -> (
        match Funs.proj_errno t with
        | 0 -> None
        | err -> failwith (Funs.proj_errno_string err))
    | Some _ as s -> s

  let id_code t idx =
    reset_errno t;
    match Funs.proj_get_id_code t idx with
    | Some _ as s -> s
    | None -> (
        match Funs.proj_errno t with
        | 0 -> None
        | err -> failwith (Funs.proj_errno_string err))

  let id_auth_name t idx =
    reset_errno t;
    match Funs.proj_get_id_auth_name t idx with
    | Some _ as s -> s
    | None -> (
        match Funs.proj_errno t with
        | 0 -> None
        | err -> failwith (Funs.proj_errno_string err))

  let identify ?(ctx = null_ctx) t auth_name =
    let null_options = Ctypes.coerce (ptr void) (ptr (ptr char)) null in
    let confidences_ptr = allocate (ptr int) (from_voidp int null) in
    let results =
      Funs.proj_identify ctx t auth_name null_options confidences_ptr
    in
    let count = Funs.proj_list_get_count results in
    let confidences = !@confidences_ptr in
    let ocaml_res =
      List.init count (fun idx ->
          let obj = Funs.proj_list_get ctx results idx in
          let code = Option.get (id_code obj 0) in
          let confidence = !@(confidences +@ idx) in
          (code, confidence))
    in
    Funs.proj_list_destroy results;
    Funs.proj_int_list_destroy confidences;
    ocaml_res
end

module Coord = struct
  type t = Types.coord union

  let make ~x ~y ~z ~t = Funs.proj_coord x y z t
  let v coord = Types.v coord |> CArray.to_list |> Array.of_list
  let x coord = Array.get (v coord) 0
  let y coord = Array.get (v coord) 1
end

type direction = Types.direction = Forward | Inverse | Ident

module Transformation = struct
  type t = Types.obj structure ptr

  let of_string ?area ?(ctx = null_ctx) ~src tgt =
    let area =
      match area with
      | Some area -> area
      | None -> Ctypes.coerce (ptr void) (ptr Types.pj_area) null
    in
    let crs = Funs.proj_create_crs_to_crs ctx src tgt area in
    if Ctypes.is_null crs then check_and_raise_error_with_context ctx
    else finalize_proj_objects crs

  let of_crs ?area ?(options = []) ?(ctx = null_ctx) ~src tgt =
    let area =
      match area with
      | Some area -> area
      | None -> Ctypes.coerce (ptr void) (ptr Types.pj_area) null
    in
    let array = CArray.of_list string options in
    let options_ptr = CArray.start array in
    let cast_options_ptr =
      Ctypes.coerce (ptr string) (ptr (const (ptr (const char)))) options_ptr
    in
    let crs =
      Funs.proj_create_crs_to_crs_from_pj ctx src tgt area cast_options_ptr
    in
    if Ctypes.is_null crs then check_and_raise_error_with_context ctx
    else finalize_proj_objects crs

  let normalize_for_visualization ?(ctx = null_ctx) t =
    Funs.proj_normalize_for_visualization ctx t

  let transform ?(direction = Types.Forward) t c = Funs.proj_trans t direction c
end

type area = Types.area structure ptr
