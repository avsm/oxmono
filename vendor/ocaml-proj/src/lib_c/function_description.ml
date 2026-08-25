open Ctypes

module Functions (F : Ctypes.FOREIGN) = struct
  open F
  module T = Types_generated

  (* Errors *)
  let proj_errno_reset =
    foreign "proj_errno_reset" (ptr T.pj_obj @-> returning int)

  let proj_errno = foreign "proj_errno" (ptr T.pj_obj @-> returning int)

  let proj_context_errno =
    foreign "proj_context_errno" (ptr T.pj_ctx @-> returning int)

  let proj_errno_string = foreign "proj_errno_string" (int @-> returning string)

  let proj_context_errno_string =
    foreign "proj_context_errno_string"
      (ptr T.pj_ctx @-> int @-> returning string)

  (* Context *)
  let proj_context_create =
    foreign "proj_context_create" (void @-> returning @@ ptr T.pj_ctx)

  (* Objects *)
  let proj_destroy =
    foreign "proj_destroy" (ptr T.pj_obj @-> returning @@ ptr T.pj_obj)

  let proj_list_get_count =
    foreign "proj_list_get_count" (ptr T.pj_obj_list @-> returning @@ int)

  let proj_list_get =
    foreign "proj_list_get"
      (ptr T.pj_ctx @-> ptr T.pj_obj_list @-> int @-> returning @@ ptr T.pj_obj)

  let proj_list_destroy =
    foreign "proj_list_destroy" (ptr T.pj_obj_list @-> returning @@ void)

  let proj_int_list_destroy =
    foreign "proj_int_list_destroy" (ptr int @-> returning @@ void)

  let proj_get_id_auth_name =
    foreign "proj_get_id_auth_name"
      (ptr T.pj_obj @-> int @-> returning @@ string_opt)

  let proj_get_id_code =
    foreign "proj_get_id_code" (ptr T.pj_obj @-> int @-> returning @@ string_opt)

  let proj_get_name =
    foreign "proj_get_name" (ptr T.pj_obj @-> returning @@ string_opt)

  let proj_create =
    foreign "proj_create" (ptr T.pj_ctx @-> string @-> returning @@ ptr T.pj_obj)

  let proj_create_from_wkt =
    foreign "proj_create_from_wkt"
      (ptr T.pj_ctx @-> string
      @-> ptr (const (ptr (const char)))
      @-> ptr (ptr (ptr char))
      @-> ptr (ptr (ptr char))
      @-> returning @@ ptr T.pj_obj)

  let proj_create_crs_to_crs =
    foreign "proj_create_crs_to_crs"
      (ptr T.pj_ctx @-> string @-> string @-> ptr T.pj_area @-> returning
     @@ ptr T.pj_obj)

  let proj_create_crs_to_crs_from_pj =
    foreign "proj_create_crs_to_crs_from_pj"
      (ptr T.pj_ctx @-> ptr T.pj_obj @-> ptr T.pj_obj @-> ptr T.pj_area
      @-> ptr (const (ptr (const char)))
      @-> returning @@ ptr T.pj_obj)

  let proj_identify =
    foreign "proj_identify"
      (ptr T.pj_ctx @-> ptr T.pj_obj @-> string
      @-> ptr (const (ptr (const char)))
      @-> ptr (ptr int)
      @-> returning @@ ptr T.pj_obj_list)

  let proj_coord =
    foreign "proj_coord"
      (float @-> float @-> float @-> float @-> returning T.pj_coord)

  let proj_trans =
    foreign "proj_trans"
      (ptr T.pj_obj @-> T.pj_direction @-> T.pj_coord @-> returning T.pj_coord)

  let proj_normalize_for_visualization =
    foreign "proj_normalize_for_visualization"
      (ptr T.pj_ctx @-> ptr T.pj_obj @-> returning @@ ptr T.pj_obj)
end
