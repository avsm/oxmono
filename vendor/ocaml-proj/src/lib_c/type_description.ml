open Ctypes

module Types (F : Ctypes.TYPE) = struct
  type obj

  let pj_obj : obj structure typ = structure "PJconsts"

  type obj_list

  let pj_obj_list : obj_list structure typ = structure "PJ_OBJ_LIST"

  type context

  let pj_ctx : context structure typ = structure "pj_ctx"

  type area

  let pj_area : area structure typ = structure "PJ_AREA"

  type dll

  let pj_dll : dll structure typ = structure "PJ_DLL"

  type coord

  let pj_coord : coord union typ = union "PJ_COORD"
  let v_field = field pj_coord "v" (array 4 double)
  let () = seal pj_coord
  let v t = getf t v_field

  type direction = Forward | Inverse | Ident

  let pj_fwd = F.constant "PJ_FWD" F.int64_t
  let pj_ident = F.constant "PJ_IDENT" F.int64_t
  let pj_inv = F.constant "PJ_INV" F.int64_t

  let pj_direction =
    F.enum "PJ_DIRECTION"
      [ (Forward, pj_fwd); (Ident, pj_ident); (Inverse, pj_inv) ]
end
