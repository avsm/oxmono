open! Ppxlib
open! Stdppx

(* register attributes *)
let () =
  let rules = List.concat_map ~f:Function.attributes Function.all in
  Driver.register_transformation "array_base" ~rules
;;

(* exports *)
module Common = Common
module Function = Function
