open! Stdppx
module Error = Astlib.Location.Error

type t = Error.t

let createf ~loc fmt =
  Format.kasprintf
    (fun txt -> Error.make { loc = { loc with loc_ghost = true }; txt } ~sub:[])
    fmt
;;

let combine t list =
  Error.make
    (Error.main_msg t)
    ~sub:
      (Error.sub_msgs t
       @ List.concat_map list ~f:(fun err -> Error.main_msg err :: Error.sub_msgs err))
;;

let to_extension = Ppxlib_ast.Location_error.to_extension
