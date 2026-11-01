(* Toplevel printers for Json_pointer.t, Jsont.json, and Jsont.Error.t

   Usage in toplevel:
     #require "json-pointer.top";;

   Printers are automatically installed when the library is loaded.
*)

let pointer_printer ppf (p : Json_pointer.t) =
  Format.fprintf ppf "%S" (Json_pointer.to_string p)

let json_printer = Jsont.pp_json
let error_printer = Jsont.Error.pp

(* Automatic printer installation *)

let printers =
  [
    "Json_pointer_top.pointer_printer";
    "Json_pointer_top.json_printer";
    "Json_pointer_top.error_printer";
  ]

(* Suppress stderr during printer installation to avoid noise in MDX tests *)
let null_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

let eval_string_quiet str =
  try
    let lexbuf = Lexing.from_string str in
    let phrase = !Toploop.parse_toplevel_phrase lexbuf in
    Toploop.execute_phrase false null_formatter phrase
  with _ -> false

let rec do_install_printers = function
  | [] -> true
  | printer :: rest ->
      let cmd = Printf.sprintf "#install_printer %s;;" printer in
      eval_string_quiet cmd && do_install_printers rest

let install () =
  (* Silently ignore failures - this handles non-toplevel contexts like MDX *)
  ignore (do_install_printers printers)

(* Only auto-install when OCAML_TOPLEVEL_NAME is set, indicating a real toplevel *)
let () = if Sys.getenv_opt "OCAML_TOPLEVEL_NAME" <> None then install ()
