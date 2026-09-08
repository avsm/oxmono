(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let json_printer ppf (json : Jsont.json) =
  match Jmap.Proto.Json.encode Jsont.json json with
  | Ok s -> Format.pp_print_string ppf s
  | Error e ->
      Format.fprintf ppf "<json encoding error: %s>" (Jsont.Error.to_string e)

let jsont_error_printer ppf (e : Jsont.Error.t) =
  Format.pp_print_string ppf (Jsont.Error.to_string e)

let handles_printer (type rs) ppf (hs : rs Jmap.Chain.Handles.t) =
  let rec elements : type rs.
      Format.formatter -> rs Jmap.Chain.Handles.t -> unit =
   fun ppf -> function
     | [] -> ()
     | [ h ] ->
         Format.fprintf ppf "%s %s" (Jmap.Chain.method_name h)
           (Jmap.Chain.call_id h)
     | h :: rest ->
         Format.fprintf ppf "%s %s;@ %a" (Jmap.Chain.method_name h)
           (Jmap.Chain.call_id h) elements rest
  in
  Format.fprintf ppf "@[<hov 1>[%a]@]" elements hs

let decode (type a) (codec : a Jsont.t) (json : Jsont.json) : a =
  match Jsont.Json.decode' codec json with
  | Ok v -> v
  | Error e -> invalid_arg (Jsont.Error.to_string e)

let decode_string ?max_depth (type a) (codec : a Jsont.t) (s : string) : a =
  match Jmap.Proto.Json.decode ?max_depth codec s with
  | Ok v -> v
  | Error e -> invalid_arg (Jsont.Error.to_string e)

let encode (type a) (codec : a Jsont.t) (value : a) : Jsont.json =
  match Jsont.Json.encode codec value with
  | Ok json -> json
  | Error e -> invalid_arg e

let encode_string (type a) (codec : a Jsont.t) (value : a) : string =
  match Jmap.Proto.Json.encode codec value with
  | Ok s -> s
  | Error e -> invalid_arg (Jsont.Error.to_string e)

let pp_as_json (type a) (codec : a Jsont.t) ppf (value : a) =
  Format.pp_print_string ppf (encode_string codec value)

let printers =
  [
    "Jmap.Proto.Id.pp";
    "Jmap.Proto.Keyword.pp";
    "Jmap.Proto.Email_address.pp";
    "Jmap.Proto.Error.Request_error.pp";
    "Jmap.Proto.Error.Method_error.pp";
    "Jmap.Proto.Error.Set_error.pp";
    "Jmap.Proto.Request.pp";
    "Jmap.Proto.Response.pp";
    "Jmap.Chain.pp_parse_error";
    "Jmap_top.json_printer";
    "Jmap_top.jsont_error_printer";
    "Jmap_top.handles_printer";
  ]

(* Topdirs.dir_install_printer reports a failure by writing to the formatter
   it is given rather than by raising, so the message has to be collected to
   be seen at all. *)
let install_printer name =
  match Longident.unflatten (String.split_on_char '.' name) with
  | None -> Error "not a value path"
  | Some path -> (
      let buf = Buffer.create 128 in
      let ppf = Format.formatter_of_buffer buf in
      (try Topdirs.dir_install_printer ppf path
       with e -> Format.fprintf ppf "%s" (Printexc.to_string e));
      Format.pp_print_flush ppf ();
      match String.trim (Buffer.contents buf) with
      | "" -> Ok ()
      | msg -> Error msg)

let install () =
  List.iter
    (fun name ->
      match install_printer name with
      | Ok () -> ()
      | Error msg -> Format.eprintf "Jmap_top: cannot install %s: %s@." name msg)
    printers
