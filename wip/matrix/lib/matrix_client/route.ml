module Template = Httpz_uri.Template

type t = { template : Template.t; variables : string list }

let invalid format = Printf.ksprintf invalid_arg format
let template_error error = Format.asprintf "%a" Template.pp_error error

let reserved_expansions source =
  let length = String.length source in
  let buffer = Buffer.create (length + 8) in
  for index = 0 to length - 1 do
    let character = source.[index] in
    Buffer.add_char buffer character;
    if character = '{' then Buffer.add_char buffer '+'
  done;
  Buffer.contents buffer

let v source =
  if not (String.is_valid_utf_8 source) then
    invalid "Matrix_client.Route.v: template is not valid UTF-8";
  if source = "" || source.[0] <> '/' then
    invalid "Matrix_client.Route.v: endpoint path must begin with '/'";
  if String.exists (function '?' | '#' -> true | _ -> false) source then
    invalid
      "Matrix_client.Route.v: endpoint path must not contain a query or \
       fragment delimiter";
  let parsed =
    match Template.of_string source with
    | Ok template -> template
    | Error error -> invalid "Matrix_client.Route.v: %s" (template_error error)
  in
  if Template.level parsed <> `Level_1 then
    invalid
      "Matrix_client.Route.v: placeholders must be simple {name} expressions";
  let variables = Template.variables parsed in
  let template =
    match Template.of_string (reserved_expansions source) with
    | Ok template -> template
    | Error error ->
        (* The Level-1 parse above means adding the reserved-expansion operator
           cannot fail. Keep the check explicit in case HTTPz grows syntax. *)
        invalid "Matrix_client.Route.v: %s" (template_error error)
  in
  { template; variables }

let variables route = route.variables

let expand route bindings =
  let rec prepare seen encoded = function
    | [] -> Ok (List.rev encoded)
    | (name, value) :: rest ->
        if not (List.mem name route.variables) then
          Error (Printf.sprintf "unknown route binding %S" name)
        else if List.mem name seen then
          Error (Printf.sprintf "duplicate route binding %S" name)
        else if not (String.is_valid_utf_8 value) then
          Error (Printf.sprintf "route binding %S is not valid UTF-8" name)
        else
          let value = Uriz.pct_encode ~component:`Segment value in
          prepare (name :: seen) ((name, `String value) :: encoded) rest
  in
  match prepare [] [] bindings with
  | Error _ as error -> error
  | Ok encoded -> (
      match
        List.find_opt
          (fun variable -> not (List.mem_assoc variable encoded))
          route.variables
      with
      | Some variable ->
          Error (Printf.sprintf "missing route binding %S" variable)
      | None -> (
          match Template.expand_assoc route.template encoded with
          | Ok path -> Ok path
          | Error error -> Error (template_error error)))

let expand_exn route bindings =
  match expand route bindings with
  | Ok path -> path
  | Error message -> invalid_arg ("Matrix_client.Route.expand: " ^ message)
