module Names = Map.Make(String)
type t = {names : string option Names.t; dois : string option Names.t}
let empty = {names=Names.empty;dois=Names.empty}
let key s = String.split_on_char ' ' (String.map (function
  | '\n' | '\r' | '\t' -> ' ' | c -> Char.lowercase_ascii c) s)
  |> List.filter ((<>) "") |> String.concat " "
let codec = Jsont.Object.map (fun journal doi source_id -> journal,doi,source_id)
  |> Jsont.Object.opt_mem "journal" Jsont.string ~enc:(fun (j,_,_) -> j)
  |> Jsont.Object.opt_mem "doi" Jsont.string ~enc:(fun (_,d,_) -> d)
  |> Jsont.Object.mem "source_id" Jsont.string ~enc:(fun (_,_,s) -> s)
  |> Jsont.Object.finish
let insert name id map =
  let value = match Names.find_opt name map with
    | None -> Some id | Some (Some old) when old = id -> Some id | _ -> None in
  Names.add name value map
let of_jsonl data =
  let rec parse n map = function
    | [] -> Ok map
    | line::rest when String.trim line = "" -> parse (n+1) map rest
    | line::rest -> match Jsont_bytesrw.decode_string codec line with
      | Error e -> Error (Printf.sprintf "Journal link line %d: %s" n e)
      | Ok (journal,doi,id) ->
          let valid = String.length id > 1 && id.[0] = 'S' &&
            String.for_all (function '0'..'9' -> true | _ -> false)
              (String.sub id 1 (String.length id-1)) in
          let doi = Option.map Taposaur.Doi.of_string doi in
          if not valid || (journal = None && doi = None) ||
             Option.fold ~none:false ~some:(fun j -> key j = "") journal ||
             Option.fold ~none:false ~some:Result.is_error doi then
            Error (Printf.sprintf "Invalid journal, DOI or source ID on line %d" n)
          else
            let names = Option.fold ~none:map.names ~some:(fun j ->
              insert (key j) id map.names) journal in
            let dois = match doi with
              | Some (Ok d) -> insert (Taposaur.Doi.to_string d) id map.dois
              | _ -> map.dois in
            parse (n+1) {names;dois} rest in
  parse 1 empty (String.split_on_char '\n' data)
let url = function Some (Some id) -> Some ("https://openalex.org/" ^ id) | _ -> None
let find map name = url (Names.find_opt (key name) map.names)
let source map doi = url (Names.find_opt (String.lowercase_ascii doi) map.dois)
let known_doi map doi = Names.mem (String.lowercase_ascii doi) map.dois
