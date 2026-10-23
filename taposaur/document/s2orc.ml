open Taposaur

let jsont =
  Jsont.Object.map (fun title authors abstract ->
    { Bibliography.empty with title; abstract;
      authors = Option.value ~default:[] authors
        |> List.filter_map (fun name ->
          let family = String.trim name in
          if family = "" then None else
            Some { Author.family; given = None }) })
  |> Jsont.Object.opt_mem "title" Jsont.string
       ~enc:(fun (h : Bibliography.t) -> h.Bibliography.title)
  |> Jsont.Object.opt_mem "authors" (Jsont.list Jsont.string)
       ~enc:(fun (h : Bibliography.t) ->
         Some (List.map (fun a -> a.Author.family) h.authors))
  |> Jsont.Object.opt_mem "abstract" Jsont.string
       ~enc:(fun (h : Bibliography.t) -> h.Bibliography.abstract)
  |> Jsont.Object.finish

let metadata ?(max_bytes = 16 * 1024 * 1024) flow =
  if max_bytes <= 0 || max_bytes = max_int then
    invalid_arg "max_bytes must be positive and less than max_int";
  match Eio.Buf_read.parse ~max_size:(max_bytes + 1)
    Eio.Buf_read.take_all flow with
  | Error (`Msg message) -> Error message
  | Ok s -> Jsont_bytesrw.decode_string jsont s
