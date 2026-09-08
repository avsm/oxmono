module Direction = struct
  type t = Forward | Backward

  let to_string = function Forward -> "f" | Backward -> "b"

  let of_string = function
    | "f" -> Ok Forward
    | "b" -> Ok Backward
    | s -> Error (`Msg (Printf.sprintf "unknown direction %S" s))

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)
  let jsont = Jsont.enum [ ("f", Forward); ("b", Backward) ]
end

module Visibility = struct
  type t = Public | Private

  let to_string = function Public -> "public" | Private -> "private"

  let of_string = function
    | "public" -> Ok Public
    | "private" -> Ok Private
    | s -> Error (`Msg (Printf.sprintf "unknown visibility %S" s))

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)
  let jsont = Jsont.enum [ ("public", Public); ("private", Private) ]
end

module Page = struct
  type 'a t = {
    chunk : 'a list;
    next_batch : string option;
    prev_batch : string option;
  }

  let v ?next_batch ?prev_batch chunk = { chunk; next_batch; prev_batch }

  let jsont ?(chunk = "chunk") ?(next_batch = "next_batch")
      ?(prev_batch = "prev_batch") item =
    Jsont.Object.(
      map (fun chunk next_batch prev_batch -> { chunk; next_batch; prev_batch })
      |> mem chunk (Jsont.list item)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.chunk)
      |> opt_mem next_batch Matrix_json.Codec.string ~enc:(fun t ->
          t.next_batch)
      |> opt_mem prev_batch Matrix_json.Codec.string ~enc:(fun t ->
          t.prev_batch)
      |> finish)
end
