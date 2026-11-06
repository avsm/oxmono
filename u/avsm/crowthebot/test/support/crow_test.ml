let check name ok = if not ok then failwith name

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let json text = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json text)
