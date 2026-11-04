type holdings = {
  pdf : bool; xml : bool; tei : bool; s2orc : bool; text : bool option;
}
type t = {
  uid : Uid.t;
  bibliography : Bibliography.t;
  holdings : holdings;
}

let of_index (i : Index.t) = {
  uid = i.uid;
  bibliography = Bibliography.of_index i;
  holdings = { pdf = i.pdf_path <> ""; xml = i.has_xml;
    tei = i.tei_path <> ""; s2orc = i.has_s2orc; text = None };
}

let holdings_jsont =
  Jsont.Object.map (fun pdf xml tei s2orc text ->
    { pdf; xml; tei; s2orc; text })
  |> Jsont.Object.mem "pdf" Jsont.bool ~enc:(fun h -> h.pdf)
  |> Jsont.Object.mem "xml" Jsont.bool ~enc:(fun h -> h.xml)
  |> Jsont.Object.mem "tei" Jsont.bool ~enc:(fun h -> h.tei)
  |> Jsont.Object.mem "s2orc" Jsont.bool ~enc:(fun h -> h.s2orc)
  |> Jsont.Object.opt_mem "text" Jsont.bool ~enc:(fun h -> h.text)
  |> Jsont.Object.finish

let full_text h = h.pdf || h.s2orc ||
  (match h.text with Some text -> text | None -> h.xml || h.tei)

let jsont =
  Jsont.Object.map (fun uid bibliography holdings ->
    { uid; bibliography; holdings })
  |> Jsont.Object.mem "uid" Uid.jsont ~enc:(fun s -> s.uid)
  |> Jsont.Object.mem "bibliography" Bibliography.jsont
       ~enc:(fun s -> s.bibliography)
  |> Jsont.Object.mem "holdings" holdings_jsont ~enc:(fun s -> s.holdings)
  |> Jsont.Object.finish

let of_string = Jsont_bytesrw.decode_string jsont
let to_string = Jsont_bytesrw.encode_string jsont
