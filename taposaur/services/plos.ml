type t = Fetch.plain

let origin = "https://journals.plos.org"

(* One path serves every PLOS journal, whichever of them the DOI
   belongs to, so the slug in it is a spelling rather than a lookup. *)
let base = origin ^ "/plosone/article/file"

let url ~doi ~kind =
  Uri.to_string
    (Uri.with_query'
       (Uri.of_string base)
       [ ("id", Taposaur.Doi.to_string doi); ("type", kind) ])

let xml_url ~doi = url ~doi ~kind:"manuscript"
let pdf_url ~doi = url ~doi ~kind:"printable"

(* Not restricted to the origin: the site answers a file request with a
   redirect to the signed storage URL the bytes are actually at, and
   that is another origin. *)
let client t = Fetch.read_only t

let article ~sw t doi = Fetch.get ~sw t (xml_url ~doi)
let pdf ~sw t doi = Fetch.get ~sw t (pdf_url ~doi)
