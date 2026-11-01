open Taposaur
type holdings = Any | Pdf | Xml | Tei | S2orc | Unconverted
type metadata_field = Title | Abstract | Publisher
type availability = Present | Missing
type t = {
  text : string; scheme : Uid.Scheme.t option; publisher : string;
  journal : string; year_from : int option; year_to : int option;
  holdings : holdings; metadata : (metadata_field * availability) list;
  after : string option; limit : int;
}
let make ?(text = "") () =
  { text; scheme = None; publisher = ""; journal = "";
    year_from = None; year_to = None; holdings = Any; metadata = [];
    after = None; limit = 25 }
