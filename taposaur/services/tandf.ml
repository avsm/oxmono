type t = Fetch.plain

let origin = "https://www.tandfonline.com"

let url ~doi = origin ^ "/doi/pdf/" ^ Taposaur.Doi.path doi

let client t = Fetch.read_only (Fetch.restrict ~under:[ origin ] t)
let pdf ~sw t doi = Fetch.get ~sw t (url ~doi)
