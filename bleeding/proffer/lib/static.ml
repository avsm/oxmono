let confine segs =
  let unsafe s =
    String.equal s "" || String.equal s "." || String.equal s ".."
    || String.contains s '/'
    || String.contains s '\000'
  in
  if List.exists unsafe segs then None else Some (String.concat "/" segs)

type t = { root : string; cache : Cache_control.t option }

let v ~root ?cache () = { root; cache }
let root t = t.root
let cache t = t.cache
