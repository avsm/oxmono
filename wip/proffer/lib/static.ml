(* The one lexical validator for already-decoded path segments. [Site.with_auth]
   and [Site.mount] apply it to their own segment lists so that a scope, a mount
   prefix and a confined capture agree on what a segment may hold. *)
let invalid_segment s =
  String.equal s ""
  || String.equal s "."
  || String.equal s ".."
  || String.contains s '/'
  || String.contains s '\\'
  || String.exists
       (fun c ->
          let n = Char.code c in
          n < 0x20 || n = 0x7f)
       s
;;

let confine segs =
  match segs with
  | [] -> None
  | _ -> if List.exists invalid_segment segs then None else Some (String.concat "/" segs)
;;

type t =
  { root : string
  ; cache : Cache_control.t option
  }

let v ~root ?cache () = { root; cache }
let root t = t.root
let cache t = t.cache
