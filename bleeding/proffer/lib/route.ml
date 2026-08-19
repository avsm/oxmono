type 'env handler = 'env -> Req.t -> Resp.t

(* A converter turns one path segment into a value. [name] appears nowhere on
   the wire and exists so a pattern can be described in a diagnostic. *)
type 'a conv = { name : string; parse : string -> 'a option @@ portable }

(* Patterns use a final encoding, so a capture becomes a curried handler
   argument instead of a tuple element. ['f] is the handler type the pattern
   demands and ['r] what is left once every capture has been applied, which
   the route constructors fix to ['env handler].

   The prefix type [frag] and the complete type [pat] are separate because
   [rest] must be last. [rest] is a [pat] and only ever appears as the right
   operand of [/*], so no pattern can continue past it and no combinator needs
   a partial case. *)

type ('f, 'r) frag =
  | Fnil : ('r, 'r) frag
  | Flit : string * ('f, 'r) frag -> ('f, 'r) frag
  | Fcap : 'a conv * ('f, 'r) frag -> ('a -> 'f, 'r) frag

type ('f, 'r) pat =
  | End : ('r, 'r) pat
  | Rest : (string list -> 'r, 'r) pat
  | Lit : string * ('f, 'r) pat -> ('f, 'r) pat
  | Cap : 'a conv * ('f, 'r) pat -> ('a -> 'f, 'r) pat

let nil : 'r. ('r, 'r) pat = End
let rest : 'r. (string list -> 'r, 'r) pat = Rest
let s name = Flit (name, Fnil)
let conv ~name (parse @ portable) = Fcap ({ name; parse }, Fnil)

let str : 'r. (string -> 'r, 'r) frag =
  Fcap ({ name = "str"; parse = (fun x -> Some x) }, Fnil)

let int : 'r. (int -> 'r, 'r) frag =
  Fcap ({ name = "int"; parse = int_of_string_opt }, Fnil)

let rec ( / ) : type f g r. (f, g) frag -> (g, r) frag -> (f, r) frag =
 fun p q ->
  match p with
  | Fnil -> q
  | Flit (l, tl) -> Flit (l, tl / q)
  | Fcap (c, tl) -> Fcap (c, tl / q)

let rec ( /? ) : type f g r. (f, g) frag -> (g, r) pat -> (f, r) pat =
 fun p q ->
  match p with
  | Fnil -> q
  | Flit (l, tl) -> Lit (l, tl /? q)
  | Fcap (c, tl) -> Cap (c, tl /? q)

let ( /* ) = ( /? )

(* The handler is read at [contended] because a route stores it in a portable
   closure, and a value captured by one is contended there. Applying a
   contended function is allowed, passing it on at the legacy mode is not. *)
let rec apply :
    type f r.
    (f, r) pat -> f @ contended -> string list -> r option @ contended =
 fun pat h segs ->
  match (pat, segs) with
  | End, [] -> Some h
  | Rest, tail -> Some (h tail)
  | Lit (l, tl), x :: xs -> if String.equal l x then apply tl h xs else None
  | Cap ({ parse; _ }, tl), x :: xs -> (
      match parse x with Some v -> apply tl (h v) xs | None -> None)
  | (End | Lit _ | Cap _), _ -> None

type 'env t = {
  meth : Method.t;
  run : string list -> 'env handler option @@ portable;
}

let route meth pat (handler @ portable) =
  { meth; run = (fun segs -> apply pat handler segs) }

let get pat handler = route `GET pat handler
let post pat handler = route `POST pat handler

(* A redirect pattern captures nothing, so ['f] is ['env handler] itself and
   the location is fixed. A capture in the location needs a plain [get]. *)

let moved pat location =
  route `GET pat (fun _env _req -> Resp.redirect ~permanent:true location)

let found pat location =
  route `GET pat (fun _env _req -> Resp.redirect location)

(* [prefix at t] is [t] with the literal segments [at] prepended to its
   pattern. A route holds a matcher rather than its pattern, so the prefix is
   stripped from the path before the matcher sees it. Used by [Site.mount]. *)
let prefix at t =
  let run segs =
    let rec strip pfx s =
      match (pfx, s) with
      | [], rest -> Some rest
      | pc :: pt, sc :: st -> if String.equal pc sc then strip pt st else None
      | _ :: _, [] -> None
    in
    match strip at segs with Some rest -> t.run rest | None -> None
  in
  { t with run }

let meth t = t.meth
let run t segs = t.run segs
