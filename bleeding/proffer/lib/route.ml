(* The responder is taken at [local]: it is built per request in the region
   [Backend.handle] runs the handler in, and a handler that stashed one would
   hold a closure over a connection about to be reused. *)
module M = Httpz.Method

type 'env handler =
  'env -> Req.t @ local -> Resp.respond @ local -> unit

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
   contended function is allowed, passing it on at the legacy mode is not.

   [apply] walks the path where it lies rather than over a list built for
   every request. A literal segment is compared in place and allocates
   nothing; only a capture allocates, and only what it binds. [rest] is the
   one arm that materialises a list, and only when its route has matched
   everything before it. *)
(* The result is [or_null] rather than [option]. A handler is a closure and so
   never null, and [or_null] is represented as the value itself, so a match
   costs nothing where [Some h] cost two words on every request that matched a
   route. *)
let rec apply :
    type f r.
    (f, r) pat ->
    f @ contended ->
    string ->
    int ->
    int ->
    r or_null @ contended =
 fun pat h path i n ->
  let off = Pct.seg_start path i n in
  match pat with
  | Rest -> This (h (Pct.seg_list path off n))
  | End -> if off >= n then This h else Null
  | Lit (l, tl) ->
      if off >= n then Null
      else
        let stop = Pct.seg_stop path off n in
        if Pct.seg_is path off stop l then apply tl h path stop n else Null
  | Cap ({ parse; _ }, tl) -> (
      if off >= n then Null
      else
        let stop = Pct.seg_stop path off n in
        let x = Pct.decode_sub ~plus:false path off (stop - off) in
        match parse x with
        | Some v -> apply tl (h v) path stop n
        | None -> Null)

type 'env t = {
  meth : Method.t;
  (* The matcher takes the path and the offset to start at, so [prefix] can
     hand on a suffix without cutting a string. *)
  run : string -> int -> 'env handler or_null @@ portable;
}

let route meth pat (handler @ portable) =
  { meth; run = (fun path i -> apply pat handler path i (String.length path)) }

let get pat handler = route M.Get pat handler
let post pat handler = route M.Post pat handler

(* A redirect pattern captures nothing, so ['f] is ['env handler] itself and
   the location is fixed. A capture in the location needs a plain [get]. *)

let moved pat location =
  route M.Get pat
    (fun _env (_req : Req.t @ local) (respond : Resp.respond @ local) ->
      Resp.redirect respond ~permanent:true location)

let found pat location =
  route M.Get pat
    (fun _env (_req : Req.t @ local) (respond : Resp.respond @ local) ->
      Resp.redirect respond location)

(* [prefix at t] is [t] with the literal segments [at] prepended to its
   pattern. A route holds a matcher rather than its pattern, so the prefix is
   stripped from the path before the matcher sees it: [strip] advances the
   offset past each prefix segment and hands the rest on. Used by
   [Site.mount]. *)
let prefix at t =
  let run path i =
    let n = String.length path in
    let rec strip pfx i =
      match pfx with
      | [] -> Some i
      | pc :: pt ->
          let off = Pct.seg_start path i n in
          if off >= n then None
          else
            let stop = Pct.seg_stop path off n in
            if Pct.seg_is path off stop pc then strip pt stop else None
    in
    match strip at i with Some rest -> t.run path rest | None -> Null
  in
  { t with run }

let meth t = t.meth
let run t path = t.run path 0
