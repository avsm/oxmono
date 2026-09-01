module M = Httpz.Method

(* The closure left after [env] is local, so a handler that received a
   captured segment, which lives in the request's region, is a handler too. *)
type 'env handler =
  'env -> (Req.t @ local -> Resp.respond @ local -> unit) @ local

(* A converter reads the decoded segment at [local]. [conv] wraps one an
   application supplies, which takes a heap copy and answers with an option. *)
type 'a conv = { name : string; parse : string @ local -> 'a or_null @@ portable }

type 'a conv_local =
  { lname : string; lparse : string @ local -> 'a or_null @ local @@ portable }

(* Paths use a final encoding, so a capture becomes a curried handler
   argument instead of a tuple element. ['f] is the handler type the path
   demands and ['r] what is left once every capture has been applied, which
   the route constructors fix to ['env handler].

   The third index says whether the path can still be extended. [Root] and
   everything built from it is [open_], and [Rest] is [closed]. Since [( / )]
   demands an open left operand, [rest] can only ever come last, and the
   exhaustiveness checker knows [( / )] never meets [Rest] on its left.

   [Cap_local] hands the handler a segment built in the request's region, so
   its arrow takes the capture at [local]. *)

type open_ = Open_
type closed = Closed_

type ('f, 'r, 'k) path =
  | Root : ('r, 'r, open_) path
  | Rest : (string list @ local -> 'r @ local, 'r, closed) path
  | Lit : string * ('f, 'r, 'k) path -> ('f, 'r, 'k) path
  | Cap : 'a conv * ('f, 'r, 'k) path -> ('a -> 'f @ local, 'r, 'k) path
  | Cap_local :
      'a conv_local * ('f, 'r, 'k) path -> ('a @ local -> 'f @ local, 'r, 'k) path

let root : 'r. ('r, 'r, open_) path = Root
let rest : 'r. (string list @ local -> 'r @ local, 'r, closed) path = Rest
let s name = Lit (name, Root)

let conv ~name (parse @ portable) =
  let parse (s : string @ local) =
    match parse (Pct.copy_all s) with Some v -> This v | None -> Null
  in
  Cap ({ name; parse }, Root)

let str : 'r. (string @ local -> 'r @ local, 'r, open_) path =
  Cap_local
    ({ lname = "str"; lparse = (fun (x : string @ local) -> exclave_ This x) },
     Root)

(* [int_of_string] takes OCaml literal syntax, so it would accept [0x1f],
   [1_000] and [+3] as path segments and give two spellings of one resource.
   A segment is plain decimal, optionally signed. The value is accumulated
   negative so that [min_int] is reachable and overflow is a bound check. *)
let[@zero_alloc] rec digits_neg (s : string @ local) i n acc =
  if i >= n then This (-acc)
  else
    match String.unsafe_get s i with
    | '0' .. '9' as c ->
        let d = Char.code c - Char.code '0' in
        if acc < (min_int + d) / 10 then Null
        else digits_neg s (i + 1) n ((acc * 10) - d)
    | _ -> Null

let[@zero_alloc] parse_int (s : string @ local) =
  let n = String.length s in
  let first = if n > 0 && Char.equal (String.unsafe_get s 0) '-' then 1 else 0 in
  if n = first then Null
  else if
    (n - first > 1 && Char.equal (String.unsafe_get s first) '0')
    || (first = 1 && n = 2 && Char.equal (String.unsafe_get s 1) '0')
  then Null
  else
    match digits_neg s first n 0 with
    | Null -> Null
    | This v ->
        (* [-v] is [min_int] only when [v] is, and negating that is itself. *)
        if first = 1 then (if v = min_int then Null else This (-v)) else This v

let int : 'r. (int -> 'r @ local, 'r, open_) path =
  Cap ({ name = "int"; parse = parse_int }, Root)

let rec ( / ) :
    type f g r k. (f, g, open_) path -> (g, r, k) path -> (f, r, k) path =
 fun p q ->
  match p with
  | Root -> q
  | Lit (l, tl) -> Lit (l, tl / q)
  | Cap (c, tl) -> Cap (c, tl / q)
  | Cap_local (c, tl) -> Cap_local (c, tl / q)

(* Match directly against the encoded path and decode only captured
   segments.

   The handler is read at [contended] because a route stores it in a portable
   closure, and a value captured by one is contended there. Applying a
   contended function is allowed, passing it on at the legacy mode is not.

   The result is [or_null] rather than [option]. A handler is a closure and so
   never null, and [or_null] is represented as the value itself, so a match
   costs nothing where [Some h] cost two words on every request that
   matched a route. Once a capture has been applied the handler is a closure
   over a local segment, so the result is local. *)
let rec apply :
    type f r k.
    (f, r, k) path ->
    f @ local contended ->
    string @ local ->
    int ->
    int ->
    r or_null @ local contended =
 fun pat h path i n -> exclave_
  let off = Pct.seg_start path i n in
  match pat with
  | Rest -> This (h (Pct.seg_list_local path off n))
  | Root -> if off >= n then This h else Null
  | Lit (l, tl) ->
      if off >= n then Null
      else
        let stop = Pct.seg_stop path off n in
        if Pct.seg_is path off stop l then apply tl h path stop n else Null
  | Cap ({ parse; _ }, tl) -> (
      if off >= n then Null
      else
        let stop = Pct.seg_stop path off n in
        let local_ x = Pct.decode_local ~plus:false path off (stop - off) in
        match parse x with
        | This v -> apply tl (h v) path stop n
        | Null -> Null)
  | Cap_local ({ lparse; _ }, tl) -> (
      if off >= n then Null
      else
        let stop = Pct.seg_stop path off n in
        let local_ x = Pct.decode_local ~plus:false path off (stop - off) in
        match lparse x with
        | This v -> apply tl (h v) path stop n
        | Null -> Null)

type 'env t = {
  meth : Method.t;
  (* The matcher takes the path and the offset to start at, so [prefix] can
     hand on a suffix without cutting a string. *)
  run : string @ local -> int -> 'env handler or_null @ local @@ portable;
}

let route meth pat (handler @ portable) =
  { meth;
    run =
      (fun (path : string @ local) i -> exclave_
        apply pat handler path i (String.length path)) }

let get pat handler = route M.Get pat handler
let post pat handler = route M.Post pat handler

let moved pat location =
  route M.Get pat
    ((fun _env (_req : Req.t @ local) (respond : Resp.respond @ local) ->
       Resp.redirect respond ~permanent:true location)
     : _ handler)

let found pat location =
  route M.Get pat
    ((fun _env (_req : Req.t @ local) (respond : Resp.respond @ local) ->
       Resp.redirect respond location)
     : _ handler)

(* A mounted route strips its prefix before running its existing matcher. *)
let prefix at t =
  let run (path : string @ local) i = exclave_
    let n = String.length path in
    let rec strip pfx i =
      match pfx with
      | [] -> i
      | pc :: pt ->
          let off = Pct.seg_start path i n in
          if off >= n then -1
          else
            let stop = Pct.seg_stop path off n in
            if Pct.seg_is path off stop pc then strip pt stop else -1
    in
    let rest = strip at i in
    if rest < 0 then Null else t.run path rest
  in
  { t with run }

let meth t = t.meth
let run t (path : string @ local) = exclave_ t.run path 0

(* The environment callback supports dynamic codec selection. Since Media.t is
   portable, it may also ignore [env] and return a captured module-level codec. *)
let with_body :
    type env a.
    (env -> a Httpz.Media.t) @ portable ->
    (a -> env handler) @ portable ->
    env handler @ portable =
 fun codec_of_env f env (req : Req.t @ local)
     (respond : Resp.respond @ local) ->
  match Req.decode (codec_of_env env) req with
  | Ok x -> f x env req respond
  | Error (Httpz.Media.Unsupported _) ->
      Resp.text respond ~status:Httpz.Res.Unsupported_media_type
        "Unsupported Media Type\n"
  | Error (Httpz.Media.Malformed { message; _ }) ->
      Resp.text respond ~status:Httpz.Res.Bad_request
        ("Bad Request: " ^ message ^ "\n")
  | Error (Httpz.Media.Too_large _) ->
      Resp.text respond ~status:Httpz.Res.Payload_too_large
        "Payload Too Large\n"
