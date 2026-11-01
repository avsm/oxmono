(* Self-hosted service roots are validated when capabilities are created. *)
let root s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

(* Parsed where it is written, so a mistyped address fails at mint time
   rather than on the first request. A query names more than a root, so
   refusing it is better than accepting an address that would quietly
   mean less than it says. *)
let server ~caller s =
  let s = root s in
  (match Fetch.Middleware.Url.of_string s with
   | Error msg ->
     invalid_arg
       (Printf.sprintf "%s: server %S is not a URL: %s" caller s msg)
   | Ok u ->
     if Fetch.Middleware.Url.has_query u then
       invalid_arg
         (Printf.sprintf
            "%s: server %S has a query, which names more than an origin \
             and a path" caller s));
  s

(* A capability holds one client whatever row the caller's wore, and
   every wrapper returns [plain] already, so this only re-wraps the one
   client that has been through none. *)
let plain t = Fetch.Middleware.of_handler (Fetch.Middleware.handler t)
