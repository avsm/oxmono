(* Dispatch is a linear scan over the site's routes. The type is abstract so a
   trie can replace the list without touching a backend. *)

type 'env t = {
  routes : 'env Route.t list;
  fallback : 'env Route.handler @@ portable;
}

(* The fields are read directly rather than through [Site]'s accessors: an
   accessor returns the fallback at the legacy mode, and the field here demands
   a portable one. *)
let compile (site : 'env Site.t) =
  { routes = site.Site.routes; fallback = site.Site.fallback }

let routes t = t.routes
let fallback t = t.fallback
