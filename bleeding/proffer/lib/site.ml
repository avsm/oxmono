type 'env t = {
  routes : 'env Route.t list;
  fallback : 'env Route.handler @@ portable;
}

let default_fallback _env _req = Resp.text ~status:`Not_found "Not Found\n"
let of_routes routes = { routes; fallback = default_fallback }
let with_fallback (fallback @ portable) t = { t with fallback }
