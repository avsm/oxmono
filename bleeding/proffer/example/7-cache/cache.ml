open Proffer
open Proffer.Route

type env = { clock : float Eio.Time.clock_ty Eio.Resource.t; cache : Cache.t }

let logo = "<!doctype html>\n<h1>The logo never changes</h1>\n"
let logo_etag = Etag.strong (Digest.to_hex (Digest.string logo))

let expensive_report env () =
  Eio.Time.sleep env.clock 2.0;
  Printf.sprintf "Report generated at %.0f\n" (Eio.Time.now env.clock)

let site =
  Site.of_routes
    [ get (s "logo") (fun _env _request respond ->
        Resp.html respond ~etag:logo_etag
          ~cache:(Cache_control.public ~max_age:(`Days 365) ~immutable:true ())
          logo);

      get (s "report") (fun env _request respond ->
        let body, etag =
          Cache.memoize env.cache ~now:(Eio.Time.now env.clock) ~key:"report"
            (expensive_report env)
        in
        Resp.media respond ~etag "text/plain" body);

      get (s "clock") (fun env _request respond ->
        Resp.media respond ~cache:Cache_control.no_store "text/plain"
          (Printf.sprintf "%.0f\n" (Eio.Time.now env.clock))) ]

let () =
  Eio_main.run @@ fun stdenv ->
  let clock = Eio.Stdenv.clock stdenv in
  Proffer_httpz.run stdenv ~env:{ clock; cache = Cache.create ~ttl:10.0 () } site
