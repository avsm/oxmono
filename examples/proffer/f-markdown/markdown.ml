open Proffer
open Proffer.Route

let page =
  Cmarkit.Doc.of_string
    {|# Good morning, world!

This page is written in *Markdown*.

- A browser is shown it as HTML.
- A client that asks for `text/markdown` gets the source.
|}

type env = {
  page : Cmarkit.Doc.t;
  representations : Cmarkit.Doc.t Media.t list;
}

let site =
  Site.of_routes
    [ get root (fun env request respond ->
        Negotiate.encode respond request env.representations env.page) ]

let () =
  Eio_main.run @@ fun stdenv ->
  let env =
    { page;
      representations =
        [ Markdown.html (); Markdown.markdown () ] }
  in
  Proffer_httpz.run stdenv ~env site
