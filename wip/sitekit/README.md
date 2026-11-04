# Sitekit

Server-rendered web pages for Proffer sites, safe by construction. It grew
out of Taposaur's interface and is shared by the sites in this monorepo.

```ocaml
let site =
  Sitekit.site ~name:"Spindle" ~brand:"sp"
    ~nav:[ Sitekit.nav "Pipelines" (Sitekit.Url.path [ "pipelines" ]) ]
    ()

let routes =
  Proffer.Route.get Proffer.Route.(s "pipelines") (fun page _ respond ->
      Sitekit.respond_html respond page)
  :: Sitekit.Asset.routes []
```

## Security

- Every component is built with Htmlit, which escapes text and attribute
  values. No Sitekit function takes raw HTML or an attribute name, so a
  caller cannot add an event handler, a style or a script.
- Links and form targets take a `Sitekit.Url.t`. `Url.path` encodes each
  segment and query value. `Url.of_path` accepts only printable ASCII paths
  starting with one `/`, so `//host` and `/\host` cannot leave the site.
  `Url.external_` accepts only `http:` and `https:` addresses, so
  `javascript:` and `data:` links cannot be built.
- Pages carry no inline style or script. Sitekit's stylesheet and script, and
  any a site adds with `Asset.css` and `Asset.js`, are served at
  `/_sitekit/NAME.HASH.EXT`, where `HASH` is part of the SHA-256 of the
  content, and cached as immutable. Even the coverage bar is an SVG sized by
  attributes.
- `Headers.html` is the Content Security Policy that this allows:
  `default-src 'none'`, scripts, styles, images and connections only from the
  site, forms only to it, `base-uri 'none'` and `frame-ancestors 'none'`. It
  refuses `'unsafe-inline'` and `'unsafe-eval'` in the sources a site adds.
  With it come `nosniff`, `Referrer-Policy: same-origin`,
  `Cross-Origin-Opener-Policy: same-origin` and `Cache-Control: no-store`,
  since pages may show private data.
- `account` and `post_button` render forms that post a hidden field, such
  as a CSRF token, that the caller supplies and checks.

## Components

The page shell has a skip link, a header with the site's brand, navigation
marked `aria-current` on the server, an account area and a footer. The
components are badges, metrics, sortable tables, coverage bars, state icons
for jobs, description lists, next-page links, labelled form fields, log
blocks and screen-reader text, with formatters for counts, sizes, times and
durations.

Tables sort in the browser. The script reorders the rendered rows only, and
announces each sort. A site that adds content after loading dispatches a
`sitekit:content` event on the document to enhance it.

The stylesheet uses custom properties, prefixed `--sk-`, with a light and a
dark theme chosen by `prefers-color-scheme`. A site whose own styles are
light only passes `~theme:`Light`. Motion stops under
`prefers-reduced-motion`.
