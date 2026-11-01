# Service client provenance

Adapted under ISC from `~/ocaml/u/etap/taposaur/lib/services`, revision
`4230fdcba57a5a5bebb624557417a1e7d4651b31`. The source files were clean at
extraction. The package licence is in `../LICENSE.md`.

Imported modules are Elsevier, Springer, PLOS, Wiley, Taylor & Francis,
CORE, Glutton, DataCite, Cambridge University Press, GROBID and Pub2TEI.
Local changes use `Taposaur.Doi`, adapt Jsont
absent-field callbacks and duration arguments, and remove environment readers.
The executable owns environment configuration. Cambridge identifier URLs
encode the DOI suffix as one path component, and resolved article URLs are
validated before constructing the content-service request.

The downloader also adapts publisher selection, short metadata deadlines,
publisher busy retries, and CORE output fallback from `u/etap/taposaur/download.ml`
at the same revision. CORE's ID fallback requires a matching DOI; a missing
DOI is refused. The existing oxmono `wip/fetch` supplies the shared HTTP
libraries; they are not duplicated inside this package.

The acquisition tests adapt production URL and credential-scope cases and
exercise publisher fallback through Fetch's mock backend. These tests do not
establish current publisher access or network behaviour.
