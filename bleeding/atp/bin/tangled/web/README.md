# OAuth client metadata

`tangled auth login --oauth` identifies itself by the URL of
`tangled-oauth-client.json`. An authorization server fetches the file from

    https://www.cl.cam.ac.uk/~avsm2/tangled-oauth-client.json

and grants only the scopes it declares. Serve it there over HTTPS as
`application/json`. Its `redirect_uris` names the paste page published with
the ocaml-atp CLI's metadata, `oauth-login.html` in the same directory.

The file is the output of `tangled auth client-metadata`, and the test suite
fails when they differ. Republish it whenever the CLI starts writing another
record collection or calling another method with a service token. Until then
a server refuses the CLI's request, because it asks for an undeclared scope.
