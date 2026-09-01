# Building an API-specific capability

A library wrapping a web API should take *any* HTTP client and mint
exactly the capability it needs: narrowed to the API's origin, with the
credential and identification headers attached. The caller keeps the
broad client; the library (and anything it passes the derived client to)
can only reach the API, with the right headers, and cannot recover the
token.

```ocaml
# #require "fetch";;
# #require "fetch.mock";;
# #require "eio.mock";;
```

```ocaml
let () = Printexc.record_backtrace false
open Fetch

```

This is the complete capability-building pattern:

```ocaml
(* A capability for the GitHub REST API: any client in, a client that can
   only speak (authenticated) GitHub out. *)
let github ?(ua = "my-app/1.0") ~token cap =
  let gh = [ "https://api.github.com" ] in
  cap
  (* innermost: policy — gates every request, including redirect hops *)
  |> Fetch.restrict ~under:gh
  (* identification: defaults the caller may override *)
  |> Fetch.with_headers ~mode:`If_absent
       Header.[ user_agent, ua;
                accept, [ pref "application/vnd.github+json" ];
                raw "X-GitHub-Api-Version" "2022-11-28" ]
  (* outermost: the credential — scoped, and the holder cannot replace it *)
  |> Fetch.with_credentials ~scope:gh
       Fetch.Credential.[ Bearer (fun () -> token) ]
```

The following mock "GitHub" service reports what it receives:

```ocaml
let show_api (req : Middleware.request) =
  Fmt.pr "> %s %s@." (Http.Method.to_string req.meth) (Middleware.Url.to_string req.url);
  List.iter (fun name ->
      match Http.Header.get req.headers name with
      | Some v -> Fmt.pr ">   %s: %s@." name v
      | None -> ())
    [ "authorization"; "user-agent"; "accept"; "x-github-api-version" ];
  Fetch_mock.respond {|{"login":"alice"}|} req

let run fn = Eio_mock.Backend.run fn
```

## The derived client attaches everything

```ocaml
# run @@ fun () ->
  let gh = github ~token:"ghp_SECRET" (Fetch_mock.client show_api) in
  Fetch.read gh "https://api.github.com/user";;
> GET https://api.github.com/user
>   authorization: Bearer ghp_SECRET
>   user-agent: my-app/1.0
>   accept: application/vnd.github+json
>   x-github-api-version: 2022-11-28
- : string = "{\"login\":\"alice\"}"
```

## The holder can refine identification, but not the credential

`User-Agent` was attached with `` `If_absent``, so a caller-supplied one
wins; the bearer token uses `` `Set``, so a spoofed `Authorization` is
replaced:

```ocaml
# run @@ fun () ->
  let gh = github ~token:"ghp_SECRET" (Fetch_mock.client show_api) in
  Eio.Switch.run @@ fun sw ->
  let headers = Header.[ user_agent, "my-app-tests/0.1";
                         authorization, `Bearer "stolen" ] in
  status (Fetch.get ~sw ~headers gh "https://api.github.com/user");;
> GET https://api.github.com/user
>   authorization: Bearer ghp_SECRET
>   user-agent: my-app-tests/0.1
>   accept: application/vnd.github+json
>   x-github-api-version: 2022-11-28
- : int = 200
```

## Nothing outside the API is reachable

The token can never leak to another origin, because no request to
another origin ever leaves the client — including redirect hops:

```ocaml
# run @@ fun () ->
  let gh = github ~token:"ghp_SECRET" (Fetch_mock.client show_api) in
  Fetch.read gh "https://files.githubusercontent.com/archive.tgz";;
Exception:
Eio.Io Http Denied "url https://files.githubusercontent.com/archive.tgz not permitted",
  GET https://files.githubusercontent.com/archive.tgz
```

(If the API legitimately redirects downloads to another host — as GitHub
does — grant that prefix too, but scope the bearer to the API alone:
`restrict ~under:[api; downloads]` with
`with_credentials ~scope:[api]`.
The hop to the download host then carries no token, automatically.)
