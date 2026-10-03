let read p = In_channel.with_open_bin ("fixtures/" ^ p) In_channel.input_all
let list codec = Jsont.list codec
let dec = Openapi.Runtime.Json.decode

let failures = ref []

let ok name = function
  | Ok _ -> ()
  | Error e -> failures := (name ^ ": " ^ e) :: !failures

let () =
  ok "registries"
    (dec (list Ecosystems.Registry.T.jsont) (read "registries.json"));
  ok "package" (dec Ecosystems.Package.T.jsont (read "package.json"));
  ok "versions" (dec (list Ecosystems.Version.T.jsont) (read "versions.json"));
  ok "version"
    (dec Ecosystems.VersionWithDependencies.T.jsont (read "version.json"));
  ok "maintainers"
    (dec (list Ecosystems.Maintainer.T.jsont) (read "maintainers.json"));
  ok "namespaces"
    (dec (list Ecosystems.Namespace.T.jsont) (read "namespaces.json"));
  ok "keywords" (dec (list Ecosystems.Keyword.T.jsont) (read "keywords.json"));
  ok "keyword"
    (dec Ecosystems.KeywordWithPackages.T.jsont (read "keyword.json"));
  ok "opam package"
    (dec Ecosystems.Package.T.jsont (read "opam_package.json"));
  ok "advisories" (dec Ecosystems.Package.T.jsont (read "advisories.json"));
  ok "lookup"
    (dec (list Ecosystems.PackageWithRegistry.T.jsont) (read "lookup.json"));
  ok "version lookup"
    (dec (list Ecosystems.VersionLookup.T.jsont) (read "vlookup.json"));
  ok "codemeta" (dec Ecosystems.CodeMeta.T.jsont (read "codemeta.json"))

let () =
  match List.rev !failures with
  | [] -> ()
  | fs -> failwith (String.concat "\n" fs)
