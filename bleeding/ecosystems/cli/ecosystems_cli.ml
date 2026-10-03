open Cmdliner

type common = {
  json : bool;
  base_url : string option;
  user_agent : string option;
}

let common =
  let json =
    Arg.(
      value & flag
      & info [ "json" ] ~doc:"Print the decoded response as JSON.")
  in
  let base_url =
    Arg.(
      value
      & opt (some string) None
      & info [ "base-url" ] ~docv:"URL" ~doc:"API base URL.")
  in
  let user_agent =
    Arg.(
      value
      & opt (some string) None
      & info [ "user-agent" ] ~docv:"STRING" ~doc:"User-Agent to send.")
  in
  Term.(
    const (fun json base_url user_agent -> { json; base_url; user_agent })
    $ json $ base_url $ user_agent)

let positive =
  Arg.conv
    ( (fun s ->
        match int_of_string_opt s with
        | Some n when n >= 1 -> Ok n
        | _ -> Error (`Msg "must be a positive integer")),
      Format.pp_print_int )

let limit ?default () =
  let doc =
    match default with
    | None -> "Print at most $(docv) items."
    | Some n ->
        Printf.sprintf "Print at most $(docv) items. The default is %d." n
  in
  Arg.(
    value
    & opt (some positive) default
    & info [ "limit"; "n" ] ~docv:"N" ~doc)

let registry =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"REGISTRY" ~doc:"Registry name, such as crates.io.")

let opt = function Some s when s <> "" -> s | _ -> "-"

(* [truncate n s] is [s] cut to at most [n] characters, ending in "..." when
   it was cut. It never splits a UTF-8 sequence. *)
let truncate n s =
  let step i = i + Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
  let rec length i count =
    if i >= String.length s then count else length (step i) (count + 1)
  in
  let rec prefix i count =
    if count = 0 then i else prefix (step i) (count - 1)
  in
  if length 0 0 <= n then s else String.sub s 0 (prefix 0 (n - 3)) ^ "..."

let take limit seq =
  match limit with Some n -> Seq.take n seq | None -> seq

let to_json codec v =
  match Openapi.Runtime.Json.encode codec v with
  | Ok s -> s
  | Error e -> failwith e

(* Rows are printed as they arrive. JSON is one array, so it waits for all. *)
let emit_list ~out ~json codec row items =
  if json then
    Format.fprintf out "%s@." (to_json (Jsont.list codec) (List.of_seq items))
  else Seq.iter (fun v -> Format.fprintf out "%s@." (row v)) items

(* [per_page] is the page size to ask for when at most [limit] items are
   wanted. *)
let per_page = function Some n when n >= 1 && n < 100 -> n | _ -> 100

let listing limit f =
  let seq = Ecosystems_client.pages ~per_page:(per_page limit) f in
  take limit seq

(* [one_line s] joins the lines of [s] with single spaces. Error payloads
   print an embedded newline as the four characters backslash, x, 0, A, so
   those count as line breaks too. *)
let one_line s =
  let escaped = String.make 1 (Char.chr 92) ^ "x0A" in
  let n = String.length escaped in
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i >= String.length s then ()
    else if i + n <= String.length s && String.sub s i n = escaped then (
      Buffer.add_char b (Char.chr 10);
      go (i + n))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  String.split_on_char (Char.chr 10) (Buffer.contents b)
  |> List.map String.trim
  |> List.filter (fun l -> l <> "")
  |> String.concat " "

let guard ~err f =
  match f () with
  | () -> 0
  | exception Openapi.Runtime.Api_error { status; operation; _ } ->
      Format.fprintf err "oecosystems: %s: HTTP %d@." operation status;
      1
  | exception (Invalid_argument msg | Failure msg) ->
      Format.fprintf err "oecosystems: %s@." msg;
      1
  | exception Sys_error msg when String.ends_with ~suffix:"Broken pipe" msg ->
      0
  | exception Sys_error msg ->
      Format.fprintf err "oecosystems: %s@." msg;
      1
  | exception (Eio.Io _ as ex) ->
      Format.fprintf err "oecosystems: %s@."
        (one_line (Format.asprintf "%a" Eio.Exn.pp ex));
      1

let make ~env ~err ~name ~doc term =
  let run common f =
    guard ~err (fun () ->
        Eio.Switch.run @@ fun sw ->
        let client =
          Ecosystems_client.create ?user_agent:common.user_agent
            ?base_url:common.base_url ~sw env
        in
        f common.json client)
  in
  Cmd.v (Cmd.info name ~doc) Term.(const run $ common $ term)

let registry_row r =
  let module R = Ecosystems.Registry.T in
  Printf.sprintf "%-18s %-8s %10Ld packages  %s" (R.name r) (R.ecosystem r)
    (R.packages_count r) (R.url r)

let registries ~env ~out ~err =
  make ~env ~err ~name:"registries" ~doc:"List registries."
    Term.(
      const (fun limit json c ->
          listing limit (fun ~page ~per_page ->
              Ecosystems.Registry.get_registries ~page ~per_page c ())
          |> emit_list ~out ~json Ecosystems.Registry.T.jsont registry_row)
      $ limit ())

let package_name =
  Arg.(
    required
    & pos 1 (some string) None
    & info [] ~docv:"PACKAGE" ~doc:"Package name.")

let version_number =
  Arg.(
    required
    & pos 2 (some string) None
    & info [] ~docv:"VERSION" ~doc:"Version number.")

let emit ~out ~json codec detail v =
  if json then Format.fprintf out "%s@." (to_json codec v) else detail out v

let field out label v = Format.fprintf out "%-12s %s@." label v

let package_row p =
  let module P = Ecosystems.Package.T in
  Printf.sprintf "%-30s %-12s %s" (P.name p)
    (opt (P.latest_release_number p))
    (truncate 60 (opt (P.description p)))

let package_detail out p =
  let module P = Ecosystems.Package.T in
  let f = field out in
  f "name" (P.name p);
  f "ecosystem" (P.ecosystem p);
  f "latest" (opt (P.latest_release_number p));
  f "licenses" (opt (P.licenses p));
  f "description" (opt (P.description p));
  f "homepage" (opt (P.homepage p));
  f "downloads"
    (match P.downloads p with
    | Some n -> Printf.sprintf "%d %s" n (opt (P.downloads_period p))
    | None -> "-");
  f "dependents"
    (Printf.sprintf "%d packages, %d repositories"
       (P.dependent_packages_count p)
       (P.dependent_repos_count p));
  f "advisories" (string_of_int (List.length (P.advisories p)));
  f "purl" (P.purl p)

let version_row v =
  let module V = Ecosystems.Version.T in
  Printf.sprintf "%-14s %-26s %s" (V.number v)
    (opt (V.published_at v))
    (opt (V.licenses v))

let dependency_row d =
  let module D = Ecosystems.Dependency.T in
  Printf.sprintf "  %-10s %-30s %s" (opt (D.kind d)) (D.package_name d)
    (opt (D.requirements d))

let version_detail out v =
  let module V = Ecosystems.VersionWithDependencies.T in
  let f = field out in
  f "number" (V.number v);
  f "published" (opt (V.published_at v));
  f "licenses" (opt (V.licenses v));
  f "integrity" (opt (V.integrity v));
  f "purl" (V.purl v);
  let deps = V.dependencies v in
  Format.fprintf out "dependencies (%d)@." (List.length deps);
  List.iter (fun d -> Format.fprintf out "%s@." (dependency_row d)) deps

let advisory_row a =
  let module A = Ecosystems.Advisory.T in
  Printf.sprintf "%-10s %s (%s)" (opt (A.severity a)) (opt (A.title a))
    (String.concat ", " (List.filter_map Fun.id (A.identifiers a)))

let package ~env ~out ~err =
  make ~env ~err ~name:"package" ~doc:"Show a package."
    Term.(
      const (fun registry_name package_name json c ->
          Ecosystems.Package.get_registry_package ~registry_name ~package_name c
            ()
          |> emit ~out ~json Ecosystems.Package.T.jsont package_detail)
      $ registry $ package_name)

let versions ~env ~out ~err =
  make ~env ~err ~name:"versions" ~doc:"List the versions of a package."
    Term.(
      const (fun registry_name package_name limit json c ->
          listing limit (fun ~page ~per_page ->
              Ecosystems.Version.get_registry_package_versions ~registry_name
                ~package_name ~page ~per_page c ())
          |> emit_list ~out ~json Ecosystems.Version.T.jsont version_row)
      $ registry $ package_name $ limit ())

let version ~env ~out ~err =
  make ~env ~err ~name:"version"
    ~doc:"Show one version of a package, with its dependencies."
    Term.(
      const (fun registry_name package_name version_number json c ->
          Ecosystems.VersionWithDependencies.get_registry_package_version
            ~registry_name ~package_name ~version_number c ()
          |> emit ~out ~json Ecosystems.VersionWithDependencies.T.jsont
               version_detail)
      $ registry $ package_name $ version_number)

let dependents ~env ~out ~err =
  make ~env ~err ~name:"dependents"
    ~doc:"List the packages that depend on a package."
    Term.(
      const (fun registry_name package_name limit json c ->
          listing limit (fun ~page ~per_page ->
              Ecosystems.Package.get_registry_package_dependent_packages
                ~registry_name ~package_name ~page ~per_page c ())
          |> emit_list ~out ~json Ecosystems.Package.T.jsont package_row)
      $ registry $ package_name $ limit ~default:100 ())

let advisories ~env ~out ~err =
  make ~env ~err ~name:"advisories" ~doc:"List the advisories on a package."
    Term.(
      const (fun registry_name package_name limit json c ->
          Ecosystems.Package.get_registry_package ~registry_name ~package_name c
            ()
          |> Ecosystems.Package.T.advisories |> List.to_seq |> take limit
          |> emit_list ~out ~json Ecosystems.Advisory.T.jsont advisory_row)
      $ registry $ package_name $ limit ())

let target =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"TARGET"
        ~doc:
          "A package URL starting with pkg:, or the URL of a source \
           repository.")

let maintainer_login =
  Arg.(
    required
    & pos 1 (some string) None
    & info [] ~docv:"LOGIN" ~doc:"Maintainer login or UUID.")

let keyword_name =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"KEYWORD" ~doc:"Keyword name.")

let lookup_row p =
  let module P = Ecosystems.PackageWithRegistry.T in
  Printf.sprintf "%-14s %-30s %-12s %s"
    (Ecosystems.Registry.T.name (P.registry p))
    (P.name p)
    (opt (P.latest_release_number p))
    (truncate 50 (opt (P.description p)))

let maintainer_detail out m =
  let module M = Ecosystems.Maintainer.T in
  let f = field out in
  f "login" (opt (M.login m));
  f "name" (opt (M.name m));
  f "email" (opt (M.email m));
  f "uuid" (M.uuid m);
  f "packages" (string_of_int (M.packages_count m));
  f "url" (opt (M.url m))

let keyword_detail limit out k =
  let module K = Ecosystems.KeywordWithPackages.T in
  let f = field out in
  f "name" (K.name k);
  f "packages"
    (match K.packages_count k with Some n -> string_of_int n | None -> "-");
  K.packages k |> List.to_seq |> take limit
  |> Seq.iter (fun p -> Format.fprintf out "%s@." (package_row p))

let lookup ~env ~out ~err =
  make ~env ~err ~name:"lookup"
    ~doc:"Find packages by package URL or repository URL."
    Term.(
      const (fun target limit json c ->
          (if String.starts_with ~prefix:"pkg:" target then
             Ecosystems.PackageWithRegistry.lookup_package ~purl:target c ()
           else
             Ecosystems.PackageWithRegistry.lookup_package
               ~repository_url:target c ())
          |> List.to_seq |> take limit
          |> emit_list ~out ~json Ecosystems.PackageWithRegistry.T.jsont
               lookup_row)
      $ target $ limit ())

let maintainer ~env ~out ~err =
  make ~env ~err ~name:"maintainer" ~doc:"Show a maintainer."
    Term.(
      const (fun registry_name maintainer_login_or_uuid json c ->
          Ecosystems.Maintainer.get_registry_maintainer ~registry_name
            ~maintainer_login_or_uuid c ()
          |> emit ~out ~json Ecosystems.Maintainer.T.jsont maintainer_detail)
      $ registry $ maintainer_login)

let keyword ~env ~out ~err =
  make ~env ~err ~name:"keyword" ~doc:"Show a keyword and some of its packages."
    Term.(
      const (fun keyword_name limit json c ->
          Ecosystems.KeywordWithPackages.get_keyword ~keyword_name c ()
          |> emit ~out ~json Ecosystems.KeywordWithPackages.T.jsont
               (keyword_detail limit))
      $ keyword_name $ limit ())

let cli_version = if Version.v = "" then "dev" else Version.v

let main ~out ~err env =
  Cmd.group
    (Cmd.info "oecosystems" ~version:cli_version
       ~doc:"Query the packages.ecosyste.ms API.")
    [ registries ~env ~out ~err; package ~env ~out ~err;
      versions ~env ~out ~err; version ~env ~out ~err;
      dependents ~env ~out ~err; advisories ~env ~out ~err;
      lookup ~env ~out ~err; maintainer ~env ~out ~err;
      keyword ~env ~out ~err ]
