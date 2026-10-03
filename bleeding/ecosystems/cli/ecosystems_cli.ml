open Cmdliner

type common = {
  json : bool;
  base_url : string option;
  user_agent : string option;
}

let common =
  let json =
    Arg.(value & flag & info [ "json" ] ~doc:"Print the full response as JSON.")
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

let limit =
  Arg.(
    value
    & opt (some int) None
    & info [ "limit"; "n" ] ~docv:"N" ~doc:"Print at most $(docv) items.")

let registry =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"REGISTRY" ~doc:"Registry name, such as crates.io.")

let opt = function Some s when s <> "" -> s | _ -> "-"

let truncate n s =
  if String.length s <= n then s else String.sub s 0 (n - 1) ^ "..."

let to_json codec v =
  match Openapi.Runtime.Json.encode codec v with
  | Ok s -> s
  | Error e -> failwith e

let emit_list ~out ~json codec row items =
  if json then Format.fprintf out "%s@." (to_json (Jsont.list codec) items)
  else List.iter (fun v -> Format.fprintf out "%s@." (row v)) items

(* [per_page] is the page size to ask for when at most [limit] items are
   wanted. *)
let per_page = function Some n when n >= 1 && n < 100 -> n | _ -> 100

let listing limit f =
  let seq = Ecosystems_client.pages ~per_page:(per_page limit) f in
  (match limit with Some n -> Seq.take n seq | None -> seq) |> List.of_seq

let guard ~err f =
  match f () with
  | () -> 0
  | exception Openapi.Runtime.Api_error { status; operation; _ } ->
      Format.fprintf err "oecosystems: %s: HTTP %d@." operation status;
      1
  | exception (Eio.Io _ as ex) ->
      Format.fprintf err "oecosystems: %s@." (Printexc.to_string ex);
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
      $ limit)

let main ~out ~err env =
  Cmd.group
    (Cmd.info "oecosystems" ~version:"0.1.0"
       ~doc:"Query the packages.ecosyste.ms API.")
    [ registries ~env ~out ~err ]
