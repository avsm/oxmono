module Driver = Agentkit.Driver
module Model = Ds4_cli.Model

let model_dir () =
  Eio_main.run @@ fun env -> Model.dir (Xdge.create (Eio.Stdenv.fs env) "ds4")

let fetch_ds4 name ~token =
  match Model.find name with
  | None ->
      Error
        ("ds4/" ^ name
       ^ " is not a download target. Run models list for the available targets"
        )
  | Some target ->
      Ds4_cli.Cli.run @@ fun env xdg ->
      let dir = Model.dir xdg in
      if Model.present ~dir target then
        Printf.eprintf "ds4/%s is already installed in %s\n%!" target.name dir
      else begin
        Model.download ~fs:(Eio.Stdenv.fs env)
          ~proc:(Eio.Stdenv.process_mgr env)
          ~dir ?token target;
        Printf.eprintf "Installed ds4/%s in %s\n%!" target.name dir
      end

let strip_status (model : Driver.model) =
  let description = model.description in
  let strip suffix =
    if String.ends_with ~suffix description then
      String.sub description 0 (String.length description - String.length suffix)
    else description
  in
  let description =
    if String.ends_with ~suffix:" [downloaded]" description then
      strip " [downloaded]"
    else strip " [download required]"
  in
  { model with description }

let registry () =
  let dir = model_dir () in
  let ds4 =
    Driver.v ~name:"ds4"
      ~models:(fun () -> Agentkit_ds4.models ~dir () |> List.map strip_status)
      ~create:(fun _ -> ())
    |> Driver.manage
         ~availability:(fun name ->
           if name = "auto" then
             match Model.resolve ~dir None with
             | Ok _ -> Driver.Ready
             | Error _ -> Driver.Unavailable
           else if String.starts_with ~prefix:"local/" name then Driver.Ready
           else
             match Model.find name with
             | Some target when Model.present ~dir target -> Driver.Ready
             | Some _ -> Driver.Needs_download
             | None -> Driver.Unavailable)
         ~fetch:fetch_ds4
         ~canonical:(fun name ->
           match Model.find name with
           | Some target -> target.name
           | None -> name)
  in
  let apple =
    if Agentkit_apple_support.available then
      [
        Driver.v ~name:"apple" ~models:Agentkit_apple_support.models
          ~create:(fun _ -> ())
        |> Driver.manage ~availability:(fun _ -> Driver.Managed);
      ]
    else []
  in
  Driver.merge (ds4 :: apple)

let command () = Driver.Cli.models_cmd (registry ())
