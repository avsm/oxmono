open Proffer
open Proffer.Route

type env = { clock : float Eio.Time.clock_ty Eio.Resource.t }

let site =
  Site.of_routes
    [ get (s "countdown") (fun env _request respond ->
        Resp.stream respond "text/plain" @@ fun sink ->
        for i = 5 downto 1 do
          Body.Sink.write sink (string_of_int i ^ "\n");
          Eio.Time.sleep env.clock 1.0
        done;
        Body.Sink.write sink "Liftoff!\n");

      get (s "alphabet") (fun _env _request respond ->
        Resp.stream respond ~length:27L "text/plain" @@ fun sink ->
        for c = Char.code 'a' to Char.code 'z' do
          Body.Sink.write sink (String.make 1 (Char.chr c))
        done;
        Body.Sink.write sink "\n") ]

let () =
  Eio_main.run @@ fun stdenv ->
  let clock = Eio.Stdenv.clock stdenv in
  Proffer_httpz.run stdenv ~env:{ clock } site
