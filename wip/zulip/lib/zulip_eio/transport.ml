type t = {
  fetch : Fetch.plain;
  poll : float -> Fetch.plain;
  clock : float Eio.Time.clock_ty Eio.Resource.t option;
}

let v ?https ?retry ?(max_concurrent = 8) ?(idle_timeout = 30.) env =
  if (not (Float.is_finite idle_timeout)) || idle_timeout <= 0. then
    invalid_arg
      "Zulip_eio.Transport.v: idle_timeout must be finite and positive";
  let idle_timeout = Duration.of_f idle_timeout in
  let retry = Option.value retry ~default:(Fetch.Retry.v ()) in
  let retry =
    {
      retry with
      Fetch.Retry.allowed_methods =
        List.filter
          (function `GET | `HEAD -> true | _ -> false)
          retry.allowed_methods;
    }
  in
  let fetch =
    Fetch_httpz.std ?https ~cookies:`Off ~retry ~max_concurrent ~idle_timeout
      env
  in
  let poll timeout =
    Fetch_httpz.std ?https ~cookies:`Off
      ~retry:(Fetch.Retry.v ~max_retries:0 ())
      ~max_concurrent:1
      ~idle_timeout:(Duration.of_f (timeout +. 5.))
      env
  in
  {
    fetch;
    poll;
    clock =
      Some (Eio.Stdenv.clock env :> float Eio.Time.clock_ty Eio.Resource.t);
  }

let of_fetch ?clock ?poll_fetch fetch =
  let fetch = Fetch.Middleware.(of_handler (handler fetch)) in
  {
    fetch;
    poll = (fun _ -> Option.value poll_fetch ~default:fetch);
    clock :> float Eio.Time.clock_ty Eio.Resource.t option;
  }

let fetch t = t.fetch
let poll_fetch t ~timeout = t.poll timeout
let clock t = t.clock
