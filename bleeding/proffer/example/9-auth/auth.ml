open Proffer
open Proffer.Route

(* "alice:secret" in the Base64 form that browsers send. *)
let alice = "Basic YWxpY2U6c2VjcmV0"

(* [String.equal] stops at the first differing byte, so how long a comparison
   takes says how much of the credential was right; an attacker who can time
   requests recovers it byte by byte. This reads both strings whole and folds
   every difference into one accumulator, so the time depends only on the
   lengths. *)
let constant_time_equal a b =
  let n = String.length a in
  if n <> String.length b then false
  else begin
    let diff = ref 0 in
    for i = 0 to n - 1 do
      diff := !diff lor (Char.code a.[i] lxor Char.code b.[i])
    done;
    !diff = 0
  end

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Anyone can read this.\n");
      get (s "admin") (fun () _request respond ->
        Resp.text respond "Welcome, alice.\n");
      get (s "admin" / s "settings") (fun () _request respond ->
        Resp.text respond "Settings for alice.\n") ]
  |> Site.with_auth ~scope:[ [ "admin" ] ] ~realm:"tutorial"
       ~check:(fun authorization ->
         match authorization with
         | Some given -> constant_time_equal given alice
         | None -> false)

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~env:() site
