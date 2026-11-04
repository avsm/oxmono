(* SPDX-License-Identifier: ISC *)
open Json

type t = {
  store : Store.t;
  network : Network.t;
  http : Fetch.plain;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  handles : (string, float * string option) Hashtbl.t;
  avatars : (string, float * (string * string) option) Hashtbl.t;
}

let v ~store ~network ~http ~clock =
  {
    store;
    network;
    http;
    clock :> float Eio.Time.clock_ty Eio.Resource.t;
    handles = Hashtbl.create 64;
    avatars = Hashtbl.create 64;
  }

(* An answer is kept for an hour, a failure for ten minutes, and a handle in
   the store for a day, which covers a restart. *)
let found = 3600.
let missing = 600.
let stored = 86400.
let now t = Eio.Time.now t.clock
let fresh t (at, value) = now t -. at < if value = None then missing else found

(* A lookup takes at most five seconds, and any failure is no answer. *)
let bounded t f =
  match Eio.Time.with_timeout t.clock 5. (fun () -> Ok (f ())) with
  | Ok v -> v
  | Error `Timeout -> None
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception _ -> None

let lookup t did =
  bounded t (fun () ->
      let doc = decode (Network.resolve t.network did) in
      if Option.map string (field "id" doc) <> Some did then None
      else
        let claimed =
          match field "alsoKnownAs" doc with
          | Some aka ->
              List.find_map
                (fun j ->
                  let s = string j in
                  if String.starts_with ~prefix:"at://" s then
                    Some (String.sub s 5 (String.length s - 5))
                  else None)
                (list aka)
          | None -> None
        in
        match claimed with
        | Some h when Atp.Handle.is_valid h -> (
            match Xrpc.Identity.did_of_handle t.http h with
            | Ok back when back = did -> Some (String.lowercase_ascii h)
            | _ -> None)
        | _ -> None)

let keep t did h =
  if Hashtbl.length t.handles > 10_000 then Hashtbl.reset t.handles;
  Hashtbl.replace t.handles did (now t, h);
  Option.iter
    (fun h ->
      Store.put t.store "handle" did
        (encode (obj [ ("handle", str h); ("at", Jsont.Json.number (now t)) ])))
    h

let remember t did h = if Atp.Did.is_valid did then keep t did (Some h)

let handle t did =
  if not (Atp.Did.is_valid did) then None
  else
    match Hashtbl.find_opt t.handles did with
    | Some entry when fresh t entry -> snd entry
    | _ -> (
        let kept =
          Option.bind (Store.get t.store "handle" did) (fun raw ->
              match decode raw with
              | j when now t -. number (required "at" j) < stored ->
                  Some (get "handle" j)
              | _ -> None
              | exception _ -> None)
        in
        match kept with
        | Some h ->
            Hashtbl.replace t.handles did (now t, Some h);
            Some h
        | None ->
            let h = lookup t did in
            keep t did h;
            h)

let handles t dids =
  Eio.Fiber.List.iter ~max_fibers:8
    (fun did -> ignore (handle t did))
    (List.sort_uniq String.compare dids)

let images = [ "image/png"; "image/jpeg"; "image/webp"; "image/gif" ]

let fetch_avatar t did =
  bounded t (fun () ->
      let pds = Network.pds t.network did in
      match
        Network.record t.network ~service:pds ~did
          ~collection:"app.bsky.actor.profile" ~rkey:"self"
      with
      | None -> None
      | Some r -> (
          match field "avatar" (required "value" r) with
          | None -> None
          | Some blob ->
              let mime = get "mimeType" blob in
              let cid = get "$link" (required "ref" blob) in
              if not (List.mem mime images) then None
              else
                let url =
                  Network.query
                    (pds ^ "/xrpc/com.atproto.sync.getBlob")
                    [ ("did", did); ("cid", cid) ]
                in
                Fetch.with_response t.http `GET url (fun r ->
                    if Fetch.status r <> 200 then None
                    else
                      Some
                        ( mime,
                          Fetch.decode ~limit:(1024 * 1024) Fetch.Media.octets r
                        ))))

let avatar t did =
  if not (Atp.Did.is_valid did) then None
  else
    match Hashtbl.find_opt t.avatars did with
    | Some entry when fresh t entry -> snd entry
    | _ ->
        let a = fetch_avatar t did in
        if Hashtbl.length t.avatars > 500 then Hashtbl.reset t.avatars;
        Hashtbl.replace t.avatars did (now t, a);
        a
