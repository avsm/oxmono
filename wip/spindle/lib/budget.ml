(* SPDX-License-Identifier: ISC *)

type bucket = { mutable tokens : float; mutable filled : float }

type t = {
  now : unit -> float;
  rate : float;
  burst : float;
  client_rate : float;
  client_burst : float;
  shared : bucket;
  clients : (string, bucket) Hashtbl.t;
}

let max_clients = 4096

let v ~now ~rate ~burst ~client_rate ~client_burst () =
  {
    now;
    rate;
    burst;
    client_rate;
    client_burst;
    shared = { tokens = burst; filled = now () };
    clients = Hashtbl.create 64;
  }

let refill b ~rate ~burst ~now =
  b.tokens <- Float.min burst (b.tokens +. ((now -. b.filled) *. rate));
  b.filled <- now

(* A client whose bucket has refilled is the same as one never seen, so
   those are forgotten first when the table is full. *)
let client t name ~now =
  match Hashtbl.find_opt t.clients name with
  | Some b -> b
  | None ->
      if Hashtbl.length t.clients >= max_clients then
        Hashtbl.filter_map_inplace
          (fun _ b ->
            refill b ~rate:t.client_rate ~burst:t.client_burst ~now;
            if b.tokens >= t.client_burst then None else Some b)
          t.clients;
      if Hashtbl.length t.clients >= max_clients then Hashtbl.reset t.clients;
      let b = { tokens = t.client_burst; filled = now } in
      Hashtbl.replace t.clients name b;
      b

let spend t ~client:name =
  let now = t.now () in
  refill t.shared ~rate:t.rate ~burst:t.burst ~now;
  let own =
    Option.map
      (fun name ->
        let b = client t name ~now in
        refill b ~rate:t.client_rate ~burst:t.client_burst ~now;
        b)
      name
  in
  let has b = b.tokens >= 1. in
  has t.shared
  && Option.fold ~none:true ~some:has own
  &&
  (t.shared.tokens <- t.shared.tokens -. 1.;
   Option.iter (fun b -> b.tokens <- b.tokens -. 1.) own;
   true)
