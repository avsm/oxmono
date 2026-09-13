(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open! Core
open Bonsai_term
open Bonsai.Let_syntax

type action = Enqueue of Event.t | Consume of int

let component ~handler ~can_batch ~ready (local_ graph) =
  let queue, inject =
    Bonsai.state_machine ~default_model:Fqueue.empty
      ~apply_action:(fun _ queue -> function
        | Enqueue event -> Fqueue.enqueue queue event
        | Consume count ->
            let rec drop queue count =
              if count = 0 then queue
              else
                match Fqueue.dequeue queue with
                | None -> queue
                | Some (_, tail) -> drop tail (count - 1)
            in
            drop queue count)
      graph
  in
  let consume =
    let%arr queue and handler and can_batch and ready and inject in
    let rec take queue count acc =
      if count = 4096 then List.rev acc
      else
        match Fqueue.dequeue queue with
        | None -> List.rev acc
        | Some (event, _) when not (ready event) -> List.rev acc
        | Some (event, tail) ->
            if can_batch event then take tail (count + 1) (event :: acc)
            else if count = 0 then [ event ]
            else List.rev acc
    in
    let events = take queue 0 [] in
    if List.is_empty events then Effect.Ignore
    else
      Effect.Many
        (List.map events ~f:handler @ [ inject (Consume (List.length events)) ])
  in
  (* The terminal driver shares a handler snapshot across an input burst.
     Cross focus and paste boundaries only after Bonsai has applied state. *)
  Bonsai.Edge.after_display consume graph;
  let%arr inject in
  fun event -> inject (Enqueue event)
