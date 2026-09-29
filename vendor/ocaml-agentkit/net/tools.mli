(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The tools numpty gives a model for reaching the network.

    Each is one call to the numptyd that holds the programs, so nothing here
    forks: a fork of a process holding a model costs minutes on macOS, which is
    the whole reason numptyd exists.

    The wording of every answer is {!Report}'s, refusals included, so a result
    reads the same whether it was driven by a model or by a test. What numptyd
    answers is the tool's result. The death of the session is a result too: a
    model told that the network child is gone can say so and write down what it
    was doing, where an empty answer would send it looking for a wall it cannot
    see. *)

val fetch : client:Client.t -> Ds4.Tool.t
(** [fetch ~client] fetches a URL and reports its status, its final URL, its
    content type, its byte count and then its body. *)

val head : client:Client.t -> Ds4.Tool.t
(** [head ~client] reports what [fetch] reports without the body. *)

val run : client:Client.t -> Ds4.Tool.t
(** [run ~client] runs a named program with arguments and reports its exit
    status and its combined output. It carries this process's whole authority,
    as a shell in okitd does, and every call of it is journalled before it is
    made. *)
