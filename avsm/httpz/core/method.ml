(* method.ml - HTTP methods *)

type t =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch
  | Propfind
  | Proppatch
  | Mkcol
  | Copy
  | Move
  | Lock
  | Unlock
  | Report

let to_string = function
  | Get -> "GET"
  | Head -> "HEAD"
  | Post -> "POST"
  | Put -> "PUT"
  | Delete -> "DELETE"
  | Connect -> "CONNECT"
  | Options -> "OPTIONS"
  | Trace -> "TRACE"
  | Patch -> "PATCH"
  | Propfind -> "PROPFIND"
  | Proppatch -> "PROPPATCH"
  | Mkcol -> "MKCOL"
  | Copy -> "COPY"
  | Move -> "MOVE"
  | Lock -> "LOCK"
  | Unlock -> "UNLOCK"
  | Report -> "REPORT"
;;

let pp fmt t = Stdlib.Format.fprintf fmt "%s" (to_string t)
