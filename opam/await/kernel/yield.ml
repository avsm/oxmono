open! Base
open! Portable_kernel

type t = Await.t

let of_await await = await
let yield = Await.yield
