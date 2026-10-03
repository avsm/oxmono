(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a t = elapsed:float -> 'a

let v f = f
let const x = fun ~elapsed:_ -> x
let frame a ~elapsed = a ~elapsed:(Float.max 0. elapsed)
let map f a = fun ~elapsed -> f (a ~elapsed)
let map2 f a b = fun ~elapsed -> f (a ~elapsed) (b ~elapsed)
let all xs = fun ~elapsed -> List.map (fun a -> a ~elapsed) xs
