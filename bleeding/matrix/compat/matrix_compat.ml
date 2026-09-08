(* Local OCaml 5.5 surface for the OxCaml 5.2 import. *)
module Option = struct
  include Stdlib.Option

  let exists f = function None -> false | Some x -> f x
  let for_all f = function None -> true | Some x -> f x
end

module Result = struct
  include Stdlib.Result

  let product a b =
    match (a, b) with
    | Ok a, Ok b -> Ok (a, b)
    | Error e, _ | _, Error e -> Error e

  module Syntax = struct
    let ( let* ) = bind
    let ( and* ) = product
    let ( let+ ) r f = map f r
    let ( and+ ) = product
  end
end

module List = struct
  include Stdlib.List

  let singleton x = [ x ]

  let take n xs =
    let rec loop n acc = function
      | [] -> rev acc
      | _ when n <= 0 -> rev acc
      | x :: rest -> loop (n - 1) (x :: acc) rest
    in
    loop n [] xs

  let rec drop n xs =
    if n <= 0 then xs
    else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest
end
