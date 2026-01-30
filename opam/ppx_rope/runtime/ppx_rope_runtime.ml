(* Implement the [Ppx_string_runtime.S] interface. *)

let empty = Rope.empty
let of_string = Rope.of_string
let convert = Rope.of_string
let concat list = Rope.concat list

let pad rope ~len =
  let n = Rope.length rope in
  if n >= len then rope else Rope.( ^ ) (Rope.of_string (String.make (len - n) ' ')) rope
;;

external finish_one : Rope.t -> Rope.t = "%identity"
