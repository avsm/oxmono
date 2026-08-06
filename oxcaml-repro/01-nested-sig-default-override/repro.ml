let evil = ref 0

module M = struct
  let ok x = x + 1
  let pp fmt x = incr evil; Format.fprintf fmt "%d" (x + !evil)
end
