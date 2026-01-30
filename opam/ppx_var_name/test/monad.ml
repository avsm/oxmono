type 'a t = 'a

let return x = x
let bind x ~f = f x
let map x ~f = f x
let both x y = x, y
let map2 a b ~f = map (both a b) ~f:(fun (a, b) -> f a b)
let map3 a b c ~f = map2 (both a b) c ~f:(fun (a, b) c -> f a b c)

module Let_syntax = struct
  let return = return

  module Let_syntax = struct
    let return = return
    let bind = bind
    let map = map
    let map2 = map2
    let map3 = map3
    let both = both

    module Open_on_rhs = struct
      let return = return
    end
  end
end
