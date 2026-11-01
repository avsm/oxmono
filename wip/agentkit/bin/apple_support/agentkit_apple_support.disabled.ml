let available = false
let models () = []

let create ~sw:_ ~model:_ ~system:_ _ =
  failwith "Apple Foundation Models is not linked into this build"

let context_size _ =
  failwith "Apple Foundation Models is not linked into this build"
