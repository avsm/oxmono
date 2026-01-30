open Hardcaml_axi

include Stream.Make (struct
    let data_bits = 32
    let user_bits = 2
  end)
