# yamlt - YAML codec using Jsont type descriptions

Yamlt provides YAML streaming encode/decode that interprets Jsont.t type descriptions, allowing the same codec definitions to work for both JSON and YAML.

## Key Features

- Use the same Jsont.t codec for both JSON and YAML formats
- Streaming encode/decode with configurable depth and node limits
- Support for YAML-specific features (scalars, sequences, mappings)
- Billion laughs protection with configurable limits
- Multiple output formats (block, flow, layout preservation)

## Usage

```ocaml
(* Define a codec once using Jsont *)
module Config = struct
  type t = { name: string; port: int }
  let make name port = { name; port }
  let jsont =
    Jsont.Object.map ~kind:"Config" make
    |> Jsont.Object.mem "name" Jsont.string ~enc:(fun c -> c.name)
    |> Jsont.Object.mem "port" Jsont.int ~enc:(fun c -> c.port)
    |> Jsont.Object.finish
end

(* Use the same codec for both JSON and YAML *)
let from_json = Jsont_bytesrw.decode_string Config.jsont json_str
let from_yaml = Yamlt.decode_string Config.jsont yaml_str
```

For encoding:

```ocaml
(* Encode to YAML with different formats *)
let config = Config.make "server" 8080

(* Block style (default) *)
let yaml_block = Yamlt.encode_string Config.jsont config

(* Flow style (JSON-like) *)
let yaml_flow = Yamlt.encode_string ~format:Flow Config.jsont config
```

## Installation

```
opam install yamlt
```

## Documentation

API documentation is available at https://tangled.org/@anil.recoil.org/ocaml-yamlt or via:

```
opam install yamlt
odig doc yamlt
```

## License

ISC
