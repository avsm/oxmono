type t = { handle : string; version : string }

let pp ppf { handle; version } =
  if version = "" then Fmt.pf ppf "@%s" handle
  else Fmt.pf ppf "@%s/%s" handle version

let codec =
  let open Jsont in
  Object.map ~kind:"overlay" (fun handle version -> { handle; version })
  |> Object.mem "handle" string ~enc:(fun o -> o.handle)
  |> Object.mem "version" string ~enc:(fun o -> o.version)
  |> Object.finish
