type t = { path : string; sha256 : string; strip_components : int }

let pp ppf t =
  Fmt.pf ppf "@[archive %s (sha256=%s, strip=%d)@]" t.path t.sha256
    t.strip_components
