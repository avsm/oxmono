let integer =
  let check n =
    if n >= Int64.of_int max_int then
      Jsont.Error.msg Jsont.Meta.none "Tool integer exceeds the supported range"
  in
  Jsont.map ~kind:"non-negative tool integer"
    ~dec:(fun n ->
      check n;
      Int64.to_int n)
    ~enc:(fun n ->
      let n = Int64.of_int n in
      check n;
      n)
    Jmap.Proto.Int53.Unsigned.jsont
