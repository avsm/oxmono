let valid n =
  Float.is_finite n && Float.is_integer n
  && Float.abs n <= 9_007_199_254_740_991.

let jsont =
  Jsont.map ~kind:"exact JSON integer"
    ~dec:(fun n ->
      if not (valid n) then
        Jsont.Error.msgf Jsont.Meta.none "expected an exact JSON integer";
      int_of_float n)
    ~enc:(fun n ->
      if n < -9_007_199_254_740_991 || n > 9_007_199_254_740_991 then
        Jsont.Error.msgf Jsont.Meta.none "expected an exact JSON integer";
      float_of_int n)
    Jsont.number
