let number =
  let check n =
    if not (Float.is_finite n) then
      Jsont.Error.msgf Jsont.Meta.none "Expected a finite number"
  in
  Jsont.iter ~dec:check ~enc:check Jsont.number

let integer =
  let dec n =
    if Float.trunc n <> n || Float.abs n > 9007199254740991. then
      Jsont.Error.msgf Jsont.Meta.none "Expected an exact JSON integer";
    int_of_float n
  in
  let enc n =
    let f = float_of_int n in
    if Float.abs f > 9007199254740991. then
      Jsont.Error.msgf Jsont.Meta.none "Integer exceeds JSON's exact range";
    f
  in
  Jsont.map ~dec ~enc number

let tagged tag body =
  let case = Jsont.Object.Case.map tag body ~dec:Fun.id in
  Jsont.Object.map ~kind:tag Fun.id
  |> Jsont.Object.case_mem "_type" Jsont.string
       [ Jsont.Object.Case.make case ]
       ~enc:Fun.id
       ~enc_case:(fun v -> Jsont.Object.Case.value case v)
  |> Jsont.Object.finish
