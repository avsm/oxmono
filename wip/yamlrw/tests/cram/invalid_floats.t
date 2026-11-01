Test that invalid float representations are treated as strings
(See: https://github.com/avsm/ocaml-yaml/issues/82)

Per YAML spec, only .nan/.NaN/.NAN (with leading dot) are valid NaN values,
and only .inf/.Inf/.INF (with leading dot) are valid infinity values.

OCaml's Float.of_string accepts "nan", "inf", "infinity" without the dot,
which causes incorrect parsing if used as a fallback.

  $ yamlcat invalid_floats.yml
  valid_nan: .nan
  valid_nan_title: .nan
  valid_nan_upper: .nan
  valid_inf: .inf
  valid_inf_title: .inf
  valid_inf_upper: .inf
  valid_neg_inf: -.inf
  valid_neg_inf_title: -.inf
  valid_neg_inf_upper: -.inf
  valid_pos_inf: .inf
  invalid_nan: 'nan'
  invalid_nan_title: 'NaN'
  invalid_nan_upper: 'NAN'
  invalid_inf: 'inf'
  invalid_inf_title: 'Inf'
  invalid_inf_upper: 'INF'
  invalid_infinity: 'infinity'
  invalid_infinity_title: 'Infinity'
  invalid_infinity_upper: 'INFINITY'
  invalid_neg_inf: '-inf'
  invalid_neg_infinity: '-infinity'
  invalid_pos_inf: '+inf'
  invalid_pos_infinity: '+infinity'
  quoted_nan: 'nan'
  quoted_inf: 'inf'
  quoted_infinity: 'infinity'
  dependencies:
    'nan': 2.14.0
    'inf': 1.0.0

  $ yamlcat --json invalid_floats.yml
  {"valid_nan": nan, "valid_nan_title": nan, "valid_nan_upper": nan, "valid_inf": inf, "valid_inf_title": inf, "valid_inf_upper": inf, "valid_neg_inf": -inf, "valid_neg_inf_title": -inf, "valid_neg_inf_upper": -inf, "valid_pos_inf": inf, "invalid_nan": "nan", "invalid_nan_title": "NaN", "invalid_nan_upper": "NAN", "invalid_inf": "inf", "invalid_inf_title": "Inf", "invalid_inf_upper": "INF", "invalid_infinity": "infinity", "invalid_infinity_title": "Infinity", "invalid_infinity_upper": "INFINITY", "invalid_neg_inf": "-inf", "invalid_neg_infinity": "-infinity", "invalid_pos_inf": "+inf", "invalid_pos_infinity": "+infinity", "quoted_nan": "nan", "quoted_inf": "inf", "quoted_infinity": "infinity", "dependencies": {"nan": "2.14.0", "inf": "1.0.0"}}
