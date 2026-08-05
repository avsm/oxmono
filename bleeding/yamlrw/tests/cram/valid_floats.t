Test valid YAML float number formats

This tests various real-world ways to write floating point numbers in YAML:
- Basic decimal notation (3.14, -0.5)
- Leading dot notation (.5 for 0.5)
- Scientific notation (1e10, 1E-10, 1.5e+10)
- Positive/negative signs
- Edge cases (very large, very small, negative zero)

  $ yamlcat valid_floats.yml
  basic_positive: 3.14
  basic_negative: -3.14
  basic_zero: 0
  small_positive: 0.5
  small_negative: -0.5
  leading_dot: 0.5
  leading_dot_negative: -0.5
  leading_dot_positive: 0.5
  leading_dot_long: 0.123457
  trailing_zero: 1
  trailing_zeros: 1
  many_trailing: 3.14
  sci_positive: 10000000000
  sci_negative: 1e-10
  sci_explicit_positive: 10000000000
  sci_with_decimal: 15000000000
  sci_small: 6.022e+23
  sci_tiny: 1.6e-19
  sci_upper: 10000000000
  sci_upper_negative: 1e-10
  sci_upper_explicit: 10000000000
  sci_upper_decimal: 250000000
  neg_sci: -10000000000
  neg_sci_negative_exp: -1e-10
  neg_sci_decimal: -314000
  positive_sign: 3.14
  positive_sci: 10000000000
  positive_small: 0.001
  very_large: 1e+100
  very_small: 1e-100
  large_decimal: 1e+09
  small_decimal: 1e-09
  negative_zero: -0
  positive_zero: 0
  one: 1
  minus_one: -1
  pi: 3.14159
  euler: 2.71828
  golden_ratio: 1.61803
  planck: 6.62607e-34
  avogadro: 6.02214e+23
  speed_of_light: 299792458
  gravitational_constant: 6.674e-11
  coordinates:
    latitude: 37.7749
    longitude: -122.419
    altitude: 16
  prices:
    item1: 19.99
    item2: 0.99
    discount: -5
    tax_rate: 0.0825
  measurements:
    temperature_c: 23.5
    temperature_f: 74.3
    humidity: 0.65
    pressure_hpa: 1013.25
  float_sequence:
    - 1
    - -1
    - 0.5
    - -0.5
    - 10000000000
    - 1e-10
    - 0.5
    - -0.5
  integers:
    simple: 42
    negative: -17
    zero: 0
    hex: 26
    octal: 12

  $ yamlcat --json valid_floats.yml
  {"basic_positive": 3.14, "basic_negative": -3.14, "basic_zero": 0, "small_positive": 0.5, "small_negative": -0.5, "leading_dot": 0.5, "leading_dot_negative": -0.5, "leading_dot_positive": 0.5, "leading_dot_long": 0.123457, "trailing_zero": 1, "trailing_zeros": 1, "many_trailing": 3.14, "sci_positive": 10000000000, "sci_negative": 1e-10, "sci_explicit_positive": 10000000000, "sci_with_decimal": 15000000000, "sci_small": 6.022e+23, "sci_tiny": 1.6e-19, "sci_upper": 10000000000, "sci_upper_negative": 1e-10, "sci_upper_explicit": 10000000000, "sci_upper_decimal": 250000000, "neg_sci": -10000000000, "neg_sci_negative_exp": -1e-10, "neg_sci_decimal": -314000, "positive_sign": 3.14, "positive_sci": 10000000000, "positive_small": 0.001, "very_large": 1e+100, "very_small": 1e-100, "large_decimal": 1e+09, "small_decimal": 1e-09, "negative_zero": -0, "positive_zero": 0, "one": 1, "minus_one": -1, "pi": 3.14159, "euler": 2.71828, "golden_ratio": 1.61803, "planck": 6.62607e-34, "avogadro": 6.02214e+23, "speed_of_light": 299792458, "gravitational_constant": 6.674e-11, "coordinates": {"latitude": 37.7749, "longitude": -122.419, "altitude": 16}, "prices": {"item1": 19.99, "item2": 0.99, "discount": -5, "tax_rate": 0.0825}, "measurements": {"temperature_c": 23.5, "temperature_f": 74.3, "humidity": 0.65, "pressure_hpa": 1013.25}, "float_sequence": [1, -1, 0.5, -0.5, 10000000000, 1e-10, 0.5, -0.5], "integers": {"simple": 42, "negative": -17, "zero": 0, "hex": 26, "octal": 12}}
