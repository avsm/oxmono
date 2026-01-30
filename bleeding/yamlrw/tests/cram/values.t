Test YAML null values from values_null.yml

  $ yamlcat values_null.yml
  explicit_null: null
  tilde_null: null
  empty_null: null
  flow_null:
    - null
    - null
  sequence_nulls:
    - null
    - null
    - null
    - explicit: null
    - tilde: null
    - empty: null
  mapping_nulls:
    key1: null
    key2: null
    key3: null
  'null': null key with string value
  '~': tilde key with string value
  nested:
    level1:
      null_value: null
      tilde_value: null
      empty_value: null
      list:
        - null
        - null
        - null
        - some_value
      map:
        a: null
        b: null
        c: null
  string_nulls:
    quoted_null: 'null'
    quoted_tilde: '~'
    null_in_string: this is null
    word_null: 'null'

  $ yamlcat --json values_null.yml
  {"explicit_null": null, "tilde_null": null, "empty_null": null, "flow_null": [null, null], "sequence_nulls": [null, null, null, {"explicit": null}, {"tilde": null}, {"empty": null}], "mapping_nulls": {"key1": null, "key2": null, "key3": null}, "null": "null key with string value", "~": "tilde key with string value", "nested": {"level1": {"null_value": null, "tilde_value": null, "empty_value": null, "list": [null, null, null, "some_value"], "map": {"a": null, "b": null, "c": null}}}, "string_nulls": {"quoted_null": "null", "quoted_tilde": "~", "null_in_string": "this is null", "word_null": "null"}}

Test YAML boolean values from values_bool.yml

  $ yamlcat values_bool.yml
  bool_true: true
  bool_false: false
  capitalized_true: true
  capitalized_false: false
  yes_value: true
  no_value: false
  Yes_value: true
  No_value: false
  YES_value: true
  NO_value: false
  on_value: true
  off_value: false
  On_value: true
  Off_value: false
  ON_value: true
  OFF_value: false
  bool_sequence:
    - true
    - false
    - true
    - false
    - true
    - false
  flow_bools:
    - true
    - false
    - true
    - false
  bool_mapping:
    active: true
    disabled: false
    enabled: true
    stopped: false
  quoted_bools:
    quoted_true: 'true'
    quoted_false: 'false'
    quoted_yes: 'yes'
    quoted_no: 'no'
    single_true: 'true'
    single_false: 'false'
  nested_bools:
    settings:
      debug: true
      verbose: false
      legacy_yes: true
      legacy_no: false
    flags:
      - true
      - false
      - true
      - false
  mixed_case:
    'TRUE': true
    'FALSE': false
    'TrUe': true
    'FaLsE': false
  bool_like_strings:
    truthy: truely
    falsy: falsetto
    yes_sir: yessir
    no_way: noway

  $ yamlcat --json values_bool.yml
  {"bool_true": true, "bool_false": false, "capitalized_true": true, "capitalized_false": false, "yes_value": true, "no_value": false, "Yes_value": true, "No_value": false, "YES_value": true, "NO_value": false, "on_value": true, "off_value": false, "On_value": true, "Off_value": false, "ON_value": true, "OFF_value": false, "bool_sequence": [true, false, true, false, true, false], "flow_bools": [true, false, true, false], "bool_mapping": {"active": true, "disabled": false, "enabled": true, "stopped": false}, "quoted_bools": {"quoted_true": "true", "quoted_false": "false", "quoted_yes": "yes", "quoted_no": "no", "single_true": "true", "single_false": "false"}, "nested_bools": {"settings": {"debug": true, "verbose": false, "legacy_yes": true, "legacy_no": false}, "flags": [true, false, true, false]}, "mixed_case": {"TRUE": true, "FALSE": false, "TrUe": true, "FaLsE": false}, "bool_like_strings": {"truthy": "truely", "falsy": "falsetto", "yes_sir": "yessir", "no_way": "noway"}}

Test YAML number values from values_numbers.yml

  $ yamlcat values_numbers.yml
  int_zero: 0
  int_positive: 42
  int_negative: -17
  int_large: 1000000
  int_with_underscores: 1000000
  octal_value: 12
  octal_zero: 0
  octal_large: 511
  hex_lowercase: 26
  hex_uppercase: 26
  hex_mixed: 3735928559
  hex_zero: 0
  float_simple: 3.14
  float_negative: -0.5
  float_zero: 0
  float_leading_dot: 0.5
  float_trailing_zero: 1
  scientific_positive: 10000000000
  scientific_negative: 0.0015
  scientific_uppercase: 250
  scientific_no_sign: 300000
  positive_infinity: .inf
  negative_infinity: -.inf
  not_a_number: .nan
  infinity_upper: .inf
  infinity_caps: .inf
  nan_upper: .nan
  nan_caps: .nan
  number_sequence:
    - 0
    - 42
    - -17
    - 3.14
    - 10000000000
    - .inf
    - .nan
  flow_numbers:
    - 0
    - 42
    - -17
    - 3.14
    - 26
    - 12
  number_mapping:
    count: 100
    price: 19.99
    discount: -5
    hex_color: 16734003
    octal_perms: 493
    scientific: 6.022e+23
  quoted_numbers:
    string_int: '42'
    string_float: '3.14'
    string_hex: '0x1A'
    string_octal: 0o14
    string_inf: '.inf'
    string_nan: '.nan'
  numeric_strings:
    phone: 555-1234
    version: 1.2.3
    code: 123
    leading_zero: 7
    plus_sign: 123
  edge_cases:
    min_int: -9.22337e+18
    max_int: 9.22337e+18
    very_small: 1e-100
    very_large: 1e+100
    negative_zero: -0
    positive_zero: 0
  nested_numbers:
    coordinates:
      x: 10.5
      y: -20.3
      z: 0
    measurements:
      - 1.1
      - 2.2
      - 3.3
    stats:
      count: 1000
      average: 45.67
      max: .inf
      min: -.inf
  legacy_octal: 14
  binary_like: 10
  format_tests:
    no_decimal: 42
    with_decimal: 42
    leading_zero_decimal: 0.42
    no_leading_digit: 0.42
    trailing_decimal: 42

  $ yamlcat --json values_numbers.yml
  {"int_zero": 0, "int_positive": 42, "int_negative": -17, "int_large": 1000000, "int_with_underscores": 1000000, "octal_value": 12, "octal_zero": 0, "octal_large": 511, "hex_lowercase": 26, "hex_uppercase": 26, "hex_mixed": 3735928559, "hex_zero": 0, "float_simple": 3.14, "float_negative": -0.5, "float_zero": 0, "float_leading_dot": 0.5, "float_trailing_zero": 1, "scientific_positive": 10000000000, "scientific_negative": 0.0015, "scientific_uppercase": 250, "scientific_no_sign": 300000, "positive_infinity": inf, "negative_infinity": -inf, "not_a_number": nan, "infinity_upper": inf, "infinity_caps": inf, "nan_upper": nan, "nan_caps": nan, "number_sequence": [0, 42, -17, 3.14, 10000000000, inf, nan], "flow_numbers": [0, 42, -17, 3.14, 26, 12], "number_mapping": {"count": 100, "price": 19.99, "discount": -5, "hex_color": 16734003, "octal_perms": 493, "scientific": 6.022e+23}, "quoted_numbers": {"string_int": "42", "string_float": "3.14", "string_hex": "0x1A", "string_octal": "0o14", "string_inf": ".inf", "string_nan": ".nan"}, "numeric_strings": {"phone": "555-1234", "version": "1.2.3", "code": 123, "leading_zero": 7, "plus_sign": 123}, "edge_cases": {"min_int": -9.22337e+18, "max_int": 9.22337e+18, "very_small": 1e-100, "very_large": 1e+100, "negative_zero": -0, "positive_zero": 0}, "nested_numbers": {"coordinates": {"x": 10.5, "y": -20.3, "z": 0}, "measurements": [1.1, 2.2, 3.3], "stats": {"count": 1000, "average": 45.67, "max": inf, "min": -inf}}, "legacy_octal": 14, "binary_like": 10, "format_tests": {"no_decimal": 42, "with_decimal": 42, "leading_zero_decimal": 0.42, "no_leading_digit": 0.42, "trailing_decimal": 42}}

Test YAML timestamp values from values_timestamps.yml

  $ yamlcat values_timestamps.yml
  date_simple: 2001-12-15
  date_earliest: 1970-01-01
  date_leap_year: 2020-02-29
  date_current: 2025-12-04
  datetime_utc: '2001-12-15T02:59:43.1Z'
  datetime_utc_full: '2001-12-15T02:59:43.123456Z'
  datetime_utc_no_frac: '2001-12-15T02:59:43Z'
  datetime_offset_pos: '2001-12-15T02:59:43.1+05:30'
  datetime_offset_neg: '2001-12-15T02:59:43.1-05:00'
  datetime_offset_hours: '2001-12-15T02:59:43+05'
  datetime_spaced: '2001-12-14 21:59:43.10 -5'
  datetime_spaced_utc: '2001-12-15 02:59:43.1 Z'
  datetime_spaced_offset: '2001-12-14 21:59:43.10 -05:00'
  datetime_no_frac: '2001-12-15T14:30:00Z'
  date_only: 2001-12-15
  timestamp_formats:
    iso_date: 2001-12-15
    iso_datetime_z: '2001-12-15T02:59:43Z'
    iso_datetime_offset: '2001-12-15T02:59:43+00:00'
    spaced_datetime: '2001-12-14 21:59:43.10 -5'
    canonical: '2001-12-15T02:59:43.1Z'
  timestamp_sequence:
    - 2001-12-15
    - '2001-12-15T02:59:43.1Z'
    - '2001-12-14 21:59:43.10 -5'
    - '2025-01-01T00:00:00Z'
  events:
    created: '2001-12-15T02:59:43.1Z'
    modified: '2001-12-16T10:30:00Z'
    published: '2001-12-14 21:59:43.10 -5'
  quoted_timestamps:
    string_date: 2001-12-15
    string_datetime: '2001-12-15T02:59:43.1Z'
    string_spaced: '2001-12-14 21:59:43.10 -5'
  edge_cases:
    midnight: '2001-12-15T00:00:00Z'
    end_of_day: '2001-12-15T23:59:59Z'
    microseconds: '2001-12-15T02:59:43.123456Z'
    no_seconds: '2001-12-15T02:59Z'
    hour_only: 2001-12-15T02Z
  nested_timestamps:
    project:
      start_date: 2001-12-15
      milestones:
        - date: 2001-12-20
          time: '2001-12-20T14:00:00Z'
        - date: 2002-01-15
          time: '2002-01-15T09:30:00-05:00'
      metadata:
        created: '2001-12-14 21:59:43.10 -5'
        updated: '2001-12-15T02:59:43.1Z'
  invalid_timestamps:
    bad_date: 2001-13-45
    bad_time: '2001-12-15T25:99:99Z'
    incomplete: 2001-12
    no_leading_zero: 2001-1-5
  timezones:
    utc_z: '2001-12-15T02:59:43Z'
    utc_offset: '2001-12-15T02:59:43+00:00'
    est: '2001-12-14T21:59:43-05:00'
    ist: '2001-12-15T08:29:43+05:30'
    jst: '2001-12-15T11:59:43+09:00'
  date_range:
    past: 1900-01-01
    unix_epoch: '1970-01-01T00:00:00Z'
    y2k: '2000-01-01T00:00:00Z'
    present: 2025-12-04
    future: '2099-12-31T23:59:59Z'

  $ yamlcat --json values_timestamps.yml
  {"date_simple": "2001-12-15", "date_earliest": "1970-01-01", "date_leap_year": "2020-02-29", "date_current": "2025-12-04", "datetime_utc": "2001-12-15T02:59:43.1Z", "datetime_utc_full": "2001-12-15T02:59:43.123456Z", "datetime_utc_no_frac": "2001-12-15T02:59:43Z", "datetime_offset_pos": "2001-12-15T02:59:43.1+05:30", "datetime_offset_neg": "2001-12-15T02:59:43.1-05:00", "datetime_offset_hours": "2001-12-15T02:59:43+05", "datetime_spaced": "2001-12-14 21:59:43.10 -5", "datetime_spaced_utc": "2001-12-15 02:59:43.1 Z", "datetime_spaced_offset": "2001-12-14 21:59:43.10 -05:00", "datetime_no_frac": "2001-12-15T14:30:00Z", "date_only": "2001-12-15", "timestamp_formats": {"iso_date": "2001-12-15", "iso_datetime_z": "2001-12-15T02:59:43Z", "iso_datetime_offset": "2001-12-15T02:59:43+00:00", "spaced_datetime": "2001-12-14 21:59:43.10 -5", "canonical": "2001-12-15T02:59:43.1Z"}, "timestamp_sequence": ["2001-12-15", "2001-12-15T02:59:43.1Z", "2001-12-14 21:59:43.10 -5", "2025-01-01T00:00:00Z"], "events": {"created": "2001-12-15T02:59:43.1Z", "modified": "2001-12-16T10:30:00Z", "published": "2001-12-14 21:59:43.10 -5"}, "quoted_timestamps": {"string_date": "2001-12-15", "string_datetime": "2001-12-15T02:59:43.1Z", "string_spaced": "2001-12-14 21:59:43.10 -5"}, "edge_cases": {"midnight": "2001-12-15T00:00:00Z", "end_of_day": "2001-12-15T23:59:59Z", "microseconds": "2001-12-15T02:59:43.123456Z", "no_seconds": "2001-12-15T02:59Z", "hour_only": "2001-12-15T02Z"}, "nested_timestamps": {"project": {"start_date": "2001-12-15", "milestones": [{"date": "2001-12-20", "time": "2001-12-20T14:00:00Z"}, {"date": "2002-01-15", "time": "2002-01-15T09:30:00-05:00"}], "metadata": {"created": "2001-12-14 21:59:43.10 -5", "updated": "2001-12-15T02:59:43.1Z"}}}, "invalid_timestamps": {"bad_date": "2001-13-45", "bad_time": "2001-12-15T25:99:99Z", "incomplete": "2001-12", "no_leading_zero": "2001-1-5"}, "timezones": {"utc_z": "2001-12-15T02:59:43Z", "utc_offset": "2001-12-15T02:59:43+00:00", "est": "2001-12-14T21:59:43-05:00", "ist": "2001-12-15T08:29:43+05:30", "jst": "2001-12-15T11:59:43+09:00"}, "date_range": {"past": "1900-01-01", "unix_epoch": "1970-01-01T00:00:00Z", "y2k": "2000-01-01T00:00:00Z", "present": "2025-12-04", "future": "2099-12-31T23:59:59Z"}}
