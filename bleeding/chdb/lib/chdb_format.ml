type t =
  | CSV
  | CSVWithNames
  | TSV
  | TSVWithNames
  | JSON
  | JSONEachRow
  | JSONCompact
  | JSONCompactEachRow
  | Native
  | RowBinary
  | RowBinaryWithNames
  | Arrow
  | Parquet
  | Pretty
  | PrettyCompact
  | Vertical
  | Null
  | Custom of string

let to_string = function
  | CSV -> "CSV"
  | CSVWithNames -> "CSVWithNames"
  | TSV -> "TSV"
  | TSVWithNames -> "TSVWithNames"
  | JSON -> "JSON"
  | JSONEachRow -> "JSONEachRow"
  | JSONCompact -> "JSONCompact"
  | JSONCompactEachRow -> "JSONCompactEachRow"
  | Native -> "Native"
  | RowBinary -> "RowBinary"
  | RowBinaryWithNames -> "RowBinaryWithNames"
  | Arrow -> "Arrow"
  | Parquet -> "Parquet"
  | Pretty -> "Pretty"
  | PrettyCompact -> "PrettyCompact"
  | Vertical -> "Vertical"
  | Null -> "Null"
  | Custom s -> s

let pp fmt t = Format.pp_print_string fmt (to_string t)
