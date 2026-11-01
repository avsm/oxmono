let of_mtime mtime =
  let seconds=Float.floor mtime in
  if not (Float.is_finite seconds) || seconds < -1e12 || seconds > 1e12 then
    Error (Printf.sprintf
      "Maildir modification time %g is outside the supported range" mtime)
  else Imap.Internal_date.of_unix_seconds (Int64.of_float seconds)

let of_occurrence (o:Maildir.occurrence) = of_mtime o.mtime

let to_mtime date =
  Result.map Int64.to_float (Imap.Internal_date.to_unix_seconds date)
