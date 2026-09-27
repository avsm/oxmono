type t = {
  day : int; month : int; year : int;
  hour : int; minute : int; second : int;
  zone_sign : char; zone_hour : int; zone_minute : int;
}

let months = [|
  "Jan";"Feb";"Mar";"Apr";"May";"Jun";
  "Jul";"Aug";"Sep";"Oct";"Nov";"Dec"
|]

let digits s start count =
  let rec loop i value =
    if i=count then Some value else
    let c=s.[start+i] in
    if c<'0' || c>'9' then None
    else loop (i+1) (value*10+Char.code c-Char.code '0') in
  loop 0 0

let leap year = year mod 4=0 && (year mod 100<>0 || year mod 400=0)
let days_in_month year = function
  | 2 -> if leap year then 29 else 28
  | 4 | 6 | 9 | 11 -> 30
  | _ -> 31

let zone_offset_minutes ~sign ~hour ~minute =
  let offset=hour*60+minute in
  if sign='+' then offset else -offset

(* A leap second is inserted only after 23:59:59 UTC. *)
let valid_second ~hour ~minute ~second ~sign ~zone_hour ~zone_minute =
  second<=59 ||
  (second=60 &&
   let utc=hour*60+minute -
     zone_offset_minutes ~sign ~hour:zone_hour ~minute:zone_minute in
   ((utc mod 1440)+1440) mod 1440 = 1439)

let of_string s =
  if String.length s<>26 || s.[2]<>'-' || s.[6]<>'-' ||
     s.[11]<>' ' || s.[14]<>':' || s.[17]<>':' || s.[20]<>' ' ||
     (s.[21]<>'+' && s.[21]<>'-') then
    Error "invalid IMAP date-time syntax"
  else
    let day=if s.[0]=' ' then digits s 1 1 else digits s 0 2 in
    let month=String.sub s 3 3 |> String.lowercase_ascii in
    let month=Array.find_index (fun name ->
      String.lowercase_ascii name=month) months in
    match day,month,digits s 7 4,digits s 12 2,digits s 15 2,
          digits s 18 2,digits s 22 2,digits s 24 2 with
    | Some day,Some month,Some year,Some hour,Some minute,
      Some second,Some zone_hour,Some zone_minute
      when year>=1 && day>=1 && day<=days_in_month year (month+1) &&
           hour<=23 && minute<=59 &&
           zone_hour<=23 && zone_minute<=59 &&
           valid_second ~hour ~minute ~second ~sign:s.[21] ~zone_hour
             ~zone_minute ->
        Ok {day;month=month+1;year;hour;minute;second;
            zone_sign=s.[21];zone_hour;zone_minute}
    | _ -> Error "invalid IMAP date-time value"

let to_string t =
  Printf.sprintf "%2d-%s-%04d %02d:%02d:%02d %c%02d%02d"
    t.day months.(t.month-1) t.year t.hour t.minute t.second
    t.zone_sign t.zone_hour t.zone_minute

let to_wire t = "\"" ^ to_string t ^ "\""

let days_before_year year =
  let previous=year-1 in
  previous*365 + previous/4 - previous/100 + previous/400

let days_before_month year month =
  let cumulative=[|0;31;59;90;120;151;181;212;243;273;304;334|] in
  cumulative.(month-1) + if month>2 && leap year then 1 else 0

let second_count t =
  let days=days_before_year t.year +
    days_before_month t.year t.month + t.day-1 in
  let local=Int64.of_int
    (days*86_400 + t.hour*3600 + t.minute*60 + t.second) in
  let offset=60*zone_offset_minutes ~sign:t.zone_sign ~hour:t.zone_hour
    ~minute:t.zone_minute in
  Int64.sub local (Int64.of_int offset)

let equal_instant a b =
  (* A leap second has no unique representation in ordinary POSIX time.
     Avoid equating it with the next minute's first second. *)
  (a.second=b.second || (a.second<>60 && b.second<>60)) &&
  second_count a=second_count b

let unix_epoch_days=Int64.of_int (days_before_year 1970)
let min_unix_seconds=Int64.mul (Int64.neg unix_epoch_days) 86_400L
let max_unix_seconds=Int64.pred (Int64.mul
  (Int64.sub (Int64.of_int (days_before_year 10000)) unix_epoch_days) 86_400L)

let of_unix_seconds seconds =
  if seconds<min_unix_seconds || seconds>max_unix_seconds then
    Error "Unix timestamp is outside the IMAP date-time year range"
  else
    let seconds=Int64.sub seconds min_unix_seconds in
    let serial=Int64.to_int (Int64.div seconds 86_400L) in
    let remaining=Int64.to_int (Int64.rem seconds 86_400L) in
    let rec year_between low high =
      if high-low=1 then low else
      let middle=(low+high)/2 in
      if days_before_year middle<=serial then
        year_between middle high else year_between low middle in
    let year=year_between 1 10000 in
    let day_of_year=serial-days_before_year year in
    let rec month_of_day month =
      if month=12 || day_of_year<days_before_month year (month+1)
      then month else month_of_day (month+1) in
    let month=month_of_day 1 in
    let day=day_of_year-days_before_month year month+1 in
    Ok {year;month;day;hour=remaining/3600;
      minute=(remaining mod 3600)/60;second=remaining mod 60;
      zone_sign='+';zone_hour=0;zone_minute=0}

let to_unix_seconds t =
  if t.second=60 then Error "leap seconds cannot be represented in POSIX time"
  else
    let seconds=Int64.add (second_count t) min_unix_seconds in
    if seconds<min_unix_seconds || seconds>max_unix_seconds then
      Error "date-time is outside the IMAP date-time year range in UTC"
    else Ok seconds
