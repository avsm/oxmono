let index raw =
  match Ical.one_of_string raw with
  | Error _ -> (raw, false)
  | Ok calendar ->
      let priority =
        [
          "SUMMARY";
          "DTSTART";
          "DTEND";
          "DUE";
          "LOCATION";
          "DESCRIPTION";
          "STATUS";
        ]
      in
      let rec component (c : Ical.Component.t) =
        if c.name = "VTIMEZONE" then []
        else
          let first, rest =
            List.partition
              (fun p -> List.mem (Ical.Property.name p) priority)
              c.properties
          in
          List.map
            (fun p -> Ical.Property.name p ^ ": " ^ Ical.Property.text p)
            (first @ rest)
          @ List.concat_map component c.components
      in
      ( String.concat "\n" (List.concat_map component (Ical.components calendar)),
        true )
