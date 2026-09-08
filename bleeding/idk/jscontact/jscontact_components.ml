(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let rec has_consecutive p = function
  | a :: (b :: _ as rest) -> (p a && p b) || has_consecutive p rest
  | _ -> false

let validate ~is_separator ~has_phonetic ~is_ordered ~default_separator
    ~has_phonetic_system components =
  Jscontact_valid.(
    match components with
    | None ->
        check
          (Option.is_none default_separator)
          "defaultSeparator: must not be set if components is not set"
    | Some cs ->
        let* () =
          check
            (List.exists (fun c -> not (is_separator c)) cs)
            "components: no entry has a kind other than \"separator\""
        in
        let* () =
          check
            (not (has_consecutive is_separator cs))
            "components: two \"separator\" components must not be consecutive"
        in
        let* () =
          if is_ordered then ok ()
          else
            let* () =
              check
                (not (List.exists is_separator cs))
                "components: a \"separator\" component must not be set if \
                 isOrdered is false"
            in
            check
              (Option.is_none default_separator)
              "defaultSeparator: must not be set if isOrdered is false"
        in
        check
          (has_phonetic_system || not (List.exists has_phonetic cs))
          "components: a phonetic is set but neither phoneticScript nor \
           phoneticSystem is")
