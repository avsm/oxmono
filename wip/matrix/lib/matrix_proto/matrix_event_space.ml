module Space_child_content = struct
  type t = {
    via : string list option;
    order : string option;
    suggested : bool option;
  }

  let make ?via ?order ?suggested () = { via; order; suggested }
  let via t = t.via
  let order t = t.order
  let suggested t = t.suggested

  let pp ppf t =
    Format.fprintf ppf "@[<v>";
    (match t.via with
    | Some v -> Format.fprintf ppf "via: [%s]@," (String.concat ", " v)
    | None -> ());
    (match t.order with
    | Some o -> Format.fprintf ppf "order: %s@," o
    | None -> ());
    (match t.suggested with
    | Some s -> Format.fprintf ppf "suggested: %b@," s
    | None -> ());
    Format.fprintf ppf "@]"

  let jsont =
    Jsont.Object.(
      map (fun via order suggested -> { via; order; suggested })
      |> opt_mem "via" (Jsont.list Matrix_json.Codec.string) ~enc:(fun t ->
          t.via)
      |> opt_mem "order" Matrix_json.Codec.string ~enc:(fun t -> t.order)
      |> opt_mem "suggested" Jsont.bool ~enc:(fun t -> t.suggested)
      |> finish)
end

module Space_parent_content = struct
  type t = { via : string list option; canonical : bool option }

  let make ?via ?canonical () = { via; canonical }
  let via t = t.via
  let canonical t = t.canonical

  let pp ppf t =
    Format.fprintf ppf "@[<v>";
    (match t.via with
    | Some v -> Format.fprintf ppf "via: [%s]@," (String.concat ", " v)
    | None -> ());
    (match t.canonical with
    | Some c -> Format.fprintf ppf "canonical: %b@," c
    | None -> ());
    Format.fprintf ppf "@]"

  let jsont =
    Jsont.Object.(
      map (fun via canonical -> { via; canonical })
      |> opt_mem "via" (Jsont.list Matrix_json.Codec.string) ~enc:(fun t ->
          t.via)
      |> opt_mem "canonical" Jsont.bool ~enc:(fun t -> t.canonical)
      |> finish)
end
