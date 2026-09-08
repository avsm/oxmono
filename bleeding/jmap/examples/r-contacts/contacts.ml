(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Proto = Jmap.Proto
module Results = Jmap.Chain.Results
module Card = Jscontact.Card

let doc = "Create, find, patch and destroy contact cards in an address book"

(* RFC 9553 Section 2.2.1: a Name has a full form, a list of components, or
   both. A component of kind "separator" holds punctuation rather than a word,
   so joining skips it. *)
let display_name (card : Card.t) =
  match card.name with
  | Some { full = Some full; _ } -> full
  | Some { components = Some parts; _ } ->
      let word (c : Jscontact.Name.Component.t) =
        match c.kind with `Separator -> None | _ -> Some c.value
      in
      String.concat " " (List.filter_map word parts)
  | Some _ | None -> "(unnamed)"

let pp_card ppf (c : Proto.Contact_card.t) =
  let emails =
    List.map
      (fun (_, (e : Jscontact.Contact.Email_address.t)) -> e.address)
      (Option.value c.card.emails ~default:[])
  in
  Fmt.pf ppf "%a  %-20s %a"
    Fmt.(option ~none:(any "?") Proto.Id.pp)
    c.id (display_name c.card)
    Fmt.(list ~sep:comma string)
    emails

let refuse what set =
  match Proto.Method.set_failures set with
  | f :: _ ->
      Fmt.failwith "%s: ContactCard/set refused %a" what
        Proto.Method.pp_set_failure f
  | [] -> ()

(* A card the server will accept: RFC 9610 Section 3 requires it to belong to
   at least one AddressBook, and RFC 9553 Section 2.1 to carry a uid. *)
let card ~book ~uid ~given ~surname ~email =
  let name =
    Jscontact.Name.make ~is_ordered:true
      ~components:
        [
          Jscontact.Name.Component.make `Given given;
          Jscontact.Name.Component.make `Surname surname;
        ]
      ()
  in
  let emails =
    [ (Jscontact.Id.v "e1", Jscontact.Contact.Email_address.make email) ]
  in
  Proto.Contact_card.make
    ~address_book_ids:[ (book, true) ]
    (Card.make ~name ~emails uid)

let () =
  Jmap_eio.Cli.main "contacts" ~doc ~capability:Proto.Capability.contacts
  @@ fun ctx ->
  let client = ctx.client and account_id = ctx.account_id in

  (* Section 2.1: ids may be null, which fetches every AddressBook at once. *)
  let books = Client.call_exn client (Chain.address_book_get ~account_id ()) in
  Fmt.pr "@[<v>Address books, state %s@," books.state;
  List.iter
    (fun (b : Proto.Address_book.t) ->
      Fmt.pr "  %a %s%s@,"
        Fmt.(option ~none:(any "?") Proto.Id.pp)
        b.id
        (Option.value b.name ~default:"(unnamed)")
        (if b.is_default = Some true then " [default]" else ""))
    books.list;
  Fmt.pr "@]@.";
  let book =
    match
      List.find_opt
        (fun (b : Proto.Address_book.t) -> b.is_default = Some true)
        books.list
    with
    | Some b -> Option.get b.id
    | None -> Fmt.failwith "the account has no default address book"
  in

  (* Two cards in one request, each named by a creation id. *)
  let uid n = "urn:uuid:example-r-contacts-" ^ n in
  let ada = Proto.Contact_card.creation "ada" in
  let grace = Proto.Contact_card.creation "grace" in
  let created =
    Client.call_exn client
      (Chain.contact_card_set ~account_id
         ~create:
           [
             ( ada,
               card ~book ~uid:(uid "ada") ~given:"Ada" ~surname:"Lovelace"
                 ~email:"ada@example.com" );
             ( grace,
               card ~book ~uid:(uid "grace") ~given:"Grace" ~surname:"Hopper"
                 ~email:"grace@example.com" );
           ]
         ())
  in
  refuse "creating" created;
  let id_of creation =
    match Proto.Method.created created creation with
    | Some (c : Proto.Contact_card.t) -> Option.get c.id
    | None -> Fmt.failwith "a card was not created"
  in
  let ada_id = id_of ada and grace_id = id_of grace in
  Fmt.pr "Created 2 cards, state %s -> %s@."
    (Option.value created.old_state ~default:"?")
    created.new_state;

  let destroy () =
    let gone =
      Client.call_exn client
        (Chain.contact_card_set ~account_id
           ~destroy:(Chain.ids [ ada_id; grace_id ])
           ())
    in
    refuse "destroying" gone;
    Fmt.pr "Destroyed %d card(s)@."
      (List.length (Option.value gone.destroyed ~default:[]))
  in
  Fun.protect ~finally:destroy @@ fun () ->
  (* Section 3.3: a query and the get that reads its ids, in one request. *)
  let Results.[ query; cards ] =
    Client.run_exn client
      Chain.(
        let* q =
          contact_card_query ~account_id
            ~filter:(Proto.Contact_card.filter ~in_address_book:book ())
            ~sort:
              [
                Proto.Filter.comparator
                  (Proto.Contact_card.Sort.to_string `Name_surname);
              ]
            ~calculate_total:true ()
        in
        let+ g = contact_card_get ~account_id ~ids:(from_query q) () in
        Handles.[ q; g ])
  in
  Fmt.pr "@[<v>The book holds %a card(s)@,%a@]@."
    Fmt.(option ~none:(any "?") int64)
    query.total
    Fmt.(list ~sep:cut pp_card)
    cards.list;

  (* RFC 8620 Section 5.1: a properties argument returns those alone, so the
     @type, version and uid RFC 9553 makes mandatory all go missing and the
     card that comes back is not a whole Card. *)
  let partial =
    Client.call_exn client
      (Chain.contact_card_get ~account_id ~ids:(Chain.ids [ ada_id ])
         ~properties:[ "name" ] ())
  in
  let one = List.hd partial.list in
  Fmt.pr "Truncated fetch: name %S, uid %S, emails %d@." (display_name one.card)
    one.card.uid
    (List.length (Option.value one.card.emails ~default:[]));
  if one.card.uid <> "" then Fmt.failwith "a truncated card should have no uid";

  (* A PatchObject over one property, guarded by the state the read saw. RFC
     8620 Section 5.3 requires every reference token before the last to exist
     already, and this card has no nicknames yet, so the patch replaces the
     whole property rather than reaching into it. *)
  let patched =
    Client.call_exn client
      (Chain.contact_card_set ~account_id ~if_in_state:created.new_state
         ~update:
           [
             ( ada_id,
               Proto.Patch.v
                 [
                   Proto.Patch.set_field "nicknames"
                     (Jsont.Json.object'
                        [
                          Jsont.Json.mem (Jsont.Json.name "n1")
                            (Jsont.Json.object'
                               [
                                 Jsont.Json.mem (Jsont.Json.name "name")
                                   (Jsont.Json.string "Countess");
                               ]);
                        ]);
                 ] );
           ]
         ())
  in
  refuse "patching" patched;
  let after =
    Client.call_exn client
      (Chain.contact_card_get ~account_id ~ids:(Chain.ids [ ada_id ]) ())
  in
  let nicknames =
    List.map
      (fun (_, (n : Jscontact.Name.Nickname.t)) -> n.name)
      (Option.value (List.hd after.list).card.nicknames ~default:[])
  in
  Fmt.pr "Nicknames after the patch: %a@."
    Fmt.(list ~sep:comma string)
    nicknames;
  if nicknames <> [ "Countess" ] then
    Fmt.failwith "the nickname patch did not take"
