(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type property =
  [ `Id
  | `Name
  | `Parent_id
  | `Role
  | `Sort_order
  | `Total_emails
  | `Unread_emails
  | `Total_threads
  | `Unread_threads
  | `My_rights
  | `Is_subscribed ]

let property_to_string : [< property ] -> string = function
  | `Id -> "id"
  | `Name -> "name"
  | `Parent_id -> "parentId"
  | `Role -> "role"
  | `Sort_order -> "sortOrder"
  | `Total_emails -> "totalEmails"
  | `Unread_emails -> "unreadEmails"
  | `Total_threads -> "totalThreads"
  | `Unread_threads -> "unreadThreads"
  | `My_rights -> "myRights"
  | `Is_subscribed -> "isSubscribed"

let property_of_string s : property option =
  match s with
  | "id" -> Some `Id
  | "name" -> Some `Name
  | "parentId" -> Some `Parent_id
  | "role" -> Some `Role
  | "sortOrder" -> Some `Sort_order
  | "totalEmails" -> Some `Total_emails
  | "unreadEmails" -> Some `Unread_emails
  | "totalThreads" -> Some `Total_threads
  | "unreadThreads" -> Some `Unread_threads
  | "myRights" -> Some `My_rights
  | "isSubscribed" -> Some `Is_subscribed
  | _ -> None

module Rights = struct
  type t = {
    may_read_items : bool;
    may_add_items : bool;
    may_remove_items : bool;
    may_set_seen : bool;
    may_set_keywords : bool;
    may_create_child : bool;
    may_rename : bool;
    may_delete : bool;
    may_submit : bool;
  }

  let jsont =
    let kind = "MailboxRights" in
    let make may_read_items may_add_items may_remove_items may_set_seen
        may_set_keywords may_create_child may_rename may_delete may_submit =
      {
        may_read_items;
        may_add_items;
        may_remove_items;
        may_set_seen;
        may_set_keywords;
        may_create_child;
        may_rename;
        may_delete;
        may_submit;
      }
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "mayReadItems" Jsont.bool ~enc:(fun t ->
        t.may_read_items)
    |> Jsont.Object.mem "mayAddItems" Jsont.bool ~enc:(fun t -> t.may_add_items)
    |> Jsont.Object.mem "mayRemoveItems" Jsont.bool ~enc:(fun t ->
        t.may_remove_items)
    |> Jsont.Object.mem "maySetSeen" Jsont.bool ~enc:(fun t -> t.may_set_seen)
    |> Jsont.Object.mem "maySetKeywords" Jsont.bool ~enc:(fun t ->
        t.may_set_keywords)
    |> Jsont.Object.mem "mayCreateChild" Jsont.bool ~enc:(fun t ->
        t.may_create_child)
    |> Jsont.Object.mem "mayRename" Jsont.bool ~enc:(fun t -> t.may_rename)
    |> Jsont.Object.mem "mayDelete" Jsont.bool ~enc:(fun t -> t.may_delete)
    |> Jsont.Object.mem "maySubmit" Jsont.bool ~enc:(fun t -> t.may_submit)
    |> Jsont.Object.finish
end

type special_use = Mail_flag.Mailbox_attr.special_use

type role =
  [ `All
  | `Archive
  | `Drafts
  | `Flagged
  | `Important
  | `Inbox
  | `Junk
  | `Sent
  | `Trash
  | `Snoozed
  | `Scheduled
  | `Memos
  | `Other of string ]

(* RFC 8621 Section 2 has no "subscribed" role, and
   Mail_flag.Mailbox_attr.to_jmap_role agrees. *)
let role_of_special_use : special_use -> role option = function
  | `Subscribed -> None
  | ( `All | `Archive | `Drafts | `Flagged | `Important | `Inbox | `Junk | `Sent
    | `Trash | `Snoozed | `Scheduled | `Memos ) as r ->
      Some (r :> role)

let special_use_of_role : role -> special_use option = function
  | `Other _ -> None
  | ( `All | `Archive | `Drafts | `Flagged | `Important | `Inbox | `Junk | `Sent
    | `Trash | `Snoozed | `Scheduled | `Memos ) as su ->
      Some (su :> special_use)

let role_to_string : role -> string = function
  | `All -> "all"
  | `Archive -> "archive"
  | `Drafts -> "drafts"
  | `Flagged -> "flagged"
  | `Important -> "important"
  | `Inbox -> "inbox"
  | `Junk -> "junk"
  | `Sent -> "sent"
  | `Trash -> "trash"
  | `Snoozed -> "snoozed"
  | `Scheduled -> "scheduled"
  | `Memos -> "memos"
  | `Other s -> s

let role_of_string s : role =
  match
    Option.bind (Mail_flag.Mailbox_attr.of_jmap_role s) role_of_special_use
  with
  | Some r -> r
  | None -> `Other s

let role_jsont =
  Jsont.map ~kind:"MailboxRole" ~dec:role_of_string ~enc:role_to_string
    Jsont.string

type t = {
  id : Proto_id.t option;
  name : string option;
  parent_id : Proto_id.t option;
  role : role option;
  sort_order : int64 option;
  total_emails : int64 option;
  unread_emails : int64 option;
  total_threads : int64 option;
  unread_threads : int64 option;
  my_rights : Rights.t option;
  is_subscribed : bool option;
}

let empty =
  {
    id = None;
    name = None;
    parent_id = None;
    role = None;
    sort_order = None;
    total_emails = None;
    unread_emails = None;
    total_threads = None;
    unread_threads = None;
    my_rights = None;
    is_subscribed = None;
  }

let id t = t.id

(* RFC 8621 Section 2: [name] "MUST be a Net-Unicode string of at least 1
   character in length", and [sortOrder] "MUST be an integer in the range
   0 <= sortOrder < 2^31".  The maximum name length is server policy
   (maxSizeMailboxName in the mail capability), so it is not checked here. *)
let max_sort_order = 0x8000_0000L

let create ~name ?parent_id ?role ?sort_order ?is_subscribed () =
  if String.equal name "" then
    Error "a Mailbox name must be at least 1 character long"
  else if Result.is_error (Proto_json.check_value (Jsont.Json.string name)) then
    Error "a Mailbox name must be a Net-Unicode string"
  else
    match sort_order with
    | Some o when o < 0L || o >= max_sort_order ->
        Error
          (Printf.sprintf
             "sortOrder must be in the range 0 <= sortOrder < 2^31, but is %Ld"
             o)
    | _ ->
        Ok
          {
            empty with
            name = Some name;
            parent_id;
            role;
            sort_order;
            is_subscribed;
          }

let create_exn ~name ?parent_id ?role ?sort_order ?is_subscribed () =
  match create ~name ?parent_id ?role ?sort_order ?is_subscribed () with
  | Ok m -> m
  | Error e -> invalid_arg ("Mail_mailbox.create_exn: " ^ e)

let jsont =
  let kind = "Mailbox" in
  let make id name parent_id role sort_order total_emails unread_emails
      total_threads unread_threads my_rights is_subscribed =
    {
      id;
      name;
      parent_id;
      role;
      sort_order;
      total_emails;
      unread_emails;
      total_threads;
      unread_threads;
      my_rights;
      is_subscribed;
    }
  in
  Jsont.Object.map ~kind make
  |> Jsont.Object.opt_mem "id" Proto_id.jsont ~enc:(fun t -> t.id)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun t -> t.name)
  (* RFC 8621 Section 2 types parentId Id|null and role String|null, null
     meaning the top level and no role, so an omitted member would lose a
     fact the server stated. *)
  |> Proto_json_map.nullable_mem_null "parentId" Proto_id.jsont ~enc:(fun t ->
      t.parent_id)
  |> Proto_json_map.nullable_mem_null "role" role_jsont ~enc:(fun t -> t.role)
  |> Jsont.Object.opt_mem "sortOrder" Proto_int53.Unsigned.jsont ~enc:(fun t ->
      t.sort_order)
  |> Jsont.Object.opt_mem "totalEmails" Proto_int53.Unsigned.jsont
       ~enc:(fun t -> t.total_emails)
  |> Jsont.Object.opt_mem "unreadEmails" Proto_int53.Unsigned.jsont
       ~enc:(fun t -> t.unread_emails)
  |> Jsont.Object.opt_mem "totalThreads" Proto_int53.Unsigned.jsont
       ~enc:(fun t -> t.total_threads)
  |> Jsont.Object.opt_mem "unreadThreads" Proto_int53.Unsigned.jsont
       ~enc:(fun t -> t.unread_threads)
  |> Jsont.Object.opt_mem "myRights" Rights.jsont ~enc:(fun t -> t.my_rights)
  |> Jsont.Object.opt_mem "isSubscribed" Jsont.bool ~enc:(fun t ->
      t.is_subscribed)
  |> Jsont.Object.finish

module Filter_condition = struct
  type t = {
    parent_id : Proto_id.t option option;
    name : string option;
    role : role option option;
    has_any_role : bool option;
    is_subscribed : bool option;
  }

  let empty =
    {
      parent_id = None;
      name = None;
      role = None;
      has_any_role = None;
      is_subscribed = None;
    }

  let jsont =
    let kind = "MailboxFilterCondition" in
    let make parent_id name role has_any_role is_subscribed =
      { parent_id; name; role; has_any_role; is_subscribed }
    in
    (* RFC 8621 Section 2.3: parentId and role are Id|null and String|null,
       so an absent member and an explicit null are two different filters. *)
    let nullable_id = Jsont.(option Proto_id.jsont) in
    let nullable_role = Jsont.(option role_jsont) in
    Jsont.Object.map ~kind make
    |> Jsont.Object.opt_mem "parentId" nullable_id ~enc:(fun f -> f.parent_id)
    |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun f -> f.name)
    |> Jsont.Object.opt_mem "role" nullable_role ~enc:(fun f -> f.role)
    |> Jsont.Object.opt_mem "hasAnyRole" Jsont.bool ~enc:(fun f ->
        f.has_any_role)
    |> Jsont.Object.opt_mem "isSubscribed" Jsont.bool ~enc:(fun f ->
        f.is_subscribed)
    |> Jsont.Object.finish
end

type filter = Filter_condition.t Proto_filter.filter

let filter_jsont = Proto_filter.filter_jsont Filter_condition.jsont

let filter ?parent_id ?name ?role ?has_any_role ?is_subscribed () =
  Proto_filter.Condition
    { Filter_condition.parent_id; name; role; has_any_role; is_subscribed }

type sort_property = [ `Sort_order | `Name ]

let sort ?ascending ?collation p =
  let property = match p with `Sort_order -> "sortOrder" | `Name -> "name" in
  Proto_filter.comparator ?is_ascending:ascending ?collation property

type changes_response = {
  changes : Proto_method.changes_response;
  updated_properties : string list option;
}

let changes_response_jsont =
  let kind = "MailboxChangesResponse" in
  let make account_id old_state new_state has_more_changes created updated
      destroyed updated_properties =
    {
      changes =
        {
          Proto_method.account_id;
          old_state;
          new_state;
          has_more_changes;
          created;
          updated;
          destroyed;
        };
      updated_properties;
    }
  in
  Jsont.Object.map ~kind make
  |> Proto_method.changes_response_mems ~changes:(fun r -> r.changes)
  (* RFC 8621 Section 2.2 has updatedProperties always present, an array or
     null. *)
  |> Proto_json_map.nullable_mem_null "updatedProperties"
       (Jsont.list Jsont.string) ~enc:(fun r -> r.updated_properties)
  |> Jsont.Object.finish

let creation = Proto_id.creation
