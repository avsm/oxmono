module Sdp = struct
  type t = { type_ : string; sdp : string }

  let v ~type_ ~sdp = { type_; sdp }

  let pp ppf t =
    Format.fprintf ppf "@[sdp %s (%d bytes)@]" t.type_ (String.length t.sdp)

  let jsont =
    Jsont.Object.(
      map (fun type_ sdp -> { type_; sdp })
      |> mem "type" Matrix_json.Codec.string ~enc:(fun t -> t.type_)
      |> mem "sdp" Matrix_json.Codec.string ~enc:(fun t -> t.sdp)
      |> finish)
end

module Hangup_reason = struct
  type t =
    | Ice_failed
    | Invite_timeout
    | User_hangup
    | User_media_failed
    | User_busy
    | Unknown_error

  let to_string = function
    | Ice_failed -> "ice_failed"
    | Invite_timeout -> "invite_timeout"
    | User_hangup -> "user_hangup"
    | User_media_failed -> "user_media_failed"
    | User_busy -> "user_busy"
    | Unknown_error -> "unknown_error"

  let of_string = function
    | "ice_failed" -> Ok Ice_failed
    | "invite_timeout" -> Ok Invite_timeout
    | "user_hangup" -> Ok User_hangup
    | "user_media_failed" -> Ok User_media_failed
    | "user_busy" -> Ok User_busy
    | "unknown_error" -> Ok Unknown_error
    | s -> Error (`Msg (Printf.sprintf "unknown hangup reason %S" s))

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.enum
      [
        ("ice_failed", Ice_failed);
        ("invite_timeout", Invite_timeout);
        ("user_hangup", User_hangup);
        ("user_media_failed", User_media_failed);
        ("user_busy", User_busy);
        ("unknown_error", Unknown_error);
      ]
end

module Call_invite_content = struct
  type t = {
    call_id : string;
    party_id : string option;
    version : int;
    lifetime : int;
    offer : Sdp.t;
    invitee : string option;
  }

  let make ~call_id ?party_id ?(version = 0) ~lifetime ~offer ?invitee () =
    { call_id; party_id; version; lifetime; offer; invitee }

  let call_id t = t.call_id
  let party_id t = t.party_id
  let version t = t.version
  let lifetime t = t.lifetime
  let offer t = t.offer
  let invitee t = t.invitee

  let pp ppf t =
    Format.fprintf ppf "@[<v>call_id: %s@,version: %d@,lifetime: %d@]" t.call_id
      t.version t.lifetime

  let jsont =
    Jsont.Object.(
      map (fun call_id party_id version lifetime offer invitee ->
          { call_id; party_id; version; lifetime; offer; invitee })
      |> mem "call_id" Matrix_json.Codec.string ~enc:(fun t -> t.call_id)
      |> opt_mem "party_id" Matrix_json.Codec.string ~enc:(fun t -> t.party_id)
      |> mem "version" Matrix_json.Codec.int
           ~dec_absent:(fun () -> 0)
           ~enc:(fun t -> t.version)
      |> mem "lifetime" Matrix_json.Codec.int ~enc:(fun t -> t.lifetime)
      |> mem "offer" Sdp.jsont ~enc:(fun t -> t.offer)
      |> opt_mem "invitee" Matrix_json.Codec.string ~enc:(fun t -> t.invitee)
      |> finish)
end

module Call_answer_content = struct
  type t = {
    call_id : string;
    party_id : string option;
    version : int;
    answer : Sdp.t;
  }

  let make ~call_id ?party_id ?(version = 0) ~answer () =
    { call_id; party_id; version; answer }

  let call_id t = t.call_id
  let party_id t = t.party_id
  let version t = t.version
  let answer t = t.answer

  let pp ppf t =
    Format.fprintf ppf "@[<v>call_id: %s@,version: %d@]" t.call_id t.version

  let jsont =
    Jsont.Object.(
      map (fun call_id party_id version answer ->
          { call_id; party_id; version; answer })
      |> mem "call_id" Matrix_json.Codec.string ~enc:(fun t -> t.call_id)
      |> opt_mem "party_id" Matrix_json.Codec.string ~enc:(fun t -> t.party_id)
      |> mem "version" Matrix_json.Codec.int
           ~dec_absent:(fun () -> 0)
           ~enc:(fun t -> t.version)
      |> mem "answer" Sdp.jsont ~enc:(fun t -> t.answer)
      |> finish)
end

module Call_hangup_content = struct
  type t = {
    call_id : string;
    party_id : string option;
    version : int;
    reason : Hangup_reason.t option;
  }

  let make ~call_id ?party_id ?(version = 0) ?reason () =
    { call_id; party_id; version; reason }

  let call_id t = t.call_id
  let party_id t = t.party_id
  let version t = t.version
  let reason t = t.reason

  let pp ppf t =
    Format.fprintf ppf "@[<v>call_id: %s@,version: %d" t.call_id t.version;
    (match t.reason with
    | Some r -> Format.fprintf ppf "@,reason: %a" Hangup_reason.pp r
    | None -> ());
    Format.fprintf ppf "@]"

  let jsont =
    Jsont.Object.(
      map (fun call_id party_id version reason ->
          { call_id; party_id; version; reason })
      |> mem "call_id" Matrix_json.Codec.string ~enc:(fun t -> t.call_id)
      |> opt_mem "party_id" Matrix_json.Codec.string ~enc:(fun t -> t.party_id)
      |> mem "version" Matrix_json.Codec.int
           ~dec_absent:(fun () -> 0)
           ~enc:(fun t -> t.version)
      |> opt_mem "reason" Hangup_reason.jsont ~enc:(fun t -> t.reason)
      |> finish)
end

module Call_candidates_content = struct
  type candidate = {
    candidate : string;
    sdp_mid : string;
    sdp_m_line_index : int;
  }

  let make_candidate ~candidate ~sdp_mid ~sdp_m_line_index =
    { candidate; sdp_mid; sdp_m_line_index }

  let candidate_jsont =
    Jsont.Object.(
      map (fun candidate sdp_mid sdp_m_line_index ->
          { candidate; sdp_mid; sdp_m_line_index })
      |> mem "candidate" Matrix_json.Codec.string ~enc:(fun t -> t.candidate)
      |> mem "sdpMid" Matrix_json.Codec.string ~enc:(fun t -> t.sdp_mid)
      |> mem "sdpMLineIndex" Matrix_json.Codec.int ~enc:(fun t ->
          t.sdp_m_line_index)
      |> finish)

  type t = {
    call_id : string;
    party_id : string option;
    version : int;
    candidates : candidate list;
  }

  let make ~call_id ?party_id ?(version = 0) ?(candidates = []) () =
    { call_id; party_id; version; candidates }

  let call_id t = t.call_id
  let party_id t = t.party_id
  let version t = t.version
  let candidates t = t.candidates

  let pp ppf t =
    Format.fprintf ppf "@[<v>call_id: %s@,version: %d@,candidates: %d@]"
      t.call_id t.version (List.length t.candidates)

  let jsont =
    Jsont.Object.(
      map (fun call_id party_id version candidates ->
          { call_id; party_id; version; candidates })
      |> mem "call_id" Matrix_json.Codec.string ~enc:(fun t -> t.call_id)
      |> opt_mem "party_id" Matrix_json.Codec.string ~enc:(fun t -> t.party_id)
      |> mem "version" Matrix_json.Codec.int
           ~dec_absent:(fun () -> 0)
           ~enc:(fun t -> t.version)
      |> mem "candidates"
           (Jsont.list candidate_jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.candidates)
      |> finish)
end

module Call_member_content = struct
  type focus = {
    type_ : string;
    livekit_service_url : string option;
    livekit_alias : string option;
  }

  let make_focus ~type_ ?livekit_service_url ?livekit_alias () =
    { type_; livekit_service_url; livekit_alias }

  let focus_jsont =
    Jsont.Object.(
      map (fun type_ livekit_service_url livekit_alias ->
          { type_; livekit_service_url; livekit_alias })
      |> mem "type" Matrix_json.Codec.string ~enc:(fun t -> t.type_)
      |> opt_mem "livekit_service_url" Matrix_json.Codec.string ~enc:(fun t ->
          t.livekit_service_url)
      |> opt_mem "livekit_alias" Matrix_json.Codec.string ~enc:(fun t ->
          t.livekit_alias)
      |> finish)

  type membership = {
    call_id : string;
    scope : string;
    application : string;
    device_id : string;
    expires : int64;
    foci_active : focus list option;
    membership_id : string option;
  }

  let make_membership ~call_id ?(scope = "m.room") ~application ~device_id
      ~expires ?foci_active ?membership_id () =
    {
      call_id;
      scope;
      application;
      device_id;
      expires;
      foci_active;
      membership_id;
    }

  let membership_jsont =
    Jsont.Object.(
      map
        (fun
          call_id
          scope
          application
          device_id
          expires
          foci_active
          membership_id
        ->
          {
            call_id;
            scope;
            application;
            device_id;
            expires;
            foci_active;
            membership_id;
          })
      |> mem "call_id" Matrix_json.Codec.string ~enc:(fun t -> t.call_id)
      |> mem "scope" Matrix_json.Codec.string
           ~dec_absent:(fun () -> "m.room")
           ~enc:(fun t -> t.scope)
      |> mem "application" Matrix_json.Codec.string ~enc:(fun t ->
          t.application)
      |> mem "device_id" Matrix_json.Codec.string ~enc:(fun t -> t.device_id)
      |> mem "expires" Matrix_json.Codec.int64 ~enc:(fun t -> t.expires)
      |> opt_mem "foci_active" (Jsont.list focus_jsont) ~enc:(fun t ->
          t.foci_active)
      |> opt_mem "membership_id" Matrix_json.Codec.string ~enc:(fun t ->
          t.membership_id)
      |> finish)

  type t = { memberships : membership list }

  let make ?(memberships = []) () = { memberships }
  let memberships t = t.memberships

  let pp ppf t =
    Format.fprintf ppf "@[<v>memberships: %d entries@]"
      (List.length t.memberships)

  let jsont =
    Jsont.Object.(
      map (fun memberships -> { memberships })
      |> mem "memberships"
           (Jsont.list membership_jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.memberships)
      |> finish)
end
