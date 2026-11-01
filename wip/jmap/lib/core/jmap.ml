(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Proto = struct
  module Id = Proto_id
  module Int53 = Proto_int53
  module Json = Proto_json
  module Date = Proto_date
  module Json_map = Proto_json_map
  module Template = Proto_template
  module Unknown = Proto_unknown
  module Error = Proto_error
  module Capability = Proto_capability
  module Filter = Proto_filter
  module Method = Proto_method
  module Patch = Proto_patch
  module Invocation = Proto_invocation
  module Request = Proto_request
  module Response = Proto_response
  module Session = Proto_session
  module Push = Proto_push
  module Blob = Proto_blob
  module Keyword = Mail_keyword
  module Email_address = Mail_address
  module Email_header = Mail_header
  module Email_body = Mail_body
  module Mailbox = Mail_mailbox
  module Thread = Mail_thread
  module Email = Mail_email
  module Search_snippet = Mail_snippet
  module Identity = Mail_identity
  module Submission = Mail_submission
  module Vacation = Mail_vacation
  module Address_book = Contacts_addressbook
  module Contact_card = Contacts_card
  module Calendar_types = Calendar_types
  module Calendar = Calendar_calendar
  module Calendar_event = Calendar_event
  module Participant_identity = Calendar_participant_identity
end

module Chain = Chain

module Mirror = Mirror
