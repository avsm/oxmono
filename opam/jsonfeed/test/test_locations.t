Location tracking tests for JSON Feed parser
===========================================

This test suite verifies that jsont combinators correctly track location
information for both valid and invalid JSON feeds.

Valid Feeds
-----------

Test minimal valid feed:
  $ ./test_location_errors.exe data/minimal_valid.json title
  {"status":"ok","field":"title","value":"Minimal Feed"}

  $ ./test_location_errors.exe data/minimal_valid.json version
  {"status":"ok","field":"version","value":"https://jsonfeed.org/version/1.1"}

  $ ./test_location_errors.exe data/minimal_valid.json item_count
  {"status":"ok","field":"item_count","value":"0"}

Test complete feed with all fields:
  $ ./test_location_errors.exe data/complete_valid.json title
  {"status":"ok","field":"title","value":"Complete Feed"}

  $ ./test_location_errors.exe data/complete_valid.json item_count
  {"status":"ok","field":"item_count","value":"1"}

  $ ./test_location_errors.exe data/complete_valid.json first_item_id
  {"status":"ok","field":"first_item_id","value":"https://example.com/item1"}

Test mixed content types:
  $ ./test_location_errors.exe data/mixed_content.json item_count
  {"status":"ok","field":"item_count","value":"3"}

Test feed with extensions:
  $ ./test_location_errors.exe data/with_extensions.json title
  {"status":"ok","field":"title","value":"Feed with Extensions"}


Missing Required Fields
------------------------

Test missing title field:
  $ ./test_location_errors.exe data/missing_title.json title
  {"status":"error","message":"Missing member title in JSON Feed object","location":{"file":"data/missing_title.json","line":1,"column":1,"byte_start":0,"byte_end":65},"context":"$"}
  [1]

Test missing version field:
  $ ./test_location_errors.exe data/missing_version.json title
  {"status":"error","message":"Missing member version in JSON Feed object","location":{"file":"data/missing_version.json","line":1,"column":1,"byte_start":0,"byte_end":51},"context":"$"}
  [1]

Test missing items field:
  $ ./test_location_errors.exe data/missing_items.json title
  {"status":"error","message":"Missing member items in JSON Feed object","location":{"file":"data/missing_items.json","line":1,"column":1,"byte_start":0,"byte_end":83},"context":"$"}
  [1]

Test missing item id:
  $ ./test_location_errors.exe data/missing_item_id.json first_item_id
  {"status":"error","message":"Missing member id in Item object","location":{"file":"data/missing_item_id.json","line":5,"column":5,"byte_start":108,"byte_end":161},"context":"$.items[0]"}
  [1]

Test missing item content:
  $ ./test_location_errors.exe data/missing_item_content.json first_item_id
  {"status":"error","message":"Item must have at least one of content_html or content_text","location":{"file":"-","line":-1,"column":1,"byte_start":-1,"byte_end":-1},"context":"$.items[0]"}
  [1]


Type Errors
-----------

Test wrong type for version (number instead of string):
  $ ./test_location_errors.exe data/wrong_type_version.json title
  {"status":"error","message":"Expected string but found number","location":{"file":"data/wrong_type_version.json","line":2,"column":14,"byte_start":15,"byte_end":15},"context":"$.version"}
  [1]

Test wrong type for items (object instead of array):
  $ ./test_location_errors.exe data/wrong_type_items.json item_count
  {"status":"error","message":"Expected array<Item object> but found object","location":{"file":"data/wrong_type_items.json","line":4,"column":12,"byte_start":102,"byte_end":102},"context":"$.items"}
  [1]

Test wrong type for title (boolean instead of string):
  $ ./test_location_errors.exe data/wrong_type_title.json title
  {"status":"error","message":"Expected string but found bool","location":{"file":"data/wrong_type_title.json","line":3,"column":12,"byte_start":62,"byte_end":62},"context":"$.title"}
  [1]

Test wrong type for expired (string instead of boolean):
  $ ./test_location_errors.exe data/wrong_type_expired.json title
  {"status":"error","message":"Expected bool but found string","location":{"file":"data/wrong_type_expired.json","line":4,"column":14,"byte_start":111,"byte_end":111},"context":"$.expired"}
  [1]


Nested Errors
-------------

Test invalid date format in item:
  $ ./test_location_errors.exe data/invalid_date_format.json first_item_id
  {"status":"error","message":"RFC 3339 timestamp: invalid RFC 3339 timestamp: \"not-a-valid-date\"","location":{"file":"-","line":-1,"column":1,"byte_start":-1,"byte_end":-1},"context":"$.items[0].date_published"}
  [1]

Test invalid author type (string instead of object):
  $ ./test_location_errors.exe data/invalid_author_type.json title
  {"status":"error","message":"Expected Author object but found string","location":{"file":"data/invalid_author_type.json","line":5,"column":5,"byte_start":109,"byte_end":109},"context":"$.authors[0]"}
  [1]

Test invalid attachment field type (deeply nested):
  $ ./test_location_errors.exe data/invalid_nested_attachment.json first_item_id
  {"status":"error","message":"Expected string but found number","location":{"file":"data/invalid_nested_attachment.json","line":11,"column":24,"byte_start":296,"byte_end":296},"context":"$.items[0].attachments[0].mime_type"}
  [1]

Test missing required field in hub:
  $ ./test_location_errors.exe data/invalid_hub_type.json title
  {"status":"error","message":"Missing member url in Hub object","location":{"file":"data/invalid_hub_type.json","line":5,"column":5,"byte_start":103,"byte_end":132},"context":"$.hubs[0]"}
  [1]


JSON Syntax Errors
------------------

Test trailing comma:
  $ ./test_location_errors.exe data/extra_comma.json title
  {"status":"error","message":"Expected object member but found }","location":{"file":"data/extra_comma.json","line":5,"column":1,"byte_start":105,"byte_end":105},"context":"$"}
  [1]

Test malformed JSON (missing comma):
  $ ./test_location_errors.exe data/malformed_json.json title
  {"status":"error","message":"Expected , or } after object member but found: \"","location":{"file":"data/malformed_json.json","line":3,"column":3,"byte_start":52,"byte_end":52},"context":"$"}
  [1]
