#!/bin/sh
set -eu

root=${1:-.}

if rg -n \
  --glob '*.ml' \
  --glob '*.mli' \
  --glob '!matrix_json.ml' \
  --glob '!matrix_json.mli' \
  --glob '!matrix_event.ml' \
  --glob '!matrix_event.mli' \
  --glob '!json_codec.ml' \
  --glob '!json_codec.mli' \
  'Jsont\.(int|int64|int32|int16|int8|uint16|uint8|number|string)\b|Jsont\.Object\.(as_string_map|Mems\.string_map)' \
  "$root/lib" "$root/bin" "$root/test"
then
  echo "Matrix codecs must use Matrix_proto.Json.Codec checked primitives" >&2
  exit 1
fi

if rg -n \
  --glob '*.ml' \
  --glob '*.mli' \
  --glob '!matrix_event.ml' \
  'Unsigned\.persisted_jsont\b' \
  "$root/lib" "$root/bin" "$root/test"
then
  echo "The legacy unsigned-event codec is internal to persisted raw events" >&2
  exit 1
fi

if rg -n \
  --glob '*.ml' \
  --glob '*.mli' \
  --glob '!store.ml' \
  --glob '!sliding_sync_state.ml' \
  --glob '!thread_info.ml' \
  'Raw_event\.persisted_jsont\b' \
  "$root/lib" "$root/bin" "$root/test"
then
  echo "The legacy raw-event codec is restricted to on-disk schemas" >&2
  exit 1
fi

if rg -n \
  --glob '*.ml' \
  --glob '*.mli' \
  --glob '!read_state.ml' \
  --glob '!store.ml' \
  --glob '!send_queue.ml' \
  'Json_codec\.persisted_timestamp\b' \
  "$root/lib" "$root/bin" "$root/test"
then
  echo "The legacy timestamp codec is restricted to on-disk schemas" >&2
  exit 1
fi

# The permissive codecs deliberately preserve old on-disk formats. Keep the
# allowlist explicit so a new Matrix wire codec cannot silently accept numeric
# strings or integers outside the interoperable range.
if rg -n \
  --glob '*.ml' \
  --glob '*.mli' \
  --glob '!matrix_json.ml' \
  --glob '!matrix_json.mli' \
  --glob '!json_codec.ml' \
  --glob '!json_codec.mli' \
  --glob '!session_pickle.ml' \
  --glob '!session.ml' \
  --glob '!store.ml' \
  --glob '!crypto_store.ml' \
  --glob '!send_queue.ml' \
  --glob '!sliding_sync_state.ml' \
  --glob '!thread_subscriptions.ml' \
  --glob '!thread_info.ml' \
  --glob '!thread_cache.ml' \
  'Matrix_proto\.Json\.Codec\.Legacy\.(int|int64)\b' \
  "$root/lib" "$root/bin" "$root/test"
then
  echo "Legacy integer codecs are restricted to reviewed persistence modules" >&2
  exit 1
fi
