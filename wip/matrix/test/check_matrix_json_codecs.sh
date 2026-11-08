#!/bin/sh
set -eu

root=${1:-.}

if grep -rnE \
  --include='*.ml' \
  --include='*.mli' \
  --exclude='matrix_json.ml' \
  --exclude='matrix_json.mli' \
  --exclude='matrix_event.ml' \
  --exclude='matrix_event.mli' \
  --exclude='json_codec.ml' \
  --exclude='json_codec.mli' \
  'Jsont\.(int|int64|int32|int16|int8|uint16|uint8|number|string)\b|Jsont\.Object\.(as_string_map|Mems\.string_map)' \
  "$root/lib" "$root/bin" "$root/test"
then
  echo "Matrix codecs must use Matrix_proto.Json.Codec checked primitives" >&2
  exit 1
fi

if grep -rnE \
  --include='*.ml' \
  --include='*.mli' \
  --exclude='matrix_event.ml' \
  'Unsigned\.persisted_jsont\b' \
  "$root/lib" "$root/bin" "$root/test"
then
  echo "The legacy unsigned-event codec is internal to persisted raw events" >&2
  exit 1
fi

if grep -rnE \
  --include='*.ml' \
  --include='*.mli' \
  --exclude='store.ml' \
  --exclude='sliding_sync_state.ml' \
  --exclude='thread_info.ml' \
  'Raw_event\.persisted_jsont\b' \
  "$root/lib" "$root/bin" "$root/test"
then
  echo "The legacy raw-event codec is restricted to on-disk schemas" >&2
  exit 1
fi

if grep -rnE \
  --include='*.ml' \
  --include='*.mli' \
  --exclude='read_state.ml' \
  --exclude='store.ml' \
  --exclude='send_queue.ml' \
  'Json_codec\.persisted_timestamp\b' \
  "$root/lib" "$root/bin" "$root/test"
then
  echo "The legacy timestamp codec is restricted to on-disk schemas" >&2
  exit 1
fi

# The permissive codecs deliberately preserve old on-disk formats. Keep the
# allowlist explicit so a new Matrix wire codec cannot silently accept numeric
# strings or integers outside the interoperable range.
if grep -rnE \
  --include='*.ml' \
  --include='*.mli' \
  --exclude='matrix_json.ml' \
  --exclude='matrix_json.mli' \
  --exclude='json_codec.ml' \
  --exclude='json_codec.mli' \
  --exclude='session_pickle.ml' \
  --exclude='session.ml' \
  --exclude='store.ml' \
  --exclude='crypto_store.ml' \
  --exclude='send_queue.ml' \
  --exclude='sliding_sync_state.ml' \
  --exclude='thread_subscriptions.ml' \
  --exclude='thread_info.ml' \
  --exclude='thread_cache.ml' \
  'Matrix_proto\.Json\.Codec\.Legacy\.(int|int64)\b' \
  "$root/lib" "$root/bin" "$root/test"
then
  echo "Legacy integer codecs are restricted to reviewed persistence modules" >&2
  exit 1
fi
