# HTTP/2 Connection Pooling Integration Plan

## Problem Statement

The current architecture has two separate connection management systems that conflict:

1. **Conpool**: Manages TCP/TLS connections with one-connection-per-request semantics
2. **H2_adapter**: Has its own hashtable cache for HTTP/2 client state

This creates several issues:

### Issue 1: Duplicate Connection Management

```
Current Flow:

Requests.make_request
    │
    ├── Conpool.connection_with_info  ──────┐
    │        (gets TCP/TLS connection)      │
    │                                       │
    │   ┌─────────────────────────────────┐ │
    │   │ If ALPN = "h2":                 │ │
    │   │   H2_adapter.request            │◄┘
    │   │     │                           │
    │   │     ├── Own hashtable cache     │  ← Duplicates Conpool!
    │   │     └── get_or_create_client    │
    │   └─────────────────────────────────┘
    │
    └── When switch closes → Connection returned to Conpool
                             BUT H2_adapter cache still references it!
```

### Issue 2: No True Multiplexing

HTTP/2's key advantage is stream multiplexing - multiple concurrent requests on one connection. Currently:

- Each `Requests.get/post/...` call gets a fresh connection from Conpool
- Even though HTTP/2 could handle multiple streams on one connection
- We pay full connection setup cost per request

### Issue 3: Race Conditions

- `H2_adapter` uses `Mutex.t` (Unix/pthreads mutex)
- This blocks the entire OS thread in Eio, breaking cooperative scheduling
- Should use `Eio.Mutex.t` for proper Eio integration

### Issue 4: Connection Lifecycle Mismatch

- Conpool expects: get connection → use → release back to pool
- HTTP/2 expects: establish connection → keep open → multiplex many requests → eventually close
- These models are fundamentally different

---

## Proposed Solution: Protocol-Aware Connection Abstraction

### Design Goals

1. **Unified API**: Users don't need to know if they're using HTTP/1.1 or HTTP/2
2. **True HTTP/2 Multiplexing**: Multiple concurrent requests share one connection
3. **Eio-Native**: Use Eio concurrency primitives throughout
4. **Backward Compatible**: Existing Requests API unchanged
5. **Efficient Resource Use**: Minimize connection count for HTTP/2

### Architecture Overview

```
                         Requests Session
                               │
                               ▼
                    ┌────────────────────┐
                    │   Protocol Router   │
                    │  (chooses handler)  │
                    └─────────┬──────────┘
                              │
              ┌───────────────┴───────────────┐
              │                               │
              ▼                               ▼
   ┌─────────────────────┐       ┌─────────────────────┐
   │  HTTP/1.1 Handler   │       │   HTTP/2 Handler    │
   │                     │       │                     │
   │  Uses Conpool as-is │       │  H2_connection_pool │
   │  (1 conn = 1 req)   │       │  (1 conn = N reqs)  │
   └─────────────────────┘       └─────────────────────┘
              │                               │
              ▼                               ▼
        ┌──────────┐                 ┌────────────────┐
        │ Conpool  │                 │ H2_connection  │
        │ (TCP/TLS)│                 │ (multiplexed)  │
        └──────────┘                 └────────────────┘
```

### Key Components

#### 1. `H2_connection_pool` - HTTP/2 Connection Manager

A new module that manages HTTP/2 connections with proper multiplexing:

```ocaml
(** HTTP/2 Connection Pool.

    Unlike Conpool which manages one-request-per-connection for HTTP/1.1,
    this module manages long-lived HTTP/2 connections with stream multiplexing.

    Each endpoint (host:port) has at most one HTTP/2 connection with multiple
    streams. When MAX_CONCURRENT_STREAMS is reached, requests queue until
    a stream slot becomes available. *)

type t

val create : sw:Eio.Switch.t -> clock:_ Eio.Time.clock -> unit -> t
(** Create a new HTTP/2 connection pool. *)

type endpoint = {
  host : string;
  port : int;
}

val request :
  t ->
  endpoint:endpoint ->
  establish:(unit -> Eio.Flow.two_way_ty Eio.Resource.t) ->
  meth:Method.t ->
  uri:Uri.t ->
  headers:Headers.t ->
  body:Body.t ->
  Response.t
(** Make an HTTP/2 request, multiplexing on existing connection if available.

    @param establish Function to create new TCP/TLS connection if needed.
                     This is called at most once per endpoint.
    @raise Error.H2_protocol_error on protocol errors *)
```

#### 2. `H2_multiplexed_connection` - Per-Endpoint Connection State

```ocaml
(** A single multiplexed HTTP/2 connection to an endpoint. *)
type t = {
  flow : Eio.Flow.two_way_ty Eio.Resource.t;
  client : H2_client.t;
  mutex : Eio.Mutex.t;  (* Eio mutex for stream allocation *)
  mutable active_streams : int;
  max_concurrent_streams : int;  (* From SETTINGS *)
  stream_available : Eio.Condition.t;  (* Signal when stream frees *)
  mutable closed : bool;
}

val acquire_stream : t -> unit
(** Block until a stream slot is available, then reserve it. *)

val release_stream : t -> unit
(** Release a stream slot, signaling waiters. *)

val request : t -> H2_protocol.request -> H2_protocol.response
(** Make a request on this connection. *)
```

#### 3. Protocol Router Integration

Modify `Requests.make_request` to route based on cached protocol knowledge:

```ocaml
(* In requests.ml *)

type protocol_hint =
  | Unknown          (* First request to this endpoint *)
  | Definitely_h1    (* Server doesn't support H2 *)
  | Definitely_h2    (* ALPN negotiated H2 *)

(* Protocol hint cache per endpoint *)
let protocol_hints : (string, protocol_hint) Hashtbl.t = Hashtbl.create 64

let make_request_internal ... =
  let endpoint_key = Printf.sprintf "%s:%d" host port in

  match Hashtbl.find_opt protocol_hints endpoint_key with
  | Some Definitely_h2 ->
      (* Use HTTP/2 pool directly - no need for ALPN *)
      H2_connection_pool.request h2_pool ~endpoint ...

  | Some Definitely_h1 ->
      (* Use HTTP/1.1 via Conpool *)
      Conpool.with_connection http_pool endpoint (fun flow ->
        Http_client.make_request ...)

  | None | Some Unknown ->
      (* First request - use Conpool to discover protocol via ALPN *)
      let conn_info = Conpool.connection_with_info ~sw pool endpoint in
      match conn_info.tls_epoch with
      | Some { alpn_protocol = Some "h2"; _ } ->
          Hashtbl.replace protocol_hints endpoint_key Definitely_h2;
          (* Hand off connection to H2 pool, make request *)
          H2_connection_pool.adopt_connection h2_pool ~endpoint conn_info.flow;
          H2_connection_pool.request h2_pool ~endpoint ...
      | _ ->
          Hashtbl.replace protocol_hints endpoint_key Definitely_h1;
          Http_client.make_request ... conn_info.flow
```

---

## Implementation Phases

### Phase 1: Fix Immediate Issues (Low Risk)

**Goal**: Fix race conditions and improve current implementation without architectural changes.

1. **Replace Unix.Mutex with Eio.Mutex in H2_adapter**
   - File: `lib/h2/h2_adapter.ml`
   - Change `Mutex.t` to `Eio.Mutex.t`
   - Update `with_mutex` to use `Eio.Mutex.use_rw`

2. **Add connection validity checks**
   - Before reusing cached H2_client, verify the underlying flow is still open
   - Handle GOAWAY frames properly

3. **Add tests for concurrent HTTP/2 requests**
   - Test multiple fibers making requests to same endpoint
   - Verify no race conditions

**Estimated scope**: ~50 lines changed, low risk

### Phase 2: H2_connection_pool Module (Medium Risk)

**Goal**: Create proper HTTP/2 connection pooling with multiplexing.

1. **Create `lib/h2/h2_connection_pool.ml[i]`**
   - Connection state per endpoint
   - Stream slot management with Eio.Condition
   - Automatic connection establishment on first request
   - Connection health monitoring

2. **Create `lib/h2/h2_multiplexed_connection.ml[i]`**
   - Wrap H2_client with stream counting
   - MAX_CONCURRENT_STREAMS enforcement
   - Proper cleanup on connection close

3. **Add comprehensive tests**
   - Multiplexing: N concurrent requests on 1 connection
   - Stream exhaustion: >MAX_CONCURRENT_STREAMS requests
   - Connection failure: mid-request disconnection
   - GOAWAY handling

**Estimated scope**: ~400 lines new code, 2 new modules

### Phase 3: Protocol Router Integration (Medium Risk)

**Goal**: Integrate H2_connection_pool with Requests session.

1. **Add H2_connection_pool to Requests.t**
   ```ocaml
   type t = T : {
     ...
     h2_pool : H2_connection_pool.t;  (* NEW *)
     protocol_hints : (string, protocol_hint) Hashtbl.t;  (* NEW *)
   } -> t
   ```

2. **Modify connection routing in make_request**
   - Check protocol hints before connecting
   - Update hints based on ALPN results
   - Route HTTP/2 to H2_connection_pool

3. **Handle protocol downgrade**
   - If server sends HTTP_1_1_REQUIRED error, update hint
   - Retry request via HTTP/1.1

4. **Update statistics tracking**
   - Add HTTP/2 connection/stream stats
   - Expose via `Requests.stats`

**Estimated scope**: ~200 lines changed in requests.ml

### Phase 4: Connection Handoff (Higher Risk)

**Goal**: Seamlessly transfer connections from Conpool to H2_pool.

1. **Add connection "adoption" to H2_connection_pool**
   - Accept an already-established TLS flow
   - Perform H2 handshake
   - Add to pool for future reuse

2. **Prevent Conpool from reclaiming H2 connections**
   - When connection is handed to H2_pool, don't return it to Conpool
   - This requires careful lifetime management

3. **Handle edge cases**
   - Connection fails during adoption
   - Server rejects H2 after ALPN (rare but possible)
   - TLS session resumption

**Estimated scope**: ~150 lines, careful lifetime management needed

### Phase 5: Optimizations (Lower Priority)

1. **Preemptive connection establishment**
   - For known H2 endpoints, establish connection before first request
   - Reduces latency for subsequent requests

2. **Connection warming**
   - Maintain minimum connections to frequently-used endpoints
   - Background PING to keep connections alive

3. **Load balancing across connections**
   - For very high throughput, allow multiple H2 connections per endpoint
   - Distribute streams across connections

---

## Data Structures

### H2_connection_pool State

```
┌─────────────────────────────────────────────────────────────┐
│                    H2_connection_pool.t                      │
├─────────────────────────────────────────────────────────────┤
│  connections: (endpoint, H2_multiplexed_connection.t) Hashtbl│
│  mutex: Eio.Mutex.t                                         │
│  sw: Eio.Switch.t                                           │
│  clock: Eio.Time.clock                                      │
└─────────────────────────────────────────────────────────────┘
                              │
                              │ per endpoint
                              ▼
┌─────────────────────────────────────────────────────────────┐
│              H2_multiplexed_connection.t                     │
├─────────────────────────────────────────────────────────────┤
│  flow: Eio.Flow.two_way                                     │
│  client: H2_client.t                                        │
│  hpack_encoder: H2_hpack.Encoder.t                          │
│  hpack_decoder: H2_hpack.Decoder.t                          │
│  active_streams: int (mutable)                              │
│  max_concurrent_streams: int (from SETTINGS)                │
│  stream_available: Eio.Condition.t                          │
│  closed: bool (mutable)                                     │
│  reader_fiber: unit Eio.Fiber.t (reads frames)              │
│  last_stream_id: int32 (for GOAWAY)                         │
└─────────────────────────────────────────────────────────────┘
```

### Request Flow with Multiplexing

```
Fiber A: GET /users    ───┐
                          │
Fiber B: GET /posts    ───┼───► H2_connection_pool.request
                          │              │
Fiber C: GET /comments ───┘              │
                                         ▼
                              ┌─────────────────────┐
                              │ acquire_stream()    │
                              │ (blocks if at max)  │
                              └─────────┬───────────┘
                                        │
                    ┌───────────────────┼───────────────────┐
                    │                   │                   │
                    ▼                   ▼                   ▼
               Stream 1            Stream 3            Stream 5
              (Fiber A)           (Fiber B)           (Fiber C)
                    │                   │                   │
                    └───────────────────┼───────────────────┘
                                        │
                                        ▼
                              ┌─────────────────────┐
                              │ Single TCP/TLS conn │
                              │ to example.com:443  │
                              └─────────────────────┘
```

---

## API Changes

### New Types in Requests

```ocaml
(** HTTP/2 connection statistics *)
type h2_stats = {
  connections : int;        (** Active HTTP/2 connections *)
  total_streams : int;      (** Total streams opened *)
  active_streams : int;     (** Currently active streams *)
  max_concurrent : int;     (** Max concurrent streams (from SETTINGS) *)
}

(** Extended session statistics *)
type stats = {
  (* existing fields *)
  requests_made : int;
  total_time : float;
  retries_count : int;
  (* new HTTP/2 fields *)
  h2_stats : h2_stats option;  (** HTTP/2 statistics, if any H2 connections *)
}
```

### Backward Compatibility

- All existing `Requests.*` functions unchanged
- Same API for HTTP/1.1 and HTTP/2 (protocol transparent)
- New `h2_stats` field is optional, existing code ignores it

---

## Testing Strategy

### Unit Tests

1. **H2_connection_pool tests**
   - Create pool, make request, verify stream counting
   - MAX_CONCURRENT_STREAMS enforcement
   - Connection reuse verification

2. **Stream slot management**
   - Concurrent requests within limit
   - Blocking when at limit
   - Proper cleanup on request completion

### Integration Tests

1. **Protocol selection**
   - ALPN negotiation (mock TLS)
   - Protocol hint caching
   - Fallback to HTTP/1.1

2. **Concurrent requests**
   - N fibers making requests to same H2 endpoint
   - Verify single connection used
   - Verify all requests complete

3. **Error handling**
   - Connection drops mid-stream
   - GOAWAY during request
   - Server-side RST_STREAM

### Stress Tests

1. **High concurrency**
   - 100 concurrent requests to same endpoint
   - Verify multiplexing works
   - Measure latency vs HTTP/1.1

2. **Connection churn**
   - Repeated connect/disconnect cycles
   - No resource leaks

---

## Risks and Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Stream deadlock | Medium | High | Timeout on stream acquisition, tests |
| Memory leak | Medium | Medium | Careful resource cleanup, Eio.Switch |
| Race conditions | Medium | High | Eio.Mutex throughout, no Unix.Mutex |
| Performance regression | Low | Medium | Benchmark before/after |
| Compatibility issues | Low | Medium | Extensive testing with real servers |

---

## Success Criteria

1. **Correctness**: All existing tests pass
2. **Multiplexing**: Concurrent H2 requests share single connection
3. **Performance**: H2 requests to same host faster than H1.1
4. **Resource efficiency**: Connection count reduced for H2 hosts
5. **No regressions**: HTTP/1.1 behavior unchanged

---

## References

- RFC 9113 Section 5.1.2 (Stream Concurrency)
- RFC 9113 Section 6.5.2 (SETTINGS_MAX_CONCURRENT_STREAMS)
- RFC 9113 Section 6.8 (GOAWAY)
- Eio documentation (structured concurrency, Mutex, Condition)
- Current H2_client implementation
