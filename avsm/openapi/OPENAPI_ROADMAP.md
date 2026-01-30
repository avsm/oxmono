# OpenAPI Code Generator Roadmap

This document outlines future enhancements for the OCaml OpenAPI code generator.

## Recently Completed

### Union Type Generation (v0.2)
- Generate OCaml variant types for `oneOf`/`anyOf` schemas at the top level
- Support discriminator-based codec (using `Jsont.Object.Case`)
- Support try-each codec for schemas without discriminators
- Currently handles schema-level unions; field-level unions fallback to `Jsont.json`

### Typed Error Responses (v0.2)
- Enhanced `api_error` type with `parsed_body` field
- `error_body` type: `Raw`, `Json`, or `Typed` variants
- Generate error parsing code for operations with typed error schemas
- Fallback to JSON/raw parsing for untyped errors

### Field-Level Union Types (v0.3)
- Detect `oneOf`/`anyOf` in property definitions
- For primitive-only unions (string|int|bool), generate polymorphic variants
- For unions with schema references, fall back to `Jsont.json` due to module ordering constraints
- Example: `oneOf: [{type: string}, {type: integer}]` → `[ \`String of string | \`Int of int ]`

### Runtime Validation (v0.3)
- Added validation functions: `validated_string`, `validated_int`, `validated_float`, `validated_list`
- String validation: `minLength`, `maxLength`, `pattern` (using Re library with PCRE syntax)
- Number validation: `minimum`, `maximum`, `exclusiveMinimum`, `exclusiveMaximum`
- List validation: `minItems`, `maxItems`, `uniqueItems`
- Validation errors reported via `Jsont.Error.msgf`
- Added `poly_union_decoder` helper for polymorphic variant union types

### Two-Phase Module Generation (v0.4)
- Solved the module ordering problem for union types referencing sibling schemas
- Each prefix module now uses a two-phase structure:
  - **Phase 1 (Types module)**: All type definitions, ordered by TYPE dependencies
  - **Phase 2 (Full modules)**: Full modules with `include Types.X` + codecs, ordered by CODEC dependencies
- Benefits:
  - Types can reference any sibling type (they're all in the same Types module)
  - Codecs can reference any sibling codec (properly ordered by codec dependencies)
  - Union type codecs can now use try-each decoding across multiple sibling schemas
- Generated structure:
  ```ocaml
  module Prefix = struct
    module Types = struct
      module Schema_a = struct type t = { ... } end
      module Union_c = struct type t = A of Schema_a.t | B of Schema_b.t end
    end
    module Schema_a = struct include Types.Schema_a let jsont = ... end
    module Union_c = struct include Types.Union_c let jsont = (* uses Schema_a.jsont *) end
  end
  ```
- Preserves the user-facing API: `Prefix.Schema.t`, `Prefix.Schema.jsont`, etc.

## Planned Enhancements

### 1. Streaming Support

**Priority:** High

Add support for `text/event-stream` media type handling for Server-Sent Events.

**Requirements:**
- Detect SSE endpoints in OpenAPI spec
- Generate async iterator types for streaming responses
- Requires `requests` library enhancement for chunked/streaming reads

**Target API:**
```ocaml
val stream_events :
  t -> unit ->
  (Event.t, Openapi.Runtime.api_error) Seq.t
```

### 2. File Upload Support

**Priority:** High

Handle `multipart/form-data` with binary parts for file uploads.

**Requirements:**
- Detect multipart endpoints in spec
- Generate proper file upload functions accepting `Eio.Flow.source`
- Requires `requests` library multipart encoding support

**Target API:**
```ocaml
val upload_file :
  filename:string ->
  content_type:string ->
  body:_ Eio.Flow.source ->
  t -> unit ->
  UploadResponse.t
```

### 3. Authentication Code Generation

**Priority:** Medium

Generate auth header injection based on `securitySchemes`.

**Supported schemes:**
- `apiKey` (header, query, cookie)
- `http` (basic, bearer)
- `oauth2` flows (implicit, password, clientCredentials, authorizationCode)
- `openIdConnect`

**Target API:**
```ocaml
module Auth : sig
  type t =
    | Api_key of string
    | Bearer of string
    | Basic of { username: string; password: string }
    | OAuth2 of { access_token: string; refresh_token: string option }

  val with_auth : t -> client -> client
end
```

### 4. Additional Schema Features

**Priority:** Medium

#### 4.1 `additionalProperties`
Convert to OCaml string maps:
```ocaml
type t = { known_field: string; extra: Jsont.json StringMap.t }
```

#### 4.2 `const`
Generate literal type validation or unit variants.

#### 4.3 `default`
Handle default values for optional fields:
- Make fields with defaults optional in constructors
- Use the default value when field is absent during decoding
- Consider generating builder-style constructors for complex schemas

#### 4.4 `readOnly`/`writeOnly`
Generate separate request/response types when fields differ.

### 5. Requests Library Enhancements

**Priority:** Varies

These depend on enhancements to the `requests` library:

| Feature | Requests Support | OpenAPI Use Case |
|---------|-----------------|------------------|
| Streaming responses | Needed | SSE, large downloads |
| Multipart form data | Needed | File uploads |
| Connection pooling | Nice to have | Performance |
| Retry with backoff | Nice to have | Resilience |
| WebSocket | Future | Real-time APIs |

## Architecture Notes

### Current Module Structure
```
openapi
├── Spec        -- OpenAPI 3.x types
├── Codegen     -- Code generation
├── Runtime     -- Runtime utilities
└── Nestjs      -- NestJS error handling
```

### Generated Code Structure
```
generated_client
├── t           -- Client type
├── create      -- Constructor
└── Module1     -- Per-prefix modules
    ├── Schema1 -- Per-schema submodules
    │   ├── t
    │   ├── jsont
    │   └── accessors
    └── operation1  -- Operations
```

### Design Principles

1. **Type safety over flexibility**: Prefer typed codecs over `Jsont.json`
2. **Minimal runtime**: Keep `Runtime` module small
3. **Idiomatic OCaml**: Use modules, not objects
4. **Eio-native**: No blocking IO, cooperative concurrency
5. **Forward-compatible**: Handle unknown fields gracefully

## Contributing

To add a new feature:

1. Update `openapi_spec.ml` if new OpenAPI fields are needed
2. Update `openapi_codegen.ml` with analysis and generation
3. Update `openapi_runtime.ml` if new runtime support is needed
4. Regenerate test specs: `dune exec openapi-gen -- generate ...`
5. Verify with `dune build @check` and `dune build @doc`
