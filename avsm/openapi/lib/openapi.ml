(** OCaml OpenAPI - Code generator for OpenAPI specifications.

    This library provides:
    - {!module:Spec} - OpenAPI 3.x specification types with jsont codecs
    - {!module:Codegen} - Code generation from spec to OCaml
    - {!module:Runtime} - Runtime utilities for generated clients
    - {!module:Nestjs} - NestJS/Express error handling (optional)
*)

module Spec = Openapi_spec
module Codegen = Openapi_codegen
module Runtime = Openapi_runtime
module Nestjs = Openapi_nestjs
