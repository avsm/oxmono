# Unreleased

Provide strict TOML field readers that reject unknown keys and wrong types
without including configuration values in errors.

Validate UTF-8 without temporary strings and reject malformed prefixes followed
by valid bytes that previously bypassed validation.
