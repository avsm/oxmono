# HF

`Hf.run ~process_mgr args` invokes the Hugging Face CLI through Eioext. It uses
`hf`, falling back to `uvx hf` when the executable is absent. Permission errors,
signals and cancellation never trigger fallback. `retry_failed:true` also
retries an explicit nonzero exit through `uvx hf`.

`token` overrides `HF_TOKEN` in the child environment without changing the
parent environment or putting the token in argv. Model selection, cache layout
and downloaded-file validation belong to consumers. Tests use local scripts.
