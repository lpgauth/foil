# Changelog

## 0.1.6

### Added

- `foil:namespaces/0` returns `{ok, [namespace()]}` — the list of
  registered namespaces. Saves callers from having to peek at
  `foil_modules:lookup/1` internals to enumerate state.

- `README` gained a "Performance characteristics" section spelling
  out the compile-cost vs. lookup-cost trade-off (foil's `load/1` is
  O(n); `lookup/2` beats ETS at ~0.1 μs vs 0.4 μs for `*_direct_*`
  patterns). Helps callers pick foil vs. ETS for new use cases.

### Plan-time notes

- B4's "fix `foil_compiler:load/2` spec to propagate `compile_failed`"
  recommendation was checked on inspection and not acted on:
  `compile:forms/2` can only fail when the forms produced by
  `foil_compiler:forms/2` are malformed, which is a foil internal
  bug rather than a runtime user-facing error. The current
  `{ok, Module, Bin} = compile:forms(...)` badmatch crashes loudly
  on that path, which is the right behaviour. Surfacing it as
  `{error, compile_failed}` would force every caller to write a
  branch they can't sensibly handle.

## 0.1.5

### Added

- `foil_compiler:to_syntax/1` now accepts maps, references, pids,
  ports, funs, and non-byte-aligned bitstrings. Maps compile to a
  proper `map_expr` literal. The rest have no abstract literal form,
  so they ride through an integer syntax node — the BEAM module
  loader carries them in the compiled module's constant pool as-is,
  and `foil:lookup/2` returns them unchanged.
- eunit case `non_literal_terms_test` round-trips each new type
  (including a nested tuple/map combination that mixes literal and
  non-literal values).

## 0.1.4

### Changed

- CI moved from Travis (decommissioned years ago) to GitHub Actions.
  Matrix now covers OTP 25, 26, 27, 28.
- Documentation migrated from `edown` to `rebar3_ex_doc`. Generated
  `doc/` directory removed; HTML docs are now published to hexdocs.
- Bumped `metal` dependency from `0.1.1` to `0.1.2`.
- Tightened the `error/0` type from `{error, atom()}` to a sum:
  `{error, foil_not_started | key_not_found | module_exists |
  module_not_found}`. Dialyzer-checkable; same atoms the code already
  returns. No behavioural change.
- `foil.erl` refactored to dispatch through a `?WITH_MODULE(...)` macro
  (defined in `foil.hrl`). Removes ~40 lines of repeated try/catch
  scaffolding from `all/1`, `delete/1`, `delete/2`, `insert/3`,
  `load/1`, and `lookup/2`. Macro rather than function so the cache
  hot path stays branch-only; bench numbers unchanged.

### Removed

- `.travis.yml`, `elvis.config`, `bin/elvis`, `rebar.config.script`
  (rebar2 compatibility) — all unused.
- `coveralls` plugin and Makefile target — Travis-specific tooling.
- Coveralls and Travis build badges from the README.
