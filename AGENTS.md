# aequitas

Erlang/OTP library that regulates *fairness* — and optionally rate — when many
actors compete for a limited resource (a database, a web service, ...). Each
named **category** tracks recent work acceptances per actor in a sliding window
and rejects requests from actors whose share is a statistical outlier (Tukey's
fences over the interquartile range), optionally enforcing a collective
per-second rate limit on top.

## Build, test, check

```bash
make compile         # rebar3 compile
make test            # eunit + CT + coverage
make check           # check-fast + check-slow
make check-fast      # format check (erlfmt) + xref + dead code (hank) + lint (elvis)
make check-slow      # dialyzer
make format          # auto-format sources with erlfmt
make eunit           # unit tests only
make ct              # common_test suites + coverage
make dialyzer        # type analysis
make doc             # download ex_doc escript -> tmp/, render EEP-48 docs to doc/
make shell           # rebar3 shell with the app started
```

All checks run sequentially (`.NOTPARALLEL`). CI runs `make check-fast`,
`make test` and `make check-slow` on OTP 24-29 (ubuntu-22.04).

## Compiler flags

`warn_export_vars`, `warn_missing_spec`, `warn_unused_import` and
`warnings_as_errors` are always on — every exported function needs a `-spec`.
The `test` and `shell` profiles relax `warn_missing_spec`/`warnings_as_errors`.

## Architecture

```
aequitas_sup            (rest_for_one)
├── aequitas_proc_reg        gen_server: category name -> pid registry
├── aequitas_cfg            gen_server: per-category settings store
├── aequitas_work_stats_sup supervisor
│   └── aequitas_work_stats  one worker per category; crunches IQR stats
├── aequitas_category_sup   supervisor
│   └── aequitas_category    one worker per category; the core process
└── aequitas_boot_categories gen_server: starts foreknown categories at boot
```

- `aequitas` is the public API; every function delegates to `aequitas_category`.
- `aequitas_category` is **not** a gen_server: it's a manual `proc_lib` + `sys`
  loop. It keeps the per-actor sliding window of work shares in ETS and asks its
  paired `aequitas_work_stats` process to (re)compute the distribution
  asynchronously, then accepts/rejects new requests against the latest stats.
- Settings declared under `{aequitas, [{categories, #{...}}]}` in `sys.config`
  are launched at boot by `aequitas_boot_categories`; application config reloads
  flow through `aequitas_app:config_change/3` into the relevant category
  processes. `aequitas:reconfigure/2` overrides them at runtime.

### Key modules

| Module | Role |
|---|---|
| `aequitas` | Public API: `start/1,2`, `stop/1`, `ask/2,3`, `async_ask/2,3`, `reconfigure/2` |
| `aequitas_category` | Per-category process: sliding window, outlier detection, rate limiting; owns `setting_opt/0` and `ask_opt/0` |
| `aequitas_work_stats` | Per-category stats worker; computes Q1/Q2/Q3/IQR; owns the stats type `t/0` |
| `aequitas_time_interval` | The `t/0` time-interval type (e.g. `{seconds, 5}`) used in settings |
| `aequitas_cfg` | Stores/serves per-category settings |
| `aequitas_proc_reg` | Maps category terms to category-process pids |
| `aequitas_boot_categories` | Starts categories declared in application config |
| `aequitas_app` / `aequitas_sup` | OTP application + top supervisor |

## Code conventions

- Module names follow `aequitas_<subsystem>[_<role>].erl`.
- Code is formatted with `erlfmt`; run `make format` before committing. The bulk
  reformat commit is listed in `.git-blame-ignore-revs`.
- Documentation is **EEP-48 native**: `-moduledoc`/`-doc` attributes, each
  guarded by `-ifdef(E48). ... -endif.` (the `E48` macro is defined only on
  OTP 27+ via `rebar.config`). ex_doc renders these; legacy edoc `@private`
  tags are kept and merged in by the edoc chunk doclet (`rebar3 as docs edoc`).
- Lint exceptions live in `elvis.config` and are documented inline (lowercase
  `is_*` guard macros, internal `state/0` types, the monitor-guarded `receive`
  in `aequitas_category`, relaxed test rules). Hank ignores the
  `unnecessary_function_arguments` rule for the OTP-mandated `sys` callbacks.

## Tests

`test/aequitas_SUITE.erl` is the single Common Test suite (grouped test cases);
`test/etc/sys.config` provides the static-configuration fixtures. Internal
test-only helpers in `aequitas_category` are gated behind `-ifdef(TEST)`.

## OTP version support

Supported OTP 24-29 (declared `minimum_otp_vsn` is lower, but 24+ is what's
tested). `rebar.config.script` removes the `erlfmt`, `rebar3_hank` and
`rebar3_lint` plugins on OTP ≤ 25 (incompatible there) and works around
`rebar3_hank` on OTP 29.

## Releasing

`make publish` runs `rebar3 hex publish --doc-dir=doc` (builds docs first).
Versioning follows SemVer; history is in `CHANGELOG.md` (Keep a Changelog).
