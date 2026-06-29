# Sayid Roadmap

This document is a proposed direction for Sayid, not a commitment. It captures
*why the project exists* and the handful of changes that would make it deliver on
that promise for real-world code rather than blog-post examples.

## North star

Sayid's defensible edge is not a prettier UI - [FlowStorm](https://github.com/flow-storm/flow-storm-debugger)
already owns the slick stepping-debugger experience. Sayid is different in three
ways worth leaning into deliberately:

1. **nREPL-native** - it lives where Clojure developers already work, driven from
   the REPL and the editor, no separate process or window.
2. **Programmatic** - tracing is code (`trace a ns, a fn, a dir`), so it composes
   with scripts, fixtures, and CI.
3. **Data-first** - the recorded execution is a queryable data structure, not a
   view. This is the crown jewel, and it is currently trapped behind a renderer.

Every initiative below should push Sayid further toward *"the execution of your
program, as data, that you can script."* When a design choice is ambiguous,
that's the tie-breaker.

## The codebase today

~4,500 lines of Clojure. The shape tells a story of accreted rewrites:

| namespace | LOC | role |
|-----------|-----|------|
| `inner-trace` | 857 | inner tracing by re-reading and rewriting source - fragile |
| `string-output` | 662 | rendering to text + text-property spans |
| `core` | 600 | public REPL API / engine hub |
| `nrepl_middleware` | 549 | wire protocol, but also owns rendering |
| `query` | 471 | the query DSL |
| `util/other` | 402 | grab-bag utilities |
| `trace` | 289 | outer tracing |
| `workspace` / `recording` / `shelf` | 178 / 87 / 46 | the in-memory capture store |
| `view` / `profiling` / `sayid_multifn` | 107 / 114 / 56 | views, profiling, AOT multimethod support |

The version-suffixed names (`query2`, `string_output2`, `inner_trace3`) have been
de-versioned - that was the easy part. The deeper issue remains: the engine, the
wire protocol, and the renderer are entangled, which is why there is exactly one
client.

## Initiatives

Sequenced so each one unblocks the next. Start at the foundation.

### 1. Consolidate and decouple the core *(do this first)*

**Goal:** a core where *engine*, *wire protocol*, and *rendering* are cleanly
separated, with no version-suffixed namespaces.

**Why first:** it's the lowest-risk work (mostly mechanical restructuring, no new
behavior) and it's the prerequisite that makes initiatives 2 and 3 tractable. We
gave the Emacs client this treatment recently; the Clojure core deserves the same
pass.

**Approach:**
- *Done.* Locked `core`'s public surface with a `clojure.test` characterization
  suite before touching anything - the safety net for the moves below.
- *Done.* Dropped the version suffixes: `query2` -> `query`, `string_output2` ->
  `string-output`, `inner_trace3` -> `inner-trace`. No aliases; Sayid has no
  external clients.
- *Done.* Broke up `util/other`: deleted the dead code and split the
  symbol/namespace and source-reading helpers into `util.sym` and `util.source`.
- Draw a hard line between three layers and stop letting them reach across it:
  - **engine** - `trace`, `inner-trace`, `workspace`, `recording`, `shelf`
  - **query/render** - `query`, `string-output`, `view`
  - **protocol** - `nrepl_middleware` (which should call the engine and *return
    data*, see initiative 3).

  Mostly there already: the engine has no upward deps, and once the stray unused
  require and a dead scratch block were removed, `query`/`view`/`string-output`
  stopped reaching into the engine too. The one real breach left is the protocol
  owning rendering - the middleware calls `string-output` to build text-property
  pairs for Emacs. Closing that *is* initiative 3 (return data, not strings), so
  the hard line gets finished there.

**Risks:** `sayid_multifn` is AOT-compiled and the namespace names are part of the
deployed contract; renames need deprecated forwarders. Inner-trace rewriting reads
namespace/source by name, so any move there must keep symbol resolution intact.

**Done when:** no `\d`-suffixed namespaces remain, the layers don't have circular
requires, and the characterization suite is green.

### 2. Bound the recording so it's safe on real workloads

**Goal:** tracing a whole namespace and running a test suite should never OOM or
capture values that mutate out from under you.

**Why:** this is *the* adoption blocker. The workspace holds every call (and every
inner expression value) with no cap, sampling, or eviction, and it captures live
references rather than snapshots. That's why Sayid feels like a toy-example tool.

**Approach:**
- Snapshot values at capture time honoring `*print-length*` / `*print-level*`
  (and a configurable size budget), so a fat map, a mutable object, or a lazy/
  infinite seq can't blow the heap or change after the fact.
- Add bounds to `workspace`/`recording`: per-fn call caps, a global ring-buffer /
  eviction policy, and optional sampling (record 1 in N calls).
- Surface a clear signal when a bound kicks in ("hit the 1000-call cap on `foo`")
  rather than silently truncating.

**Risks:** snapshotting changes what users see for reference types; it must be
opt-outable for the cases where you genuinely want the live value. Bounds interact
with the query layer (a truncated tree must still be queryable).

**Done when:** `trace-ns` + a realistic test run completes in bounded memory, and
capturing an infinite seq doesn't hang.

### 3. Return data, not pre-rendered strings

**Goal:** the middleware returns the trace tree *as data*; clients render.

**Why:** today `sayid-get-workspace` ships a `[text text-properties query-args]`
triple - the server owns presentation and the Emacs client is a dumb terminal.
That single decision is why there's one client and why the data can't flow into
the modern Clojure inspection ecosystem.

**Approach:**
- Define a stable, documented data shape for a recorded call tree (ids, fn,
  args, return, children, inner values, timings).
- Add data-returning ops alongside the existing rendered ones; move rendering
  into a `render` namespace the *client* can call, keeping the Emacs experience
  identical.
- Once data ops exist, the payoffs are nearly free: tap a workspace into
  [Portal](https://github.com/djblue/portal) / Reveal / Morse, or `(->> (ws-deref)
  (filter ...))` with plain Clojure and the existing query DSL.

**Risks:** value encoding over bencode (the `clj->nrepl` rules) needs to round-trip
the new shape without forcing clients to re-read strings - the same class of bug as
the #29 fix. Keep the rendered ops until clients migrate.

**Done when:** an editor-agnostic client (or a REPL one-liner) can fetch and
navigate a workspace without any Sayid-specific rendering.

### 4. Make inner tracing robust via the AST

**Goal:** inner tracing that doesn't detonate on missing source, reader
conditionals, or the next macro nobody tested.

**Why:** `inner-trace` re-reads the `.clj` from disk and rewrites raw forms,
which is why it throws on `NO_SOURCE_FILE` and needed a fix just for `letfn`. It's
the least trustworthy part of an otherwise solid tool (and the `inner_trace3` it
grew out of had been rewritten twice already).

**Approach:**
- Rebuild instrumentation on
  [`tools.analyzer.jvm`](https://github.com/clojure/tools.analyzer.jvm) so it
  works off the analyzed AST instead of raw forms. Walk the AST, wrap
  sub-expressions with capture, emit instrumented code.
- Keep the current rewriter behind a flag during migration and diff their captures
  on a corpus of real namespaces (this is where initiative 2's snapshots help -
  captures become comparable data).

**Risks:** this is the largest single chunk and the highest-cleverness code in the
project. It must preserve evaluation semantics exactly (no double-eval of
side-effecting forms, correct `recur`/`loop`/`letfn` handling).

**Done when:** inner tracing works on a corpus that breaks the current rewriter,
with no special-case patches per macro.

### 5. Position and extend

**Goal:** turn the data-first foundation into reach and a clear identity.

**Why:** once the execution is safe (2), exposed as data (3), and the engine is
clean (1), the strategic moves become possible rather than aspirational.

**Approach (pick based on appetite):**
- **Golden-trace testing** - because captures are now bounded, comparable data, a
  test can assert "this run's trace matches the recorded baseline." Genuinely
  novel regression testing, and a use case neither tools.trace nor a debugger can
  touch.
- **ClojureScript capture** - wide-open territory; the data-first protocol makes a
  cljs engine a backend swap rather than a rewrite of everything.
- **Ecosystem integrations** - first-class Portal/Reveal/Morse taps, and a small
  web UI that consumes the data ops.

**Done when:** Sayid has at least one capability (golden-trace or cljs) that is
clearly its own, not a worse version of what FlowStorm or a debugger offers.

## Sequencing rationale

```
(1) decouple the core   ─┬─►  (3) data, not strings  ──►  (5) position & extend
                         └─►  (4) AST inner tracing
(2) bound the recording  ────────────────────────────►  (5)
```

Initiative 1 is the foundation. Initiatives 2, 3, and 4 are largely independent
once it lands, so they can be reordered by appetite - 2 is the biggest *adoption*
win, 3 is the biggest *identity* win, 4 is the biggest *robustness* win.
Initiative 5 is the payoff and should come last, when there's a safe, data-first,
clean engine to build it on.
