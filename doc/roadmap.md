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
   view. This is the crown jewel; it used to be trapped behind a renderer, but the
   data ops (initiative 3) now expose it directly.

Every initiative below should push Sayid further toward *"the execution of your
program, as data, that you can script."* When a design choice is ambiguous,
that's the tie-breaker.

## The codebase today

~4,000 lines of Clojure. Much of the accreted-rewrite cruft has been cleared out:

| namespace | LOC | role |
|-----------|-----|------|
| `nrepl_middleware` | 650 | wire protocol / nREPL ops (data + rendered) |
| `string-output` | 625 | rendering to text + text-property spans (legacy path) |
| `core` | 620 | public REPL API / engine hub |
| `trace` | 491 | outer tracing + recording bounds |
| `query` | 458 | the query DSL |
| `util/other` | 237 | grab-bag utilities |
| `inner-ast` | 202 | inner tracing via the analyzed AST |
| `workspace` / `recording` / `shelf` | 173 / 89 / 47 | the in-memory capture store |
| `view` / `profiling` / `sayid_multifn` | 109 / 109 / 56 | views, profiling, AOT multimethod support |

The version-suffixed names (`query2`, `string_output2`, `inner_trace3`) are gone,
the layers are decoupled (initiative 1), the wire protocol returns data
(initiative 3), and the fragile source-rewriting inner tracer has been replaced by
an AST-based one and deleted (initiative 4). The main entanglement that remains is
the *legacy text renderer* (`string-output` + the rendered ops + the `tamarin`
dependency), now that the client renders from data - see initiative 6.

## Initiatives

Sequenced so each one unblocks the next. Start at the foundation.

### 1. Consolidate and decouple the core *(done)*

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
- *Done.* Drew a hard line between three layers and stopped letting them reach
  across it:
  - **engine** - `trace`, `inner-ast`, `workspace`, `recording`, `shelf`
  - **query/render** - `query`, `string-output`, `view`
  - **protocol** - `nrepl_middleware`, which calls the engine and *returns data*
    (see initiative 3).

  The engine has no upward deps; removing a stray unused require and a dead scratch
  block stopped `query`/`view`/`string-output` reaching into it; and the last knot
  - the protocol owning rendering - was untied in initiative 3, where the
  middleware moved to returning data or calling a small `*->text-prop-pair` render
  API in `string-output` instead of composing rendering steps itself.

**Risks:** `sayid_multifn` is AOT-compiled and the namespace names are part of the
deployed contract; renames need deprecated forwarders. Inner-trace rewriting reads
namespace/source by name, so any move there must keep symbol resolution intact.

**Done when:** no `\d`-suffixed namespaces remain, the layers don't have circular
requires, and the characterization suite is green.

### 2. Bound the recording so it's safe on real workloads *(done, bar a snapshot refinement)*

**Goal:** tracing a whole namespace and running a test suite should never OOM or
capture values that mutate out from under you.

**Why:** this is *the* adoption blocker. The workspace holds every call (and every
inner expression value) with no cap, sampling, or eviction, and it captures live
references rather than snapshots. That's why Sayid feels like a toy-example tool.

**Approach:**
- *Done.* A `*suppress-recording*` flag is the foundation: skipping a call now
  runs it (and its whole subtree) untraced without the nested calls leaking in as
  spurious roots, and inner tracing honours it, which also closed a latent
  nil-parent NPE. On top of it, a full set of bounds: `trace/*record-limit*`
  (default 50k) bounds the recorded top-level calls (breadth), `trace/*max-trace-depth*`
  (default nil) bounds recording depth, `trace/*sample-rate*` (default 1) records
  one in every N top-level calls for hot entry points, `trace/*per-fn-limit*`
  (default nil) caps how many calls of any single function are recorded, and
  `trace/*evict-old-calls*` switches the record limit from keep-first-N to a
  keep-last-N ring (the root end-trace is looked up by node id, not position, so
  eviction is safe even with concurrent root calls).
- Snapshot values at capture time honoring `*print-length*` / `*print-level*`
  (and a configurable size budget), so a fat map, a mutable object, or a lazy/
  infinite seq can't blow the heap or change after the fact. *First cut done at
  the serialization boundary: the data ops print captured values under bounded
  `*print-length*`/`*print-level*` (`*value-print-length*`/`*value-print-level*`),
  so an infinite seq can't hang the serializer or produce a runaway payload.
  Capture-time snapshotting (to also cap heap and freeze reference types) is still
  to do.*
- Surface a clear signal when a bound kicks in ("hit the 1000-call cap on `foo`")
  rather than silently truncating. *(Done for the global cap.)*

**Risks:** snapshotting changes what users see for reference types; it must be
opt-outable for the cases where you genuinely want the live value. Bounds interact
with the query layer (a truncated tree must still be queryable).

**Done when:** `trace-ns` + a realistic test run completes in bounded memory, and
capturing an infinite seq doesn't hang.

### 3. Return data, not pre-rendered strings *(done)*

**Goal:** the middleware returns the trace tree *as data*; clients render.

**Why:** today `sayid-get-workspace` ships a `[text text-properties query-args]`
triple - the server owns presentation and the Emacs client is a dumb terminal.
That single decision is why there's one client and why the data can't flow into
the modern Clojure inspection ecosystem.

**Approach:**
- *Done.* Documented a stable data shape for a recorded call tree (ids, fn, args,
  arg-map, return or throw, children, timings) - see doc/nrepl-api.md.
- *Done.* Added data ops alongside the rendered ones (`sayid-get-workspace-data`,
  `sayid-query-data` and friends), and moved rendering composition into a small
  `*->text-prop-pair` API in `string-output`, so the middleware no longer owns it.
  The Emacs client now renders the workspace and traced-fn views from these data
  ops on CIDER's `cider-tree-view`, with value inspection via CIDER's inspector.
- *Done.* Inner-trace captured values are in the data shape too: each node carries
  its recorded `form` and value, so the data ops expose the inner-expression
  tree, not just the outer calls.
- Once data ops exist, the payoffs are nearly free: tap a workspace into
  [Portal](https://github.com/djblue/portal) / Reveal / Morse, or `(->> (ws-deref)
  (filter ...))` with plain Clojure and the existing query DSL.

**Risks:** value encoding over bencode (the `clj->nrepl` rules) needs to round-trip
the new shape without forcing clients to re-read strings - the same class of bug as
the #29 fix. Keep the rendered ops until clients migrate.

**Done when:** an editor-agnostic client (or a REPL one-liner) can fetch and
navigate a workspace without any Sayid-specific rendering.

### 4. Make inner tracing robust via the AST *(done)*

**Goal:** inner tracing that doesn't detonate on missing source, reader
conditionals, or the next macro nobody tested.

**Why:** `inner-trace` re-reads the `.clj` from disk and rewrites raw forms,
which is why it throws on `NO_SOURCE_FILE` and needed a fix just for `letfn`. It's
the least trustworthy part of an otherwise solid tool (and the `inner_trace3` it
grew out of had been rewritten twice already).

**Approach:**
- *Done.* Rebuilt instrumentation on
  [`tools.analyzer.jvm`](https://github.com/clojure/tools.analyzer.jvm) in
  `sayid.inner-ast`: it walks the analyzed AST, wraps each call-like
  sub-expression with a capture call, and emits the result. Macros and special
  forms are already expanded in the AST, so there's no per-macro special-casing.
  Two analyzer facts replace machinery the legacy code reinvents by hand -
  `:raw-forms` recovers the surface syntax for display, and node context marks
  tail position so `recur` is left legal. The runtime capture re-raises after
  recording, so it's exception-transparent (an inner-traced `try/catch` now sees
  the exception, which the legacy rewriter swallowed).
- *Done.* Grew the AST impl to parity behind a flag, diffing both impls across a
  corpus (`sayid.inner-ast-test`): instrumenting never changes a fn's result, and
  the AST impl captures a superset of the old one's sub-expression values. Then
  made it the default and **deleted the legacy rewriter** outright - the whole
  `sayid.inner-trace` namespace, with its disk-source reading and path-symbol
  correlation machinery, is gone.
- *Known limit (inherent, not a to-do).* On-demand tracing still needs the source
  *form* (you can't analyze a compiled fn), so a genuinely source-less fn can't be
  inner-traced. That's a property of the on-demand model, not the instrumenter;
  lifting it would need compile-time instrumentation.

**Risks:** this is the largest single chunk and the highest-cleverness code in the
project. It must preserve evaluation semantics exactly (no double-eval of
side-effecting forms, correct `recur`/`loop`/`letfn` handling).

**Done when:** inner tracing works on a corpus that breaks the current rewriter,
with no special-case patches per macro. *(Met: the AST impl handles the corpus
uniformly, is the default, and the legacy rewriter has been deleted.)*

### 5. Position and extend *(in progress: golden-trace testing)*

**Goal:** turn the data-first foundation into reach and a clear identity.

**Why:** once the execution is safe (2), exposed as data (3), and the engine is
clean (1), the strategic moves become possible rather than aspirational.

**Approach (pick based on appetite):**
- **Golden-trace testing** *(in progress)* - because captures are now bounded,
  comparable data, a test can assert "this run's trace matches the recorded
  baseline." Genuinely novel regression testing, and a use case neither
  tools.trace nor a debugger can touch. This is the chosen first move: it has the
  best value-to-effort ratio and it's the clearest thing that's *Sayid's own*
  rather than a lesser version of what FlowStorm offers.
- **ClojureScript capture** - wide-open territory; the data-first protocol makes a
  cljs engine a backend swap rather than a rewrite of everything.
- **Ecosystem integrations** - first-class Portal/Reveal/Morse taps, and a small
  web UI that consumes the data ops.

**Done when:** Sayid has at least one capability (golden-trace or cljs) that is
clearly its own, not a worse version of what FlowStorm or a debugger offers.

### 6. Retire the legacy text renderer *(proposed)*

**Goal:** finish the data-first migration by removing the server-side text
renderer, so rendering lives entirely in the client.

**Why:** with the Emacs client rendering from the data ops (initiative 3), the
text-output path - `string-output`, the rendered nREPL ops, and the `tamarin`
value-pretty-printer dependency - is now largely legacy. It's the last place the
protocol layer owns presentation, and the biggest chunk of code the data-first
direction makes redundant.

**Approach:**
- Audit which rendered ops and Emacs commands are still live (the classic text
  `sayid-get-workspace` buffer, `sayid-query`, `sayid-pprint-value` / `c p`,
  `sayid-def-value` / `c d`).
- Re-home the still-wanted ones onto the data ops plus CIDER's inspector (value
  inspection is already there via `c i`).
- Delete `string-output`, the rendered ops, and drop `tamarin`.

**Risks:** it's a user-visible change - some people may rely on the text buffer or
the pprint/def-value commands; those need a data-op-backed replacement before the
old path goes.

**Done when:** the middleware returns only data, `tamarin` is gone, and no Emacs
command depends on a server-rendered op.

## Sequencing rationale

```
(1) decouple the core   ─┬─►  (3) data, not strings  ──►  (5) position & extend
     [done]              │         [done]                      [in progress]
                         └─►  (4) AST inner tracing  ──►  (6) retire text renderer
                                   [done]                      [proposed]
(2) bound the recording  ────────────────────────────►  (5)
     [done]
```

Initiatives 1-4 are done: the core is decoupled, the recording is bounded, the
protocol returns data, and inner tracing runs off the AST. What's left is the
payoff and the last cleanup. Initiative 5 (position & extend) is the point of the
whole revival - a capability that's clearly Sayid's own - and golden-trace testing
is the first move. Initiative 6 (retire the legacy text renderer) is the natural
close of the data-first migration and can happen whenever; it's independent of 5.
