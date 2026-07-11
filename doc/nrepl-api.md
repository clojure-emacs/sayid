# Sayid nREPL API

Sayid ships an nREPL middleware (`sayid.nrepl-middleware/wrap-sayid`)
that exposes its tracing engine over the wire. The bundled Emacs client talks to
Sayid exclusively through these ops, and any other editor or tool can do the same.

This document is the reference for that wire API.

## Loading the middleware

With Leiningen, add Sayid as a plugin (it registers the middleware automatically):

```clojure
{:user {:plugins [[mx.cider/sayid "0.7.1"]]}}
```

Or add the middleware explicitly when you start the server, e.g. with the
Clojure CLI / `nrepl.server`:

```clojure
(nrepl.server/start-server
 :handler (nrepl.server/default-handler #'sayid.nrepl-middleware/wrap-sayid))
```

## Conventions

Send a normal nREPL message with an `"op"` key naming one of the ops below, plus
whatever parameters that op takes. Every op terminates the request with a
`:status` of `"done"`, so a synchronous client can block until it arrives.

Ops fall into two shapes:

- **Command ops** just do their work and reply with `:status "done"` (no value).
- **Query ops** reply with a `:value`, then `:status "done"`.

Response **values are already decoded** - Sayid encodes them into plain
bencode-friendly data (nested lists, strings and numbers) before they go on the
wire, so by the time your nREPL client hands you a value it is ordinary data. Do
**not** run it through a reader. A few encoding rules are worth knowing:

- keywords arrive as their bare name strings (`:foo` -> `"foo"`),
- booleans arrive as `1` / `nil`,
- anything Sayid can't represent structurally arrives as its printed string.

If an op throws, the response carries `:status #{"error" "done"}` along with
`:err` (a stack trace) and `:ex` (the exception class), the same convention
cider-nrepl uses, so the failure surfaces instead of hanging the client.

Several ops resolve "the thing at the cursor" server-side. They take the buffer
`source` (the whole file as a string) plus a 1-based `line` and `column`, and
Sayid parses the source to find the symbol there. This keeps editors from having
to reimplement Clojure-aware symbol resolution.

## Applying and managing traces

### `sayid-trace-fn-at-point`

Apply a trace action to the function whose symbol is at the cursor.

| param | meaning |
|-------|---------|
| `action` | one of `add-outer`, `add-inner`, `enable`, `disable`, `remove` |
| `file` | path of the buffer's file |
| `line`, `column` | 1-based cursor position |
| `source` | the buffer's full text |

Replies with the resolved qualified symbol (e.g. `"my.ns/my-fn"`), or `nil` when
nothing resolves at that position.

### `sayid-trace-fn`

Apply a trace action to an explicitly named function.

| param | meaning |
|-------|---------|
| `action` | one of `add-outer`, `add-inner`, `enable`, `disable`, `remove` |
| `fn-ns` | the function's namespace |
| `fn-name` | the function's name |

`add-outer` records calls to the function; `add-inner` additionally records every
expression evaluated inside it. `enable`/`disable` flip an existing trace on and
off; `remove` tears it down. Replies with `:status "done"`.

### `sayid-trace-ns`

Apply a trace action to a whole namespace.

| param | meaning |
|-------|---------|
| `action` | one of `enable`, `disable`, `remove` |
| `ns` | the namespace name |

Replies with `:status "done"`.

### `sayid-all-traces`

Apply a trace action to every trace at once.

| param | meaning |
|-------|---------|
| `action` | one of `enable`, `disable`, `remove` |

Replies with `:status "done"`.

### `sayid-trace-ns-in-file`

Trace the namespace declared in `file`. Param: `file`. Replies `:status "done"`.

### `sayid-trace-ns-by-pattern`

Trace every loaded namespace whose name matches a pattern.

| param | meaning |
|-------|---------|
| `ns-pattern` | a namespace pattern, where `*` is a wildcard |
| `ref-ns` | a namespace to resolve the pattern relative to |

Replies with `:status "done"`.

### `sayid-trace-all-ns-in-dir`

Recursively trace every namespace found under `dir`. Param: `dir`. Replies
`:status "done"`.

## Inspecting recorded calls

### `sayid-get-workspace`

Render the current workspace with the active view. Takes no params. Replies with
the rendered tree as a `[text text-properties query-args]` triple, where `text`
is the printable output, `text-properties` describes per-span coloring/metadata,
and `query-args` is the printed query that produced it.

### `sayid-get-workspace-data`

Return the recorded call tree *as data* instead of pre-rendered text, for clients
that want to render or analyze it themselves: an editor that isn't Emacs, a
[Portal](https://github.com/djblue/portal) tap, a REPL one-liner. Takes no
params. Replies with a `:value` that is a list of root call nodes, sent as native
bencode data (real nested maps and lists, not a printed string), so you navigate
it by key without reading anything back.

Each node is a map with these string keys:

| key | meaning |
|-----|---------|
| `id` | the call's id |
| `name` | the fully qualified function name |
| `form` | for an inner-trace node, the recorded source expression (absent for a plain call) |
| `depth` | nesting depth (1 for a root call) |
| `started-at`, `ended-at` | capture timestamps, in milliseconds |
| `args` | the arguments, each `pr-str`'d |
| `arg-map` | arg name to `pr-str`'d value |
| `return` | the return value, `pr-str`'d; absent if the call threw |
| `throw` | present instead of `return` when the call threw: `{cause, class, data}` |
| `file`, `line` | source location, for jumping to the definition |
| `children` | the calls (and, under an inner trace, the sub-expressions) made within this one, same shape |

The structural fields are native data; the captured values (`args`, `arg-map`,
`return`, the `throw` parts) are printed strings, because an arbitrary Clojure
value can't always round-trip over the wire. They're printed under bounded
`*print-length*`/`*print-level*`, so a fat or infinite value produces a truncated
string rather than a runaway payload. The rendered counterpart is
`sayid-get-workspace`.

### `sayid-query`

Run a query against the workspace. Param: `query` (a printed Clojure query
form). Replies with the same `[text properties query-args]` triple as
`sayid-get-workspace`.

### `sayid-query-data`

Data counterpart of `sayid-query`. Same `query` param, but replies with the
matched calls as a list of node-data maps (the `sayid-get-workspace-data` shape)
instead of the rendered triple.

### `sayid-query-form-at-point`

Query the workspace for the calls that best match the cursor context. Params:
`file`, `line`. Replies with a `[text properties]` pair.

### `sayid-query-by-id`

Query for a recorded call by id, optionally with a modifier.

| param | meaning |
|-------|---------|
| `trace-id` | the id of the recorded call |
| `mod` | a modifier string, e.g. `"a"` (ancestors), `"d"` (descendants), optionally with a depth |

Replies with a `[text properties query-args]` triple.

### `sayid-query-by-id-data`

Data counterpart of `sayid-query-by-id`. Same `trace-id` and `mod` params,
replies with node data (the `sayid-get-workspace-data` shape).

### `sayid-query-by-fn`

Like the above, but selects by function name. Params: `fn-name`, `mod`. Replies
with a `[text properties query-args]` triple.

### `sayid-query-by-fn-data`

Data counterpart of `sayid-query-by-fn`. Same `fn-name` and `mod` params,
replies with node data (the `sayid-get-workspace-data` shape).

## Acting on a recorded call

These operate on a single node in the rendered output, identified by `trace-id`
(and `path` for nested values).

### `sayid-gen-instance-expr`

Generate an expression that reproduces a recorded call, def-ing its arguments to
`$s/*` vars along the way. Param: `trace-id`. Replies with the expression string
(e.g. `"(my.ns/my-fn $s/a $s/b)"`).

### `sayid-def-value`

Def the value at `path` within the call `trace-id` to `$s/*`. Params: `trace-id`,
`path`. Replies with a confirmation value.

### `sayid-pprint-value`

Pretty-print the value at `path` within the call `trace-id`. Params: `trace-id`,
`path`. Replies with the pretty-printed text.

## Views

### `sayid-set-view`

Select a named view for subsequent rendering. Param: `view-name`. Replies
`:status "done"`.

### `sayid-toggle-view`

Toggle between the default and the verbose view. Takes no params. Replies with
the new state.

### `sayid-get-views`

List the registered view names. Takes no params. Replies with the view names.

## Workspace lifecycle

### `sayid-clear-log`

Clear the recorded calls but keep the traces in place. Takes no params. Replies
`:status "done"`.

### `sayid-reset-workspace`

Remove all traces and clear the recorded calls. Takes no params. Replies
`:status "done"`.

## Introspection

### `sayid-version`

Replies with the Sayid version string (e.g. `"0.7.1"`).

### `sayid-get-trace-count`

Replies with the number of traces in the workspace.

### `sayid-get-enabled-trace-count`

Replies with the number of currently enabled traces.

### `sayid-get-meta-at-point`

Resolve the symbol at the cursor and report its metadata. Params: `source`,
`file`, `line`.

### `sayid-find-all-ns-roots`

Replies with the distinct source roots of the loaded namespaces (e.g.
`["src" "test"]`), handy for offering a directory to scan.

### `sayid-show-traced`

Render the list of what's currently traced. Optional param: `ns` (limit to one
namespace). Replies with the rendered tree.

### `sayid-show-traced-data`

Data counterpart of `sayid-show-traced`. Optional param: `ns`. Replies with a
list of namespace groups, each a map of `{ns, fns}` where `fns` is a list of
`{name, ns, file, line, trace-type}` maps - what's traced, as data for the client
to render.
