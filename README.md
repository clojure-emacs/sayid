![Sayid logo](./sayid-logo.png)

----------
[![Clojure CI](https://github.com/clojure-emacs/sayid/actions/workflows/clojure.yml/badge.svg)](https://github.com/clojure-emacs/sayid/actions/workflows/clojure.yml)
[![Clojars Project](https://img.shields.io/clojars/v/mx.cider/sayid.svg)](https://clojars.org/mx.cider/sayid)
[![cljdoc badge](https://cljdoc.org/badge/mx.cider/sayid)](https://cljdoc.org/d/mx.cider/sayid/CURRENT)

Sayid *(siy EED)* is an omniscient debugger and profiler for Clojure. It extracts secrets from code at run-time.

The name is a nod to Sayid Jarrah from [*Lost*](https://en.wikipedia.org/wiki/Sayid_Jarrah) - the show's resident interrogator, who was famously good at extracting secrets. Which is exactly what this tool does to your code, hence the tagline. And yes, the *siy EED* pronunciation above is the character's too, so you can say it with confidence.

Sayid works by intercepting and recording the inputs and outputs of
functions. It can even record function calls that occur inside of
functions. The user can select which functions to trace. Functions can
be selected individually or by namespace. The recorded data can be
displayed, queried and profiled.

Sayid currently has three components:

* `sayid.core` and its supporting namespaces
* [nREPL](https://nrepl.org) middleware
* A [CIDER](https://cider.mx) plugin

The `sayid.core` namespace is designed to be used directly via a REPL and does
not require Emacs or CIDER. **BUT** the CIDER integration offers a far
better experience, so it is the current focus of this page and my
development efforts.

**We're looking for more maintainers for the project. If you're interested in helping out please ping @bbatsov.**

## Why Sayid?

Sayid is an *omniscient* debugger: you run your code first, at full speed, and
explore what happened afterwards. That's a fundamentally different model from the
two tools people usually reach for.

**Versus `clojure.tools.trace`.** tools.trace prints call/return lines as a side
effect - a wall of text that scrolls past and is gone. Sayid records the same
calls (and, with inner tracing, every intermediate value *inside* a function)
into a navigable, queryable data structure. You can slice it by function or by
call id, walk into a call's children, or jump to its ancestors. One is
scrollback; the other is a record you can interrogate.

**Versus a stepping debugger.** A breakpoint debugger asks you to predict where
the bug is *before* you run, stops the world when it hits a breakpoint (rough on
concurrency, timing-sensitive code, hot loops, and async callbacks), shows you
one stack frame at a time, and forgets everything the moment you continue. Sayid
inverts all of that: run first, explore after. The whole execution is already
captured, you see the entire call tree at once, and you can revisit any part of
it as many times as you like, because it's data, not a paused process.

In short: tools.trace tells you what happened in the order it happened; a
debugger lets you watch it happen *if* you knew where to stand; Sayid hands you
the whole execution as data, after the fact, to query however you want.

## Installation & Requirements

> [!NOTE]
> Starting with 0.2.0 Sayid is published under the `mx.cider/sayid` coordinates.
> The old `com.billpiel/sayid` coordinates are deprecated but still receive the
> same releases for now, so existing dependencies keep working. Please switch to
> `mx.cider/sayid` when you get a chance.

### Requirements

Basic usage requires Clojure 1.10+ running on Java 8 or newer. The optional
nREPL middleware requires nREPL 1.0+, and the Emacs client requires CIDER 1.0+
on Emacs 28+.

(Sayid is tested against Clojure 1.10, 1.11 and 1.12 on a range of JDKs. Older
Clojure versions may still work, but they're no longer part of the test matrix.)

nREPL-powered editor plugins are encouraged to make use of the bundled middleware that
provides a very flexible Sayid API. Its ops are documented in
[doc/nrepl-api.md](doc/nrepl-api.md).

### Leiningen

Add this to the dependencies in your project.clj or lein profiles.clj:

    [mx.cider/sayid "0.7.0"]

To use the bundled nREPL middleware, you'll want to include Sayid as a
plug-in. Here's an example of a bare-bones profiles.clj that works for
me:

```clojure
{:user {:plugins [[mx.cider/sayid "0.7.0"]]}}
```

### Clojure CLI - deps.edn

Add a the Sayid dependency to your `:deps` key. Depending on your
desired setup, you may want to add it to an optional profile, or your
tools.deps config directory (often `$HOME/.clojure`).

```clojure
{:deps
  {mx.cider/sayid {:mvn/version "0.7.0"}}}
```

### Emacs Integration

CIDER setup also requires that the Emacs package `sayid` is installed. It's
available on [MELPA](https://melpa.org/#/sayid) and [MELPA
Stable](https://stable.melpa.org/#/sayid). Put this code in `init.el`, or
somewhere, to load keybindings for clojure-mode buffers.

```elisp
(with-eval-after-load 'clojure-mode
  (sayid-setup-package))
```

If you use CIDER's jack-in commands, then Sayid automatically adds the
Maven dependency when starting a REPL. This means you don't need to
manually add the dependency to your `project.clj` or `deps.edn` file.

If you don't use CIDER's jack-in commands, you'll need to add a
dependency manually. Here's an example of a bare-bones profiles.clj
that works for me:

```clojure
{:user {:plugins [[cider/cider-nrepl "0.59.0"]
                  [mx.cider/sayid "0.7.0"]]
        :dependencies [[nrepl/nrepl "1.3.1"]]}}
```

Usually you'll want to use the latest versions of `cider-nrepl` and nREPL here.

### Other Editors

A 3rd-party vim plugin also exists. See
[this](http://arsenerei.com/blog/posts/2017-02-24-vim-sayid/) and
[this](https://github.com/arsenerei/vim-sayid).

## Using Sayid from the REPL

You don't need Emacs or CIDER to use Sayid. The `sayid.core`
namespace (conventionally aliased to `sd`) is a complete API on its own. Trace
a namespace or a function, exercise your code, then print the recorded
workspace:

```clojure
(require '[sayid.core :as sd])

(defn add [a b] (+ a b))
(defn add-twice [a b] (+ (add a b) (add a b)))

;; Trace every function in the current namespace.
(sd/ws-add-trace-ns! user)

(add-twice 3 4)

(sd/ws-print)
```

That prints the full call tree, including the nested calls to `add`, with the
arguments and return value of each invocation:

```
v user/add-twice  :2887
| a => 3
| b => 4
| returns =>  14
|v user/add  :2888
|| a => 3
|| b => 4
|| returned =>  7
|^
|v user/add  :2889
|| a => 3
|| b => 4
|| returned =>  7
|^
| user/add-twice  :2887
| returned =>  14
^
```

Some other useful entry points:

* `(sd/ws-add-trace-fn! my-ns/my-fn)` traces a single function.
* `(sd/ws-add-inner-trace-fn! my-ns/my-fn)` adds an *inner* trace that also
  captures every expression evaluated inside the function.
* `(sd/ws-clear-log!)` clears the recorded calls without removing the traces.
* `(sd/ws-reset!)` removes all traces and clears the log.

## Querying the workspace

For anything bigger than a toy example, printing the whole tree is too much. The
query DSL pulls out just the calls you care about. `sd/ws-query-print` (aliased
`sd/q`) queries the active workspace and prints the matches; `sd/ws-query` returns
them as data.

```clojure
;; every recorded call of a particular function, by name
(sd/q [:name 'my.ns/place-order])

;; a bare symbol is shorthand for [:name ...]
(sd/q my.ns/place-order)

;; match names with a regex
(sd/q [:name #"my\.ns/.*"])
```

A query can also pull in a match's context - `:a` for its ancestors (the calls
that led to it) and `:d` for its descendants (everything underneath):

```clojure
(sd/q :a [:name 'my.ns/deep-fn])   ; the matching calls plus their ancestors
```

The same queries run against a saved recording (`sd/rec-query`) or any trace tree
you already hold (`sd/tree-query`).

## Profiling

Because every call is timed, the workspace doubles as a profiler. `sd/pro-analyze`
rolls the per-function timings up from a trace tree, and the report printers sort
them for you:

```clojure
(sd/ws-add-trace-ns! my.ns)
(my.ns/run)

(-> (sd/ws-deref!) sd/pro-analyze sd/pro-net-time)
```

```
|          :name | :net-time-sum | :net-time-avg | :count | :gross-time-sum | :gross-time-avg |
|----------------+---------------+---------------+--------+-----------------+-----------------|
| :my.ns/collect |            12 |           6.0 |      2 |             820 |           410.0 |
|    :my.ns/slow |           808 |         404.0 |      2 |             808 |           404.0 |
```

`sd/pro-net-time` sorts by *net* time - time in the function minus the time spent
in its children, so high scorers are candidates for optimisation.
`sd/pro-gross-repeats` instead sorts by time spent on *repeated* arguments,
flagging functions that might benefit from memoisation.

## Saving & loading recordings

A workspace is transient - the next `ws-reset!` wipes it. To keep a trace around,
snapshot it into a *recording* and shelve it under a name:

```clojure
(sd/ws-add-trace-ns! my.ns)
(my.ns/run)

(sd/rec-load-from-ws!)      ; snapshot the workspace into the active recording
(sd/rec-save-as! 'before)   ; shelve that recording under the slot `before`
```

Later - even after resetting the workspace, or tracing something else - load it
back and print or query it like any other trace:

```clojure
(sd/rec-load! 'before)
(sd/rec-print)
(sd/rec-query [:name 'my.ns/place-order])
```

## Views

Both the printed and the tree output are filtered through a *view* - a small
predicate that decides which parts of each node to show (arguments, return value,
inner-trace selections, and so on). The default view keeps the output readable on
large traces; `sd/set-view!` swaps it out, and in the editor `C-c s V s` sets a
view while the workspace buffer's `v` / `V` keys toggle and change it.

## The trace is data

The recorded execution isn't just something to look at - it's a plain Clojure data
structure you can script. `sayid.data/trace-data` hands you the call tree with
keyword keys and the captured values kept *live*, so ordinary `map`/`filter`/`->>`
work on it:

```clojure
(require '[sayid.core :as sd] '[sayid.data :as sd-data])

(sd/ws-add-trace-ns! my.ns)
(my.ns/run)

;; every recorded call that took longer than 100ms
(->> (sd-data/trace-data)
     (tree-seq :children :children)
     (filter #(some-> (:ms %) (> 100))))
```

And since it's just data, exploring a trace in
[Portal](https://github.com/djblue/portal), Reveal or Morse is a one-liner -
`(sd-data/tap-trace!)` taps the whole tree to your data viewer of choice, no
Sayid-specific rendering involved. From the editor that's `C-c s d t`
(`sayid-tap-trace`); `C-c s d b` then `C-c s d d` snapshot the current trace and
diff a later run against it.

## Golden-trace testing

Because a recorded trace is just data, you can pin it down as a test baseline.
`sayid.golden` traces some code, runs it, and asserts the recorded call tree - the
functions called, their arguments and return values, the nesting, and (with inner
tracing) every intermediate expression value - still matches a stored golden file.
It's regression testing at the level of *how* your code ran, which a plain
return-value assertion can't see:

```clojure
(require '[sayid.core :as sd]
         '[sayid.golden :as gold]
         '[clojure.test :refer [deftest is]])

(deftest orders-golden
  (sd/ws-reset!)
  (sd/ws-add-trace-ns! my.orders)
  (my.orders/place-order sample-order)
  (is (gold/matches-golden? "place-order")))
```

The first run writes `test/golden/place-order.edn` for you to review and commit;
later runs compare against it and fail with a diff when the execution changes.
When a change is intentional, regenerate the goldens by running with
`SAYID_GOLDEN_UPDATE=1` set (or `(binding [gold/*update* true] ...)`).

For ad-hoc comparison there's `gold/diff-traces`, which structurally diffs two
captured traces and reports exactly which calls and values changed - handy for
confirming a refactor didn't alter behaviour:

```clojure
(def before (do (sd/ws-reset!) (sd/ws-add-trace-ns! my.ns) (my.ns/run) (gold/golden-trace)))
;; ... change and reload my.ns ...
(def after  (do (sd/ws-reset!) (sd/ws-add-trace-ns! my.ns) (my.ns/run) (gold/golden-trace)))

(gold/diff-traces before after)   ; [] means the two runs executed identically
```

With an *inner* trace the golden captures the values *inside* each function too, so
a baseline for `(fn [a b] (let [s (+ a b)] (if (> s 10) (* s 2) s)))` records
`(+ a b) => 13`, `(> s 10) => true` and `(* s 2) => 26` - a regression net neither
`tools.trace` nor a stepping debugger can give you.

## Using Sayid

**Note: this assumes you're using the official CIDER plugin.**

The keybindings are grouped by buffer below. Every list is also available
from within Emacs: press `h` in any Sayid buffer (or `C-c s h` in a Clojure
buffer) to pop up the matching help buffer.

API docs for the core namespaces are available on
[cljdoc](https://cljdoc.org/d/mx.cider/sayid/CURRENT).

In a clojure-mode buffer, press `C-c s h` (`sayid-show-help`) to
pop up the help buffer.

    C-c s f -- Queries the active workspace for entries that most closely match the context of the cursor position
    C-c s ! -- Disable traces, load the current buffer, enable traces, and clear the workspace log
    C-c s w -- Show the recorded workspace as a navigable, foldable tree
    C-c s t y -- Prompts for a dir, recursively traces all ns's in that dir and subdirs
    C-c s t p -- Prompts for a pattern (* = wildcard), and applies a trace to all *loaded* ns's whose name matches the pattern
    C-c s t b -- Trace the ns in the current buffer
    C-c s t e -- Enable the *existing* (if any) trace of the function at point
    C-c s t E -- Enable all traces
    C-c s t d -- Disable the *existing* (if any) trace of the function at point
    C-c s t D -- Disable all traces
    C-c s t n -- Apply an inner trace to the symbol at point
    C-c s t o -- Apply an outer trace to the symbol at point
    C-c s t r -- Remove existing trace from the symbol at point
    C-c s t K -- Remove all traces
    C-c s d t -- Tap the recorded workspace to your data tool (Portal, Reveal, Morse)
    C-c s d b -- Snapshot the current trace as a baseline for comparison
    C-c s d d -- Diff the current trace against the baseline and tap the result
    C-c s c -- Clear the workspace trace log
    C-c s x -- Blow away workspace -- traces and logs
    C-c s s -- Popup buffer showing what it currently traced
    C-c s S -- Popup buffer showing what it currently traced in buffer's ns
    C-c s V s -- Set the view
    C-c s h -- show this help


The workspace opens in the `*sayid-tree*` buffer, a foldable tree built on
CIDER's `cider-tree-view`. Navigation and folding come from there; Sayid adds a
few actions on top:

    TAB -- fold or unfold the call at point
    RET, . -- jump to the call's source
    n, p -- move to the next / previous call
    f -- show every recorded call of the function at point (prefix: a modifier)
    i -- focus the call at point and its subtree (prefix: ancestors/descendants)
    c i -- inspect a captured value in CIDER's inspector (prefix: pick which)
    w -- back to the full workspace
    q -- quit the window

The older text-rendered view is still available (`M-x sayid-get-workspace`, and
where the pretty-print and query-at-point commands land); press `h` in it for its
own keybinding help.
    g -- generate instance expression and put in kill ring
    h -- help
    q -- quit window


`C-c s s` (`sayid-show-traced`) shows what's traced as a namespaces to functions
tree in the `*sayid-traced*` buffer. `RET` on a function jumps to its source;
`TAB` folds a namespace, `n`/`p` move, `q` quits. `e`/`d`/`r` enable, disable and
remove the trace at point, and `i`/`o` switch a function to an inner or outer
trace.


In the `*sayid-pprint*` buffer, press `h` to pop up the help
buffer.

    ENTER -- show path in mini-buffer
    i -- jump into child node
    o -- jump out to parent node
    n -- jump to next sibling node
    p -- jump to previous sibling node
    l -- back to trace buffer
    h -- help
    q -- quit window

## Demos

### Conj 2016 Presentation

I [presented Sayid](https://www.youtube.com/watch?v=ipDhvd1NsmE) at the Clojure
Conj conference in Austin in 2016.

[![Becoming Omniscient with Sayid - Bill
Piel](http://img.youtube.com/vi/ipDhvd1NsmE/0.jpg)](http://www.youtube.com/watch?v=ipDhvd1NsmE
"Becoming Omniscient with Sayid - Bill Piel")

### Demo \#1 - Video

A [demo video](https://www.youtube.com/watch?v=wkduA4py-qk) I recorded after the
very first alpha release. You can find the [contrived
example](http://github.com/bpiel/contrived-example) project here.

[![Sayid v0.0.1 - Demo
#1](http://img.youtube.com/vi/wkduA4py-qk/0.jpg)](http://www.youtube.com/watch?v=wkduA4py-qk
"Sayid v0.0.1 - Demo #1")

### Demo \#1 - Walkthrough

Let's hunt a bug with Sayid. Drop this namespace into a file and open it in
Emacs:

```clojure
(ns demo.coins)

(def coin-values
  {:quarter 25
   :dime 10
   :nickel 5
   :penny 5})

(defn total-cents
  [coins]
  (->> coins
       (map coin-values)
       (apply +)))

(defn can-afford?
  [coins price]
  (>= (total-cents coins) price))
```

It adds up a handful of coins and checks whether they cover a price. There's a
bug in here; see if you can spot it before Sayid does.

Load the buffer with `C-c C-k` (`cider-load-buffer`), then trace its namespace
with `C-c s t b` (`sayid-trace-ns-in-file`).

Now exercise the code from the REPL. A quarter, a dime, a nickel and a penny add
up to 41 cents, so this should be `false`:

```clojure
demo.coins=> (can-afford? [:quarter :dime :nickel :penny] 45)
true
```

It says `true`. Something's off. Pop open the Sayid workspace with `C-c s w`
(`sayid-tree-view-workspace`):

```
 Sayid workspace   n/p: move   TAB: expand   RET/.: visit   q: quit
▾ (demo.coins/can-afford? [:quarter :dime :nickel :penny] 45) => true
    (demo.coins/total-cents [:quarter :dime :nickel :penny]) => 45
```

Every traced call is a foldable node, with its arguments and return value inline
(`TAB` folds a subtree, `RET` jumps to source). `total-cents` got our four coins
and returned `45`, but four coins worth 41 cents can't total 45. The bug lives
inside `total-cents`.

This is where Sayid earns its keep. Put your cursor on `total-cents` and add an
*inner* trace with `C-c s t n` (`sayid-inner-trace-fn`). Clear the log with
`C-c s c` (`sayid-clear-log`) so we start fresh, run the call again, and reopen
the workspace:

```
▾ (demo.coins/can-afford? [:quarter :dime :nickel :penny] 45) => true
  ▾ (demo.coins/total-cents [:quarter :dime :nickel :penny]) => 45
    ▾ (apply + (map coin-values coins)) => 45
        (map coin-values coins) => (25 10 5 5)
```

An inner trace records the output of *every expression* inside the function, each
one a node you can fold into. Follow it down to `(map coin-values coins)`: it
turns our coins into `(25 10 5 5)`. There it is. The last value should be `1`, not
`5` - a penny is worth five cents in our map.

Press `RET` on that line to jump straight to the source. Fix `coin-values` so
`:penny` maps to `1`, then reload the Sayid way with `C-c s !`
(`sayid-load-enable-clear`): it removes the traces, reloads the buffer,
re-applies the traces and clears the log in one go. Run the call once more:

```clojure
demo.coins=> (can-afford? [:quarter :dime :nickel :penny] 45)
false
```

Bug fixed, and we never reached for a single `println`. That's Sayid.

## License

Distributed under the Apache 2.0 License. See [LICENSE](LICENSE) for details.
