![Sayid logo](./sayid-logo.png)

----------
[![Clojure CI](https://github.com/clojure-emacs/sayid/actions/workflows/clojure.yml/badge.svg)](https://github.com/clojure-emacs/sayid/actions/workflows/clojure.yml)
[![Clojars Project](https://img.shields.io/clojars/v/mx.cider/sayid.svg)](https://clojars.org/mx.cider/sayid)
[![cljdoc badge](https://cljdoc.org/badge/mx.cider/sayid)](https://cljdoc.org/d/mx.cider/sayid/CURRENT)

Sayid *(siy EED)* is an omniscient debugger and profiler for Clojure. It extracts secrets from code at run-time.

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

    [mx.cider/sayid "0.3.0"]

To use the bundled nREPL middleware, you'll want to include Sayid as a
plug-in. Here's an example of a bare-bones profiles.clj that works for
me:

```clojure
{:user {:plugins [[mx.cider/sayid "0.3.0"]]}}
```

### Clojure CLI - deps.edn

Add a the Sayid dependency to your `:deps` key. Depending on your
desired setup, you may want to add it to an optional profile, or your
tools.deps config directory (often `$HOME/.clojure`).

```clojure
{:deps
  {mx.cider/sayid {:mvn/version "0.3.0"}}}
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
                  [mx.cider/sayid "0.3.0"]]
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
    C-c s w -- Shows workspace, using the current view
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
    C-c s c -- Clear the workspace trace log
    C-c s x -- Blow away workspace -- traces and logs
    C-c s s -- Popup buffer showing what it currently traced
    C-c s S -- Popup buffer showing what it currently traced in buffer's ns
    C-c s V s -- Set the view
    C-c s h -- show this help


In the `*sayid*` buffer, press `h` to pop up a help buffer listing the current
keybindings (it's generated from the keymap, so it's always accurate):

    ENTER -- pop to function
    d -- def value to $s/*
    f -- query for calls to function (with a prefix arg, prompt for a modifier)
    i -- show only this instance (with a prefix arg, prompt for a modifier)
    r -- refresh the view (rerun the last query)
    w -- show full workspace trace
    n -- jump to next call node
    p -- jump to prev call node
    P -- pretty print value
    C -- clear workspace trace log
    v -- toggle view
    V -- set view (see register-view)
    l, backspace -- previous buffer state
    L, S-backspace -- forward buffer state
    c i -- inspect value at point
    g -- generate instance expression and put in kill ring
    h -- help
    q -- quit window


In the `*sayid-traced*` buffer, press `h` to pop up the help
buffer.

    enter -- Drill into ns at point
    e -- Enable trace
    d -- Disable trace
    E -- Enable ALL traces
    D -- Disable ALL traces
    i -- Apply inner trace to func at point
    o -- Apply outer trace to func at point
    r -- Remove trace from func at point
    l, backspace -- go back to trace overview (if in ns view)
    h -- help
    q -- quit window


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
(`sayid-get-workspace`):

```
v demo.coins/can-afford?  :6303
| coins => [:quarter :dime :nickel :penny]
| price => 45
| returns =>  true
|v demo.coins/total-cents  :6304
|| coins => [:quarter :dime :nickel :penny]
|| returned =>  45
|^
| demo.coins/can-afford?  :6303
| returned =>  true
^
```

Every traced call is here, with its arguments and return value. `total-cents`
got our four coins and returned `45`, but four coins worth 41 cents can't total
45. The bug lives inside `total-cents`.

This is where Sayid earns its keep. Put your cursor on `total-cents` and add an
*inner* trace with `C-c s t n` (`sayid-inner-trace-fn`). Clear the log with
`C-c s c` (`sayid-clear-log`) so we start fresh, run the call again, and reopen
the workspace:

```
v demo.coins/can-afford?  :6346
| coins => [:quarter :dime :nickel :penny]
| price => 45
| returns =>  true
|v demo.coins/total-cents  :6347
|| coins => [:quarter :dime :nickel :penny]
|| returns =>  45
||v (->> coins (map coin-values) (apply +)) => (apply + (map coin-values coins))  demo.coins/total-cents  :6348
||| returns =>  45
|||v (apply + (map coin-values coins))  demo.coins/total-cents  :6349
|||| (25 10 5 5)
|||| returns =>  45
||||v (map coin-values coins)  demo.coins/total-cents  :6350
||||| {:quarter 25 :dime 10 :nickel 5 :penny 5}
||||| [:quarter :dime :nickel :penny]
||||| returned =>  (25 10 5 5)
||||^
...
```

An inner trace records the inputs and output of *every expression* inside the
function. Follow it down to `(map coin-values coins)`: it turns our coins into
`(25 10 5 5)`. There it is. The last value should be `1`, not `5` - a penny is
worth five cents in our map.

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
