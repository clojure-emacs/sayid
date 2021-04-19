![Sayid logo](./sayid-logo.png)

----------
[![CircleCI](https://circleci.com/gh/clojure-emacs/sayid/tree/master.svg?style=svg)](https://circleci.com/gh/clojure-emacs/sayid/tree/master)
[![Clojars Project](https://img.shields.io/clojars/v/com.billpiel/sayid.svg)](https://clojars.org/com.billpiel/sayid)
[![cljdoc badge](https://cljdoc.org/badge/com.billpiel/sayid)](https://cljdoc.org/d/com.billpiel/sayid/CURRENT)

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

The `sayid.core` namespace is designed to be used directly via a repl and does
not require Emacs or CIDER. **BUT** the CIDER integration offers a far
better experience, so it is the current focus of this page and my
development efforts.

**We're looking for more maintainers for the project. If you're interested in helping out please ping @bbatsov.**

## Installation & Requirements

### Requirements

Basic usage requires Clojure 1.7 and the optional nREPL middleware requires nREPL 0.4+.

nREPL-powered editor plugins are encouraged to make use of the bundled middleware that
provides a very flexible Sayid API.

### Leiningen

Add this to the dependencies in your project.clj or lein profiles.clj:

    [com.billpiel/sayid "0.1.0"]

To use the bundled nREPL middleware, you'll want to include Sayid as a
plug-in. Here's an example of a bare-bones profiles.clj that works for
me:

```clojure
{:user {:plugins [[com.billpiel/sayid "0.1.0"]]}}
```

### Clojure CLI - deps.edn

Add a the Sayid dependency to your `:deps` key. Depending on your
desired setup, you may want to add it to an optional profile, or your
tools.deps config directory (often `$HOME/.clojure`).

```clojure
{:deps
  {com.billpiel/sayid {:mvn/version "0.1.0"}}}
```

### Emacs Integration

CIDER setup also requires that the Emacs package `sayid` is installed. It's
available on [MELPA](https://melpa.org/#/sayid) and [MELPA
Stable](https://stable.melpa.org/#/sayid). Put this code in `init.el`, or
somewhere, to load keybindings for clojure-mode buffers.

```elisp
(eval-after-load 'clojure-mode
  '(sayid-setup-package))
```

If you use CIDER's jack-in commands, then Sayid automatically adds the
Maven dependency when starting a REPL. This means you don't need to
manually add the dependency to your `project.clj` or `deps.edn` file.

If you don't use CIDER's jack-in commands, you'll need to add a
dependency manually. Here's an example of a bare-bones profiles.clj
that works for me:

```clojure
{:user {:plugins [[cider/cider-nrepl "0.25.3"]
                  [com.billpiel/sayid "0.1.0"]]
        :dependencies [[nrepl/nrepl "0.7.0"]]}}
```

Usually you'll want to use the latest versions of `cider-nrepl` and nREPL here.

### Other Editors

A 3rd-party vim plugin also exists. See
[this](http://arsenerei.com/blog/posts/2017-02-24-vim-sayid/) and
[this](https://github.com/arsenerei/vim-sayid).

## Using Sayid

**Note: This assumes you're using the official CIDER plugin.**.

Documentation is a little light at the moment. There are lists of
keybindings. Helpfully, they are easily accessible from within emacs.
Below are the contents of the various help buffers, as well as
instructions on how to pop them up in time of need.

Generated docs are also available for the core namespace [here](doc).

In a clojure-mode buffer, press `C-c s h` (`sayid-show-help`) to
pop up the help buffer.

    C-c s ! -- Disable traces, eval current buffer, enable traces, clear the workspace log
    C-c s e -- Enables traces, evals the expression at point, disables traces, displays results with terse view
    C-c s f -- Queries the active workspace for entries that most closely match the context of the cursor position
    C-c s n -- Applies an inner trace to the function at point, replays workspace, displays results
    C-c s r -- Replays workspace, queries results by context of cursor
    C-c s w -- Shows workspace, using the current view
    C-c s t y -- Prompts for a dir, recursively traces all ns's in that dir and subdirs
    C-c s t p -- Prompts for a pattern (* = wildcare), and applies a trace to all *loaded* ns's whose name matches the patten
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


In the `*sayid*` buffer, press `h` to pop up the help buffer.

    ENTER -- pop to function
    d -- def value to $s/*
    f -- query for calls to function
    F -- query for calls to function with modifier
    i -- show only this instance
    I -- query for this instance with modifier
    w -- show full workspace trace
    n -- jump to next call node
    N -- apply inner trace and reply workspace
    p -- jump to prev call node
    P -- pretty print value
    C -- clear workspace trace log
    v -- toggle view
    V -- set view (see register-view)
    l, backspace -- previous buffer state
    L, S-backspace -- forward buffer state
    g -- generate instance expression and put in kill ring
    h -- help


In the `*sayid-traced*` buffer, press `h` to pop up the help
buffer.

    enter -- Drill into ns at point
    e -- Enable trace
    d -- Disable trace
    E -- Enable ALL traces
    D -- Disable ALL traces
    i -- Apply inner trace to func at point
    o -- Apply outer trace to func at point
    r -- Remove trace from fun at point
    l, backspace -- go back to trace overview (if in ns view)


In the `*sayid-pprint*` buffer, press `h` to pop up the help
buffer.

    ENTER -- show path in mini-buffer
    i -- jump into child node
    o -- jump out to parent node
    n -- jump to next sibling node
    p -- jump to previous sibling node

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

This is a written walkthrough of the same steps illustrated in the demo
video above, but with Sayid v0.0.8. You can find the [contrived
example](http://github.com/bpiel/contrived-example) project here.

Below is the code to the test namespace. You can see that we have a
vending machine that dispenses tacos for 85 cents. We execute the
`test1` function, which inserts 41 cents worth of change and presses the
taco button.

```clojure
(ns contrived-example.core-test
  (:require [clojure.test :refer :all]
            [contrived-example.core :as ce]))


(def test-vending-machine {:inventory {:a1 {:name :taco
                                            :price 0.85
                                            :qty 10}}
                           :coins-inserted []
                           :coins-returned []
                           :dispensed nil
                           :err-msg nil})

(defn test1 []
  (-> test-vending-machine
      (ce/insert-coin :quarter) ;; 25
      (ce/insert-coin :dime)    ;; 35
      (ce/insert-coin :nickel)  ;; 40
      (ce/insert-coin :penny)   ;; 41 cents
      (ce/press-button :a1)))   ;; taco costs 85 cents

(test1)
```

Let's press some keys to get Sayid going.

eval the namespace `C-c C-k` (probably) (`cider-load-buffer`)

trace the project namespaces [C-c s t p]{.kbd}
(`sayid-trace-ns-by-pattern`) then `contrived-example.*`

This should pop up. It shows how many functions have been traced in
which namespaces. Execute `test1`!

    Traced namespaces:
      5 / 5  contrived-example.core
      1 / 1  contrived-example.core-test
      8 / 8  contrived-example.inner-workings


    Traced functions:

You can't tell yet, but something magical happened. Press `C-c s
w` (`sayid-get-workspace`) to get an overview of what has been
captured in the Sayid workspace. This monster should appear:

    v contrived-example.core-test/test1  :13446
    |v contrived-example.core/insert-coin  :13447
    |^
    |v contrived-example.core/insert-coin  :13448
    |^
    |v contrived-example.core/insert-coin  :13449
    |^
    |v contrived-example.core/insert-coin  :13450
    |^
    |v contrived-example.core/press-button  :13451
    ||v contrived-example.inner-workings/valid-selection  :13452
    |||v contrived-example.inner-workings/get-selection  :13453
    |||^
    |||v contrived-example.inner-workings/calc-coin-value  :13454
    |||^
    ||| contrived-example.inner-workings/valid-selection  :13452
    ||^
    ||v contrived-example.inner-workings/process-transaction  :13455
    |||v contrived-example.inner-workings/get-selection  :13456
    |||^
    |||v contrived-example.inner-workings/calc-change-to-return  :13457
    ||||v contrived-example.inner-workings/calc-coin-value  :13458
    ||||^
    ||||v contrived-example.inner-workings/round-to-pennies  :13459
    ||||^
    ||||v contrived-example.inner-workings/calc-change-to-return*  :13460
    |||||v contrived-example.inner-workings/calc-coin-value  :13461
    |||||^
    |||||v contrived-example.inner-workings/calc-change-to-return*  :13462
    ||||||v contrived-example.inner-workings/calc-coin-value  :13463
    ||||||^
    ||||||v contrived-example.inner-workings/calc-change-to-return*  :13464
    |||||||v contrived-example.inner-workings/calc-coin-value  :13465
    |||||||^
    |||||||v contrived-example.inner-workings/calc-change-to-return*  :13466
    ||||||||v contrived-example.inner-workings/calc-coin-value  :13467
    ||||||||^
    |||||||| contrived-example.inner-workings/calc-change-to-return*  :13466
    |||||||^
    |||||||v contrived-example.inner-workings/calc-change-to-return*  :13468
    ||||||||v contrived-example.inner-workings/calc-coin-value  :13469
    ||||||||^
    |||||||| contrived-example.inner-workings/calc-change-to-return*  :13468
    |||||||^
    |||||||v contrived-example.inner-workings/calc-change-to-return*  :13470
    ||||||||v contrived-example.inner-workings/calc-coin-value  :13471
    ||||||||^
    |||||||| contrived-example.inner-workings/calc-change-to-return*  :13470
    |||||||^
    ||||||| contrived-example.inner-workings/calc-change-to-return*  :13464
    ||||||^
    |||||| contrived-example.inner-workings/calc-change-to-return*  :13462
    |||||^
    ||||| contrived-example.inner-workings/calc-change-to-return*  :13460
    ||||^
    |||| contrived-example.inner-workings/calc-change-to-return  :13457
    |||^
    ||| contrived-example.inner-workings/process-transaction  :13455
    ||^
    || contrived-example.core/press-button  :13451
    |^
    | contrived-example.core-test/test1  :13446
    ^

What's the meaning of this? These are all the function calls that were
made in the traced namespaced when we execute `test1`.

Let's explore. Get your cursor to the first line of the output and
press `i` (`sayid-query-id`).

     v contrived-example.core-test/test1  :13446
     | returned =>  {:inventory {:a1 {:name :taco :price 0.85 :qty 9}}
     |               :coins-inserted []
     |               :coins-returned [:quarter :quarter :nickel]
     |               :dispensed {:name :taco :price 0.85 :qty 10}
     |               :err-msg nil}
     ^


This shows us the details of that ***i**nstance* of `test1` being
called. We can see that a hash map was returned. Despite us inserting
only 41 cents for an 85 cent taco, we see that a taco was dispensed,
plus change! That's a BUG.

Hit `backspace` (`sayid-buf-back`). We're back at the overview.
Scan the list of functions that are called. Let's assume some
programmer's intuition and decide that `valid-selection` is the first
place of interest. Get your cursor to that line and press these keys to
view the ***i**nstance* and all of its ***d**escendants*. `I`
`d` `ENTER (`sayid-query-id-w-mode`)

     ||v contrived-example.inner-workings/valid-selection  :13452
     ||| machine => {:inventory {:a1 {:name :taco :price 0.85 :qty 10}}
     |||             :coins-inserted [:quarter :dime :nickel :penny]
     |||             :coins-returned []
     |||             :dispensed nil
     |||             :err-msg nil}
     ||| button => :a1
     ||| returns =>  true
     |||v contrived-example.inner-workings/get-selection  :13453
     |||| machine => {:inventory {:a1 {:name :taco :price 0.85 :qty 10}}
     ||||             :coins-inserted [:quarter :dime :nickel :penny]
     ||||             :coins-returned []
     ||||             :dispensed nil
     ||||             :err-msg nil}
     |||| button => :a1
     |||| returned =>  {:name :taco :price 0.85 :qty 10}
     |||^
     |||v contrived-example.inner-workings/calc-coin-value  :13454
     |||| coins => [:quarter :dime :nickel :penny]
     |||| returned =>  1.4
     |||^
     ||| contrived-example.inner-workings/valid-selection  :13452
     ||| machine => {:inventory {:a1 {:name :taco :price 0.85 :qty 10}}
     |||             :coins-inserted [:quarter :dime :nickel :penny]
     |||             :coins-returned []
     |||             :dispensed nil
     |||             :err-msg nil}
     ||| button => :a1
     ||| returned =>  true
     ||^

We can see that `valid-selection` makes calls to `get-selection` and
`calc-coin-value`. Looking at the return values, we might notice a
problem: `calc-coin-value` receives
`[:quarter :dime :nickel                       :penny]` but returns
\$1.40 as the value. Let's dig deeper. Press `n`
(`sayid-buffer-nav-to-next`) a couple times to get the cursor to the
call to `calc-coin-value`. Now press `N`
(`sayid-buf-replay-with-inner-trace`) and hold onto your hat.

     ||||v (->> coins (keep coin-values) (apply +)) => (apply + (keep coin-values coins))  contrived-example.inner-workings/calc-coin-value  :13491
     ||||| returns =>  1.4
     |||||v (apply + (keep coin-values coins))  contrived-example.inner-workings/calc-coin-value  :13492
     |||||| #function[clojure.core/+]
     |||||| (0.25 0.1 0.05 1)
     |||||| returns =>  1.4
     ||||||v (keep coin-values coins)  contrived-example.inner-workings/calc-coin-value  :13493
     ||||||| {:quarter 0.25 :dime 0.1 :nickel 0.05 :penny 1}
     ||||||| [:quarter :dime :nickel :penny]
     ||||||| returned =>  (0.25 0.1 0.05 1)
     ||||||^
     |||||| (apply + (keep coin-values coins))  contrived-example.inner-workings/calc-coin-value  :13492
     |||||| #function[clojure.core/+]
     |||||| (0.25 0.1 0.05 1)
     |||||| returned =>  1.4
     |||||^
     ||||| (->> coins (keep coin-values) (apply +)) => (apply + (keep coin-values coins))  contrived-example.inner-workings/calc-coin-value  :13491
     ||||| returned =>  1.4
     ||||^
    ...truncated...

*(jump to the top of the buffer)*

What did we do? We applied an *inner trace* to the function
`calc-coin-value` and then replayed the call to `test1` that we had
captured originally.

**An INNER trace?** YES! We can see the inputs and output values of
each expression in the function. Look at it. Where do things go wrong?
It's when we pass a hash map to `keep` that defines a penny as being
worth a dollar. Bug located! Press `n` a couple times to get your
cursor to that call. Press `RET` to jump to that line of code.

```clojure
 (ns contrived-example.inner-workings)

 (def coin-values
   {:quarter 0.25
    :dime 0.10
    :nickel 0.05
    :penny 1})

 (defn- calc-coin-value
   [coins]
   (->> coins
        (keep coin-values)
        (apply +)))

...truncated...
```

We now find ourselves at the troublesome call to `keep` causing our bug.
The hash map, `coin-values`, is just above. Change the value of a penny
from `1` to `0.01`. Let's eval our corrected code the Sayid way \--
press `C-c s !` (`sayid-load-enable-clear`). This will remove the
traces, eval the buffer, then re-apply the traces. It also clears the
workspace log. This is all helpful. Navigate back to `core-test` and run
`test1` again. Repeating steps above, you can verify the output is now
correct: no taco.

     v contrived-example.core-test/test1  :13579
     | returned =>  {:inventory {:a1 {:name :taco :price 0.85 :qty 10}}
     |               :coins-inserted [:quarter :dime :nickel :penny]
     |               :coins-returned []
     |               :dispensed nil
     |               :err-msg true}
     ^

Great work!

## License

Distributed under the Apache 2.0 License. See [LICENSE](LICENSE) for details.
