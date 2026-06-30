# Change Log

## Unreleased

* [#100](https://github.com/clojure-emacs/sayid/pull/100): Add the `sayid-get-workspace-data` nREPL op, which returns the recorded call tree as data (see [doc/nrepl-api.md](doc/nrepl-api.md)) for editor-agnostic clients.

## [0.3.0] - 2026-06-29

* [#92](https://github.com/clojure-emacs/sayid/pull/92): Stop freezing Emacs during the `!` reload workflow; the re-enable/clear now runs on the reload's completion callback instead of fixed `sleep-for` delays.
* [#91](https://github.com/clojure-emacs/sayid/pull/91): Collapse the `query-*-with-modifier` commands into prefix-aware `f`/`i` in the sayid buffer (use a prefix arg to prompt for a modifier; the `F`/`I` keys are gone), and bind `r` to refresh the view.
* [#91](https://github.com/clojure-emacs/sayid/pull/91): Generate the in-Emacs help buffers from the keymaps so they can't drift, and base the output buffers on `special-mode`.
* [#86](https://github.com/clojure-emacs/sayid/pull/86): Fix a state leak in the traced-buffer outer-trace command, a crash when navigating to a numeric trace id, and a couple of messages that broke on a literal `%` in a path.

## [0.2.0] - 2026-06-29

* Publish under the `mx.cider/sayid` coordinates. The old `com.billpiel/sayid` coordinates are deprecated but still receive the same releases for now, so existing dependencies keep working.
* [#13](https://github.com/clojure-emacs/sayid/issues/13): Document the nREPL middleware API (see [doc/nrepl-api.md](doc/nrepl-api.md)).
* Rewrite the README's demo walkthrough around a small, self-contained example with current keybindings.
* Consolidate the trace-management nREPL ops into four `action`-parametrized ops (`sayid-trace-fn`, `sayid-trace-fn-at-point`, `sayid-trace-ns`, `sayid-all-traces`), trimming the middleware from 37 ops to 26. (Breaking for any third-party nREPL client; the bundled Emacs client is updated in lockstep.)
* Rename the buffer-oriented nREPL ops to client-agnostic names: `sayid-buf-query-id-w-mod` -> `sayid-query-by-id`, `sayid-buf-query-fn-w-mod` -> `sayid-query-by-fn`, `sayid-buf-def-at-point` -> `sayid-def-value`, `sayid-buf-pprint-at-point` -> `sayid-pprint-value`.
* [#29](https://github.com/clojure-emacs/sayid/issues/29): Fix the `wrong-type-argument` error when pressing `g` (and similar commands) by no longer re-reading nREPL response values, which already arrive decoded on nREPL 1.0+.
* [#14](https://github.com/clojure-emacs/sayid/issues/14): Fix inner tracing of functions that use `letfn`.
* [#31](https://github.com/clojure-emacs/sayid/issues/31): Keep the generated reproduction expression in the kill ring and report clearly when the source file can't be located (`sayid-gen-instance-expr`, bound to `g`).
* [#68](https://github.com/clojure-emacs/sayid/issues/68): Fix automatic dependency injection at `cider-jack-in` time for non-Leiningen projects.
* Bump the minimum requirements to Clojure 1.10, nREPL 1.0, CIDER 1.0 and Emacs 28.
* Bump the bundled `tools.reader` and `tools.namespace` dependencies.
* Fix the broken Clojure version matrix that prevented the test suite from running on recent Leiningen.
* Modernize CI: run against a JDK/Clojure matrix, lint the Clojure sources with clj-kondo, and byte-compile/lint the Emacs Lisp client.
* Enable lexical binding in the Emacs Lisp client and tidy up its docstrings.
* Modernize the Emacs client's nREPL usage: route all requests through CIDER's sender and drop the obsolete `cider-current-connection`.
* Sync the in-Emacs help buffers and the README keybinding tables with the actual keybindings (drop entries for commands that no longer exist, add the missing ones).
* Eliminate reflection warnings in the Clojure namespaces.
* Report nREPL op failures with a CIDER-renderable error status instead of printing to the server console, and stop intercepting errors raised by other middleware.
* [#61](https://github.com/clojure-emacs/sayid/issues/61): Remove version extraction logic.
* Decouple the injected `sayid` plugin version from the version of the Emacs client (see `sayid-injected-plugin-version`).
* `sayid-trace-ns-by-pattern` accepts interactive argument.

## [0.1.0] - 2020-09-02

* [#57](https://github.com/clojure-emacs/sayid/issues/57): Fix version extraction logic.

## [0.0.19] - 2020-08-20

### Fixed

* Fix nREPL middleware and Lein plugin version numbers.

## [0.0.18] - 2019-08-26

### Changed

* Removed support for nREPL 0.2.x.
* Removed hard dep on Clojure.

## [0.0.17] - 2018-09-01

### Added

* Added support for nREPL 0.4.

## [0.0.16] - 2018-04-04

### Added

* Auto inject depencencies at cider-jack-in time. Thanks, Benedek Fazekas!

### Fixed

* upgrade to org.clojure/tools.reader "1.3.0-alpha3"

## [0.0.15] - 2017-05-02

### Fixed

* emacs: disable undo to avoid buffer limit error
* emacs: fix background colors in pretty-print buffer
* support inner-tracing of `loop` form with multi-form body

## [0.0.14] - 2017-03-06

### Changed

* emacs: several misc things for MELPA

## [0.0.13] - 2017-02-22

### Fixed

* emacs: messed up some function names

## [0.0.12] - 2017-02-21

### Changed

* emacs: improve render speed
* emacs: prepare package for submission to melpa
* emacs: useful message when sayid not responding

### Fixed

* support sets in pretty-print buffer

## [0.0.11] - 2017-01-10

### Added

* CHANGELOG.md

### Changed

* Improved inner tracing
  * ex. `recur` no longer triggers explosion
* Improved multimethod tracing
  * traced multimethod is still a MultiFn
  * dispatcher return value captured
* Emacs : keybinding `q` to quit window
