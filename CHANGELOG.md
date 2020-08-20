# Change Log

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
