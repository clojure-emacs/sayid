# Change Log

## [0.0.12] - 2017-02-21

### Changed
- emacs: improve render speed
- emacs: prepare package for submission to melpa
- emacs: useful message when sayid not responding

### Fixed
- support sets in pretty-print buffer

## [0.0.11] - 2017-01-10
### Added
- CHANGELOG.md

### Changed
- Improved inner tracing
 - ex. `recur` no longer triggers explosion
- Improved multimethod tracing
 - traced multimethod is still a MultiFn
 - dispatcher return value captured
- Emacs : keybinding `q` to quit window
