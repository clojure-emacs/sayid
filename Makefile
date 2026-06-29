.PHONY: test test-all kondo repl elisp-compile elisp-lint elisp-test elisp clean

# Run the Clojure test suite against the default Clojure version.
test:
	lein test

# Run the Clojure test suite against the full version matrix.
test-all:
	lein test-all

# Lint the Clojure sources with clj-kondo.
kondo:
	lein with-profile +clj-kondo clj-kondo --lint src test

# Start a REPL with the nREPL middleware loaded.
repl:
	lein repl

# Byte-compile the Emacs Lisp client (requires Eldev).
elisp-compile:
	eldev -p -dtT compile --warnings-as-errors

# Check the Emacs Lisp client docstrings (requires Eldev).
elisp-lint:
	eldev -dtT lint doc

# Run the Emacs Lisp test suite with Buttercup (requires Eldev).
elisp-test:
	eldev -dtT test

# Run all the Emacs Lisp checks.
elisp: elisp-compile elisp-lint elisp-test

clean:
	lein clean
	rm -rf .eldev

# end
