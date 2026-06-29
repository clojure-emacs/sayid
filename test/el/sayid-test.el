;;; sayid-test.el --- Tests for the Sayid Emacs client  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov and Sayid contributors

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;;; Commentary:

;; Buttercup tests for the pure, REPL-independent parts of sayid.el.  Anything
;; that talks to nREPL lives behind `sayid--send-sync-request' and is covered by
;; the Clojure-side middleware tests instead.

;;; Code:

(require 'buttercup)
(require 'sayid)

(describe "sayid-list-take"
  (it "takes the first N items from the front of the list"
    (expect (sayid-list-take 2 '(a b c d e)) :to-equal '(a b)))
  (it "returns the whole list when N exceeds its length"
    (expect (sayid-list-take 10 '(a b)) :to-equal '(a b)))
  (it "returns nil when N is zero"
    (expect (sayid-list-take 0 '(a b c)) :to-equal nil))
  (it "returns nil for the empty list"
    (expect (sayid-list-take 3 '()) :to-equal nil)))

(describe "sayid-str-to-sym"
  (it "reads a string into the matching symbol"
    (expect (sayid-str-to-sym "foo") :to-be 'foo))
  (it "produces interned symbols that compare with eq"
    (expect (eq (sayid-str-to-sym "bar") (sayid-str-to-sym "bar")) :to-be t)))

(describe "the buffer-state ring"
  (before-each
    (setq sayid-ring '()))

  (it "pushes new state onto the front"
    (sayid-push-to-ring :a)
    (sayid-push-to-ring :b)
    (expect (sayid-peek-first-in-ring) :to-be :b))

  (it "caps the ring at five entries"
    (dolist (x '(1 2 3 4 5 6 7))
      (sayid-push-to-ring x))
    (expect (length sayid-ring) :to-equal 5)
    (expect (sayid-peek-first-in-ring) :to-be 7))

  (it "swaps the first entry without growing the ring"
    (sayid-push-to-ring :a)
    (sayid-swap-first-in-ring :z)
    (expect (sayid-peek-first-in-ring) :to-be :z)
    (expect (length sayid-ring) :to-equal 1))

  (it "cycles entries forward and back symmetrically"
    (setq sayid-ring '(:a :b :c))
    (expect (sayid-cycle-ring) :to-be :b)
    (expect sayid-ring :to-equal '(:b :c :a))
    (expect (sayid-cycle-ring-back) :to-be :a)
    (expect sayid-ring :to-equal '(:a :b :c))))

(provide 'sayid-test)
;;; sayid-test.el ends here
