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

(describe "sayid-mk-font-face"
  (it "maps a type pair to the matching face"
    (expect (sayid-mk-font-face '("type" ("int"))) :to-be 'sayid-int-face)
    (expect (sayid-mk-font-face '("type" ("keyword"))) :to-be 'sayid-keyword-face))
  (it "maps a depth pair to the matching face, cycling every ten levels"
    (expect (sayid-mk-font-face '("fg*" 0)) :to-be 'sayid-depth-1-face)
    (expect (sayid-mk-font-face '("fg*" 3)) :to-be 'sayid-depth-4-face)
    (expect (sayid-mk-font-face '("fg*" 13)) :to-be 'sayid-depth-4-face))
  (it "returns nil for an unknown property pair"
    (expect (sayid-mk-font-face '("nope" 1)) :to-be nil)))

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

(describe "sayid-try-goto-prop"
  (it "moves point to the first span whose property equals VAL, even for numeric values"
    ;; Regression: `id' values arrive as numbers, so comparing them with
    ;; `string=' used to crash with a wrong-type error.
    (with-temp-buffer
      (insert "abcdef")
      (put-text-property 3 5 'id 42)
      (goto-char (point-min))
      (sayid-try-goto-prop 'id 42)
      (expect (point) :to-equal 3)))

  (it "leaves point untouched when no span matches"
    (with-temp-buffer
      (insert "abcdef")
      (goto-char 4)
      (sayid-try-goto-prop 'id 99)
      (expect (point) :to-equal 4))))

(describe "the generated keybinding help"
  ;; The help buffers are rendered straight from the keymaps, so this also
  ;; guards the keymaps against drift.
  (it "lists the live commands bound in the sayid buffer map"
    (let ((help (substitute-command-keys "\\{sayid-mode-map}")))
      (expect help :to-match "sayid-query-id")
      (expect help :to-match "sayid-query-fn")
      (expect help :to-match "sayid-refresh-view")))

  (it "no longer references the collapsed -w-mod commands"
    (expect (substitute-command-keys "\\{sayid-mode-map}")
            :not :to-match "sayid-query-id-w-mod")))

(describe "sayid-tree--node-label"
  (it "renders the call form and its return value"
    (let ((label (sayid-tree--node-label
                  (nrepl-dict "name" "my.ns/foo" "args" '(":a" "1") "return" ":a"))))
      (expect label :to-match "my.ns/foo")
      (expect label :to-match "=>")
      (expect label :to-match ":a")))
  (it "falls back to nil when there is no recorded return"
    (expect (sayid-tree--node-label (nrepl-dict "name" "my.ns/foo"))
            :to-match "nil"))
  (it "shows the thrown cause instead of a return when the call threw"
    (let ((label (sayid-tree--node-label
                  (nrepl-dict "name" "my.ns/boom" "args" '("7")
                              "throw" (nrepl-dict "cause" "boom")))))
      (expect label :to-match "boom")
      (expect label :not :to-match "=>"))))

(describe "sayid-tree--make-node"
  (it "stashes the whole call dict as the node value"
    (let* ((call (nrepl-dict "name" "my.ns/foo" "return" "1"))
           (node (sayid-tree--make-node call)))
      (expect (cider-tree-view-node-value node) :to-equal call)))
  (it "is expandable only when the call has children"
    (let ((leaf (sayid-tree--make-node (nrepl-dict "name" "f")))
          (parent (sayid-tree--make-node
                   (nrepl-dict "name" "f" "children"
                               (list (nrepl-dict "name" "g"))))))
      (expect (cider-tree-view-node-children-fn leaf) :to-be nil)
      (expect (cider-tree-view-node-children-fn parent) :not :to-be nil)))
  (it "builds child nodes from the call's children"
    (let* ((call (nrepl-dict "name" "f" "children"
                             (list (nrepl-dict "name" "g/child" "return" "2"))))
           (node (sayid-tree--make-node call))
           (children (funcall (cider-tree-view-node-children-fn node))))
      (expect (length children) :to-equal 1)
      (expect (cider-tree-view-node-label (car children)) :to-match "g/child"))))

(provide 'sayid-test)
;;; sayid-test.el ends here
