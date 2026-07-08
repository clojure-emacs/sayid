(ns sayid.inner-ast
  "Inner-tracing instrumenter built on `tools.analyzer.jvm`.

  Instead of re-reading source and hand-rewriting raw forms (see `sayid.inner-trace`),
  this analyzes a traced function's source into a typed AST and re-emits it.  All
  macros and special forms are already expanded and enumerable in the AST, so the
  instrumenter needs no per-macro special-casing.

  This namespace is the scaffold: `instrument` is currently the identity transform
  (emit the analyzed form unchanged), which exercises the analyze -> emit -> eval
  -> redefine pipeline end to end.  The capturing walk lands in a follow-up, at
  which point this becomes the real inner tracer."
  (:require [clojure.tools.analyzer.jvm :as ana]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit]
            [sayid.util.other :as util]
            [sayid.util.sym :as sym]
            [sayid.util.source :as src]))

(defn defn->fn-form
  "Pull the `(fn* ...)` form out of a `defn` source form, macroexpanded in NS-SYM.
  Mirrors what the legacy instrumenter extracts, so both share a source shape."
  [src ns-sym]
  (let [expanded (sym/macroexpand-in-ns ns-sym src)]
    (if (and (seq? expanded)
             (= 'def (first expanded))
             (symbol? (second expanded)))
      (last expanded)
      (throw (ex-info "Sayid inner tracing expected a defn form"
                      {:form src :expanded expanded})))))

(defn analyze-in-ns
  "Analyze FORM to a `tools.analyzer.jvm` AST as if it were read in NS-SYM, so the
  function's free symbols resolve against the right namespace."
  [ns-sym form]
  (binding [*ns* (create-ns ns-sym)]
    (ana/analyze form (ana/empty-env))))

(defn instrument
  "Turn a function AST into an evaluable fn form.  For now this is the identity
  transform - emit the hygienic form unchanged - so locals keep their meaning and
  the function behaves exactly as before.  The capturing walk replaces this."
  [ast]
  (emit/emit-hygienic-form ast))

(defn inner-tracer
  "Produce an inner-traced replacement for the function named by :qual-sym by
  analyzing its source, instrumenting the AST, and evaluating the result back in
  its namespace."
  [{:keys [qual-sym ns']}]
  (let [ns-sym  (util/->symbol ns')
        source  (-> qual-sym symbol src/hunt-down-source)
        fn-form (defn->fn-form source ns-sym)
        ast     (analyze-in-ns ns-sym fn-form)]
    (sym/eval-in-ns ns-sym (instrument ast))))
