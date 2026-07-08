(ns sayid.inner-ast
  "Inner-tracing instrumenter built on `tools.analyzer.jvm`.

  Instead of re-reading source and hand-rewriting raw forms, this analyzes a
  traced function's source into a typed AST and re-emits it with capturing calls
  wrapped around its sub-expressions.  Because macros and special forms are
  already expanded and enumerable in the AST, there's no per-macro special-casing
  - the thing that made the old rewriter fragile.

  Two facts the analyzer hands us for free do the heavy lifting the legacy code
  reinvents by hand:
   - `:raw-forms` recovers each node's *surface* syntax (`(+ a b)`, not the
     lowered `clojure.lang.Numbers/add`), so captures show the user's code.
   - `:env` marks tail position and `:op` marks `recur`, so we know exactly what
     must not be wrapped to keep `recur` legal."
  (:require [clojure.tools.analyzer.jvm :as ana]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit]
            [sayid.util.other :as util]
            [sayid.util.sym :as sym]
            [sayid.util.source :as src]
            [sayid.trace :as trace]))

;;; ---- runtime ------------------------------------------------------------

(def ^:dynamic *inner-children*
  "The `:children` atom of the current inner-trace parent node, or nil when we're
  not (yet) inside an inner-trace scope for this call."
  nil)

(defn inner-root-children
  "The children atom the top-level captures of a traced call attach to: the outer
  (shallow) trace node for this call.  Nil when there's nothing to attach to -
  recording suppressed, or no outer parent - in which case captures pass through."
  []
  (when (and (not trace/*suppress-recording*)
             trace/*trace-log-parent*)
    (:children trace/*trace-log-parent*)))

(defn capture
  "Record the sub-expression FORM (its surface syntax) named NM while evaluating
  THUNK, then return THUNK's value.  Attaches a node under the current parent and
  binds itself as the parent for nested captures.  Re-raises any exception after
  recording it, so the traced code's control flow (try/catch, and callers) is
  preserved exactly."
  [form nm thunk]
  (let [children (or *inner-children* (inner-root-children))]
    (if (nil? children)
      (thunk)
      (let [node (atom (assoc (trace/mk-tree :id-prefix "in")
                              :name nm
                              :form form
                              :started-at (trace/now)))]
        (swap! children conj node)
        (binding [*inner-children* (:children @node)]
          (try
            (let [v (thunk)]
              (swap! node assoc :return v :ended-at (trace/now))
              v)
            (catch Throwable t
              (swap! node assoc
                     :throw (trace/Throwable->map** t)
                     :ended-at (trace/now))
              (throw t))))))))

;;; ---- instrumentation ----------------------------------------------------

(def ^:private wrappable-ops
  "Call-like AST ops worth capturing.  Special forms (`if`/`let`/`do`/...) are
  left as control flow and not captured as nodes themselves; their interesting
  sub-expressions are captured where they occur."
  #{:invoke :static-call :instance-call :protocol-invoke :keyword-invoke :new
    :host-call :host-interop})

(def ^:private lowered-ops
  "Ops the analyzer lowers to interop, so their `:form` is the JVM form
  (`clojure.lang.Numbers/add`) rather than the surface syntax.  For these the
  pre-lowering surface lives in `:raw-forms`."
  #{:static-call :instance-call :host-call :host-interop :new})

(defn- unwrap-do
  "Peel single-expression `(do x)` wrappers - the implicit `do` of a fn/branch
  body attaches one around its return expression."
  [form]
  (if (and (seq? form) (= 'do (first form)) (= 2 (count form)))
    (recur (second form))
    form))

(defn- surface-form
  "The node's surface form: what the user wrote for this sub-expression.  For
  lowered interop ops that's the last (most-expanded) raw form - `(+ a b)`, not
  `(. clojure.lang.Numbers (add a b))`; for ordinary calls `:form` already is the
  surface (`(reduce + 0 ...)`).  Trivial `(do x)` wrappers are peeled off."
  [node]
  (unwrap-do
   (if (contains? lowered-ops (:op node))
     (or (last (:raw-forms node)) (:form node))
     (:form node))))

(defn- node-name
  [node]
  (let [f (surface-form node)]
    (if (seq? f) (first f) f)))

(defn- contains-recur?
  "True if NODE's subtree contains a `recur`.  Since `recur` is only legal in tail
  position, any recur under a node sits on that node's tail path - so wrapping the
  node (in a thunk) would relocate the recur out of its loop.  Skip those."
  [node]
  (boolean (some #(= :recur (:op %)) (ast/nodes node))))

(defn- synthetic-form?
  "True if FORM references a compiler/macro-generated local, e.g. a destructuring
  temp like `map__7306`.  Because we emit non-hygienically, user locals keep the
  names they were written with, so a `__<digits>` symbol only ever comes from
  expansion - capturing those clutters the trace with internals (the guts of
  `{:keys [...]}` destructuring, say) the user never wrote."
  [form]
  (boolean (some #(and (symbol? %) (re-find #"__\d+" (name %)))
                 (tree-seq coll? seq (list form)))))

(defn- wrappable?
  [node]
  (and (contains? wrappable-ops (:op node))
       (not (contains-recur? node))
       (not (synthetic-form? (surface-form node)))))

(defn- wrap-node
  "Replace a wrappable node with `(capture 'form 'name (fn* [] <node>))`,
  re-analyzed in the node's own environment so its locals still resolve."
  [node]
  (if (wrappable? node)
    (let [form (surface-form node)
          nm   (node-name node)
          sub  (emit/emit-form node)
          wrapped (list `capture
                        (list 'quote form)
                        (list 'quote nm)
                        (list 'fn* [] sub))]
      (ana/analyze wrapped (:env node)))
    node))

(defn instrument
  "Turn a function AST into an evaluable, instrumented fn form: wrap each
  capturable sub-expression bottom-up, then emit.  Emission is non-hygienic so
  the function's own (already-valid) local names are preserved consistently
  between the wrappers and the code they wrap."
  [ast]
  (emit/emit-form (ast/postwalk ast wrap-node)))

;;; ---- entry point --------------------------------------------------------

(defn defn->fn-form
  "Pull the `(fn* ...)` form out of a `defn` source form, macroexpanded in NS-SYM."
  [src ns-sym]
  (let [expanded (sym/macroexpand-in-ns ns-sym src)]
    (if (and (seq? expanded)
             (= 'def (first expanded))
             (symbol? (second expanded)))
      (last expanded)
      (throw (ex-info "Sayid inner tracing expected a defn form"
                      {:form src :expanded expanded})))))

(defn analyze-in-ns
  "Analyze FORM to a `tools.analyzer.jvm` AST as if read in NS-SYM."
  [ns-sym form]
  (binding [*ns* (create-ns ns-sym)]
    (ana/analyze form (ana/empty-env))))

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

;;; ---- trace wiring -------------------------------------------------------

(defn ^{::trace/trace-type :inner-fn} composed-tracer-fn
  "Build the traced replacement for an inner-traced var: instrument it, then wrap
  it with the shallow tracer that records the call itself."
  [m _]
  (->> (inner-tracer m)
       (trace/shallow-tracer m)))

(defmethod trace/trace* :inner-fn
  [_ fn-sym workspace]
  (-> fn-sym
      resolve
      (trace/trace-var* (util/assoc-var-meta-to-fn composed-tracer-fn
                                                   ::trace/trace-type)
                        workspace)))

(defmethod trace/untrace* :inner-fn
  [_ fn-sym]
  (-> fn-sym
      resolve
      trace/untrace-var*))
