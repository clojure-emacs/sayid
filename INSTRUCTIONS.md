There are three different modes where sayid provides key bindings.

clojure mode
------------

editing a clojure file

C-c s e -- sayid-eval-last-sexp
Enables traces, evals the expression at point, disables traces, displays results with terse view

C-c s f -- sayid-query-form-at-point
Queries the active workspace for entries that most closely match the context of the cursor position

C-c s n -- sayid-replay-with-inner-trace
Applies an inner trace to the function at point, replays workspace, displays results

C-c s r -- sayid-replay-workspace-query-point
Replays workspace, queries results by context of cursor

C-c s w -- sayid-get-workspace
Shows workspace, using the current view

C-c s t y -- sayid-trace-all-ns-in-dir
Prompts for a dir, recursively traces all ns's in that dir and subdirs

C-c s t p -- sayid-trace-ns-by-pattern
Prompts for a pattern (* = wildcare), and applies a trace to all *loaded* ns's whose name matches the patten

C-c s t b -- sayid-trace-ns-in-file
Trace the ns in the current buffer

C-c s t e -- sayid-trace-fn-enable
Enable the *existing* (if any) trace of the function at point

C-c s t E -- sayid-trace-enable-all
Enable all traces

C-c s t d -- sayid-trace-fn-disable
Disable the *existing* (if any) trace of the function at point

C-c s t D -- sayid-trace-disable-all
Disable all traces

C-c s t n -- sayid-inner-trace-fn
Apply an inner trace to the symbol at point

C-c s t o -- sayid-outer-trace-fn
Apply an outer trace to the symbol at point

C-c s t r -- sayid-remove-trace-fn

C-c s t K -- sayid-kill-all-traces

C-c s c -- sayid-clear-log
Clear the workspace log

C-c s x -- sayid-reset-workspace
C-c s s -- sayid-show-traced
C-c s S -- sayid-show-traced-ns 
C-c s V s -- sayid-set-view


sayid mode
----------

buffer dispaying sayid output

<RET> -- sayid-buffer-nav-from-point
d -- sayid-buf-def-at-point
f -- sayid-query-fn
F -- sayid-query-fn-w-mod
i -- sayid-query-id
I -- sayid-query-id-w-mod
w -- sayid-get-workspace
n -- sayid-buffer-nav-to-next
N -- sayid-buf-replay-with-inner-trace
p -- sayid-buffer-nav-to-prev
P -- sayid-buf-pprint-at-point
v -- sayid-toggle-view
V -- sayid-set-view
<backspace> -- sayid-buf-back
<S-backspace> -- sayid-buf-forward
c i -- sayid-buf-inspect-at-point


sayid-traced mode
----------------- 

<RET> -- sayid-traced-buf-enter) 
e -- sayid-traced-buf-enable) 
d -- sayid-traced-buf-disable) 
E -- sayid-traced-buf-enable-all) ;TODO 
D -- sayid-traced-buf-disable-all) ;TODO 
i -- sayid-traced-buf-inner-trace-fn) 
o -- sayid-traced-buf-outer-trace-fn) 
r -- sayid-traced-buf-remove-trace) 
<backspace> -- sayid-show-traced)

