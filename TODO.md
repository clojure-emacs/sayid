- bottom line hidden in output

nrepl/sayid.el
x button to pretty print
x button to cider-inspect
x trying to trace unloaded ns problems???
~ trace by dir
- trace by ns
x enable/disable traces
x back button (backspace)
x back button - save pos
x 'n' does inner trace from buf
~ better 'show trace' buffer
  x refresh maintains ns status
  x per ns
  x check whether each func is: outer, inner, enabled
  x trace commands
    x outer
    x inner
    x enable
    x disable
    x remove
    - the rest
  - enter-> jump to func
  - query ws by func
  x enter-> jump to ns detail view
- lots of clj-mode bindings

- what happens if you apply and inner trace twice?

x rename everything (inner/outer)

- have eval-last be more sane; check for existing traces

- BUGS
 - trace doesn't take sometimes -- eval last
 - `C-s t y` doesn't work
 - e/d to enable/disable ns from *traced*
 

- write script  
- video


- query by func maintains cursor on id

- color codes instead of ansi

- lenses
