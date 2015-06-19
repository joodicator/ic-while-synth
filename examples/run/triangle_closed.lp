#const time_max=20.
#const int_min=0.
#const int_max=50.

line_instr(1, set(t, add(var(n), con(1)))). % t = n + 1;
line_instr(2, set(t, mul(var(t), var(n)))). % t = t * n;
line_instr(3, set(t, div(var(t), con(2)))). % t = t / 2;

in(run0,n,0).
in(run1,n,1).
in(run2,n,2).
in(run3,n,3).
in(run4,n,4).
in(run5,n,5).

#include "../../tools/run.lp".
