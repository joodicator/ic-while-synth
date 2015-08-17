#const time_max=30.
#const int_min=0.
#const int_max=4.
#const stack_max=1.

#include "run.lp".

array(xs, 4).

line_instr(1, set(i, con(0))).
line_instr(2, set(j, sub(var(len_xs), con(1)))).
line_instr(3, while(lt(var(i), var(j)), 3)).
line_instr(4, call(swap)).
line_instr(5, set(i, add(var(i), con(1)))).
line_instr(6, set(j, sub(var(j), con(1)))).
line_instr(7, end_while).

sub_line_instr((swap,1), set(swap, array(xs,var(i)))).
sub_line_instr((swap,2), set(array(xs,var(i)), array(xs,var(j)))).
sub_line_instr((swap,3), set(array(xs,var(j)), var(swap))).

in(r0, len_xs, 4).
in(r0, array(xs,0), 1).
in(r0, array(xs,1), 2).
in(r0, array(xs,2), 3).
in(r0, array(xs,3), 4).
