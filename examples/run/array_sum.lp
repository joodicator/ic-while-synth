#const time_max=50.
#const int_min=0.
#const int_max=50.

line_instr(1, set(s, con(0))).                    % s = 0;
line_instr(2, set(i, con(0))).                    % i = 0;
line_instr(3, while(lt(var(i), var(len_xs)), 3)). % while (i < len_xs) {
line_instr(4, set(x, array(xs,var(i)))).          %     x = xs[i];
line_instr(5, set(s, add(var(s), var(x)))).       %     s = s + x;
line_instr(6, set(i, add(var(i), con(1)))).       %     i = i + 1;
line_instr(7, end_while).                         % }

array(xs, 5).

in(r0, len_xs, 5).
in(r0, array(xs,0), 1).
in(r0, array(xs,1), 2).
in(r0, array(xs,2), 3).
in(r0, array(xs,3), 4).
in(r0, array(xs,4), 5).

#include "run.lp".
