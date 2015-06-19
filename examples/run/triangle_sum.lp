#const time_max=50.
#const int_min=0.
#const int_max=50.

line_instr(1, set(t, con(0))).               % t = 0;
line_instr(2, while(lt(con(0), var(n)), 2)). % while (n > 0) {
line_instr(3, set(t, add(var(t), var(n)))).  %     t = t + n;
line_instr(4, set(n, sub(var(n), con(1)))).  %     n = n - 1;
line_instr(5, end_while).                    % }

in(run0,n,0).
in(run1,n,1).
in(run2,n,2).
in(run3,n,3).
in(run4,n,4).
in(run5,n,5).

#include "../../tools/run.lp".
