#const int_min=0.
#const int_max=32.
#const time_max=99.

line_instr(1, set(y, con(1))).                % y = 1;
line_instr(2, while(lt(con(0), var(x)), 2)).  % while (0 < x) {
line_instr(3, set(y, mul(var(y), con(2)))).   %     y = y * 2;
line_instr(4, set(x, sub(var(x), con(1)))).   %     x = x - 1;
line_instr(5, end_while).                     % }

in(run0,x,0).
in(run1,x,1).
in(run2,x,2).
in(run3,x,3).
in(run4,x,4).
in(run5,x,5).
