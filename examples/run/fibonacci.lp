#const time_max=30.
#const int_min=0.
#const int_max=10.

line_instr(1, set(e, con(0))).                % e = 0;
line_instr(1, set(f, con(1))).                % f = 1;
line_instr(2, while(lt(con(1), var(n)), 3)).  % while (n > 1) {
line_instr(3, set(f, add(var(e), var(f)))).   %   f = e + f;
line_instr(4, set(e, sub(var(f), var(e)))).   %   e = f - e;
line_instr(5, set(n, sub(var(n), con(1)))).   %   n = n - 1;
line_instr(6, end_while).                     % }

in(run1,n,1).
in(run2,n,2).
in(run3,n,3).
in(run4,n,4).
in(run5,n,5).
in(run6,n,6).
