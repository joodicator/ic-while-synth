#const line_max=12.
#const time_max=25.
#const int_max=14.

#show run_var_out/3.

con(1; 2).
var(n; p; k; r).
write_var(n; p; k; r).

disallow(if; mul; div).

% POST: p == nth_prime(n)
%  1. p = 2
%  2. while (1 < n):
%  3.     p = p + 1
%  4.     k = 2
%  5.     while (k < p):
%  6.         r = p % k
%  7.         if (r < 1):
%  8.             n = n - 1
%  9.             k = p
% 10.         k = k + 1
% 11.     end_while
% 12. end_while

preset_line_instr( 1, set(p, con(2))).
preset_line_instr( 2, while(lt(con(1), var(n)), 9)).
preset_line_instr( 3, set(p, add(var(p), con(1)))).
preset_line_instr( 4, set(k, con(2))).
preset_line_instr( 5, while(lt(var(k), var(p)), 5)).
preset_line_instr( 6, set(r, mod(var(p), var(k)))).
preset_line_instr( 7, if(lt(var(r), con(1)), 2)).
preset_line_instr( 8, set(n, sub(var(n), con(1)))).
preset_line_instr( 9, set(k, var(p))).
preset_line_instr(10, set(k, add(var(k), con(1)))).
preset_line_instr(11, end_while).
preset_line_instr(12, end_while).

in(r1,x,1).
in(r2,x,2).
in(r3,x,3).
in(r4,x,4).
in(r5,x,5).
in(r6,x,6).
