#const time_max=200.
#const int_min=0.
#const int_max=12.

line_instr( 1, set(p, con(2))).                % p = 2;
line_instr( 2, set(k, con(1))).                % k = 1;
line_instr( 3, while(lt(var(k), var(n)), 12)). % while (k < n):
line_instr( 4, set(k, add(var(k), con(1)))).   %   k = k + 1;
line_instr( 5, set(j, con(0))).                %   j = 0;
line_instr( 6, while(le(var(j), con(0)), 8)).  %   while (j <= 0):
line_instr( 7, set(p, add(var(p), con(1)))).   %     p = p + 1;
line_instr( 8, set(l, con(2))).                %     l = 2;
line_instr( 9, while(lt(var(l), var(p)), 4)).  %     while (l < p) {
line_instr(10, set(j, mod(var(p), var(l)))).   %       j = p % l;
line_instr(11, set(l, add(var(l), con(1)))).   %       l = l + 1;
line_instr(12, if(le(var(j), con(0)), 1)).     %       if (j <= 0) {
line_instr(13, set(l, var(p))).                %         l = p }
line_instr(14, end_while).                     %     }
line_instr(15, end_while).                     %   }
line_instr(16, end_while).                     % }

#include "../../tools/run.lp".

#hide.
#show run_var_out/3.

in(r0, n, 5).
