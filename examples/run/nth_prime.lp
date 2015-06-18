#const time_max=100.
#const int_min=0.
#const int_max=11.

line_instr( 1, set(p, con(2))).                % p = 2;
line_instr( 2, if(lt(con(1), var(n)), 2)).     % if (n > 1) {
line_instr( 3, set(p, con(3))).                %   p = 3;
line_instr( 4, set(n, sub(var(n), con(1)))).   %   n = n - 1; }
line_instr( 5, while(lt(con(1), var(n)), 12)). % while (n > 1):
line_instr( 6, set(n, sub(var(n), con(1)))).   %   n = n - 1;
line_instr( 7, set(j, con(0))).                %   j = 0;
line_instr( 8, while(le(var(j), con(0)), 8)).  %   while (j <= 0):
line_instr( 9, set(p, add(var(p), con(2)))).   %     p = p + 2;
line_instr(10, set(l, con(3))).                %     l = 3;
line_instr(11, while(lt(var(l), var(p)), 4)).  %     while (l < p) {
line_instr(12, set(j, mod(var(p), var(l)))).   %       j = p % l;
line_instr(13, set(l, add(var(l), con(2)))).   %       l = l + 2;
line_instr(14, if(le(var(j), con(0)), 1)).     %       if (j <= 0) {
line_instr(15, set(l, var(p))).                %         l = p }
line_instr(16, end_while).                     %     }
line_instr(17, end_while).                     %   }
line_instr(18, end_while).                     % }

#include "../../tools/run.lp".

in(run1, n, 1).
in(run2, n, 2).
in(run3, n, 3).
%in(run4, n, 4).
%in(run5, n, 5).
%in(run6, n, 6).
