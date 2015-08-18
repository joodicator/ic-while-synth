%-------------------------------------------------------------------------------
% The Ackermann–Péter function, defined by:
%
%   A(m,n) = n+1              if m=0
%            A(m-1, 1)        if m>0 and n=0
%            A(m-1, A(m,n-1)) if m>0 and n>0

#const time_max=175.
#const stack_max=7.
#const int_min=0.
#const int_max=7.

#include "run.lp".

%*
    sub ackermann():
     1. a = n + 1
     2. if m > 0:
     3.     if n > 0:
     4.         n = n - 1
     5.         ackermann()
     6.         m = m - 1
     7.         n = a
     8.         ackermann()
     9.     if n <= 0:
    10.         m = m - 1
    11.         n = 1
    12.         ackermann()
*%

main_sub(ackermann).
local(ackermann, m; n).

in(r00,m,0). in(r00,n,0).
in(r11,m,1). in(r11,n,1).
in(r22,m,2). in(r22,n,2).
in(r33,m,3). in(r33,n,3).

sub_line_instr((ackermann, 1), set(a, add(var(n), con(1)))).
sub_line_instr((ackermann, 2), if(lt(con(0), var(m)), 10)).
sub_line_instr((ackermann, 3), if(lt(con(0), var(n)), 6)).
sub_line_instr((ackermann, 4), set(n, sub(var(n), con(1)))).
sub_line_instr((ackermann, 5), call(ackermann)).
sub_line_instr((ackermann, 6), set(m, sub(var(m), con(1)))).
sub_line_instr((ackermann, 7), set(n, var(a))).
sub_line_instr((ackermann, 8), call(ackermann)).
sub_line_instr((ackermann, 9), if(le(var(n), con(0)), 3)).
sub_line_instr((ackermann,10), set(m, sub(var(m), con(1)))).
sub_line_instr((ackermann,11), set(n, con(1))).
sub_line_instr((ackermann,12), call(ackermann)).
