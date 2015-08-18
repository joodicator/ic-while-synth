%*
 1. i = 0
 2. while (i < w):
 3.     j = 0
 4.     while (j < h):
 5.         k = w * i
 6.         k = k + j
 7.         x = m[k]
 8.         k = h * j
 9.         k = k + i
10.         t[k] = x
11.         j = j + 1
12.     end_while
13.     i = i + 1
14. end_while
*%

#const time_max=75.
#const int_min=0.
#const int_max=6.

#include "run.lp".

array(m, 6).
array(t, 6).
value(m; t; x, 0..1).

in(r,w,2).
in(r,h,3).
in(r,array(m,0),1). in(r,array(m,1),0).
in(r,array(m,2),1). in(r,array(m,3),0).
in(r,array(m,4),1). in(r,array(m,5),0).

line_instr( 1, set(i, con(0))).
line_instr( 2, while(lt(var(i), var(h)), 11)).
line_instr( 3, set(j, con(0))).
line_instr( 4, while(lt(var(j), var(w)), 7)).
line_instr( 5, set(k, mul(var(w), var(i)))).
line_instr( 6, set(k, add(var(k), var(j)))).
line_instr( 7, set(x, array(m, var(k)))).
line_instr( 8, set(k, mul(var(h), var(j)))).
line_instr( 9, set(k, add(var(k), var(i)))).
line_instr(10, set(array(t, var(k)), var(x))).
line_instr(11, set(j, add(var(j), con(1)))).
line_instr(12, end_while).
line_instr(13, set(i, add(var(i), con(1)))).
line_instr(14, end_while).
