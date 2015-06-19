#const time_max=20.
#const int_min=0.
#const int_max=100.

line_instr( 1, if(lt(var(c), var(a)), 3)). % if (a > c) {
line_instr( 2, set(m, var(a))).            %     m = a;
line_instr( 3, set(a, var(c))).            %     a = c;
line_instr( 4, set(c, var(m))).            %     c = m; }
line_instr( 5, if(lt(var(b), var(a)), 3)). % if (a > b) {
line_instr( 6, set(m, var(a))).            %     m = a;
line_instr( 7, set(a, var(b))).            %     a = b;
line_instr( 8, set(b, var(m))).            %     b = m; }
line_instr( 9, if(lt(var(c), var(b)), 3)). % if (b > c) {
line_instr(10, set(m, var(b))).            %     m = b;
line_instr(11, set(b, var(c))).            %     b = c;
line_instr(12, set(c, var(m))).            %     c = m; }
line_instr(13, set(m, var(b))).            % m = b;

in(r123,a,11). in(r123,b,12). in(r123,c,13).
in(r132,a,21). in(r132,b,23). in(r132,c,22).
in(r312,a,33). in(r312,b,31). in(r312,c,32).
in(r321,a,43). in(r321,b,42). in(r321,c,41).
in(r231,a,52). in(r231,b,53). in(r231,c,51).
in(r213,a,62). in(r213,b,61). in(r213,c,63).

#include "../../tools/run.lp".
