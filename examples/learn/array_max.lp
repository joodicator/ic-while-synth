#const time_max=25.
#const int_min=0.
#const int_max=3.

array(xs, 3).

var(len_xs; i).
var(a; m).

con(1).

write_var(m; i; a).
disallow(mul; div; mod; sub; while).

preset_line_instr(1, set(i, con(0))).
preset_line_instr(2, set(m, con(0))).
preset_line_instr(3, while(lt(var(i), var(len_xs)), line_max-4)).
%preset_line_instr(4, set(a, array(xs,var(i)))).
%preset_line_instr(5, if(lt(var(m), var(a)), 1)).
%preset_line_instr(6, set(m, var(a))).
%preset_line_instr(7, set(i, add(var(i), con(1)))).
preset_line_instr(line_max, end_while).

% max [3] == 3
array(r00, xs, 1).
in(r00, len_xs, 1).
in(r00, array(xs,0), 3).
out(r00, m, 3).

% max [0,0] == 0
array(r10, xs, 2).
in(r10, len_xs, 2).
in(r10, array(xs,0), 0).
in(r10, array(xs,1), 0).
out(r10, m, 0).

% max [2,0] == 2
array(r11, xs, 2).
in(r11, len_xs, 2).
in(r11, array(xs,0), 2).
in(r11, array(xs,1), 0).
out(r11, m, 2).

% max [0,1] == 1
array(r12, xs, 2).
in(r12, len_xs, 2).
in(r12, array(xs,0), 0).
in(r12, array(xs,1), 1).
out(r12, m, 1).

% max [2,1,2] == 2
array(r21, xs, 3).
in(r21, len_xs, 3).
in(r21, array(xs,0), 2).
in(r21, array(xs,1), 1).
in(r21, array(xs,2), 2).
out(r21, m, 2).

% max [2,2,1] == 2
array(r22, xs, 3).
in(r22, len_xs, 3).
in(r22, array(xs,0), 3).
in(r22, array(xs,1), 2).
in(r22, array(xs,2), 1).
out(r22, m, 3).

% max [1,2,3] == 3
array(r23, xs, 3).
in(r23, len_xs, 3).
in(r23, array(xs,0), 1).
in(r23, array(xs,1), 2).
in(r23, array(xs,2), 3).
out(r23, m, 3).

% max [1,3,2] == 3
array(r24, xs, 3).
in(r24, len_xs, 3).
in(r24, array(xs,0), 1).
in(r24, array(xs,1), 3).
in(r24, array(xs,2), 2).
out(r24, m, 3).
