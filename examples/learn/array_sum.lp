#const time_max=20.
#const int_min=0.
#const int_max=3.

var(len_xs; a; b; s).
array(xs, 3).

con(0; 1).
disallow(mul; div; mod; if; sub).
write_var(a; b; s).

preset_line_instr(1, set(s, con(0))).
preset_line_instr(2, set(a, con(0))).

% sum [0] == 0
array(r00, xs, 1).
in(r00, len_xs, 1).
in(r00, array(xs,0), 0).
out(r00, s, 0).

% sum [1] == 1
array(r01, xs, 1).
in(r01, len_xs, 1).
in(r01, array(xs,0), 1).
out(r01, s, 1).

% sum [0,0] == 0
array(r10, xs, 2).
in(r10, len_xs, 2).
in(r10, array(xs,0), 0).
in(r10, array(xs,1), 0).
out(r10, s, 0).

% sum [1,0] == 1
array(r11, xs, 2).
in(r11, len_xs, 2).
in(r11, array(xs,0), 1).
in(r11, array(xs,1), 0).
out(r11, s, 1).

% sum [1,1] == 2
array(r12, xs, 2).
in(r12, len_xs, 2).
in(r12, array(xs,0), 1).
in(r12, array(xs,1), 1).
out(r12, s, 2).

% sum [0,1,0] == 1
array(r21, xs, 3).
in(r21, len_xs, 3).
in(r21, array(xs,0), 0).
in(r21, array(xs,1), 1).
in(r21, array(xs,2), 0).
out(r21, s, 1).

% sum [1,0,1] == 2
array(r22, xs, 3).
in(r22, len_xs, 3).
in(r22, array(xs,0), 1).
in(r22, array(xs,1), 0).
in(r22, array(xs,2), 1).
out(r22, s, 2).

% sum [1,1,1] == 3
array(r23, xs, 3).
in(r23, len_xs, 3).
in(r23, array(xs,0), 1).
in(r23, array(xs,1), 1).
in(r23, array(xs,2), 1).
out(r23, s, 3).
