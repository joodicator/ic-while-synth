#const time_max=20.
#const int_min=0.
#const int_max=5.

con(1).
var(len_xs; x; i).
array(xs, 3).
write_var(x; i).

array(r0, xs, 1).
in(r0, len_xs, 1).
in(r0, array(xs,0), 5).
out(r0, x, 5).

array(r1, xs, 2).
in(r1, len_xs, 2).
in(r1, array(xs,0), 5).
in(r1, array(xs,1), 4).
out(r1, x, 4).

array(r2, xs, 3).
in(r2, len_xs, 3).
in(r2, array(xs,0), 5).
in(r2, array(xs,1), 4).
in(r2, array(xs,2), 3).
out(r2, x, 3).

