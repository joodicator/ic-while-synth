#const time_max=30.
#const int_min=0.
#const int_max=10.

con(2).
var(x; y). write_var(y).

{ in(r0,x,N) : N=int_min..int_max }1.

:- run_does_not_halt(r0).
:- run_var_out(r0,y,Out_y), in(r0,x,In_x), Out_y == 2*In_x.
