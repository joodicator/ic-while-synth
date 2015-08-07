#const time_max=10.
#const int_min=0.
#const int_max=9.
#const stack_max=1.

var(x; y; swap).
write_var(x; y).
disallow(arithmetic).

in(r0,x,0). in(r0,y,1). out(r0,x,1). out(r0,y,0).
in(r1,x,1). in(r1,y,0). out(r1,x,0). out(r1,y,1).

preset_sub_line_instr((swap,1), set(swap, var(x))).
preset_sub_line_instr((swap,2), set(x, var(y))).
preset_sub_line_instr((swap,3), set(y, var(swap))).
