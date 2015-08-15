#const time_max=30.
#const int_min=0.
#const int_max=4.
#const stack_max=1.

array(xs, 4, elem, index).
write_array(xs).
var(len_xs; i; j, index).
var(swap, elem).
con(0; 1, index).
write_var(i; j; swap).
disallow(div; mod; mul; if; while).

%== Preset Structure ==%
preset_line_instr(1, set(i, con(0))).
preset_line_instr(2, set(j, sub(var(len_xs), con(1)))).
preset_line_instr(3, while(lt(var(i), var(j)), 3)).
preset_line_instr(4, call(swap)).
preset_line_instr(5, set(i, add(var(i), con(1)))).
preset_line_instr(6, set(j, sub(var(j), con(1)))).
preset_line_instr(7, end_while).

sub_line_max(swap, 3).

%== Examples ==%

in(r2,len_xs,2). array(r2,xs,2).
in(r2,array(xs,0),1). out(r2,array(xs,0),2).
in(r2,array(xs,1),2). out(r2,array(xs,1),1).

in(r3,len_xs,3). array(r3,xs,3).
in(r3,array(xs,0),1). out(r3,array(xs,0),3).
in(r3,array(xs,1),2). out(r3,array(xs,1),2).
in(r3,array(xs,2),3). out(r3,array(xs,2),1).

in(r4,len_xs,4). array(r4,xs,4).
in(r4,array(xs,0),1). out(r4,array(xs,0),4).
in(r4,array(xs,1),2). out(r4,array(xs,1),3).
in(r4,array(xs,2),3). out(r4,array(xs,2),2).
in(r4,array(xs,3),4). out(r4,array(xs,3),1).
