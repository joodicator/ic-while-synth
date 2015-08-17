#const time_max=30.
#const int_min=0.
#const int_max=4.
#const stack_max=1.

array(xs, 4, elem, index).
var(len_xs; i; j, index).
var(swap, elem).
write_var(i; j).
con(0; 1, index).
disallow(mul; div; mod; if; while).

%*
sub main:
  1. i = 0
  2. j = len_xs - 1
  3. while (i < j):
  4.    swap()
  5.    i = i + 1
  6.    j = j - 1
  7. end_while
sub swap:
  1. swap = xs[i]
  2. xs[i] = xs[j]
  3. xs[j] = swap
*%

%== Preset Structure ==%
%preset_line_instr(1, set(i, con(0))).
%preset_line_instr(2, set(j, sub(var(len_xs), con(1)))).
preset_line_instr(3, while(lt(var(i), var(j)), line_max-4)).
%preset_line_instr(4, call(swap)).
%preset_line_instr(5, set(i, add(var(i), con(1)))).
%preset_line_instr(6, set(j, sub(var(j), con(1)))).
preset_line_instr(line_max, end_while).

%== Subroutines ==%
preset_sub_line_instr((swap,1), set(swap, array(xs,var(i)))).
preset_sub_line_instr((swap,2), set(array(xs,var(i)), array(xs,var(j)))).
preset_sub_line_instr((swap,3), set(array(xs,var(j)), var(swap))).

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
