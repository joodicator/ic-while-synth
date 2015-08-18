#const time_max=150.
#const int_min=0.
#const int_max=4.
#const stack_max=2.

#include "run.lp".

array(xs, 4).
in(r0, len_xs, 4).
in(r0, array(xs,0), 3).
in(r0, array(xs,1), 4).
in(r0, array(xs,2), 2).
in(r0, array(xs,3), 1).

%*
sub main():
    swapped = 1
    while (swapped > 0):
        bubble()

sub bubble():
    swapped = 0
    j = 1
    while (j < len_xs):
        i = j - 1
        x = xs[i]
        y = xs[j]
        if (x > y):
            swap()
            swapped = 1
        j = j + 1

sub swap():
    swap = xs[i]
    xs[i] = xs[j]
    xs[j] = swap
*%

sub_line_instr((main,1), set(swapped, con(1))).
sub_line_instr((main,2), while(lt(con(0), var(swapped)), 1)).
sub_line_instr((main,3), call(bubble)).
sub_line_instr((main,4), end_while).

sub_line_instr((bubble, 1), set(swapped, con(0))).
sub_line_instr((bubble, 2), set(j, con(1))).
sub_line_instr((bubble, 3), while(lt(var(j), var(len_xs)), 7)).
sub_line_instr((bubble, 4), set(i, sub(var(j), con(1)))).
sub_line_instr((bubble, 5), set(x, array(xs,var(i)))).
sub_line_instr((bubble, 6), set(y, array(xs,var(j)))).
sub_line_instr((bubble, 7), if(lt(var(y), var(x)), 2)).
sub_line_instr((bubble, 8), call(swap)).
sub_line_instr((bubble, 9), set(swapped, con(1))).
sub_line_instr((bubble,10), set(j, add(var(j),con(1)))).
sub_line_instr((bubble,11), end_while).

sub_line_instr((swap,1), set(swap, array(xs,var(i)))).
sub_line_instr((swap,2), set(array(xs,var(i)), array(xs,var(j)))).
sub_line_instr((swap,3), set(array(xs,var(j)), var(swap))).
