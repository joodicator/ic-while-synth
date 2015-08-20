#const time_max=50.
#const stack_max=1.
#const int_min=0.
#const int_max=6.
#const if_max=any.
#const while_max=any.
#include "learn.lp".
sub_line_max(main, 7).
#const line_max=7.
con(1; 0).
var(i; j; len_xs; swap).
write_var(i; j).
disallow(if; mod; div; mul).
array(cx4,xs,5). in(cx4,len_xs,5). in(cx4,array(xs,0),0). in(cx4,array(xs,1),0). in(cx4,array(xs,2),0). in(cx4,array(xs,3),1). in(cx4,array(xs,4),0).
array(cx3,xs,6). in(cx3,len_xs,6). in(cx3,array(xs,0),0). in(cx3,array(xs,1),1). in(cx3,array(xs,2),1). in(cx3,array(xs,3),0). in(cx3,array(xs,4),1). in(cx3,array(xs,5),0).
array(cx2,xs,6). in(cx2,len_xs,6). in(cx2,array(xs,0),0). in(cx2,array(xs,1),1). in(cx2,array(xs,2),1). in(cx2,array(xs,3),1). in(cx2,array(xs,4),1). in(cx2,array(xs,5),1).
array(cx1,xs,6). in(cx1,len_xs,6). in(cx1,array(xs,0),0). in(cx1,array(xs,1),1). in(cx1,array(xs,2),1). in(cx1,array(xs,3),1). in(cx1,array(xs,4),1). in(cx1,array(xs,5),1).
:- in(cx4,array(xs,4),In_xs_4), run_var_out(cx4,array(xs,0),Out_xs_0), Out_xs_0 != unset, Out_xs_0 != In_xs_4.
:- in(cx4,array(xs,3),In_xs_3), run_var_out(cx4,array(xs,1),Out_xs_1), Out_xs_1 != unset, Out_xs_1 != In_xs_3.
:- in(cx4,array(xs,2),In_xs_2), run_var_out(cx4,array(xs,2),Out_xs_2), Out_xs_2 != unset, Out_xs_2 != In_xs_2.
:- in(cx4,array(xs,1),In_xs_1), run_var_out(cx4,array(xs,3),Out_xs_3), Out_xs_3 != unset, Out_xs_3 != In_xs_1.
:- in(cx4,array(xs,0),In_xs_0), run_var_out(cx4,array(xs,4),Out_xs_4), Out_xs_4 != unset, Out_xs_4 != In_xs_0.
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- in(cx3,array(xs,5),In_xs_5), run_var_out(cx3,array(xs,0),Out_xs_0), Out_xs_0 != unset, Out_xs_0 != In_xs_5.
:- in(cx3,array(xs,4),In_xs_4), run_var_out(cx3,array(xs,1),Out_xs_1), Out_xs_1 != unset, Out_xs_1 != In_xs_4.
:- in(cx3,array(xs,3),In_xs_3), run_var_out(cx3,array(xs,2),Out_xs_2), Out_xs_2 != unset, Out_xs_2 != In_xs_3.
:- in(cx3,array(xs,2),In_xs_2), run_var_out(cx3,array(xs,3),Out_xs_3), Out_xs_3 != unset, Out_xs_3 != In_xs_2.
:- in(cx3,array(xs,1),In_xs_1), run_var_out(cx3,array(xs,4),Out_xs_4), Out_xs_4 != unset, Out_xs_4 != In_xs_1.
:- in(cx3,array(xs,0),In_xs_0), run_var_out(cx3,array(xs,5),Out_xs_5), Out_xs_5 != unset, Out_xs_5 != In_xs_0.
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- in(cx2,array(xs,5),In_xs_5), run_var_out(cx2,array(xs,0),Out_xs_0), Out_xs_0 != unset, Out_xs_0 != In_xs_5.
:- in(cx2,array(xs,4),In_xs_4), run_var_out(cx2,array(xs,1),Out_xs_1), Out_xs_1 != unset, Out_xs_1 != In_xs_4.
:- in(cx2,array(xs,3),In_xs_3), run_var_out(cx2,array(xs,2),Out_xs_2), Out_xs_2 != unset, Out_xs_2 != In_xs_3.
:- in(cx2,array(xs,2),In_xs_2), run_var_out(cx2,array(xs,3),Out_xs_3), Out_xs_3 != unset, Out_xs_3 != In_xs_2.
:- in(cx2,array(xs,1),In_xs_1), run_var_out(cx2,array(xs,4),Out_xs_4), Out_xs_4 != unset, Out_xs_4 != In_xs_1.
:- in(cx2,array(xs,0),In_xs_0), run_var_out(cx2,array(xs,5),Out_xs_5), Out_xs_5 != unset, Out_xs_5 != In_xs_0.
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- in(cx1,array(xs,5),In_xs_5), run_var_out(cx1,array(xs,0),Out_xs_0), Out_xs_0 != unset, Out_xs_0 != In_xs_5.
:- in(cx1,array(xs,4),In_xs_4), run_var_out(cx1,array(xs,1),Out_xs_1), Out_xs_1 != unset, Out_xs_1 != In_xs_4.
:- in(cx1,array(xs,3),In_xs_3), run_var_out(cx1,array(xs,2),Out_xs_2), Out_xs_2 != unset, Out_xs_2 != In_xs_3.
:- in(cx1,array(xs,2),In_xs_2), run_var_out(cx1,array(xs,3),Out_xs_3), Out_xs_3 != unset, Out_xs_3 != In_xs_2.
:- in(cx1,array(xs,1),In_xs_1), run_var_out(cx1,array(xs,4),Out_xs_4), Out_xs_4 != unset, Out_xs_4 != In_xs_1.
:- in(cx1,array(xs,0),In_xs_0), run_var_out(cx1,array(xs,5),Out_xs_5), Out_xs_5 != unset, Out_xs_5 != In_xs_0.
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
:- not run_var_out_set(cx4,array(xs,0)).
#include "reverse_sub.lp".