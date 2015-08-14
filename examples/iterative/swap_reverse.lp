int_range(0, 6).
time_limit(50).
stack_limit(1).

array(xs, 6).
input_array(xs).
output_array(xs).
input_variable(len_xs).
extra_variable(i; j; swap).
constant(0; 1).

read_only_variable(len_xs).
disallow_feature(mul; div; mod; if; while).

array(xs, 6, elem, index).
var(i; j; len_xs, index).
var(swap, elem).
con(0; 1, index).

preset_line_instr(1, set(i, con(0))).
preset_line_instr(2, set(j, sub(var(len_xs), con(1)))).
preset_line_instr(3, while(lt(var(i), var(j)), 3)).
preset_line_instr(4, call(swap)).
preset_line_instr(5, set(i, add(var(i), con(1)))).
preset_line_instr(6, set(j, sub(var(j), con(1)))).
preset_line_instr(7, end_while).

%preset_sub_line_instr((swap,1), set(swap, array(xs,var(i)))).
%preset_sub_line_instr((swap,2), set(array(xs,var(i)), array(xs,var(j)))).
%preset_sub_line_instr((swap,3), set(array(xs,var(j)), var(swap))).

precondition(haskell("in_len_xs == length in_xs")).
postcondition(haskell("out_xs == reverse in_xs")).
