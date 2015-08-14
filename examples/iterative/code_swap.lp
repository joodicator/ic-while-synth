int_range(0, 25).
time_limit(50).
stack_limit(1).

input_variable(k).
output_variable(k).
extra_variable(x; y; swap).
constant(5).
disallow_feature(if; while).

preset_line_instr(1, call(decode)).
preset_line_instr(2, call(swap)).
preset_line_instr(3, call(encode)).

preset_sub_line_instr((swap,1), set(swap,var(x))).
preset_sub_line_instr((swap,2), set(x,var(y))).
preset_sub_line_instr((swap,3), set(y,var(swap))).

preset_sub_line_instr((decode,1), set(x, mod(var(k), con(5)))).
preset_sub_line_instr((decode,2), set(y, div(var(k), con(5)))).
line_limit_max(decode, 5).

preset_sub_line_instr((encode,1), set(k, var(x))).
preset_sub_line_instr((encode,2), set(y, mul(var(y), con(5)))).
preset_sub_line_instr((encode,3), set(k, add(var(k), var(y)))).
line_limit_max(encode, 5).

precondition(haskell("let (y,x) = in_k `quotRem` 5 in x < 5 && y < 5")).
postcondition(haskell("let (y,x) = in_k `quotRem` 5 in out_k == x*5+y")).
