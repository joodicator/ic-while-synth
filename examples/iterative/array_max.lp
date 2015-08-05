time_limit(25).
int_range(0, 3).

array(xs, 3).
input_array(xs).
input_variable(len_xs).
output_variable(m).
extra_variable(i; x).

preset_line_instr(1, set(m, array(xs, con(0)))).
preset_line_instr(2, set(i, con(0))).

line_limit_min(2).

constant(0; 1).
disallow_feature(mul; div; mod; sub).
read_only_variable(len_xs).
read_only_array(xs).

array(xs, 3, elem, index).
var(i; len_xs, index).
var(x; m, elem).
con(0; 1, index).
con(0, elem).

precondition(haskell("in_len_xs == length in_xs && length in_xs > (0 :: Int)")).
postcondition(haskell("out_m == if length in_xs == (0 :: Int) then 0 else maximum in_xs")).
