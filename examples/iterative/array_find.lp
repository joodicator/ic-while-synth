time_limit(20).
int_range(0, 3).

array(xs, 3).
input_array(xs).
input_variable(len_xs; x).
output_variable(i).
extra_variable(y; j).

constant(0; 1).
disallow_feature(mul; div; mod; sub).
read_only_variable(len_xs; x).
read_only_array(xs).

array(xs, 3, elem, index).
var(j; len_xs, index).
var(x; y, elem).
var(i; j, result).
con(0; 1, index).

preset_line_instr(1, set(j, con(0))).
preset_line_instr(2, while(lt(var(j), var(len_xs)), line_max-3)).
preset_line_instr(line_max, end_while).

precondition(haskell("in_x `elem` in_xs && in_len_xs == length in_xs")).
postcondition(haskell("in_x == in_xs !! out_i")).
