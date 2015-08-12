time_limit(25).
int_range(0, 3).

array(xs, 3).
input_array(xs).
input_variable(len_xs; x).
output_variable(i).
extra_variable(y; j).

constant(1).
disallow_feature(mul; div; mod; add; while).
read_only_variable(len_xs; x).
read_only_array(xs).

array(xs, 3, elem, index).
var(j; len_xs, index).
var(x; y, elem).
var(i; j, result).
con(1, index).

preset_line_instr(2, while(lt(con(0), var(j)), line_max-3)).
preset_line_instr(line_max, end_while).
line_limit_min(3).

precondition(haskell(
    "in_x `elem` in_xs && in_len_xs == length in_xs")).
postcondition(haskell(
    "in_x==in_xs!!out_i && and[j>=out_i || in_x/=y | (j,y) <- zip [1..] in_xs]")).
