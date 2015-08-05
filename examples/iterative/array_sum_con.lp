time_limit(20).
int_range(0, 3).

array(xs, 3).
input_array(xs).
input_variable(len_xs).
output_variable(s).
extra_variable(i; x).

constant(0; 1).
disallow_feature(mul; div; mod; if; sub).
read_only_variable(len_xs).
read_only_array(xs).

array(xs, 3, elem, index).
var(i; len_xs, index).
var(x; s, elem).
con(0; 1, index).
con(0, elem).

precondition(haskell("in_len_xs == length in_xs")).
postcondition(haskell("out_s == sum in_xs")).
