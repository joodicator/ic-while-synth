time_limit(50).
int_range(0..4).

array(m; t, 4).

input_array(m).
input_variable(w; h).
read_only_variable(w; h).
output_array(t).
extra_variable(i; j; k; x).

disallow_feature(if; div; mod).
constant(0; 1).

array(m, 4, elem, index).
array(t, 4, elem, index).
var(i; j; k, index).
var(x, elem).
con(0; 1, index).
value(elem, 0..1).

precondition(haskell(
    "let l = length in_m in l == length in_t && l == in_w*in_h && l > 0"))
postcondition(haskell(
    "out_t == concat . transpose $ [ | ]"))
