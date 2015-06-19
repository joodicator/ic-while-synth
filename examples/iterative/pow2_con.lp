int_range(0, 32).
time_limit(30).
constant(1; 2).
disallow_feature(div; mod; if).

input_variable(x).
output_variable(y).

precondition("0 <= In_x, In_x < 31").
postcondition("Out_y == 2 ** In_x").

echo_clingo.
echo_asp.
