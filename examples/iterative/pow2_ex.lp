int_range(0, 16).
time_limit(20).
constant(1; 2).
disallow_feature(div; mod; if).

input_variable(x).
output_variable(y).

in(ex0, x, 0). out(ex0, y, 1).
in(ex1, x, 1). out(ex1, y, 2).
in(ex2, x, 2). out(ex2, y, 4).
in(ex3, x, 3). out(ex3, y, 8).
in(ex4, x, 4). out(ex4, y, 16).
