% Configuration entries:
#const ceiling=20.

int_range(0,ceiling).
time_limit(40).
constant(0).
disallow_feature(mul; div; add; sub).

if_statements_max(1).
while_loops_max(1).

input_variable(x; y).
output_variable(g).

precondition("In_x > 0, In_y > 0, In_x + In_y > 0").
postcondition("gcd(In_x, In_y, Out_g)").

gcd(X, Y, G) :-
    common_divisor(X,Y,G),
    G >= D : common_divisor(X,Y,D).
common_divisor(X, Y, D) :-
    X=0..ceiling, Y=0..ceiling, D=1..ceiling,
    X #mod D == 0, Y #mod D == 0.

echo_clingo.
