% Configuration entries:
#const ceiling=10.

int_range(0,ceiling).
time_limit(40).
constant(0).
disallow_feature(mul; div; add; sub; mul).

input_variable(x; y).
output_variable(g).

precondition("In_x > 0, In_y >=0, In_x > In_y").
postcondition("gcd(In_x, In_y, Out_g)").

gcd(X, Y, G) :-
    common_divisor(X,Y,G),
    G >= D : common_divisor(X,Y,D).
common_divisor(In_x, In_y, D) :-
    In_x=0..ceiling, In_y=0..ceiling, D=1..ceiling,
    In_x #mod D == 0, In_y #mod D == 0.

%echo_asp.
