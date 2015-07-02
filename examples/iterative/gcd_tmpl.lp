% Configuration entries:
#const ceiling=20.

int_range(0,ceiling).
time_limit(40).
constant(0).
disallow_feature(mul; div; add; sub).

program_variable(x; y; g).
logic_variable(x0; y0; x1; y1).

template(1, pre("X+Y > 0, X0 = X, Y0 = Y")).
template(2, mid("gcd(X,Y,G1), gcd(X0,Y0,G1)")).
template(3, instr(while(gt(var(y), con(0)), auto))).
template(4, mid("gcd(X,Y,G1), gcd(X0,Y0,G1), Y > 0, X1 = X, Y1 = Y")).
template(5, mid("gcd(X,Y,G1), gcd(X0,Y0,G1), Y < Y1")).
template(5, instr(end_while)).
template(6, mid("gcd(X,Y,G1), gcd(X0,Y0,G1), Y <= 0")).
template(7, post("gcd(X,Y,G)")).

% Domain-specific predicates:
gcd(X, Y, G) :-
    common_divisor(X,Y,G),
    G >= D : common_divisor(X,Y,D).
common_divisor(X, Y, D) :-
    X=0..ceiling, Y=0..ceiling, D=1..ceiling,
    X #mod D == 0, Y #mod D == 0.
