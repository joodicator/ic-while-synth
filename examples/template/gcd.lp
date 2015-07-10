% Configuration entries:
#const ceiling=10.

int_range(0,ceiling).
time_limit(40).
constant(0).
disallow_feature(mul; div; add; sub).

program_variable(x; y; g).
logic_variable(gg).

template(1, pre("X+Y>0, gcd(X,Y,Gg)")).
template(2, while(gt(var(y), con(0)))).
template(3, inv("gcd(X,Y,Gg), X+Y>0")).
template(4, var("Y", dec)).
template(5, end_while).
template(6, post("G=Gg")).

% Domain-specific predicates:
gcd(X, Y, G) :-
    common_divisor(X,Y,G),
    G >= D : common_divisor(X,Y,D).
common_divisor(X, Y, D) :-
    X=0..ceiling, Y=0..ceiling, D=1..ceiling,
    X #mod D == 0, Y #mod D == 0.
