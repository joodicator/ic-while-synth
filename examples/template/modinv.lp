% Modular inverse: given n>1 and 0<x<n, computes
% y = the multiplicative inverse of x, modulo n.

#const ceiling=5.
int_range(0, ceiling).
time_limit(40).

constant(0; 1).
disallow(sub; div).

program_variable(x; y; n; m).
read_only_variable(x; n).
logic_variable(x0; n0; y1).

template(1, pre("N>1, 0<X,X<N, gcd(X,N,1), X0=X, N0=N")).
template(2, while(lt(var(m), var(n)))).
template(3, inv("Y<=Xi, M<N:Y<Xi, M==N:Y>=Xi, modinv(X,N,Xi)")).
template(4, var("Y", increasing)).
template(5, end_while).
template(6, post("X0*Y #mod N0 == 1, Y<N0")).

% Domain-specific predicates:
modinv(X, N, Y) :-
    N=1..ceiling, X=1..N-1, Y=1..N-1,
    X*Y #mod N == 1.

gcd(X, Y, G) :-
    common_divisor(X,Y,G),
    G >= D : common_divisor(X,Y,D).

common_divisor(X, Y, D) :-
    X=0..ceiling, Y=0..ceiling, D=1..ceiling,
    X #mod D == 0, Y #mod D == 0.
