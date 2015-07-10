% Modular inverse: given n>1 and 0<x<n, computes
% y = the multiplicative inverse of x, modulo n.

#const ceiling=20.
int_range(0, ceiling).
time_limit(10).

constant(0; 1).
disallow(sub; div; mul; while).
if_statements_max(2).

program_variable(x; y; n; m).
read_only_variable(x; n).
logic_variable(x0; n0; xi).

template(1, pre("1<N,N<=10, 0<X,X<N, gcd(X,N,1), modinv(X,N,Xi)")).
template(2, while(lt(var(m), var(n)))).
template(3, inv("Y<=Xi, M<N : Y<Xi, M==N : Y==Xi")).
template(4, var("Y", increasing)).
template(5, end_while).
template(6, post("Y == Xi")).

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
