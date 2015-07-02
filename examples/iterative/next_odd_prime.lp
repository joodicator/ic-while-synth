% Configuration entries:
#const ceiling=20.

int_range(0,ceiling).
time_limit(50).
constant(1; 2).
disallow_feature(div; mul; sub).

input_variable(p).
output_variable(p).
extra_variable(a).

precondition("prime(In_p), In_p #mod 2 == 1").
postcondition("prime(Out_p), Out_p > In_p, not prime(Q) : Q=In_p+1..Out_p-1").

% Domain-specific predicates:
prime(N) :- N=2..ceiling, N #mod K != 0 : K=2..N-1.
