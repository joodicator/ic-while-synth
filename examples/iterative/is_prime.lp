% Configuration entries:
int_range(0,20).
time_limit(100).
constant(1).
disallow_feature(rem, add, sub).

input_variable(p).
output_variable(p).
extra_variable(a, b).

precondition("In_p >= 2").
postcondition("post(In_p, Out_p)").

% Postcondition predicate:
post(In_p, In_p) :- prime(In_p).
post(In_p, 0)    :- In_p=1..20, not prime(In_p).

% Domain-specific predicates:
prime(N) :- N=2..20, N #mod K != 0 : K=2..N-1.

