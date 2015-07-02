% Configuration entries:
#const ceiling=10.

int_range(0,ceiling).
time_limit(80).
constant(1; 2).
disallow_feature(div; mul; sub).

if_statements_max(1).
while_loops_max(1).

input_variable(p).
output_variable(p).
extra_variable(a, b).

precondition("In_p > 2").
postcondition("post(In_p, Out_p)").

% Postcondition predicate:
post(In_p, In_p) :- prime(In_p).
post(In_p, 0)    :- In_p=1..ceiling, not prime(In_p).

% Domain-specific predicates:
prime(N) :- N=2..ceiling, N #mod K != 0 : K=2..N-1.
