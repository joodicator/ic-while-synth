% Configuration entries:
int_range(0,10).
time_limit(100).
constant(1).
disallow_feature(mul, div, sub).

input_variable(n).
output_variable(p).
extra_variable(j; k; l).
read_only_variable(n).

precondition("In_n >= 1").
postcondition("nth_prime(In_n, Out_p)").

% Domain-specific predicates:
prime(N) :- N=2..20, N #mod K != 0 : K=2..N-1.
next_prime(P, Q) :- prime(P), prime(Q), P < Q, not prime(N) : N=P+1..Q-1.
nth_prime(1, 2).
nth_prime(N, Q) :- N=2..20, nth_prime(N-1, P), next_prime(P, Q).

% Display settings:
echo_clingo.
echo_asp.
