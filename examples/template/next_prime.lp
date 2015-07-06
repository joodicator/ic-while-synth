% Configuration entries:
#const ceiling=20.

int_range(0,ceiling).
time_limit(25).
constant(0; 1).
disallow_feature(div; mul; sub).

while_max(1).
if_max(1).

program_variable(p; a; b).
logic_variable(p0; p1; q).

template(1, pre("prime(P), P0=P, next_prime(P,Q)")).
template(2, mid("next_prime(P0,Q), A>0:P==Q, A<=0:P<Q, P0<=P, P<=Q, prime(P0)")).
template(3, instr(while(le(var(a), con(0)), auto))).
template(4, mid("next_prime(P0,Q), A>0:P==Q, A<=0:P<Q, P0<=P, P<=Q, prime(P0), A<=0, P1=P")).
template(5, mid("next_prime(P0,Q), A>0:P==Q, A<=0:P<Q, P0<=P, P<=Q, prime(P0), Q-P < Q-P1")).
template(6, instr(end_while)).
template(7, mid("next_prime(P0,Q), A>0:P==Q, A<=0:P<Q, P0<=P, P<=Q, prime(P0), A>0")).
template(8, post("prime(P), P > P0, not prime(Q) : Q=P0+1..P-1")).

% Domain-specific predicates:
prime(N) :- N=2..ceiling, N #mod K != 0 : K=2..N-1.
next_prime(P, Q) :- prime(P), prime(Q), P < Q, not prime(R) : R=P+1..Q-1.
