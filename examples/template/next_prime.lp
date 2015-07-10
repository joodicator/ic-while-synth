% Configuration entries:
#const ceiling=20.

int_range(0,ceiling).
time_limit(25).
constant(0; 1).
disallow_feature(div; mul; sub; while).

while_max(1).
if_max(1).

program_variable(p; a; b).
logic_variable(p0; q).

template( 1, pre("prime(P), next_prime(P,Q), P0=P")).
template( 2, while(gt(var(a), con(0)))).
template( 3,   inv("A==0 : P==Q, A>0 : P!=Q, P0<=P,P<=Q")).
template( 4,   var("P", increasing)).
template( 5,   while(gt(var(a), con(0)))).
template( 6,     inv("P #mod K != 0 : K=2..B-1, A>0 : P #mod B != 0, A<=0 : P #mod B == 0, 1<B,B<=P")).
template( 7,     var("B", increasing)).
template( 8,   end_while).
template( 9, end_while).
template(10, post("P == Q")).

% Domain-specific predicates:
prime(N) :- N=2..ceiling, N #mod K != 0 : K=2..N-1.
next_prime(P, Q) :- prime(P), prime(Q), P < Q, not prime(R) : R=P+1..Q-1.
