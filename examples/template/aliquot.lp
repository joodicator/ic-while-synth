#const ceiling=10.

time_limit(15).
int_range(0, ceiling).

constant(0..3).
disallow_feature(div; sub; mul; while).

program_variable(x; d; t; a).
logic_variable(d0).
read_only_variable(x).

while_max(1).

template(1, pre("X > 0")).
template(2, while(lt(var(a), var(x)))).
template(3, inv("partial_divisor_sum(A, X, D)")).
template(4, var("A", increasing)).
template(5, end_while).
template(6, mid("divisor_sum(X, D)")).
template(7, post("T==0:prime(X), T==1:deficient(X):not prime(X), T==2:perfect(X), T==3:excessive(X)")).

% Domain-specific predicates:
divisor_sum(X, D) :-
    partial_divisor_sum(X, X, D).

partial_divisor_sum(1, X, 0) :-
    X=1..ceiling.
partial_divisor_sum(I+1, X, D+I) :-
    X=1..ceiling, I < X, X #mod I == 0,
    partial_divisor_sum(I, X, D).
partial_divisor_sum(I+1, X, D) :-
    X=1..ceiling, I < X, X #mod I != 0,
    partial_divisor_sum(I, X, D).

prime(X)     :- divisor_sum(X, 1), X=1..ceiling.
perfect(X)   :- divisor_sum(X, X), X=1..ceiling.
deficient(X) :- divisor_sum(X, D), D < X, X=1..ceiling.
excessive(X) :- divisor_sum(X, D), D > X, X=1..ceiling.
