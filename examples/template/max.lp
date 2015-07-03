% Input:  5 integers x1-x5.
% Output: Their maximum value.

int_range(0, 5).
time_limit(10).

disallow(arithmetic).
program_variable(x1; x2; x3; x4; x5; max).
read_only_variable(x1; x2; x3; x4; x5).

template(1, mid("max(X1,X2, Max)")).
template(2, mid("max(X1,X2,X3, Max)")).
template(3, mid("max(X1,X2,X3,X4, Max)")).
template(4, post("max(X1,X2,X3,X4,X5, Max)")).

% Domain-specific predicates:
dom(0..5).
#domain dom(A;B;C;D;E;M).

max(A,B, M) :-
    any(A,B, M), M >= A, M >= B.
max(A,B,C, M) :-
    any(A,B,C, M), M >= A, M >= B, M >= C.
max(A,B,C,D, M) :-
    any(A,B,C,D, M), M >= A, M >= B, M >= C, M >= D.
max(A,B,C,D,E, M) :-
    any(A,B,C,D,E, M), M >= A, M >= B, M >= C, M >= D, M >= E.

any(A,B, A). any(A,B, B).
any(A,B,C, A). any(A,B,C, B). any(A,B,C, C).
any(A,B,C,D, A). any(A,B,C,D, B). any(A,B,C,D, C). any(A,B,C,D, D).
any(A,B,C,D,E, A). any(A,B,C,D,E, B). any(A,B,C,D,E, C). any(A,B,C,D,E, D). any(A,B,C,D,E, E).
