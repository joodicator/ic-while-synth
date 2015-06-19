% Configuration entries:
#define ceiling=10.

int_range(0,ceiling).
time_limit(40).
constant(0).
disallow_feature(rem, add, sub).

input_variable(x, y).
output_variable(g).
extra_variable(r).

precondition("In_x >= In_y, In_y >= 0").
postcondition("common_divisor(In_x,In_y,Out_g), Out_g >= D : common_divisor(In_x,In_y,D)").

common_divisor(In_x, In_y, D) :-
    In_x=1..ceiling, In_y=1..ceiling, 

% Postcondition predicate:
post(In_p, In_p) :- prime(In_p).
post(In_p, 0)    :- In_p=1..20, not prime(In_p).

% Domain-specific predicates:
prime(N) :- N=2..20, N #mod K != 0 : K=2..N-1.

