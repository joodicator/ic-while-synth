#include "run.lp".

#hide.
#show counter_in/2.
#show counter_out/2.
#show counter_array/2.

% Suppress undefined predicate warnings.
input_array(never) :- output_array(never).
output_array(never) :- input_array(never).

% Generate input/output variable declarations for each array element.
input_var(array(A,I))  :- run_var(counter, array(A,I)), input_array(A).
output_var(array(A,I)) :- run_var(counter, array(A,I)), output_array(A).

% Select a length for each array.
1{ counter_array(A,L) : L=0..Lmax }1 :- array(A,Lmax).
array(counter, A, L) :- counter_array(A, L).

% Select values for input variables which satisfy our constraints.
0{ counter_in(V, C) : int(C) }1 :- input_var(V).
counter_in_any(V) :- counter_in(V,_).

% Produce an expected output value if possible.
#maximize{ any_postcon }.

% Do not accept the case of no expected value and no actual value
% as a counterexample
:- not any_postcon, not any_actual.
any_actual :- any_actual_var(_).
any_actual_var(V) :- counter_out(V, C), C != unset, output_var(V).

% Define counter_* predicates to hide the complexity of run.lp.
in(counter, V, C) :- counter_in(V, C).
counter_out(V, C) :- run_var_out(counter, V, C).
var(V) :- input_var(V).
var(V) :- output_var(V).

% any_postcon :- postcon(_, ...).
% postcon(_, ...) :- (...).
% #show postcon/N.
