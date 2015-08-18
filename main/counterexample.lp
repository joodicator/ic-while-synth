#include "run.lp".

#hide.
#show counter_in/2.
#show counter_out/2.
#show counter_array/2.
#show counter_halt/0.
#show any_expected/0.
#show expect/2.

% Suppress undefined predicate warnings.
input_array(never) :- output_array(never).
output_array(never) :- input_array(never).

% Generate input/output variable declarations for each array element.
input_var(array(A,I))  :- run_var(counter, array(A,I)), input_array(A).
output_var(array(A,I)) :- run_var(counter, array(A,I)), output_array(A).

% Select a length for each array.
1{ counter_array(A,L) : L=0..Lmax }1 :- array(A,Lmax).
array(counter, A, L) :- counter_array(A, L).

% Select values for input variables satisfying the precondition.
0{ counter_in(V, C) : int(C) }1 :- input_var(V).
counter_in_any(V) :- counter_in(V,_).

% Select values for expected output values satisfying the postcondition.
0{ any_expected }1.
1{ expect(V, C) : int(C) }1 :- output_var(V), any_expected.

% Produce an expected output value if possible.
#maximize{ any_expected }.

% Do not accept the case of no expected value and no actual value
% as a counterexample
:- not any_expected, not any_actual.
any_actual :- any_actual_var(_).
any_actual_var(V) :- counter_out(V, C), C != unset, output_var(V).

% Define counter_* predicates to hide the complexity of run.lp.
in(counter, V, C) :- counter_in(V, C).
counter_out(V, C) :- run_var_out(counter, V, C).
counter_halt :- run_does_halt(counter).
var(V) :- input_var(V).
var(V) :- output_var(V).
