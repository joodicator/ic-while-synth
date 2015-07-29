#const time_max=50.
#const int_min=0.
#const int_max=4.

#include "run.lp".

#hide.
#show counter_array/2.
#show counter_in/2.
#show counter_out/2.
#show postcon/1.

line_instr( 1, set(m, con(0))).
line_instr( 2, set(i, con(0))).
line_instr( 3, while(lt(var(i), var(len_xs)), 4)).
line_instr( 4, set(x, array(xs,var(i)))).
line_instr( 5, if(lt(var(m), var(x)), 1)).
line_instr( 6, set(m, var(x))).
line_instr( 7, set(i, add(var(i),con(1)))).
line_instr( 8, end_while).

output_array(never) :- input_array(never).
input_array(never)  :- output_array(never).

array(xs, 4).
input_array(xs).
read_only_array(xs).
input_var(len_xs).
output_var(m).

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


any_postcon :- postcon(_).

% Precondition must hold.

:- counter_array(xs,0).

:- counter_array(xs,1), counter_in(len_xs,Len_xs), Len_xs != 1.
:- counter_array(xs,1), not counter_in_any(array(xs,0)).
:- counter_array(xs,1), not counter_in_any(len_xs).

:- counter_array(xs,2), counter_in(len_xs,Len_xs), Len_xs != 2.
:- counter_array(xs,2), not counter_in_any(array(xs,0)).
:- counter_array(xs,2), not counter_in_any(array(xs,1)).
:- counter_array(xs,2), not counter_in_any(len_xs).

:- counter_array(xs,3), counter_in(len_xs,Len_xs), Len_xs != 3.
:- counter_array(xs,3), not counter_in_any(array(xs,0)).
:- counter_array(xs,3), not counter_in_any(array(xs,1)).
:- counter_array(xs,3), not counter_in_any(array(xs,2)).
:- counter_array(xs,3), not counter_in_any(len_xs).

:- counter_array(xs,4), counter_in(len_xs,Len_xs), Len_xs != 4.
:- counter_array(xs,4), not counter_in_any(array(xs,0)).
:- counter_array(xs,4), not counter_in_any(array(xs,1)).
:- counter_array(xs,4), not counter_in_any(array(xs,2)).
:- counter_array(xs,4), not counter_in_any(array(xs,3)).
:- counter_array(xs,4), not counter_in_any(len_xs).

% Postcondition must not hold.
:- postcon(M), counter_out(m,M).

postcon(M) :-
    M==X0, counter_array(xs,1), int(M),
    counter_in(array(xs,0),X0).

postcon(M) :-
    M==X0, M>=X1, counter_array(xs,2), int(M),
    counter_in(array(xs,0),X0), counter_in(array(xs,1),X1).
postcon(M) :-
    M>=X0, M==X1, counter_array(xs,2), int(M),
    counter_in(array(xs,0),X0), counter_in(array(xs,1),X1).

postcon(M) :-
    M==X0, M>=X1, M>=X2, counter_array(xs,3), int(M),
    counter_in(array(xs,0),X0), counter_in(array(xs,1),X1),
    counter_in(array(xs,2),X2).
postcon(M) :-
    M>=X0, M==X1, M>=X2, counter_array(xs,3), int(M),
    counter_in(array(xs,0),X0), counter_in(array(xs,1),X1),
    counter_in(array(xs,2),X2).
postcon(M) :-
    M>=X0, M>=X1, M==X2, counter_array(xs,3), int(M),
    counter_in(array(xs,0),X0), counter_in(array(xs,1),X1),
    counter_in(array(xs,2),X2).

postcon(M) :-
    M==X0, M>=X1, M>=X2, M>=X3, counter_array(xs,4), int(M),
    counter_in(array(xs,0),X0), counter_in(array(xs,1),X1),
    counter_in(array(xs,2),X2), counter_in(array(xs,3),X3).
postcon(M) :-
    M>=X0, M==X1, M>=X2, M>=X3, counter_array(xs,4), int(M),
    counter_in(array(xs,0),X0), counter_in(array(xs,1),X1),
    counter_in(array(xs,2),X2), counter_in(array(xs,3),X3).
postcon(M) :-
    M>=X0, M>=X1, M==X2, M>=X3, counter_array(xs,4), int(M),
    counter_in(array(xs,0),X0), counter_in(array(xs,1),X1),
    counter_in(array(xs,2),X2), counter_in(array(xs,3),X3).
postcon(M) :-
    M>=X0, M>=X1, M>=X2, M==X3, counter_array(xs,4), int(M),
    counter_in(array(xs,0),X0), counter_in(array(xs,1),X1),
    counter_in(array(xs,2),X2), counter_in(array(xs,3),X3).
