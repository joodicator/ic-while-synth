#include "run.lp".

#hide.
#show counter_in/2.
#show counter_out/2.
#show counter_expected_out/2.

% Select values for input variables which satisfy our constraints.
1{ counter_in(V, C) : int(C) }1 :- input_var(V).

% Produce an expected output value if at all possible.
#maximize{ counter_expected_out(_,_) }.

% Define counter_* predicates to hide the complexity of run.lp.
in(counter, V, C) :- counter_in(V, C).
counter_out(V, C) :- run_var_out(counter, V, C).

% counter_expected_out(x, Out_x) :- postcon(In_x, Out_x), counter_in(x, In_x).

% input_var(x).
% output_var(x). 

% line_instr(1, set(x, var(x))).

% % Precondition must hold.
% precon(In_x) :- In_x >= 0, counter_in(x,In_x).
% :- not precon(In_x), counter_in(x,In_x).

% % Postcondition must not hold.
% postcon(In_x, Out_x) :- Out_x == In_x*In_x, counter_in(x,In_x), int(Out_x).
% :- postcon(In_x, Out_x), counter_in(x,In_x), counter_out(x,Out_x).
