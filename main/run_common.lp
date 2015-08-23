%-------------------------------------------------------------------------------
% This is the part of the program interpreter common to run.lp and learn.lp.
%-------------------------------------------------------------------------------

% Silence warnings for certain undefined predicates.
local(-1,-1)       :- main_sub(-1).
main_sub(-1)       :- value(-1,-1).
value(-1,-1)       :- con(-1,-1).
con(-1,-1)         :- var(-1,-1).
var(-1,-1)         :- array(-1,-1,-1,-1).
array(-1,-1,-1,-1) :- local(-1,-1).

% run_any if any test cases were specified.
run_any :- in(_,_,_).

% run(R) if there is a test case called R.
%run(0) :- not run_any.
run(R) :- in(R,_,_).

% run_var(R,V) if the variable V exists in run R.
run_var(R,V)          :- run(R), scalar_var(V).
run_var(R,array(A,I)) :- run_array(R,A,L), I=0..L-1.

% Default run-specific array lengths to the maximum length.
run_array_given(R,A) :- array(R,A,_).
run_array(R,A,L) :- array(R,A,L).
run_array(R,A,M) :- not run_array_given(R,A), array(A,M), run(R).

% run(R,T) if test case R runs to at least time T.
run(R,T) :- run_line(R,T,L), T<time_max.

% run_halt(R,T) if test case R halts normally at time T: i.e. if execution
% reaches the last instruction while the call stack is empty.
run_halt(R,T) :-
    run_not_line(R,T), T<time_max, run_stack_depth(R,T,0).
%:- run_fail(R,_).

run_does_halt(R)     :- run_halt(R,_).
run_does_not_halt(R) :- run(R), not run_does_halt(R).

% run_var_out(R,X,C) if the final value of X in run R is C.
run_var_out(R,X,C)   :- run_halt(R,T), run_var(R,T,X,C).

%-------------------------------------------------------------------------------
% Initialise variables to their input value, if any.
run_var(R,0,X,C)     :- in(R,X,C), int(C).
run_var(R,0,X,unset) :- not_any_in(R,X).
not_any_in(R,X)      :- run_var(R,X), not any_in(R,X).
any_in(R,X)          :- in(R,X,_).

% Identify lines where a particular variable's value may change.
not_line_set(L,X) :-
    var_set_at_line(L, scalar_var(Y)), scalar_var(X), Y != X.
not_line_set(L,X) :-
    var_set_at_line(L, array(_)), scalar_var(X).
not_line_set(L,array(A, I)) :-
    var_set_at_line(L, scalar_var(Y)), array(A, Len), I=0..Len-1.
not_line_set(L,array(A,I)) :-
    var_set_at_line(L, array(A2)), A2 != A, array(A,Len), I=0..Len-1.



not_line_set(L,array(A,I)) :-
    not_set(L), array(A, Len), I = 0..Len-1.
not_line_set(L,V) :-
    not_set(L), scalar_var(V).

not_set(L) :- line_call(L).
not_set(L) :- line_while(L).
not_set(L) :- line_if(L).
not_set(L) :- line_end_while(L).



% Frame axiom to preserve variable values not involved in a 'set' instruction.
%run_var(R,T+1,array(A,I),C) :-
%    array(R,A,Len), I=0..Len-1, I != J, run_var(R,T,array(A,I),C),
%    current_line_instr(R,T,set(array(A,JE),_)), run_expr(R,T,JE,J).
run_var(R,T+1,array(A,I),C) :-
    array(R,A,Len), I=0..Len-1, I != J, run_var(R,T,array(A,I),C),
    %current_line_instr(R,T,set(array(A,JE),_)), run_expr(R,T,JE,J).
    set_lhs(R,T,array(A,J)).


run_var(R,T+1,X,C) :-
    run_line(R,T,L), line(L), not_line_set(L,X), run_var(R,T,X,C).


:- set_rhs(R, T, unset).
:- set_lhs(R, T, array(_,unset)).


% Enforce type-based and individual constraints on variable values.
con(C)         :- con(C, _).
con_type(C, T) :- con(C, T).

var(V)         :- var(V, _).
var_type(V, T) :- var(V, T).

array(A)         :- array(A,_).
array(A,L)       :- array(A,L,_,_).
elem_type(A,ET)  :- array(A,_,ET,_).
index_type(A,IT) :- array(A,_,_,IT).

int(R,V,C)            :- int(C), run_var(R,V), not any_value(R,V).
int(R,V,C)            :- value(R,V,C).
any_value(R,V)        :- value(R,V,_).
value(R,array(A,I),C) :- run_var(R,array(A,I)), value(R,A,C).
value(R,V,C)          :- value(V,C), run(R).
value(A,C) :- value(T,C), elem_type(A,T).
value(V,C) :- value(T,C), var_type(V,T).

%-------------------------------------------------------------------------------
% Identify lines where execution may jump to a remote instruction.
not_line_jump(L) :- sub_line_instr(L, set(_, _)).

not_line_call(L) :- line_set(L).
not_line_call(L) :- line_while(L).
not_line_call(L) :- line_if(L).
not_line_call(L) :- line_end_while(L).
not_line_call(R, T) :- run_line(R,T,L), not_line_call(L).

line_call(L)      :- sub_line_instr(L, call(_)).
line_set(L)       :- sub_line_instr(L, set(_, _)).
line_while(L)     :- sub_line_instr(L, while(_, _)).
line_if(L)        :- sub_line_instr(L, if(_, _)).
line_end_while(L) :- sub_line_instr(L, end_while).

% For efficiency, project out if/while parameters occurring at these lines.
line_instr_if_guard(L, G)     :- sub_line_instr(L, if(G, _)).
line_instr_if_length(L, B)    :- sub_line_instr(L, if(_, B)).
line_instr_while_guard(L, G)  :- sub_line_instr(L, while(G, _)).
line_instr_while_length(L, B) :- sub_line_instr(L, while(_, B)).

% Start at line 1 of main, and, by default, move to the next line at each step.
run_line(R, 0, (main,1)) :- run(R), not any_main_sub.
run_line(R, 0, (P,1))    :- run(R), main_sub(P).
run_stack_depth(R, 0, 0) :- run(R).

any_main_sub  :- main_sub(_).

run_line(R,T+1,(P,N+1)) :-
    run_line(R,T,(P,N)), line((P,N)),
    not_line_jump((P,N)).

run_stack_depth(R, T+1, D) :-
    not_line_call(R, T), run_stack_depth(R,T,D).
run_stack_line(R, T+1, I, IL) :-
    run_line_projection(R,T), run_stack_line(R,T,I,IL).
run_stack_var(R, T+1, I, IV, IC) :-
    run_line_projection(R,T), run_stack_var(R,T,I,IV,IC).

% Run if statements.
run_line(R,T+1,(P,N+1)) :-
    run(R,T), run_line(R,T,(P,N)), line_instr_if_guard((P,N),B),
    run_bool(R,T,B,true).
run_line(R,T+1,(P,N+BodyLength+1)) :-
    run(R,T), run_line(R,T,(P,N)), sub_line_instr((P,N),if(B,BodyLength)),
    run_bool(R,T,B,false).

% Run while loops.
run_line(R,T+1,(P,N+1)) :-
    % Boolean guard is true upon (re-)entering loop.
    run(R,T), run_line(R,T,(P,N)), line_instr_while_guard((P,N),B),
    run_bool(R,T,B,true).
run_line(R,T+1,(P,N+BodyLength+2)) :-
    % Boolean guard is false upon (re-)entering loop.
    run(R,T), run_line(R,T,(P,N)), sub_line_instr((P,N),while(B,BodyLength)),
    run_bool(R,T,B,false).
run_line(R,T+1,(P,HeadLine)) :-
    % Re-enter loop after reaching end_while.
    run(R,T), run_line(R,T,(P,N)), sub_line_instr((P,N),end_while),
    line_instr_while_length((P,HeadLine), BodyLength),
    N == HeadLine+BodyLength+1.

%-------------------------------------------------------------------------------
% Run subroutine calls.

run_line(R,T+1,(Q,1)) :-
    run(R,T), current_line_instr(R,T,call(Q)),
    run_stack_depth(R,T,D), D < stack_max.

% Increment the stack depth.
run_stack_depth(R, T+1, D+1) :-
    run_line_call_projection(R,T), run_stack_depth(R,T,D), D < stack_max.

% Save the current line index to the stack.
run_stack_line(R, T+1, D, L) :-
    run_line(R,T,L), line_call(L), run_stack_depth(R,T,D), D < stack_max.

% Save local variables to the stack.
run_stack_var(R, T+1, D, V, C) :-
    run_line(R,T,(P,N)), line_call((P,N)), run_stack_depth(R,T,D), D < stack_max,
    local(P,V), run_var(R,T,V,C).

%-------------------------------------------------------------------------------
% Return from subroutine calls.

% Decrement the stack depth.
run_stack_depth(R, T+1, D-1) :-
    run_not_line(R,T), run_stack_depth(R,T,D), D > 0.

% Restore and increment the caller's line index.
run_line(R, T+1, (RetP,RetN+1)) :-
    run(R,T), run_not_line(R,T), run_stack_depth(R,T,D), D > 0,
    run_stack_line(R,T,D-1,(RetP,RetN)).

% Restore values of variables saved on the stack.
run_var(R, T+1, V, C) :-
    run_not_line(R,T), run_stack_depth(R,T,D), D > 0,
    run_stack_var(R,T,D-1,V,C).

% Preserve values of variables not saved on the stack.
run_var(R, T+1, V, C) :-
    run_not_line(R,T), run_stack_depth(R,T,D), D > 0,
    run_stack_line(R,T,D-1,(ReturnP,_)), not local(ReturnP,V),
    run_var(R,T,V,C).
run_var(R, T+1, V, C) :-
    run_not_line(R,T), run_stack_depth(R,T,0),
    run_var(R,T,V,C).

% Preserve any other stack entries.
run_stack_line(R, T+1, I, IL) :-
    run_not_line(R,T), run_stack_depth(R,T,D), D > 1,
    I=0..D-2, run_stack_line(R,T,I,IL).
run_stack_var(R, T+1, I, IV, IC) :-
    run_not_line(R,T), run_stack_depth(R,T,D), D > 1,
    I=0..D-2, run_stack_var(R,T,I,IV,IC).


%-------------------------------------------------------------------------------
% Mark's new rules.

current_line_instr(R, T, LI) :- run_line(R,T,(P,N)), sub_line_instr((P,N),LI).


var_set_at_line(L,array(A)) :-
  sub_line_instr(L,set(array(A,_),_)).

var_set_at_line(L,scalar_var(Y)) :-
  sub_line_instr(L,set(Y,_)), scalar_var(Y).

run_not_line(R, T) :-
  run_line(R, T, L), not line(L).

run_line_projection(R, T) :- run_line(R, T, L), line(L).


run_line_call_projection(R, T) :- run_line(R,T,L), line_call(L).


run_var(R,T+1,V,C) :-
  set_lhs(R, T, V), run_var(R, V), set_rhs(R, T, C), int(R,V,C).

set_lhs(R, T, array(A, I)) :-
  current_line_instr(R,T,set(array(A,IE),_)), run_expr(R,T,IE,I).

set_lhs(R,T,X) :-
  current_line_instr(R,T,set(X,_)), scalar_var(X).

set_rhs(R, T, C) :-
  current_line_instr(R,T,set(_,E)),
  run_expr(R,T,E,C).
