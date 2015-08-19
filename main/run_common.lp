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
    run_line(R,T,L), T<time_max, not line(L), run_stack_depth(R,T,0),
    not run_fail(R).

run_fail(R) :- run_fail(R,_).

% Identify failure due to timeout and stack overflow.
run_fail(R,time_max,L,timeout) :-
    run_line(R,time_max,L).
run_fail(R,T,L,stack_overflow) :-
    run_line(R,T,L), run(R,T), not run(R,T+1), line_call(L).

run_does_halt(R)     :- run_halt(R,_).
run_does_not_halt(R) :- run(R), not run_does_halt(R).

% run_var_out(R,X,C) if the final value of X in run R is C.
run_var_out(R,X,C)     :- run_does_halt(R), run_var(R,T,X,C), run_halt(R,T).
run_var_out(R,X,unset) :- run_does_halt(R), run_var(R,X), not run_var_out_set(R,X).
run_var_out_set(R,X)   :- run_var_out(R,X,C), C != unset.

%-------------------------------------------------------------------------------
% Initialise variables to their input value, if any.
run_var(R,0,X,C) :- in(R,X,C), int(C).

% Identify lines where a particular variable's value may change.
line_set(L,X) :-
    sub_line_instr(L,set(X,_)), scalar_var(X).
line_set(L,array(A,I)) :-
    sub_line_instr(L,set(array(A,_),_)), array(A,Len), I=0..Len-1.

% On a set statement, update variables; otherwise, unless possibly
% returning from a subroutine call, leave them constant.
run_var(R,T+1,X,C) :-
    run_line(R,T,L), line(L), not line_set(L,X),
    run_var(R,T,X,C).
run_var(R,T+1,X,C) :-
    run_line(R,T,L), sub_line_instr(L,set(X,E)), scalar_var(X),
    run_expr(R,T,E,C), int(R,X,C).
run_var(R,T+1,array(A,I),C) :-
    run_line(R,T,L), sub_line_instr(L,set(array(A,IE),E)),
    run_expr(R,T,IE,I), run_expr(R,T,E,C),
    run_var(R,array(A,I)), int(R,array(A,I),C).
run_var(R,T+1,array(A,I),C) :-
    run_line(R,T,L), sub_line_instr(L,set(array(A,JE),_)),
    run_expr(R,T,JE,J), J!=I, I=0..Len-1, array(A,Len),
    run_var(R,T,array(A,I),C).

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

% Identify value-out-of-bounds errors.
run_fail(R,T,L,(var_overflow,C)) :-
    run_line(R,T,L), sub_line_instr(L,set(V,E)), scalar_var(V),
    run_expr(R,T,E,C), not int(R,V,C).

run_fail(R,T,L,(var_overflow,C)) :-
    run_line(R,T,L), sub_line_instr(L,set(array(A,IE),E)),
    run_expr(R,T,IE,I), run_expr(R,T,E,C), not int(R,array(A,I),C).

run_fail(R,T,L,(array_index,I)) :-
    run_line(R,T,L), sub_line_instr(L,set(array(A,IE),_)),
    run_expr(R,T,IE,I), not run_var(R,array(A,I)).

% Identify expressions which evaluate to a legal result.
run_expr_not_fail(R,T,E) :- run_expr(R,T,E,_).

% Identify the time at which a run fails due to illegal arithmetic.
run_fail(R,T,L,expr) :-
    run_line(R,T,L), sub_line_instr(L,set(_,E)),
    not run_expr_not_fail(R,T,E).

%-------------------------------------------------------------------------------
% Identify lines where execution may jump to a remote instruction.
line_jump(L) :- sub_line_instr(L, if(_,_)).
line_jump(L) :- sub_line_instr(L, while(_,_)).
line_jump(L) :- sub_line_instr(L, end_while).
line_jump(L) :- sub_line_instr(L, call(_)).

line_call(L) :- sub_line_instr(L, call(_)).

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
run_fail(R,T) :- run_fail(R,T,_,_).

run_line(R,T+1,(P,N+1)) :-
    run_line(R,T,(P,N)), line((P,N)),
    not line_jump((P,N)), not run_fail(R,T).

run_stack_depth(R, T+1, D) :-
    run_line(R,T,L), line(L), not line_call(L), run_stack_depth(R,T,D).
run_stack_line(R, T+1, I, IL) :-
    run_line(R,T,L), line(L), run_stack_line(R,T,I,IL).
run_stack_var(R, T+1, I, IV, IC) :-
    run_line(R,T,L), line(L), run_stack_var(R,T,I,IV,IC).

% Run if statements.
run_line(R,T+1,(P,N+1)) :-
    run(R,T), run_line(R,T,(P,N)), line_instr_if_guard((P,N),B), run_bool(R,T,B).
run_line(R,T+1,(P,N+BodyLength+1)) :-
    run(R,T), run_line(R,T,(P,N)), sub_line_instr((P,N),if(B,BodyLength)),
    not run_bool(R,T,B).

% Run while loops.
run_line(R,T+1,(P,N+1)) :-
    % Boolean guard is true upon (re-)entering loop.
    run(R,T), run_line(R,T,(P,N)), line_instr_while_guard((P,N),B),
    run_bool(R,T,B).
run_line(R,T+1,(P,N+BodyLength+2)) :-
    % Boolean guard is false upon (re-)entering loop.
    run(R,T), run_line(R,T,(P,N)), sub_line_instr((P,N),while(B,BodyLength)),
    not run_bool(R,T,B).
run_line(R,T+1,(P,HeadLine)) :-
    % Re-enter loop after reaching end_while.
    run(R,T), run_line(R,T,(P,N)), sub_line_instr((P,N),end_while),
    line_instr_while_length((P,HeadLine), BodyLength),
    N == HeadLine+BodyLength+1.

%-------------------------------------------------------------------------------
% Run subroutine calls.

run_line(R,T+1,(Q,1)) :-
    run(R,T), run_line(R,T,L), sub_line_instr(L,call(Q)),
    run_stack_depth(R,T,D), D < stack_max.

% Increment the stack depth.
run_stack_depth(R, T+1, D+1) :-
    run_line(R,T,L), line_call(L), run_stack_depth(R,T,D), D < stack_max.

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
    run_line(R,T,L), not line(L), run_stack_depth(R,T,D), D > 0.

% Restore and increment the caller's line index.
run_line(R, T+1, (RetP,RetN+1)) :-
    run(R,T), run_line(R,T,L), not line(L), run_stack_depth(R,T,D), D > 0,
    run_stack_line(R,T,D-1,(RetP,RetN)).

% Restore values of variables saved on the stack.
run_var(R, T+1, V, C) :-
    run_line(R,T,L), not line(L), run_stack_depth(R,T,D), D > 0,
    run_stack_var(R,T,D-1,V,C).

% Preserve values of variables not saved on the stack.
run_var(R, T+1, V, C) :-
    run_line(R,T,L), not line(L), run_stack_depth(R,T,D), D > 0,
    run_stack_line(R,T,D-1,(ReturnP,_)), not local(ReturnP,V),
    run_var(R,T,V,C).
run_var(R, T+1, V, C) :-
    run_line(R,T,L), not line(L), run_stack_depth(R,T,0),
    run_var(R,T,V,C).

% Preserve any other stack entries.
run_stack_line(R, T+1, I, IL) :-
    run_line(R,T,L), not line(L), run_stack_depth(R,T,D), D > 1,
    I=0..D-2, run_stack_line(R,T,I,IL).
run_stack_var(R, T+1, I, IV, IC) :-
    run_line(R,T,L), not line(L), run_stack_depth(R,T,D), D > 1,
    I=0..D-2, run_stack_var(R,T,I,IV,IC).

%-------------------------------------------------------------------------------
% Identify the time at which a run fails due to an illegal guard.
run_fail(R,T,L,bool) :-
    run_line(R,T,L), line_instr_if_guard(L,B),
    not run_bool_not_fail(R,T,B).
run_fail(R,T,L,bool) :-
    run_line(R,T,L), line_instr_while_guard(L,B),
    not run_bool_not_fail(R,T,B).
