%-------------------------------------------------------------------------------
% This is the part of the program interpreter common to run.lp and learn.lp.
%-------------------------------------------------------------------------------

% run_any if any test cases were specified.
run_any :- in(_,_,_).

% run(R) if there is a test case called R.
run(0) :- not run_any.
run(R) :- in(R,_,_).

% run_var(R,V) if the variable V exists in run R.
run_var(R,V)          :- run(R), scalar_var(V).
run_var(R,array(A,I)) :- run_array(R,A,L), I=0..L-1.

% Default run-specific array lengths to the maximum length.
run_array_given(R,A) :- array(R,A,_).
run_array(R,A,L) :- array(R,A,L).
run_array(R,A,M) :- not run_array_given(R,A), array(A,M), run(R).

% run(R,T) if test case R runs to at least time T.
run(R,T) :- run_line(R,T,L), T<time_max, line(L).

% run_halt(R,T) if test case R halts normally at time T.
run_halt(R,T) :- run_line(R,T,L), T<time_max, not line(L).

run_does_halt(R) :- run_halt(R,_).
run_does_not_halt(R) :- run(R), not run_does_halt(R).

% run_var_out(R,X,C) if the final value of X in run R is C.
run_var_out(R,X,C)     :- run_var(R,T,X,C), not run(R,T).
run_var_out(R,X,unset) :- run_var(R,X), not run_var_out_set(R,X).
run_var_out_set(R,X)   :- run_var_out(R,X,C), C != unset.

% Initialise variables to their input value, if any.
run_var(R,0,X,C) :- in(R,X,C), int(C).

% Identify lines where a particular variable's value may change.
line_set(L,X) :-
    line_instr(L,set(X,_)), scalar_var(X).
line_set(L,array(A,I)) :-
    line_instr(L,set(array(A,_),_)), array(A,Len), I=0..Len-1.

% On a set statement, update variables; otherwise, leave them constant.
run_var(R,T+1,X,C) :-
    run_line(R,T,L), not line_set(L,X),
    run_var(R,T,X,C), int(C).
run_var(R,T+1,X,C) :-
    run_line(R,T,L), line_instr(L,set(X,E)), scalar_var(X),
    run_expr(R,T,E,C), int(C).
run_var(R,T+1,array(A,I),C) :-
    run_line(R,T,L), line_instr(L,set(array(A,IE),E)),
    run_expr(R,T,IE,I), run_expr(R,T,E,C), run_var(R,array(A,I)).
run_var(R,T+1,array(A,I),C) :-
    run_line(R,T,L), line_instr(L,set(array(A,JE),_)),
    run_expr(R,T,JE,J), J!=I, I=0..Len-1, array(A,Len),
    run_var(R,T,array(A,I),C).

% Identify expressions which evaluate to a legal result.
run_expr_not_fail(R,T,E) :- run_expr(R,T,E,_).

% Identify the time at which a run fails due to illegal arithmetic.
run_fail(R,T) :-
    run_line(R,T,L), line_set(L,X), line_instr(L,set(X,E)),
    not run_expr_not_fail(R,T,E).

% Identify lines where execution may jump to a remote instruction.
line_jump(L) :- line_instr(L, if(_,_)).
line_jump(L) :- line_instr(L, while(_,_)).
line_jump(L) :- line_instr(L, end_while).

% For efficiency, project out if/while parameters occurring at these lines.
line_instr_if_guard(L, G)     :- line_instr(L, if(G, _)).
line_instr_if_length(L, B)    :- line_instr(L, if(_, B)).
line_instr_while_guard(L, G)  :- line_instr(L, while(G, _)).
line_instr_while_length(L, B) :- line_instr(L, while(_, B)).

% Start at line 1, and, by default, move to the next line at each step.
run_line(R,0,1) :-
    run(R).
run_line(R,T+1,L+1) :-
    line(L), run(R,T), run_line(R,T,L),
    not line_jump(L), not run_fail(R,T).

% Run if statements.
run_line(R,T+1,L+1) :-
    run(R,T), run_line(R,T,L), line_instr_if_guard(L,B), run_bool(R,T,B).
run_line(R,T+1,L+BodyLength+1) :-
    run(R,T), run_line(R,T,L), line_instr(L,if(B,BodyLength)),
    not run_bool(R,T,B).

% Run while loops.
run_line(R,T+1,L+1) :-
    % Boolean guard is true upon (re-)entering loop.
    run(R,T), run_line(R,T,L), line_instr_while_guard(L,B),
    run_bool(R,T,B).
run_line(R,T+1,L+BodyLength+2) :-
    % Boolean guard is false upon (re-)entering loop.
    run(R,T), run_line(R,T,L), line_instr(L,while(B,BodyLength)),
    not run_bool(R,T,B).
run_line(R,T+1,HeadLine) :-
    % Re-enter loop after reaching end_while.
    run(R,T), run_line(R,T,L), line_instr(L,end_while),
    line_instr_while_length(HeadLine, BodyLength),
    L == HeadLine+BodyLength+1.

% Identify the time at which a run fails due to an illegal guard.
run_fail(R,T) :-
    run_line(R,T,L), line_instr_if_guard(L,B),
    not run_bool_not_fail(R,T,B).
run_fail(R,T) :-
    run_line(R,T,L), line_instr_while_guard(L,B),
    not run_bool_not_fail(R,T,B).
