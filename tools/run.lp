% The following constants MUST be defined in file a before this one:
% #const time_max=TM.   % Maximum time steps to simulate before failure.
% #const int_min=IMN.   % Least integer allowed to exist.
% #const int_max=IMX.   % Greatest integer allowed to exist.

% In specifying the program to run, the following facts may be given:
% line_instr(L,I)   % The instruction on line L >= 1 is I (see learn.lp).
% in(R,V,C).        % In run R, the variable V starts with the value C.

%------------------------------------------------------------------------------- 
% Require all runs to halt.
:- run_does_not_halt(R), run(R).

%------------------------------------------------------------------------------- 
% Domain predicates derived from constants and line_instr/2.

int(int_min..int_max).
line(L) :- line_instr(L, _).

var(V) :- line_expr_occurs(_, var(V)).
var(V) :- line_instr(_, set(V, _)).

line_expr_occurs(L, E) :- line_instr(L, set(_, E)).
line_expr_occurs(L, E) :- line_binop_occurs(L, E, _).
line_expr_occurs(L, E) :- line_binop_occurs(L, _, E).

line_bool_occurs(L, B) :- line_instr(L, if(B, _)).
line_bool_occurs(L, B) :- line_instr(L, while(B, _)).

line_binop_occurs(L, A, B) :- line_expr_occurs(L, add(A,B)).
line_binop_occurs(L, A, B) :- line_expr_occurs(L, sub(A,B)).
line_binop_occurs(L, A, B) :- line_expr_occurs(L, mul(A,B)).
line_binop_occurs(L, A, B) :- line_expr_occurs(L, div(A,B)).
line_binop_occurs(L, A, B) :- line_expr_occurs(L, mod(A,B)).
line_binop_occurs(L, A, B) :- line_bool_occurs(L, lt(A,B)).
line_binop_occurs(L, A, B) :- line_bool_occurs(L, gt(A,B)).

% Projection of if/while parameters.
line_instr_if_guard(L, G)     :- line_instr(L, if(G, _)).
line_instr_if_length(L, B)    :- line_instr(L, if(_, B)).
line_instr_while_guard(L, G)  :- line_instr(L, while(G, _)).
line_instr_while_length(L, B) :- line_instr(L, while(_, B)).

%-------------------------------------------------------------------------------
% Program interpreter.

% run_any if any test cases were specified.
run_any :- in(_,_,_).

% run(R) if there is a test case called R.
run(0) :- not run_any.
run(R) :- in(R,_,_).

% run(R,T) if test case R runs to at least time T.
run(R,T) :- run_line(R,T,L), T<time_max, line(L).

% run_halt(R,T) if test case R halts normally at time T.
run_halt(R,T) :- run_line(R,T,L), T<time_max, not line(L).

run_does_halt(R) :- run_halt(R,_).
run_does_not_halt(R) :- run(R), not run_does_halt(R).

% run_var_out(R,X,C) if the final value of X in run R is C.
run_var_out(R,X,C)     :- run_var(R,T,X,C), not run(R,T).
run_var_out(R,X,unset) :- run(R), var(X), not run_var_out_set(R,X).
run_var_out_set(R,X) :- run_var_out(R,X,C), C != unset.

% Initialise variables to their input value, if any.
run_var_in(R,X)  :- in(R,X,_).
run_var(R,0,X,C) :- in(R,X,C), int(C).

% On a set statement, update variables; otherwise, leave them constant.
run_var(R,T+1,X,C) :-
    run_line(R,T,L), not line_set(L,X), run_var(R,T,X,C), int(C).
run_var(R,T+1,X,C) :-
    run_line(R,T,L), line_set(L,X), line_instr(L,set(X,E)), run_expr(R,T,E,C), int(C).
run_fail(R,T) :-
    run_line(R,T,L), line_set(L,X), line_instr(L,set(X,E)),
    not run_expr_not_fail(R,T,E).
run_expr_not_fail(R,T,E) :- run_expr(R,T,E,_).

line_set(L,X) :- line_instr(L, set(X,_)).

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
    run(R,T), run_line(R,T,L), line_instr_while_guard(L,B), run_bool(R,T,B).
run_line(R,T+1,L+BodyLength+2) :-
    % Boolean guard is false upon (re-)entering loop.
    run(R,T), run_line(R,T,L), line_instr(L,while(B,BodyLength)),
    not run_bool(R,T,B).
run_line(R,T+1,HeadLine) :-
    % Re-enter loop after reaching end_while.
    run(R,T), run_line(R,T,L), line_instr(L,end_while),
    line_instr_while_length(HeadLine, BodyLength),
    L == HeadLine+BodyLength+1.

line_jump(L) :- line_instr(L, if(_,_)).
line_jump(L) :- line_instr(L, while(_,_)).
line_jump(L) :- line_instr(L, end_while).

% Evaluate arithmetic expressions.
run_expr(R,T,con(C),C) :-
    run_expr_occurs(R,T,con(C)).

run_expr(R,T,E,C) :-
    E=var(V), run_expr_occurs(R,T,E),
    run_var(R,T,V,C).

run_expr(R,T,E,C) :-
    E=add(E1,E2), run_expr_occurs(R,T,E),
    run_expr(R,T,E1,C1), run_expr(R,T,E2,C2),
    C = C1 + C2, int(C).

run_expr(R,T,E,C) :-
    E=sub(E1,E2), run_expr_occurs(R,T,E),
    run_expr(R,T,E1,C1), run_expr(R,T,E2,C2),
    C = C1 - C2, int(C).

run_expr(R,T,E,C) :-
    E=mul(E1,E2), run_expr_occurs(R,T,E),
    run_expr(R,T,E1,C1), run_expr(R,T,E2,C2),
    C = C1 * C2, int(C).

run_expr(R,T,E,C) :-
    E=div(E1,E2), run_expr_occurs(R,T,E),
    run_expr(R,T,E1,C1), run_expr(R,T,E2,C2),
    C2 != 0, C = (C1 #div C2), int(C).

run_expr(R,T,E,C) :-
    E=mod(E1,E2), run_expr_occurs(R,T,E),
    run_expr(R,T,E1,C1), run_expr(R,T,E2,C2),
    C2 != 0, C = (C1 #mod C2), int(C).

% Evaluate boolean expressions.
run_bool(R,T,B) :-
    B=lt(E1,E2), run_bool_occurs(R,T,B),
    run_expr(R,T,E1,C1),
    run_expr(R,T,E2,C2),
    C1 < C2.

run_bool(R,T,B) :-
    B=le(E1,E2), run_bool_occurs(R,T,B),
    run_expr(R,T,E1,C1),
    run_expr(R,T,E2,C2),
    C1 <= C2.

% Which arithmetic and boolean expressions occur in the program
% at given times and in given runs?
run_expr_occurs(R, T, E) :- run_line(R, T, L), line_expr_occurs(L, E).
run_bool_occurs(R, T, E) :- run_line(R, T, L), line_bool_occurs(L, E).
