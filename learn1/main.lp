#const time_limit=100.

var(x).
con(-15..16).
line_num(1..4).

% Exactly one instruction per line_num.
1{ line(N, I) : instr(I) }1 :- line_num(N).

instr(set(X, con(C))) :- var(X), con(C).
instr(halt).

halts :- stop_at(T), halt_at(T).
timed_out :- not halts.

output(X, V) :- stop_at(T), halt_at(T), var_at(T, X, V).

:- output(x, V), V != 3.

#hide.
#show line/2.
#show output/2.

% [jjc311@line01:~/sync/IndividualProject_C301/code/learn1]$ clingo main.lp
% Answer: 1
% line(4,halt) line(3,halt) line(2,halt) line(1,set(x,con(3))) output(x,3)
% SATISFIABLE
% 
% Models      : 1+
% Time        : 0.020
%   Prepare   : 0.010
%   Prepro.   : 0.010
%   Solving   : 0.000

%-------------------------------------------------------------------------------
% Boolean guards: Variable {equals, less-than, less-or-equals} Constant.
true_at(T) :- line_at(T,N), line(N,jif(eq(X,C),_,_)), var_at(T,X,V), V = C.
true_at(T) :- line_at(T,N), line(N,jif(lt(X,C),_,_)), var_at(T,X,V), V < C.
true_at(T) :- line_at(T,N), line(N,jif(le(X,C),_,_)), var_at(T,X,V), V <= C.

false_at(T) :- line_at(T,N), line(N,jif(eq(X,C),_,_)), var_at(T,X,V), V != C.
false_at(T) :- line_at(T,N), line(N,jif(lt(X,C),_,_)), var_at(T,X,V), V >= C.
false_at(T) :- line_at(T,N), line(N,jif(le(X,C),_,_)), var_at(T,X,V), V > C.

%-------------------------------------------------------------------------------
% Arithmetic expressions: Con, Var, Var+Con, Var-Con, Var+Var, Var-Var.
expr_at(T, C) :-
    line_at(T,N), rhs_at(N,con(C)).
expr_at(T, V) :-
    line_at(T,N), rhs_at(N,X), var_at(T,X,V).
expr_at(T, V+C) :-
    line_at(T,N), rhs_at(N,add(X,con(C))), var_at(T,X,V).
expr_at(T, V-C) :-
    line_at(T,N), rhs_at(N,sub(X,con(C))), var_at(T,X,V).
expr_at(T, V+W) :-
    line_at(T,N), rhs_at(N,add(X,Y)), var_at(T,X,V), var_at(T,Y,W).
expr_at(T, V-W) :-
    line_at(T,N), rhs_at(N,sub(X,Y)), var_at(T,X,V), var_at(T,Y,W).

rhs_at(N, X) :- line(N, set(_, X)).

%-------------------------------------------------------------------------------
% Stop execution when the program either halts or times out.
stop_at(time_limit).
stop_at(T) :- halt_at(T).

% Formally halt only upon reaching a 'halt' instruction.
halt_at(T) :- line_at(T, N), line(N, halt).

% Start at line 1.
line_at(0, 1).

jump_from(N) :- line(N, jmp(_)).
jump_from(N) :- line(N, jif(_,_,_)).

% When not jumping, step from line N to line N+1.
line_at(T+1, N+1) :-
    not stop_at(T), line_at(T, N), not jump_from(N).

% Unconditional jump.
line_at(T+1, J) :-
    not stop_at(T), line_at(T, N), line(N, jmp(J)).

% Conditional jump (true).
line_at(T+1, JT) :-
    not stop_at(T), line_at(T, N), line(N, jif(_,JT,JF)), true_at(T).

% Conditional jump (false)
line_at(T+1, JF) :-
    not stop_at(T), line_at(T, N), line(N, jif(_,JT,JF)), false_at(T).

%-------------------------------------------------------------------------------
% Initialise variables to 0.
var_at(0, X, 0) :- var(X).

not_set_at(T, X) :- line_at(T, N), line(N, set(Y, _)), Y != X, var(X).
not_set_at(T, X) :- line_at(T, N), line(N, jif(_, _, _)), var(X).
not_set_at(T, X) :- line_at(T, N), line(N, jmp(_)), var(X).
not_set_at(T, X) :- line_at(T, N), line(N, halt), var(X).

% When not setting, variables do not change.
var_at(T+1, X, V) :-
    not stop_at(T), var_at(T, X, V), not_set_at(T, X).

% Variable assignment.
var_at(T+1, X, V) :-
    not stop_at(T), line_at(T, N), line(N, set(X, E)), expr_at(T, V).

%-------------------------------------------------------------------------------
