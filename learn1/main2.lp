#const time_limit=100.

%-------------------------------------------------------------------------------
% Global limits.
var(x). var(y). var(z).
con(-15..16).
final_line(3).
line_num(1..M) :- final_line(M).

%-------------------------------------------------------------------------------
% Test examples:
%   in(ExampleID, Variable, InitialValue).
%   out(ExampleID, Variable, FinalValue).

in(1,x,1). in(1,y,1). out(1,x,1). out(1,y,1).
%in(2,x,1). in(2,y,2). out(2,x,2). out(2,y,1).
%in(3,x,1). in(3,y,3). out(3,x,3). out(3,y,1).

%in(4,x,2). in(4,y,1). out(4,x,1). out(4,y,2).
%in(5,x,2). in(5,y,2). out(5,x,2). out(5,y,2).
%in(6,x,2). in(6,y,3). out(6,x,3). out(6,y,2).

%in(7,x,3). in(7,y,1). out(7,x,1). out(7,y,3).
%in(8,x,3). in(8,y,2). out(8,x,2). out(8,y,3).
%in(9,x,3). in(9,y,3). out(9,x,3). out(9,y,3).

example(E) :- in(E,_,_).
example(E) :- out(E,_,_).

:- out(E, X, EV), halt_at(E, T), var_at(E, T, X, AV), EV != AV.
:- example(E), not halts(E).

%-------------------------------------------------------------------------------
% ASP Output
halts(E) :- halt_at(E, T).

#hide.
#show line/2.
#show var_at/4.
#show expr_at/3.

%-------------------------------------------------------------------------------
% Allow exactly one instruction per line_num.
1{ line(N, I) : instr(I) }1 :- line_num(N).

instr(set(X, con(C)))        :- var(X), con(C).
instr(set(X, Y))             :- var(X), var(Y).
instr(set(X, add(A,con(B)))) :- var(X), var(A), con(B).
instr(set(X, sub(A,con(B)))) :- var(X), var(A), con(B).
instr(set(X, add(A,B)))      :- var(X), var(A), var(B).
instr(set(X, sub(A,B)))      :- var(X), var(A), var(B).

instr(skip).

%-------------------------------------------------------------------------------
% Boolean guards: Variable {equals, less-than, less-or-equals} Constant.
true_at(E,T) :- line_at(E,T,N), line(N,jif(eq(X,C),_,_)), var_at(E,T,X,V), V = C.
true_at(E,T) :- line_at(E,T,N), line(N,jif(lt(X,C),_,_)), var_at(E,T,X,V), V < C.
true_at(E,T) :- line_at(E,T,N), line(N,jif(le(X,C),_,_)), var_at(E,T,X,V), V <= C.

false_at(E,T) :- line_at(E,T,N), line(N,jif(eq(X,C),_,_)), var_at(E,T,X,V), V != C.
false_at(E,T) :- line_at(E,T,N), line(N,jif(lt(X,C),_,_)), var_at(E,T,X,V), V >= C.
false_at(E,T) :- line_at(E,T,N), line(N,jif(le(X,C),_,_)), var_at(E,T,X,V), V > C.

%-------------------------------------------------------------------------------
% Arithmetic expressions: Con, Var, Var+Con, Var-Con, Var+Var, Var-Var.
expr_at(E, T, C) :-
    line_at(E,T,N), rhs_at(N,con(C)).
expr_at(E, T, V) :-
    line_at(E,T,N), rhs_at(N,X), var_at(E,T,X,V).
expr_at(E, T, V+C) :-
    line_at(E,T,N), rhs_at(N,add(X,con(C))), var_at(E,T,X,V).
expr_at(E, T, V-C) :-
    line_at(E, T,N), rhs_at(N,sub(X,con(C))), var_at(E,T,X,V).
expr_at(E, T, V+W) :-
    line_at(E,T,N), rhs_at(N,add(X,Y)), var_at(E,T,X,V), var_at(E,T,Y,W).
expr_at(E, T, V-W) :-
    line_at(E,T,N), rhs_at(N,sub(X,Y)), var_at(E,T,X,V), var_at(E,T,Y,W).

rhs_at(N, X) :- line(N, set(_, X)).

%-------------------------------------------------------------------------------
% Stop execution when the program either halts or times out.
stop_at(E,time_limit) :- example(E).
stop_at(E,T) :- halt_at(E,T).

% Formally halt only upon reaching the end of the program.
halt_at(E,T) :- line_at(E, T, N), final_line(N).

% Start at line 1.
line_at(E, 0, 1) :- example(E).

jump_from(N) :- line(N, jmp(_)).
jump_from(N) :- line(N, jif(_,_,_)).

% When not jumping, step from line N to line N+1.
line_at(E, T+1, N+1) :-
    not stop_at(E, T), line_at(E, T, N), not jump_from(N).

% Unconditional jump.
line_at(E, T+1, J) :-
    not stop_at(E, T), line_at(E, T, N), line(N, jmp(J)).

% Conditional jump (true).
line_at(E, T+1, JT) :-
    not stop_at(E, T), line_at(E, T, N), line(N, jif(_,JT,JF)), true_at(E, T).

% Conditional jump (false)
line_at(T+1, JF) :-
    not stop_at(E, T), line_at(E, T, N), line(N, jif(_,JT,JF)), false_at(E, T).

%-------------------------------------------------------------------------------
% Initialise variables to their given input value, or otherwise zero.
var_at(E, 0, X, V) :- in(E, X, V).
var_at(E, 0, X, 0) :- var(X), not var_has_input(E, X), example(E).
var_has_input(E, X) :- in(E, X, _).

not_set_at(E, T, X) :- line_at(E, T, N), line(N, set(Y, _)), Y != X, var(X).
not_set_at(E, T, X) :- line_at(E, T, N), line(N, jif(_, _, _)), var(X).
not_set_at(E, T, X) :- line_at(E, T, N), line(N, jmp(_)), var(X).
not_set_at(E, T, X) :- line_at(E, T, N), line(N, skip), var(X).

% When not setting, variables do not change.
var_at(E, T+1, X, V) :-
    not stop_at(E, T), var_at(E, T, X, V), not_set_at(E, T, X).

% Variable assignment.
var_at(E, T+1, X, V) :-
    not stop_at(E, T), line_at(E, T, N), line(N, set(X, E)), expr_at(E, T, V).

%-------------------------------------------------------------------------------
