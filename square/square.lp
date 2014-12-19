#const time_limit=100.
var(x). var(y). var(s). var(d).

line( 1, set(x, con(-5))).          % x = -5;
line( 2, set(y, con(0))).           % y = 0;
line( 3, jif(lt(x, 0), 4, 6)).      % if (x < 0) {
line( 4, set(s, con(-1))).          %     s = -1;
line( 5, jmp(7)).                   % } else {
line( 6, set(s, con(1))).           %     s = 1; }
line( 7, set(d, con(1))).           % d = 1;
line( 8, jif(eq(x, 0), 13, 9)).     % while (x != 0) {
line( 9, set(x, sub(x, s))).        %   x = x - s;
line(10, set(y, add(y, d))).        %   y = y + d;
line(11, set(d, add(d, con(2)))).   %   d = d + 2
line(12, jmp(8)).                   % }
line(13, halt).

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
halts :- stop_at(T), halt_at(T).

#hide.

#show timed_out.
timed_out :- not halts.

#show output/2.
output(X, V) :- stop_at(T), halt_at(T), var_at(T, X, V).
