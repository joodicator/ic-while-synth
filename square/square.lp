#const time_limit=100.
con(-127..128).

var(x). var(y). var(s). var(d).

line( 1, set(x, -5)).               % x = -5;
line( 2, set(y, 0)).                % y = 0;
line( 3, jif(lt(x, 0), 4, 6)).      % if (x < 0) {
line( 4, set(s, -1)).               %     s = -1;
line( 5, jmp(7)).                   % } else {
line( 6, set(s, 1)).                %     s = 1; }
line( 7, set(d, 1)).                % d = 1;
line( 8, jif(eq(x, 0), 13, 9)).     % while (x != 0) {
line( 9, set(x, sub(x, s))).        %   x = x - s;
line(10, set(y, add(y, d))).        %   y = y + d;
line(11, set(d, add(d, 2))).        %   d = d + 2
line(12, jmp(8)).                   % }
line(13, halt).

%-------------------------------------------------------------------------------
% Boolean guards: Variable {equals, less-than, less-or-equals} Constant.
true_at(T) :- line_at(T,N), line(N,jif(eq(X,C),_,_)), var_at(T,X,V), V = C.
true_at(T) :- line_at(T,N), line(N,jif(lt(X,C),_,_)), var_at(T,X,V), V < C.
true_at(T) :- line_at(T,N), line(N,jif(le(X,C),_,_)), var_at(T,X,V), V <= C.

%-------------------------------------------------------------------------------
% Arithmetic expressions: Con, Var, Var+Con, Var-Con, Var+Var, Var-Var.
expr_at(T, C) :-
    line_at(T,N), p(N,C), con(C).
expr_at(T, V) :-
    line_at(T,N), p(N,X), var_at(T,X,V).
expr_at(T, V+C) :-
    line_at(T,N), p(N,add(X,C)), var_at(T,X,V), con(C).
expr_at(T, V-C) :-
    line_at(T,N), p(N,sub(X,C)), var_at(T,X,V), con(C).
expr_at(T, V+W) :-
    line_at(T,N), p(N,add(X,Y)), var_at(T,X,V), var_at(T,Y,W).
expr_at(T, V-W) :-
    line_at(T,N), p(N,sub(X,Y)), var_at(T,X,V), var_at(T,Y,W).

p(N, X) :- line(N,set(_,X)).

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
    not stop_at(T), line_at(T, N), line(N, jif(_,JT,JF)), not true_at(T).

%-------------------------------------------------------------------------------
% Initialise variables to 0.
var_at(0, X, 0) :- var(X).

set_at(T, X) :- line_at(T, N), line(N, set(X, _)).

% When not setting, variables do not change.
var_at(T+1, X, V) :-
    not stop_at(T), var_at(T, X, V), not set_at(T, X), con(V).

% Variable assignment.
var_at(T+1, X, V) :-
    not stop_at(T), line_at(T, N), line(N, set(X, E)), expr_at(T, V), con(V).

%-------------------------------------------------------------------------------
halts :- stop_at(T), halt_at(T).

#hide.

#show timed_out.
timed_out :- not halts.

#show output/2.
output(X, V) :- stop_at(T), halt_at(T), var_at(T, X, V).
