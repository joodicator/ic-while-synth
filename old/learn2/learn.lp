#const line_max=4.
#const time_max=30.
#const con_max=1.

#const int_bound=11.
#const int_min=0.
#const int_max=int_bound.

#hide.
#show line_instr/2.
#show run_does_not_halt/1.

% #show run_var_out/3.
% #show line_set/2.
% #show run_line/3.

%-------------------------------------------------------------------------------
% Variables.

var(x). % input
var(z). % output: z = x(x+1)/2.

write_var(x).
write_var(z).

%-------------------------------------------------------------------------------
% Constants.

con(0).
con(1).

% 1{ con(N,int_min..int_max) }1 :- N=1..con_max.
%
% con(C) :- con(1, C).
%
% % Require constants to occur in strictly ascending order.
% :- con(N1,C1), con(N2,C2), N1 < N2, C1 >= C2.

%-------------------------------------------------------------------------------
% Test examples.

 in(R,x,X) :- ex(R,X,_).
out(R,z,Z) :- ex(R,_,Z).

ex(r0, 0, 0).
ex(r1, 1, 1).
ex(r2, 2, 3).
ex(r3, 3, 6).
ex(r4, 4, 10).

%-------------------------------------------------------------------------------
% Exactly one instruction per line.
1{ line_instr(L, I) : valid_line_instr(L, I) }1 :- line(L).

%preset_line_instr(2, while(lt(v0, x), line_max-3)).
%preset_line_instr(3, set(z, add(z,x))).
%preset_line_instr(4, set(x, sub(x,v1))).
%preset_line_instr(line_max, end_while).

valid_line_instr(L,I) :- preset_line_instr(L,I).
line_instr(L,I) :- preset_line_instr(L,I).

% All test examples must halt with the correct value.
:- run_does_not_halt(R).
:- run_var_out(R,X,Actual), out(R,X,Expected), Actual != Expected.

%------------------------------------------------------------------------------- 
% Line numbers and integer bounds.

int(int_min..int_max).
line(1..line_max).

%-------------------------------------------------------------------------------
% Instructions.
valid_line_instr(L, set(V, E))      :- line(L), write_var(V), expr(E).
valid_line_instr(L, if(B, 1..M))    :- line(L), bool(B), M=line_max-L.
valid_line_instr(L, while(B, 1..M)) :- line(L), bool(B), M=line_max-L-1.
valid_line_instr(L, end_while)      :- line(L).

% Require end_while statements to occur at exactly the correct positions.
:- line_instr(L,end_while), not end_while_block(L).
:- end_while_block(L), not line_instr(L,end_while).
end_while_block(L) :- line_instr_while_length(LH,B), L=LH+B+1.

% Forbid if/while blocks that end outside of a block they start within.
:- line_instr_if_length(L,M), block(PL,PM), PL<L, L<=PL+PM, L+M>PL+PM.
:- line_instr_while_length(L,M), block(PL,PM), PL<L, L<=PL+PM, L+M+1>PL+PM.

% block(L, M) if a code block starts after line L, and ends after line L+M.
block(L, M) :- line_instr(L,    if(_, M)).
block(L, M) :- line_instr(L, while(_, M)).

% Projection of if/while parameters.
line_instr_if_guard(L, G)     :- line_instr(L, if(G, _)).
line_instr_if_length(L, B)    :- line_instr(L, if(_, B)).
line_instr_while_guard(L, G)  :- line_instr(L, while(G, _)).
line_instr_while_length(L, B) :- line_instr(L, while(_, B)).

%-------------------------------------------------------------------------------
% Arithmetic expressions. (Constants must be assigned to a variable to be used.)

expr(con(C)) :- con(C).
expr(var(X)) :- var(X).

expr(add(var(X), con(C))) :- var(X), con(C), C != 0.
expr(add(var(X), var(Y))) :- var(X), var(Y), X <= Y.

expr(sub(var(X), con(C))) :- var(X), con(C), C != 0.
expr(sub(con(C), var(X))) :- var(X), con(C).
expr(sub(var(X), var(Y))) :- var(X), var(Y), X != Y.

% expr(mul(V1, V2)) :- var(V1), var(V2), V1 <= V2.
% expr(div(V1, V2)) :- var(V1), var(V2), V1 != V2.

%-------------------------------------------------------------------------------
% Boolean expressions. Note that:
%   x <  y  <=>  lt(x,y)
%   x >  y  <=>  lt(y,x)
%   x <= y  <=>  le(x,y)
%   x >= y  <=>  le(y,x)
%   x == y  <=>  le(x,y) AND le(y,x)
%   x != y  <=>  lt(x,y) AND lt(y,x)

bool(lt(var(X), var(Y))) :- var(X), var(Y).
bool(lt(var(X), con(C))) :- var(X), con(C).
bool(lt(con(C), var(X))) :- var(X), con(C).

bool(le(var(X), var(Y))) :- var(X), var(Y).
bool(le(var(X), con(C))) :- var(X), con(C).
bool(le(con(C), var(X))) :- var(X), con(C).

%-------------------------------------------------------------------------------
% Program interpreter.

% run_any if any test cases were specified.
run_any :- in(_,_,_).
run_any :- out(_,_,_).

% run(R) if there is a test case called R.
run(0) :- not run_any.
run(R) :- in(R,_,_).
run(R) :- out(R,_,_).

% run(R,T) if test case R runs to at least time T.
run(R,T) :- run_line(R,T,L), T<time_max, L<=line_max.

% run_halt(R,T) if test case R halts normally at time T.
run_halt(R,T) :- run_line(R,T,L), T<time_max, L>line_max.

run_does_halt(R) :- run_halt(R,_).
run_does_not_halt(R) :- run(R), not run_does_halt(R).

% run_var_out(R,X,C) if the final value of X in run R is C.
run_var_out(R,X,C) :- run_var(R,T,X,C), not run(R,T).

% Initialise variables to their input value or, by default, 0.
run_var_in(R,X)  :- in(R,X,_).
run_var(R,0,X,C) :- in(R,X,C), int(C).
run_var(R,0,X,0) :- run(R), var(X), not run_var_in(R,X).

% On a set statement, update variables; otherwise, leave them constant.
run_var(R,T+1,X,C) :-
    run_line(R,T,L), not line_set(L,X), run_var(R,T,X,C), int(C).
run_var(R,T+1,X,C) :-
    run_line(R,T,L), line_set(L,X), line_instr(L,set(X,E)), run_expr(R,T,E,C), int(C).

line_set(L,X) :- line_instr(L, set(X,_)).

% Start at line 1, and, by default, move to the next line at each step.
run_line(R,0,1)     :- run(R).
run_line(R,T+1,L+1) :- L<=line_max, run(R,T), run_line(R,T,L), not line_jump(L).

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
run_leaf_expr(R,T,con(C),C) :-
    run(R,T), int(C).
run_leaf_expr(R,T,var(V),C) :-
    run_var(R,T,V,C), int(C).

run_expr(R,T,E,C) :-
    run_leaf_expr(R,T,E,C).

run_expr(R,T,add(E1,E2),C) :-
    run_leaf_expr(R,T,E1,C1), run_leaf_expr(R,T,E2,C2),
    C = (C1 + C2) #mod int_bound, int(C).

run_expr(R,T,sub(E1,E2),C) :-
    run_leaf_expr(R,T,E1,C1), run_leaf_expr(R,T,E2,C2),
    C = (C1 - C2 + int_bound) #mod int_bound, int(C).

run_expr(R,T,mul(E1,E2),C) :-
    run_leaf_expr(R,T,E1,C1), run_leaf_expr(R,T,E2,C2),
    C = (C1 * C2) #mod int_bound, int(C).

run_expr(R,T,div(E1,E2),C) :-
    run_leaf_expr(R,T,E1,C1), run_leaf_expr(R,T,E2,C2),
    C2 != 0, C = (C1 #div C2), int(C).

% Evaluate boolean expressions.
run_bool(R,T,lt(E1,E2)) :-
    run_leaf_expr(R,T,E1,C1),
    run_leaf_expr(R,T,E2,C2),
    C1 < C2.
run_bool(R,T,le(E1,E2)) :-
    run_leaf_expr(R,T,E1,C1),
    run_leaf_expr(R,T,E2,C2),
    C1 <= C2.

% run_bool(R,T,neg(lt(V1,V2))) :-
%     run_var(R,T,V1,C1), run_var(R,T,V2,C2), C1>=C2.
% run_bool(R,T,neg(eq(V1,V2))) :-
%     run_var(R,T,V1,C1), run_var(R,T,V2,C2), C1!=C2.
