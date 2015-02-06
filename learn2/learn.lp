#const line_max=10.
#const time_max=line_max+1.

#const con_bound=8.
#const con_min=0.
#const con_max=con_bound.

#hide.
#show line_instr/2.
#show run_does_not_halt/1.

% #show run_var_out/3.
% #show line_set/2.
% #show run_expr/4.

%-------------------------------------------------------------------------------
% Variables.

var(a). var(b). var(c).
var(y). var(z).

write_var(y). write_var(z).

%-------------------------------------------------------------------------------
% Test examples.

 in(R,a,A) :- ex(R,A,_,_,_).
 in(R,b,B) :- ex(R,_,B,_,_).
 in(R,c,C) :- ex(R,_,_,C,_).
out(R,z,Z) :- ex(R,_,_,_,Z).

ex(r00, 0,0,0, 0). %ex(r01, 1,1,1, 1). ex(r02, 2,2,2, 2).

ex(r10, 7,1,1, 1). %ex(r11, 6,2,2, 2). ex(r12, 5,3,3, 3).
ex(r20, 1,7,1, 1). %ex(r21, 2,6,2, 2). ex(r22, 3,5,3, 3).
ex(r30, 1,1,7, 1). %ex(r31, 2,2,6, 2). ex(r32, 3,3,5, 3).

ex(r40, 1,7,7, 7). %ex(r41, 2,6,6, 6). ex(r42, 3,5,5, 5).
ex(r50, 7,1,7, 7). %ex(r51, 6,2,6, 6). ex(r52, 5,3,5, 5).
ex(r60, 7,7,1, 7). %ex(r61, 6,6,2, 6). ex(r62, 5,5,3, 5).

ex(r70, 1,2,3, 2). %ex(r71, 1,3,6, 3). ex(r72, 2,4,6, 4).
ex(r80, 1,3,2, 2). %ex(r81, 1,6,3, 3). ex(r82, 2,6,4, 4).
ex(r90, 2,1,3, 2). %ex(r91, 3,1,6, 3). ex(r92, 4,2,6, 4).
ex(rA0, 3,2,1, 2). %ex(rA1, 6,3,1, 3). ex(rA2, 6,4,2, 4).
ex(rB0, 2,3,1, 2). %ex(rB1, 3,6,1, 3). ex(rB2, 4,6,2, 4).
ex(rC0, 3,1,2, 2). %ex(rC1, 6,1,3, 3). ex(rC2, 6,2,4, 4).

%-------------------------------------------------------------------------------
% Constants, line numbers.
con(con_min..con_max).
line(1..line_max).

%-------------------------------------------------------------------------------
% Exactly one instruction per line.
1{ line_instr(L, I) : valid_line_instr(L, I) }1 :- line(L).

line_instr(1, set(y, var(a))).
line_instr(2, if(lt(y, b), 1)).
line_instr(3, set(y, var(b))).

% All test examples must halt with the correct value.
:- run_does_not_halt(R).
:- run_var_out(R,X,Actual), out(R,X,Expected), Actual != Expected.

%-------------------------------------------------------------------------------
% Instructions.
valid_line_instr(L, set(V, E))      :- line(L), write_var(V), expr(E).
valid_line_instr(L, if(B, 1..M))    :- line(L), bool(B), M=line_max-L.
valid_line_instr(L, while(B, 1..M)) :- line(L), bool(B), M=line_max-L.

% Forbid blocks that end outside of a block they start within.
:- block(L,M), block(PL,PM), PL<L, L<=PL+PM, L+M>PL+PM.

% block(L, M) if a code block starts after line L, and ends after line L+M.
block(L, M) :- line_instr(L,    if(_, M)).
%block(L, M) :- line_instr(L, while(_, M)).

%-------------------------------------------------------------------------------
% Arithmetic expressions. (Constants must be assigned to a variable to be used.)

% expr(con(con_min..con_max)).
expr(var(V)) :- var(V).
% expr(sub(V1, V2)) :- var(V1), var(V2), V1 != V2.
% expr(add(V1, V2)) :- var(V1), var(V2), V1 <= V2.
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

bool(lt(V1, V2)) :- var(V1), var(V2).
bool(le(V1, V2)) :- var(V1), var(V2).

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
run_var(R,0,X,C) :- in(R,X,C), con(C).
run_var(R,0,X,0) :- run(R), var(X), not run_var_in(R,X).

% On a set statement, update variables; otherwise, leave them constant.
run_var(R,T+1,X,C) :-
    run_line(R,T,L), not line_set(L,X), run_var(R,T,X,C), con(C).
run_var(R,T+1,X,C) :-
    run_line(R,T,L), line_set(L,X), line_instr(L,set(X,E)), run_expr(R,T,E,C), con(C).

line_set(L,X) :- line_instr(L, set(X,_)).

% Start at line 1, and, by default, move to the next line at each step.
run_line(R,0,1)     :- run(R).
run_line(R,T+1,L+1) :- L<=line_max, run(R,T), run_line(R,T,L), not line_jump(L).

% Run if statements.
run_line(R,T+1,L+1) :-
    run(R,T), run_line(R,T,L), line_instr_if(L,B), run_bool(R,T,B).
run_line(R,T+1,L+BodyLength+1) :-
    run(R,T), run_line(R,T,L), line_instr(L,if(B,BodyLength)),
    not run_bool(R,T,B).

% Run while loops.
run_line(R,T+1,L+1) :-
    run(R,T), run_line(R,T,L), line_instr_while(L,B), run_bool(R,T,B).
run_line(R,T+1,L+BodyLength+1) :-
    run(R,T), run_line(R,T,L), line_instr(L,while(B,BodyLength)),
    not run_bool(R,T,B).
run_line(R,T+1,HeadLine) :-
    run(R,T), run_line(R,T,L),
    line_instr_while(HeadLine, BodyLength),
    L = HeadLine + BodyLength.

line_instr_if(L, B)    :- line_instr(L, if(B, _)).
line_instr_while(L, B) :- line_instr(L, while(B, _)).

line_jump(L) :- line_instr(L, if(_,_)).
line_jump(L) :- line_instr(L, else(_)).
line_jump(L) :- line_instr(L, while(_,_)).
line_jump(L) :- line_instr(L, end_while(_)).

% Evaluate arithmetic expressions.
run_expr(R,T,con(C),C) :-
    run(R,T), con(C).
run_expr(R,T,var(V),C) :-
    run_var(R,T,V,C), con(C).
run_expr(R,T,add(V1,V2),C) :-
    run_var(R,T,V1,C1), run_var(R,T,V2,C2),
    C = (C1 + C2) #mod con_bound, con(C).
run_expr(R,T,sub(V1,V2),C) :-
    run_var(R,T,V1,C1), run_var(R,T,V2,C2),
    C = (C1 - C2 + con_bound) #mod con_bound, con(C).

%run_expr(R,T,mul(V1,V2),C) :-
%    run_var(R,T,V1,C1), run_var(R,T,V2,C2),
%    C = (C1 * C2) #mod con_bound, con(C).

%run_expr(R,T,div(V1,V2),C) :-
%    run_var(R,T,V1,C1), run_var(R,T,V2,C2),
%    C2 != 0, C = (C1 #div C2), con(C).
%run_expr(R,T,div(V1,V2),0) :-
%    run_var(R,T,V1,C1), run_var(R,T,V2,C2),
%    C2 == 0.

% Evaluate boolean expressions.
run_bool(R,T,lt(V1,V2)) :-
    run_var(R,T,V1,C1), run_var(R,T,V2,C2), C1 < C2.
run_bool(R,T,le(V1,V2)) :-
    run_var(R,T,V1,C1), run_var(R,T,V2,C2), C1 <= C2.

% run_bool(R,T,neg(lt(V1,V2))) :-
%     run_var(R,T,V1,C1), run_var(R,T,V2,C2), C1>=C2.
% run_bool(R,T,neg(eq(V1,V2))) :-
%     run_var(R,T,V1,C1), run_var(R,T,V2,C2), C1!=C2.
