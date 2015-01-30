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
1{ line_instr(L, I) : instr(I, L) }1 :- line(L).

% All test examples must halt with the correct value.
:- run_does_not_halt(R).
:- run_var_out(R,X,Actual), out(R,X,Expected), Actual != Expected.

%-------------------------------------------------------------------------------
% Instructions.
instr(set(V, E), L)            :- write_var(V), expr(E), line(L).
instr(if(B,1..X), L)           :- line(L), bool_neg(B), X=(line_max - L).
% instr(while(B,DeltaEnd))    :- bool_neg(B), line(DeltaEnd).
% instr(end_while(DeltaHead)) :- line(DeltaHead).

% This is not needed now that we constrain before generating
%% If statements may not end outside the program.
%:- line_instr_if(L,Len), line(L), line(Len),
%   L+Len>line_max.

% Keep if statements inside parent if statements.
:- line_instr_if(L1,Len1), line(L1), line(Len1),
   line_instr_if(L2,Len2), line(L2), line(Len2),
   L1 < L2, L2 <= L1+Len1, L2+Len2 >= L1+Len1.

line_instr_if(Line, Len) :- line_instr(Line, if(_, Len)).


%-------------------------------------------------------------------------------
% Arithmetic expressions. (Constants must be assigned to a variable to be used.)
%expr(sub(V1, V2)) :- var(V1), var(V2).
%expr(add(V1, V2)) :- var(V1), var(V2).
expr(var(V)) :- var(V).
%expr(con(con_min..con_max)).

% expr(mul(V1, V2)) :- var(V1), var(V2).
% expr(div(V1, V2)) :- var(V1), var(V2).

%-------------------------------------------------------------------------------
% Boolean expressions (with and without negated versions).
bool(lt(V1, V2)) :- var(V1), var(V2).
bool(eq(V1, V2)) :- var(V1), var(V2), V1 < V2. %This ensures V1 <_lex V2 (as V1 == V2 is eqiv to V2 == V1)

bool_neg(B)      :- bool(B).
bool_neg(neg(B)) :- bool(B).

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

% Move to the next line, given that a boolean guard is true.
run_line(R,T+1,L+1) :-
    run(R,T), run_line(R,T,L), line_instr_if_cond(L,B), run_bool(R,T,B).

line_instr_if_cond(L, B) :- line_instr(L, if(B, _)).

run_line(R,T+1,L+1) :-
    run(R,T), run_line(R,T,L), line_instr(L,while(B,_)), run_bool(R,T,B).

% Jump by a certain distance, given that a boolean guard is false.
run_line(R,T+1,L+BodyLength+1) :-
    run(R,T), run_line(R,T,L), line_instr(L,if(B,BodyLength)),
    not run_bool(R,T,B).
run_line(R,T+1,L+DeltaEnd+1) :-
    run(R,T), run_line(R,T,L), line_instr(L,while(B,DeltaEnd)),
    not run_bool(R,T,B),
    line_instr(L+DeltaEnd,end_while(DeltaEnd)). % Partially verify integrity.

% Automatically jump by a certain distance.
run_line(R,T+1,L-DeltaHead) :-
    run(R,T), run_line(R,T,L), line_instr(L,end_while(DeltaHead)),
    line_instr(L_head, while(_,_)).


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

% run_expr(R,T,mul(V1,V2),C) :-
%     run_var(R,T,V1,C1), run_var(R,T,V2,C2),
%     C = (C1 * C2) #mod con_bound, con(C).

% run_expr(R,T,div(V1,V2),C) :-
%     run_var(R,T,V1,C1), run_var(R,T,V2,C2),
%     C2 != 0, C = (C1 #div C2), con(C).
% run_expr(R,T,div(V1,V2),0) :-
%     run_var(R,T,V1,C1), run_var(R,T,V2,C2),
%     C2 == 0.

% Evaluate boolean expressions.
run_bool(R,T,lt(V1,V2)) :-
    run_var(R,T,V1,C1), run_var(R,T,V2,C2), C1<C2.
run_bool(R,T,eq(V1,V2)) :-
    run_var(R,T,V1,C1), run_var(R,T,V2,C2), C1==C2.

run_bool(R,T,neg(lt(V1,V2))) :-
    run_var(R,T,V1,C1), run_var(R,T,V2,C2), C1>=C2.
run_bool(R,T,neg(eq(V1,V2))) :-
    run_var(R,T,V1,C1), run_var(R,T,V2,C2), C1!=C2.
