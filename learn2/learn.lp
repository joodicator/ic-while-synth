#const line_max=5.
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

var(a). var(b). var(c). var(z).

read_only(a). read_only(b). read_only(c).

%-------------------------------------------------------------------------------
% Test examples.

in(r1,a,1). in(r1,b,1). in(r1,c,1). out(r1,z,1).
in(r2,a,2). in(r2,b,2). in(r2,c,2). out(r2,z,2).
in(r3,a,3). in(r3,b,3). in(r3,c,3). out(r3,z,3).

in(r4,a,2). in(r4,b,1). in(r4,c,1). out(r4,z,2).
in(r5,a,2). in(r5,b,3). in(r5,c,2). out(r5,z,3).
in(r6,a,3). in(r6,b,4). in(r6,c,3). out(r6,z,4).

in(r7,a,2). in(r7,b,2). in(r7,c,1). out(r7,z,2).
in(r8,a,3). in(r8,b,2). in(r8,c,3). out(r8,z,3).
in(r9,a,3). in(r9,b,4). in(r9,c,4). out(r9,z,4).

in(rA,a,3). in(rA,b,2). in(rA,c,1). out(rA,z,3).
in(rB,a,4). in(rB,b,2). in(rB,c,3). out(rB,z,4).
in(rC,a,3). in(rC,b,5). in(rC,c,4). out(rC,z,5).

in(rD,a,2). in(rD,b,3). in(rD,c,1). out(rD,z,3).
in(rE,a,3). in(rE,b,2). in(rE,c,4). out(rE,z,4).
in(rF,a,3). in(rF,b,4). in(rF,c,5). out(rF,z,5).

%-------------------------------------------------------------------------------
% Constants, line numbers.
con(con_min..con_max).
line(1..line_max).

%-------------------------------------------------------------------------------
% Exactly one instruction per line.
1{ line_instr(L, I) : instr(I) }1 :- line(L).

% All test examples must halt with the correct value.
:- run_does_not_halt(R).
:- run_var_out(R,X,Actual), out(R,X,Expected), Actual != Expected.

%-------------------------------------------------------------------------------
% Instructions.
instr(set(V, E))            :- var(V), not read_only(V), expr(E).
instr(if(B,BodyLength))     :- bool(B), line(BodyLength).
% instr(while(B,DeltaEnd))    :- bool_neg(B), line(DeltaEnd).
% instr(end_while(DeltaHead)) :- line(DeltaHead).

% If statements may not end outside the program.
:- line_instr(L,if(_,Len)), line(L), line(Len),
   L+Len>line_max.

% Nested if statements may not end outside a parent.
:- line_instr(L1,if(_,Len1)), line(L1), line(Len1),
   line_instr(L2,if(_,Len2)), line(L2), line(Len2),
   L1 < L2, L2 <= L1+Len1, L2+Len2 >= L1+Len1.

%-------------------------------------------------------------------------------
% Arithmetic expressions. (Constants must be assigned to a variable to be used.)
expr(sub(V1, V2)) :- var(V1), var(V2).
expr(add(V1, V2)) :- var(V1), var(V2).
expr(var(V)) :- var(V).
expr(con(con_min..con_max)).

% expr(mul(V1, V2)) :- var(V1), var(V2).
% expr(div(V1, V2)) :- var(V1), var(V2).

%-------------------------------------------------------------------------------
% Boolean expressions (with and without negated versions).
bool(lt(V1, V2)) :- var(V1), var(V2).
bool(eq(V1, V2)) :- var(V1), var(V2).

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
    run(R,T), run_line(R,T,L), line_instr(L,if(B,_)), run_bool(R,T,B).
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
