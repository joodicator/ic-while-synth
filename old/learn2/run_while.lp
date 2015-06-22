#const line_max=10.
#const time_max=50.
#const con_min=-31.
#const con_max=32.

#hide.
#show run_var_out/3.
#show run_does_not_halt/1.

%-------------------------------------------------------------------------------
% Variables and line numbers.
var(x). var(y). var(z0). var(z1).
line(1..line_max).

%-------------------------------------------------------------------------------
% Exactly one instruction per line.
%1{ line_instr(L, I) : instr(I) }1 :- line(L).

% As many trailing "skip" instructions as possible (weighted by line number).
%#maximize [ line_instr(L, skip)=L ].

line_instr(1, set(z0, con(0))).
line_instr(1, set(z1, con(1))).
line_instr(2, while(lt(z0, x), 4)).
line_instr(3,   set(x, sub(x, z1))).
line_instr(4,   set(y, add(y, z1))).
line_instr(5,   set(y, add(y, z1))).
line_instr(6, end_while(4)).

in(0,x,0). in(1,x,1). in(2,x,2). in(3,x,3). in(4,x,4).

%-------------------------------------------------------------------------------
% Instructions.
instr(skip).
instr(set(V, E))            :- var(V), expr(E).
instr(if(B,DeltaElse))      :- bool(B), line(DeltaElse).
instr(else(DeltaEnd))       :- line(DeltaEnd).
instr(end_if).
instr(while(B,DeltaEnd))    :- bool_neg(B), line(DeltaEnd).
instr(end_while(DeltaHead)) :- line(DeltaHead).

%-------------------------------------------------------------------------------
% Arithmetic expressions. (Constants must be assigned to a variable to be used.)
expr(con(con_min..con_max)).
expr(var(V)) :- var(V).
expr(add(V1, V2)) :- var(V1), var(V2).
expr(sub(V1, V2)) :- var(V1), var(V2).

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
run_var(R,0,X,C) :- in(R,X,C).
run_var(R,0,X,0) :- run(R), var(X), not run_var_in(R,X).

% On a set statement, update variables; otherwise, leave them constant.
run_var(R,T+1,X,C) :-
    run_line(R,T,L), not line_set(L,X), run_var(R,T,X,C).
run_var(R,T+1,X,C) :-
    run_line(R,T,L), line_set(L,X), line_instr(L,set(X,E)), run_expr(R,T,E,C).

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
run_line(R,T+1,L+DeltaElse+1) :-
    run(R,T), run_line(R,T,L), line_instr(L,if(B,DeltaElse)),
    not run_bool(R,T,B),
    line_instr(L+DeltaElse,else(_)). % Partially verify integrity.
run_line(R,T+1,L+DeltaEnd+1) :-
    run(R,T), run_line(R,T,L), line_instr(L,while(B,DeltaEnd)),
    not run_bool(R,T,B),
    line_instr(L+DeltaEnd,end_while(DeltaEnd)). % Partially verify integrity.

% Automatically jump by a certain distance.
run_line(R,T+1,L+DeltaEnd+1) :-
    run(R,T), run_line(R,T,L), line_instr(L,else(DeltaEnd)),
    line_instr(L_end,end_if).
run_line(R,T+1,L-DeltaHead) :-
    run(R,T), run_line(R,T,L), line_instr(L,end_while(DeltaHead)),
    line_instr(L_head, while(_,_)).

line_jump(L) :- line_instr(L, if(_,_)).
line_jump(L) :- line_instr(L, else(_)).
line_jump(L) :- line_instr(L, while(_,_)).
line_jump(L) :- line_instr(L, end_while(_)).

% Evaluate arithmetic expressions.
line_expr(L,E) :- line_instr(L, set(_, E)).
run_expr(R,T,con(C),C) :-
    run_line(R,T,L), line_expr(L,con(C)).
run_expr(R,T,var(V),C) :-
    run_line(R,T,L), line_expr(L,var(V)), run_var(R,T,V,C).
run_expr(R,T,add(V1,V2),C1+C2) :-
    run_line(R,T,L), line_expr(L,add(V1,V2)),
    run_var(R,T,V1,C1), run_var(R,T,V2,C2).
run_expr(R,T,sub(V1,V2),C1-C2) :-
    run_line(R,T,L), line_expr(L,sub(V1,V2)),
    run_var(R,T,V1,C1), run_var(R,T,V2,C2).

% Evaluate boolean expressions.
line_bool(L,B) :- line_instr(L, if(B,_)).
line_bool(L,B) :- line_instr(L, while(B,_)).
run_bool(R,T,lt(V1,V2)) :-
    run_line(R,T,L), line_bool(L,lt(V1,V2)),
    run_var(R,T,V1,C1), run_var(R,T,V2,C2), C1<C2.
run_bool(R,T,eq(V1,V2)) :-
    run_line(R,T,L), line_bool(L,eq(V1,V2)),
    run_var(R,T,V1,C1), run_var(R,T,V2,C2), C1==C2.
run_bool(R,T,neg(B)) :-
    run_line(R,T,L), line_bool(L,neg(B)),
    run(R,T), bool(B), not run_bool(R,T,B).
