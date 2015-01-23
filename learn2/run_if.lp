#const line_max=5.
#const time_max=20.
#const con_min=-15.
#const con_max=16.

#hide.
#show run_var_out/3.

%-------------------------------------------------------------------------------
% Exactly one instruction per line.
%1{ line_instr(L, I) : instr(I) }1 :- line(L).

% As many trailing "skip" instructions as possible (weighted by line number).
%#maximize [ line_instr(L, skip)=L ].

line_instr(1, if(lt(x, y), 3)).
line_instr(2, set(z, con(1))).
line_instr(3, else(5)).
line_instr(4, set(z, con(-1))).
line_instr(5, end_if).

in(1,x,1). in(2,x,-1). in(3,x,4).
in(1,y,2). in(2,y,1).  in(3,y,3).

%-------------------------------------------------------------------------------
% Variables and line numbers.
var(x). var(y). var(z).
line(1..line_max).

%-------------------------------------------------------------------------------
% Instructions.
instr(skip).
instr(set(V, E))         :- var(V), expr(E).
instr(if(B,L_else))      :- bool(B), line(L_else).
instr(else(L_end))       :- line(L_end).
instr(end_if).
instr(while(B,L_end))    :- bool_neg(B), line(L_end).
instr(end_while(L_head)) :- line(L_head).

%-------------------------------------------------------------------------------
% Arithmetic expressions. (Constants must be assigned to a variable to be used.)
expr(con(con_min..con_max)).
expr(var(V)) :- var(V).
expr(add(V1, V2)) :- var(V1), var(V2).
expr(sub(V1, V2)) :- var(V1), var(V2).

line_expr(L,E) :- line_instr(L, set(_, E)).

run_expr(R,T,con(C),C) :-
    run_line(R,T,L), line_expr(L,con(C)).
run_expr(R,T,var(V),C) :-
    run_line(R,T,L), line_expr(L,var(V)), run_var(R,T,V,C).
run_expr(R,T,add(V1,V2),C1+C2) :-
    run_line(R,T,L), line_expr(add(V1,V2)),
    run_var(R,T,V1,C1), run_var(R,T,V2,C2).
run_expr(R,T,sub(V1,V2),C1-C2) :-
    run_line(R,T,L), line_expr(sub(V1,V2)),
    run_var(R,T,V1,C1), run_var(R,T,V2,C2).

%-------------------------------------------------------------------------------
% Boolean expressions (with and without negated versions).
bool(lt(V1, V2)) :- var(V1), var(V2).
bool(eq(V1, V2)) :- var(V1), var(V2).

bool_neg(B)      :- bool(B).
bool_neg(neg(B)) :- bool(B).

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

% run_halt(R) if test case R halts normally.
run_halt(R) :- run_halt(R,_).

% run_var_out(R,X,C) if the final value of X in run R is C.
run_var_out(R,X,C) :- run_halt(R,T), run_var(R,T,X,C).

run_var_in(R,X) :- in(R,X,_).
run_var(R,0,X,C) :- in(R,X,C).
run_var(R,0,X,0) :- run(R), var(X), not run_var_in(R,X).

run_var(R,T+1,X,C) :-
    run_line(R,T,L), not line_set(L,X), run_var(R,T,X,C).
run_var(R,T+1,X,C) :-
    run_line(R,T,L), line_set(L,X), line_instr(L,set(X,E)), run_expr(R,T,E,C).

run_line(R,0,1)     :- run(R).
run_line(R,T+1,L+1) :- run(R,T), run_line(R,T,L), not line_jump(L).

run_line(R,T+1,L+1) :-
    run(R,T), run_line(R,T,L), line_instr(L,if(B,_)), run_bool(R,T,B).
run_line(R,T+1,L+1) :-
    run(R,T), run_line(R,T,L), line_instr(L,while(B,_)), run_bool(R,T,B).

run_line(R,T+1,L_else+1) :-
    run(R,T), run_line(R,T,L), line_instr(L,if(B,L_else)),
    not run_bool(R,T,B), line_instr(L_else,else(_)).
run_line(R,T+1,L_end) :-
    run(R,T), run_line(R,T,L), line_instr(L,else(L_end)).
run_line(R,T+1,L_head) :-
    run(R,T), run_line(R,T,L), line_instr(L,end_while(L_head)),
    line_instr(L_head, while(_,_)).

line_jump(L) :- line_instr(L, if(_,_)).
line_jump(L) :- line_instr(L, else(_)).
line_jump(L) :- line_instr(L, while(_,_)).
line_jump(L) :- line_instr(L, end_while(_)).

line_set(L,X) :- line_instr(L, set(X,_)).
