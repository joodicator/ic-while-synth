% The following constants MUST be defined in file before this one:
% #const line_max=LM.   % The maximum number of lines in the program.
% #const time_max=TM.   % Maximum time steps to simulate before failure.
% #const int_min=IMN.   % Minimum integer allowed to exist.
% #const int_max=IMX.   % Maximum integer allowed to exist.

% In specifying a program to be learned, the following facts MAY be given:
% preset_line_instr(L,I) % At line L, the instruction I is forced to occur.
% con(N).       % N is a constant that may occur as a literal in the code.
% var(V).       % V is a variable thay may be used in the program.
% write_var(V). % The variable V may be written to in the program.
% disallow(F).  % Feature F may not be used (see feature/1).
% in(R,V,C).    % In run R, the variable V starts with the value C.
% out(R,V,C).   % In run R, the variable V must finish with the value C.

#hide.
#show line_instr/2.

%-------------------------------------------------------------------------------
% Silence certain warnings for undefined predicates:
in(never,never,never)           :- out(never,never,never).
out(never,never,never)          :- preset_line_instr(never,never).
preset_line_instr(never,never)  :- in(never,never,never).

%-------------------------------------------------------------------------------
% Features allowed in the program.
subfeature(all,        if; while; arithmetic).
subfeature(arithmetic, add; sub; mul; div; mod).

feature(F) :- subfeature(F,_).
feature(G) :- subfeature(_,G).

disallow(G) :- subfeature(F,G), disallow(F).
allow(F)    :- feature(F), not disallow(F).

%-------------------------------------------------------------------------------
% Exactly one instruction per line.
1{ line_instr(L, I) : valid_line_instr(L, I) }1 :- line(L).
line(1..line_max).

% All test examples must halt with the correct value.
:- run_does_not_halt(R).
:- run_var_out(R,V,Actual), out(R,V,Expected), Actual != Expected.

%------------------------------------------------------------------------------- 
% Line numbers and integer bounds.

int(int_min..int_max).

%-------------------------------------------------------------------------------
% Instructions.
valid_line_instr(L, set(V, E)) :-
    line(L), write_var(V), expr(E).
valid_line_instr(L, if(B, 1..M)) :-
    line(L), allow(if), bool(B), M=line_max-L.
valid_line_instr(L, while(B, 1..M)) :-
    line(L), allow(while), bool(B), M=line_max-1.
valid_line_instr(L, end_while) :-
    line(L), allow(while).

valid_line_instr(L,I) :- preset_line_instr(L,I).
line_instr(L,I)       :- preset_line_instr(L,I).

% Which arithmetic and boolean expressions occur in the program
% at given times and in given runs?
line_expr_occurs(L, E) :- preset_line_instr(L, set(_, E)).
line_expr_occurs(L, E) :- line_binop_occurs(L, E, _).
line_expr_occurs(L, E) :- line_binop_occurs(L, _, E).

line_bool_occurs(L, B) :- preset_line_instr(L, if(B, _)).
line_bool_occurs(L, B) :- preset_line_instr(L, while(B, _)).

line_binop_occurs(L, A, B) :- line_expr_occurs(L, add(A,B)).
line_binop_occurs(L, A, B) :- line_expr_occurs(L, sub(A,B)).
line_binop_occurs(L, A, B) :- line_expr_occurs(L, mul(A,B)).
line_binop_occurs(L, A, B) :- line_expr_occurs(L, div(A,B)).
line_binop_occurs(L, A, B) :- line_expr_occurs(L, mod(A,B)).
line_binop_occurs(L, A, B) :- line_bool_occurs(L, lt(A,B)).
line_binop_occurs(L, A, B) :- line_bool_occurs(L, le(A,B)).

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
% Arithmetic expressions.

expr(E) :- line_expr_occurs(_, E).

expr(con(C)) :- con(C).
expr(var(X)) :- var(X).

% add: commutative; disallow x+0; allow x+x iff not 2*x, x+(-|c|) iff not x-|c|.
expr(add(var(X), con(C))) :- allow(add), var(X), con(C), C > 0.
expr(add(var(X), con(C))) :- allow(add), var(X), con(C), C < 0, not allow(sub).
expr(add(var(X), con(C))) :- allow(add), var(X), con(C), C < 0, not con(-C).
expr(add(var(X), var(Y))) :- allow(add), var(X), var(Y), X < Y.
expr(add(var(X), var(X))) :- allow(add), var(X), not allow(mul).
expr(add(var(X), var(X))) :- allow(add), var(X), not con(2).

% sub: disallow x-0, x-x; allow x-(-|c|) iff not x+|c|, 0-x iff not -1*x.
expr(sub(var(X), var(Y))) :- allow(sub), var(X), var(Y), X != Y.
expr(sub(var(X), con(C))) :- allow(sub), var(X), con(C), C > 0.
expr(sub(var(X), con(C))) :- allow(sub), var(X), con(C), C < 0, not allow(add).
expr(sub(var(X), con(C))) :- allow(sub), var(X), con(C), C < 0, not con(-C).
expr(sub(con(C), var(X))) :- allow(sub), var(X), con(C), C != 0.
expr(sub(con(0), var(X))) :- allow(sub), var(X), not allow(mul).
expr(sub(con(0), var(X))) :- allow(sub), var(X), not con(-1).

% mul: commutative; disallow 0*x, 1*x.
expr(mul(var(X), var(Y))) :- allow(mul), var(X), var(Y), X <= Y.
expr(mul(con(C), var(X))) :- allow(mul), var(X), con(C), C != 0, C != 1.

% div: disallow x/x; disallow c/x and x/c for all c < 2.
expr(div(var(X), var(Y))) :- allow(div), var(X), var(Y), X != Y.
expr(div(con(C), var(X))) :- allow(div), con(C), var(X), C > 1.
expr(div(var(X), con(C))) :- allow(div), var(X), con(C), C > 1.

% mod: disallow x%x, disallow c%x for all c < 2, x%c for all c < 1.
expr(mod(var(X), var(Y))) :- allow(mod), var(X), var(Y), x != y.
expr(mod(con(C), var(X))) :- allow(mod), var(X), con(C), c > 1.
expr(mod(var(X), con(C))) :- allow(mod), var(X), con(C), c > 0.

%-------------------------------------------------------------------------------
% Boolean expressions. Note that:
%   x <  y  <=>  lt(x,y)
%   x >  y  <=>  lt(y,x)
%   x <= y  <=>  le(x,y)
%   x >= y  <=>  le(y,x)
%   x == y  <=>  le(x,y) AND le(y,x)
%   x != y  <=>  lt(x,y) OR  lt(y,x)

bool(B) :- line_bool_occurs(_, B).

bool(lt(var(X), var(Y))) :- var(X), var(Y), X != Y.
bool(lt(var(X), con(C))) :- var(X), con(C).
bool(lt(con(C), var(X))) :- var(X), con(C).

bool(le(var(X), var(Y))) :- var(X), var(Y), X != Y.
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
run(R,T) :- run_line(R,T,L), T<time_max, line(L).

% run_halt(R,T) if test case R halts normally at time T.
run_halt(R,T) :- run_line(R,T,L), T<time_max, not line(L).

run_does_halt(R) :- run_halt(R,_).
run_does_not_halt(R) :- run(R), not run_does_halt(R).

% run_var_out(R,X,C) if the final value of X in run R is C.
run_var_out(R,X,C)     :- run_var(R,T,X,C), not run(R,T).
run_var_out(R,X,unset) :- run(R), var(X), not run_var_out_set(R,X).
run_var_out_set(R,X) :- run_var_out(R,X,C), C != unset.

% Initialise variables to their input value, if any.
run_var_in(R,X)  :- in(R,X,_).
run_var(R,0,X,C) :- in(R,X,C), int(C).

% On a set statement, update variables; otherwise, leave them constant.
run_var(R,T+1,X,C) :-
    run_line(R,T,L), not line_set(L,X), run_var(R,T,X,C), int(C).
run_var(R,T+1,X,C) :-
    run_line(R,T,L), line_set(L,X), line_instr(L,set(X,E)), run_expr(R,T,E,C), int(C).
run_fail(R,T) :-
    run_line(R,T,L), line_set(L,X), line_instr(L,set(X,E)),
    not run_expr_not_fail(R,T,E).
run_expr_not_fail(R,T,E) :- run_expr(R,T,E,_).

line_set(L,X) :- line_instr(L, set(X,_)).

% Start at line 1, and, by default, move to the next line at each step.
run_line(R,0,1) :-
    run(R).
run_line(R,T+1,L+1) :-
    line(L), run(R,T), run_line(R,T,L),
    not line_jump(L), not run_fail(R,T).

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
run_leaf_expr(R,T,E,C) :-
    E=con(C), int(C), expr(E), run(R,T).
run_leaf_expr(R,T,E,C) :-
    E=var(V), var(V), expr(E), run_var(R,T,V,C).

run_expr(R,T,E,C) :-
    run_leaf_expr(R,T,E,C).

run_expr(R,T,E,C) :-
    E=add(E1,E2), expr(E),
    run_leaf_expr(R,T,E1,C1), run_leaf_expr(R,T,E2,C2),
    C = C1 + C2, int(C).

run_expr(R,T,E,C) :-
    E=sub(E1,E2), expr(E),
    run_leaf_expr(R,T,E1,C1), run_leaf_expr(R,T,E2,C2),
    C = C1 - C2, int(C).

run_expr(R,T,E,C) :-
    E=mul(E1,E2), expr(E),
    run_leaf_expr(R,T,E1,C1), run_leaf_expr(R,T,E2,C2),
    C = C1 * C2, int(C).

run_expr(R,T,E,C) :-
    E=div(E1,E2), expr(E),
    run_leaf_expr(R,T,E1,C1), run_leaf_expr(R,T,E2,C2),
    C2 != 0, C = (C1 #div C2), int(C).

run_expr(R,T,E,C) :-
    E=mod(E1,E2), expr(E),
    run_leaf_expr(R,T,E1,C1), run_leaf_expr(R,T,E2,C2),
    C2 != 0, C = (C1 #mod C2), int(C).

% Evaluate boolean expressions.
run_bool(R,T,B) :-
    B=lt(E1,E2), bool(B),
    run_leaf_expr(R,T,E1,C1),
    run_leaf_expr(R,T,E2,C2),
    C1 < C2.

run_bool(R,T,B) :-
    B=le(E1,E2), bool(B),
    run_leaf_expr(R,T,E1,C1),
    run_leaf_expr(R,T,E2,C2),
    C1 <= C2.
