time_limit(20).
int_range(0,10).

input_variable(a; b; c).
read_only_variable(a; b; c).
output_variable(m).
extra_variable(x; y).
disallow_feature(while; arithmetic).

postcondition("median(In_a, In_b, In_c, Out_m)").

% Give some of the program:
preset_line_instr(1, set(x, var(a))).
preset_line_instr(2, if(lt(var(x), var(b)), 1)).
preset_line_instr(3, set(x, var(b))).

% Domain-specific predicates:
median(A,B,C,M) :-
    min(A,B,MinAB), min(B,C,MinBC), min(C,A,MinCA),
    max(MinAB,MinBC,MaxMin), max(MaxMin,MinCA,M).

min(X,Y,X) :- X <= Y, int(X; Y).
min(X,Y,Y) :- Y <= X, int(X; Y).

max(X,Y,X) :- X >= Y, int(X; Y).
max(X,Y,Y) :- Y >= X, int(X; Y).
