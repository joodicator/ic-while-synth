int_range(-10, 10).
time_limit(40).

input_variable(n; r).
output_variable(b).

constant(0; 1).
disallow(rem; if).

max_while_loops(1).

precondition("In_n > 0").
postcondition("binom(In_n, In_r, Out_b)").

binom(N,R,B) :-
    B=Fact_N/(Fact_R*Fact_NmR),
    fact(N,Fact_N), fact(R,Fact_R), fact(N-R, Fact_NmR),
    binom_nonzero(N,R).
binom(N,R,0) :-
    N=-10..10, R=-10..10,
    not binom_nonzero(N,R).
binom_nonzero(N,R) :-
    N=0..10, R=0..N.

fact(0,1).
fact(N,F) :-
    F=N*FactNm1, fact(N-1,FactNm1),
    N=1..10.

echo_clingo.
