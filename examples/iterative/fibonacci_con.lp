#const ceiling=10.

int_range(0, ceiling).
time_limit(30).

input_variable(n).
output_variable(f).
extra_variable(g).

constant(0; 1).
disallow(if; mul; div; rem).
while_max(1).

precondition("In_n > 0").
postcondition("fib(In_n, Out_f)").

fib(0, 0).
fib(1, 1).
fib(N, G+H) :-
    N=2..ceiling,
    fib(N-1,G), fib(N-2,H).

