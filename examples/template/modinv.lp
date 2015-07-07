int_range(0, 10).
time_limit(10).

disallow(sub; div).

program_variable(x; y; n).
read_only_variable(n).
logic_variable(x0).

template(1, pre("N > 0, 0 < X, X < N, X0=X")).
template(2, post("(X0 * Y) #mod N == 1")).
