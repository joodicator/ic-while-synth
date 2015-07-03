int_range(0, 20).
time_limit(10).

program_variable(x; y).
logic_variable(x0).

template(1, pre("X0 = X")).
template(2, post("Y = 2*X0")).
