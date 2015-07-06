% Takes x and y as input, x > 0, swaps x and y, then outputs x = |x0-y|.
% This demonstrates the need for automatic propagation of conditions.

time_limit(10).
int_range(-10,10).

program_variable(x; y; z).
logic_variable(x0; y0; x1; y1).

template(1, pre("X > Y, X0=X, Y0=Y")).
%template(2, mid("X=Y0, Y=X0")).
template(2, mid("X=Y0, Y=X0, X1 > Y1, X0=X1, Y0=Y1")).
template(3, post("X = X0-Y0, X > 0")).
