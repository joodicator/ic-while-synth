int_range(0, 32).
time_limit(50).

input_variable(n).
output_variable(e; o).
read_only_variable(n).

max_while(1).
max_if(2).

con(0; 1; 2).

precondition("In_n > 0").

% n can be written as o * 2^e, where o is odd.
postcondition("Out_o #mod 2 != 0, 2**Out_e * Out_o == In_n").
