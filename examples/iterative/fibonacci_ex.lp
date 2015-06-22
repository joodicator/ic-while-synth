time_limit(30).
int_range(0, 10).

input_variable(n).
output_variable(f).
extra_variable(e).

constant(0; 1).
disallow_feature(if; mul; div; mod).

while_max(1).
if_max(1).

% Predefined program structure:
preset_line_instr(1, set(e, con(0))).
preset_line_instr(2, set(f, con(1))).
%preset_line_instr(3, while(lt(con(1), var(n)), line_max-4)).
%preset_line_instr(line_max, end_while).

in(r1,n,1). out(r1,f,1).
in(r2,n,2). out(r2,f,1).
in(r3,n,3). out(r3,f,2).
in(r4,n,4). out(r4,f,3).
in(r5,n,5). out(r5,f,5).

line_limit_min(7).

echo_clingo.
