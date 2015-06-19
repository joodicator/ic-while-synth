#const time_max=30. % Maximum number of time steps to simulate.
#const int_min=0.
#const int_max=10.  % Maximum integer value.

% Constants allowed to occur in the code:
con(0; 1).

% Additional constraints:
disallow(if; mul; div; mod).

% Allowed variables, which variables are writable:
var(n; e; f).
write_var(n; e; f).

% Predefined program structure:
preset_line_instr(1, set(e, con(0))).
preset_line_instr(2, set(f, con(1))).
%preset_line_instr(3, while(lt(con(1), var(n)), line_max-4)).
%preset_line_instr(line_max, end_while).

% Input/output pairs specifying the program:
in(r1,n,1). out(r1,f,1).
in(r2,n,2). out(r2,f,1).
in(r3,n,3). out(r3,f,2).
in(r4,n,4). out(r4,f,3).
in(r5,n,5). out(r5,f,5).
