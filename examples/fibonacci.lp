#const time_max=30. % Maximum number of time steps to simulate.
#const int_max=10.  % Maximum integer value.

% Constants allowed to occur in the code:
con(1).

% Additional constraints:
disallow(if; mul; div; mod).

% Allowed variables, which variables are writable:
var(x; y; z). write_var(x; y; z).

% Input/output pairs specifying the program:
in(r1,x,1). out(r1,z,1).
in(r2,x,2). out(r2,z,1).
in(r3,x,3). out(r3,z,2).
in(r4,x,4). out(r4,z,3).
in(r5,x,5). out(r5,z,5).
