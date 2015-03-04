#const time_max=20. % Maximum number of time steps to simulate.
#const int_max=10.  % Maximum integer value.

% Constants allowed to occur in the code:
con(1).

% Additional constraints:
disallow(if; mul; div).

% Allowed variables, which variables are writable:
var(x). var(y). var(z).
write_var(x). write_var(y). write_var(z).

% Input/output pairs specifying the program:
in(r1,x,1). out(r1,y,2).
in(r2,x,2). out(r2,y,3).
in(r3,x,3). out(r3,y,5).
in(r4,x,4). out(r4,y,7).
in(r5,x,5). out(r5,y,11).
in(r6,x,6). out(r6,y,13).
