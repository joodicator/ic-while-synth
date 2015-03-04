#const line_max=6.      % Maximum line number.
var(x). var(y). var(z). % All variables used.

line_instr(1, set(z, con(1))).                  % z = 1;
line_instr(2, while(lt(con(1), var(x)), 3)).    % while (1 < x) {
line_instr(3, set(z, add(var(y), var(z)))).     %    z = y + z;
line_instr(4, set(y, sub(var(z), var(y)))).     %    y = z - y;
line_instr(5, set(x, sub(var(x), con(1)))).     %    x = x - 1;
line_instr(6, end_while).                       % }

% Run the program with 6 different initial values of x:
in(r1,x,1). in(r2,x,2). in(r3,x,3).
in(r4,x,4). in(r5,x,5). in(r6,x,6).
