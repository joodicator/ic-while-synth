time_limit(30).
int_range(0, 10).
constant(0; 1).

input_variable(n).
output_variable(t).

disallow(if; mul; div; mod).

in(r0,n,0). out(r0,t,0).
in(r1,n,1). out(r1,t,1).
in(r2,n,2). out(r2,t,3).
in(r3,n,3). out(r3,t,6).
in(r4,n,4). out(r4,t,10).
