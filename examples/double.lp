#const time_max=30.
#const int_max=10.

con(1).
con(2).

var(x).
var(y).

write_var(x).
write_var(y).

in(r0,x,0). out(r0,y,0).
in(r1,x,1). out(r1,y,2).
in(r2,x,2). out(r2,y,4).
in(r3,x,3). out(r3,y,6).
in(r4,x,4). out(r4,y,8).
