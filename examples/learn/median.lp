#const time_max=line_max+1.
#const int_max=8.

var(a). var(b). var(c).
var(y). var(z).

write_var(y). write_var(z).

disallow(while, arithmetic).

preset_line_instr(1, set(y, var(a))).
preset_line_instr(2, if(lt(var(y), var(b)), 1)).
preset_line_instr(3, set(y, var(b))).

% Examples:
 in(R,a,A) :- ex(R,A,_,_,_).
 in(R,b,B) :- ex(R,_,B,_,_).
 in(R,c,C) :- ex(R,_,_,C,_).
out(R,z,Z) :- ex(R,_,_,_,Z).

ex(r00, 0,0,0, 0). %ex(r01, 1,1,1, 1). ex(r02, 2,2,2, 2).

ex(r10, 7,1,1, 1). %ex(r11, 6,2,2, 2). ex(r12, 5,3,3, 3).
ex(r20, 1,7,1, 1). %ex(r21, 2,6,2, 2). ex(r22, 3,5,3, 3).
ex(r30, 1,1,7, 1). %ex(r31, 2,2,6, 2). ex(r32, 3,3,5, 3).

ex(r40, 1,7,7, 7). %ex(r41, 2,6,6, 6). ex(r42, 3,5,5, 5).
ex(r50, 7,1,7, 7). %ex(r51, 6,2,6, 6). ex(r52, 5,3,5, 5).
ex(r60, 7,7,1, 7). %ex(r61, 6,6,2, 6). ex(r62, 5,5,3, 5).

ex(r70, 1,2,3, 2). %ex(r71, 1,3,6, 3). ex(r72, 2,4,6, 4).
ex(r80, 1,3,2, 2). %ex(r81, 1,6,3, 3). ex(r82, 2,6,4, 4).
ex(r90, 2,1,3, 2). %ex(r91, 3,1,6, 3). ex(r92, 4,2,6, 4).
ex(rA0, 3,2,1, 2). %ex(rA1, 6,3,1, 3). ex(rA2, 6,4,2, 4).
ex(rB0, 2,3,1, 2). %ex(rB1, 3,6,1, 3). ex(rB2, 4,6,2, 4).
ex(rC0, 3,1,2, 2). %ex(rC1, 6,1,3, 3). ex(rC2, 6,2,4, 4).
