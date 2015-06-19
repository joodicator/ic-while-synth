time_limit(12).
int_range(0, 7).
line_limit_min(5).

input_variable(a; b; c).
read_only_variable(a; b; c).
output_variable(m).
extra_variable(x; y).

disallow_feature(while; arithmetic).

% Give some of the program:
preset_line_instr(1, set(x, var(a))).
preset_line_instr(2, if(lt(var(x), var(b)), 1)).
preset_line_instr(3, set(x, var(b))).

% Examples:
ex(r00, 0,0,0, 0). 
ex(r10, 7,1,1, 1). 
ex(r20, 1,7,1, 1). 
ex(r30, 1,1,7, 1). 
ex(r40, 1,7,7, 7). 
ex(r50, 7,1,7, 7). 
ex(r60, 7,7,1, 7). 
ex(r70, 1,2,3, 2). 
ex(r80, 1,3,2, 2). 
ex(r90, 2,1,3, 2). 
ex(rA0, 3,2,1, 2). 
ex(rB0, 2,3,1, 2). 
ex(rC0, 3,1,2, 2). 

% This predicate allows examples to be written compactly.
 in(R,a,A) :- ex(R,A,_,_,_).
 in(R,b,B) :- ex(R,_,B,_,_).
 in(R,c,C) :- ex(R,_,_,C,_).
out(R,m,M) :- ex(R,_,_,_,M).
