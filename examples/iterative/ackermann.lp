time_limit(175).
stack_limit(7).
int_range(0, 7).

input_variable(m; n).
output_variable(a).
constant(0; 1).
disallow_feature(mul; div; mod; while).

main_sub(ackermann).
local(ackermann, m; n).
line_limit_step(ackermann, 1).

precondition("In_m < 3, In_n < 3").
postcondition("Out_a = @ackermann(In_m, In_n)").

#begin_lua
function ackermann(m, n)
    if m > 3 or n > 3 then return 0 end

    if m == 0 then
        return n + 1
    elseif n == 0 then
        return ackermann(m-1, 1)
    else
        return ackermann(m-1, ackermann(m, n-1))
    end
end
#end_lua.
