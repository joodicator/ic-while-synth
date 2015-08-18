#begin_lua

function maybe_num(value, default)
    if Val.type(value) ~= Val.NUM then
        return default;
    else
        return value;
    end
end

#end_lua.
