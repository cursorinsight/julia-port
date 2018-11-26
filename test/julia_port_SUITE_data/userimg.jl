module JuliaPortTest

"""
Return a binary "ok" in case of a good input and cause an error otherwise.
"""
function check_function(data::AbstractVector{UInt8})::Vector{UInt8}
    if data == b"good"
        return b"ok"
    else
        throw(DomainError)
    end
end

end
