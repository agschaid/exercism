-module(leap).

-export([leap_year/1]).


leap_year(Year) -> 

    % Computes whether the Year parameter is divisable by a given Divisor.
    DivBy = fun(Divisor) -> Year rem Divisor == 0 end,
    
    % There surely is a perfomance hit for computing all three values and then
    % doing a pattern match.
    % But all other ways I tried were comparatively unintelligible.  So I'd 
    % stay with this version unless a profiler tells me, that this function 
    % is a problem.
    case {DivBy(4), DivBy(100), DivBy(400)} of 
        {true, false, _   } -> true;
        {true, true,  true} -> true;
        _                   -> false
    end.
