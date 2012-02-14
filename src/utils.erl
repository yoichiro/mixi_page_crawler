-module(utils).
-export([create_date_integer/1,
        zero_suppress/2,
        calc_sub_7days/1]).

create_date_integer({Year, Month, Date}) ->
    list_to_integer(
      lists:concat([integer_to_list(Year),
                    zero_suppress(integer_to_list(Month), 2),
                    zero_suppress(integer_to_list(Date), 2)])).

zero_suppress(Source, Max) ->
    Length = length(Source),
    if Length >= Max ->
            Source;
       true ->
            zero_suppress(lists:concat(["0", Source]), Max)
    end.

calc_sub_7days({Year, Month, Date}) ->
    Sub = Date - 7,
    if Sub =< 0 ->
            if Month - 1 =< 0 ->
                    ResultMonth = 12,
                    ResultYear = Year - 1;
               true ->
                    ResultMonth = Month - 1,
                    ResultYear = Year
            end,
            LastDayOfPrevMonth = calendar:last_day_of_the_month(Year, Month),
            {ResultYear, ResultMonth, LastDayOfPrevMonth + Sub};
       true ->
            {Year, Month, Sub}
    end.
