

my_stats(FetchTimeInMsecs):-
        statistics(runtime, [T0|_]),
        %import_data,
        b_begin,
        statistics(runtime, [T1|_]),
        FetchTimeInMsecs is T1 - T0.

