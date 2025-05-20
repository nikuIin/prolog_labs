% Предикат для подсчета элементов, не равных своему предыдущему
count_not_equal_prev([], 0).
count_not_equal_prev([_], 0).
count_not_equal_prev([X,Y|Rest], Count) :-
    (X \= Y -> Count1 is 1 ; Count1 is 0),
    count_not_equal_prev([Y|Rest], CountRest),
    Count is Count1 + CountRest.
