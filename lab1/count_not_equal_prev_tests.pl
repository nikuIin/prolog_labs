:- consult('count_not_equal_prev.pl'). % Загрузка основного предиката

:- begin_tests(count_not_equal_prev).

% Пустой список
test(empty_list, [nondet]) :-
    count_not_equal_prev([], 0).

% Список из одного элемента
test(single_element, [nondet]) :-
    count_not_equal_prev([5], 0).

% Все элементы одинаковые
test(all_same, [nondet]) :-
    count_not_equal_prev([1, 1, 1, 1], 0).

% Все элементы разные
test(all_different, [nondet]) :-
    count_not_equal_prev([1, 2, 3, 4], 3).

% Чередующиеся элементы
test(alternating, [nondet]) :-
    count_not_equal_prev([1, 2, 1, 2], 3).

% Два одинаковых, затем разные
test(two_same_then_different, [nondet]) :-
    count_not_equal_prev([1, 1, 2, 3], 2).

% Разные, затем два одинаковых
test(different_then_two_same, [nondet]) :-
    count_not_equal_prev([1, 2, 2, 3], 2).

:- end_tests(count_not_equal_prev).
