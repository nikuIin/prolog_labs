:- use_module(library(readln)).

% Загрузка основного предиката
:- consult('count_not_equal_prev.pl').

% Предикат для преобразования строки в список чисел
string_to_list(String, List) :-
    split_string(String, " ", "", StringList),
    maplist(atom_number, StringList, List).

% Основной предикат для взаимодействия с пользователем
main :-
    write('Введите элементы массива через пробел (например, 1 1 2 3 4): '),
    readln(Line),
    (   Line = [end_of_file] -> halt
    ;   atomic_list_concat(Line, ' ', InputString),
        (   string_to_list(InputString, List) ->
            count_not_equal_prev(List, Count),
            format('Количество элементов, не равных предыдущему: ~w~n', [Count]),
            main;
	    write('Ошибка: введите корректные числа через пробел!'), nl,
            main
        )
    ).

% Точка входа
:- initialization(main).
