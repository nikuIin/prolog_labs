% Один проход пузырька: сравниваем и меняем соседние элементы
bubble_pass([], []). % Пустой список остаётся пустым
bubble_pass([X], [X]). % Один элемент — уже отсортирован
bubble_pass([X, Y|T], [X|SortedT]) :- 
    X =< Y, % Если X <= Y, оставляем X на месте
    bubble_pass([Y|T], SortedT).
bubble_pass([X, Y|T], [Y|SortedT]) :- 
    X > Y, % Если X > Y, меняем местами
    bubble_pass([X|T], SortedT).

% Полная сортировка: повторяем проходы, пока список не отсортирован
bubble_sort(List, Sorted) :- 
    bubble_pass(List, Temp), % Делаем один проход
    (   Temp = List -> Sorted = List % Если ничего не изменилось, список отсортирован
    ;   bubble_sort(Temp, Sorted) % Иначе повторяем для нового списка
    ).
