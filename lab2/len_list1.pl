% Предикат для вычисления длины списка на прямом ходе
length_direct([], 0). % Пустой список имеет длину 0
length_direct([_|T], Length) :-
    length_direct(T, TailLength), % Рекурсивно считаем длину хвоста
    Length is TailLength + 1. % Добавляем 1 к длине хвоста
