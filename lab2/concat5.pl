% Предикат для поиска элемента (возвращает true, если элемент есть)
find_element(X, [X|_]). % Элемент найден, если он первый
find_element(X, [_|T]) :- 
    find_element(X, T). % Ищем в хвосте

% Предикат для поиска с возвратом позиции
find_position(X, List, Position) :- 
    find_position_acc(X, List, 1, Position).

% Вспомогательный предикат с аккумулятором для позиции
find_position_acc(X, [X|_], Pos, Pos). % Элемент найден
find_position_acc(X, [_|T], Acc, Pos) :- 
    NewAcc is Acc + 1, % Увеличиваем позицию
    find_position_acc(X, T, NewAcc, Pos).

