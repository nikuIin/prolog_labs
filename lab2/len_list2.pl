% Вспомогательный предикат с аккумулятором
length_acc([], Acc, Acc). % Когда список пуст, аккумулятор — это результат
length_acc([_|T], Acc, Length) :-
    NewAcc is Acc + 1, % Увеличиваем аккумулятор на 1
    length_acc(T, NewAcc, Length). % Рекурсия для хвоста

% Основной предикат
length_reverse(List, Length) :-
    length_acc(List, 0, Length). % Начинаем с аккумулятора 0

