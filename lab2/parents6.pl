% Факты о родителях
parent(lena, olya). % Лена — родитель Оли
parent(lena, kolya). % Лена — родитель Коли
parent(petya, kolya). % Петя — родитель Коли
parent(petya, vika). % Петя — родитель Вики

% Факты о поле
male(petya). % Петя — мужчина
male(kolya). % Коля — мужчина
female(lena). % Лена — женщина
female(olya). % Оля — женщина
female(vika). % Вика — женщина

% Правило: отец
father(X, Y) :- 
    parent(X, Y), 
    male(X).

% Правило: мать
mother(X, Y) :- 
    parent(X, Y), 
    female(X).

% Правило: брат
brother(X, Y) :- 
    male(X), 
    male(Y), 
    parent(Z, X), 
    parent(Z, Y), 
    X \= Y.

% Правило: дядя
uncle(X, Y) :- 
    brother(X, Z), 
    parent(Z, Y).
