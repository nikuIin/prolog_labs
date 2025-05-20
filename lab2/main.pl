% Вспомогательный предикат для выбора разных значений из списка
choose([], _).
choose([H|T], Values) :- 
    member(H, Values),
    choose(T, Values),
    \+ member(H, T).

% Предикат для определения правдивости заявлений каждого мальчика
statement_truth(kolya, Breaker, Truth1, Truth2) :-
    (Breaker \= misha -> Truth1 = true ; Truth1 = false),
    (Breaker = sasha -> Truth2 = true ; Truth2 = false).
statement_truth(misha, Breaker, Truth1, Truth2) :-
    (Breaker \= misha -> Truth1 = true ; Truth1 = false),
    (Breaker \= kolya -> Truth2 = true ; Truth2 = false).
statement_truth(sasha, Breaker, Truth1, Truth2) :-
    (Breaker \= sasha -> Truth1 = true ; Truth1 = false),
    (Breaker = misha -> Truth2 = true ; Truth2 = false).

solve(Breaker, Truthful, Liar, HalfTruth) :-
    % Выбираем Разбившего
    member(Breaker, [kolya, misha, sasha]),
    
    % Вычисляем правдивость заявлений для каждого мальчика
    statement_truth(kolya, Breaker, KolyaTruth1, KolyaTruth2),
    statement_truth(misha, Breaker, MishaTruth1, MishaTruth2),
    statement_truth(sasha, Breaker, SashaTruth1, SashaTruth2),
    
    % Назначаем роли: Правдивый (оба истинны), Лжец (оба ложны), Полуправдивый (одно истинно)
    choose([Truthful, Liar, HalfTruth], [kolya, misha, sasha]),
    
    % Правдивый: оба утверждения истинны
    (Truthful = kolya -> (KolyaTruth1 = true, KolyaTruth2 = true) ;
     Truthful = misha -> (MishaTruth1 = true, MishaTruth2 = true) ;
     Truthful = sasha -> (SashaTruth1 = true, SashaTruth2 = true)),
    
    % Лжец: оба утверждения ложны
    (Liar = kolya -> (KolyaTruth1 = false, KolyaTruth2 = false) ;
     Liar = misha -> (MishaTruth1 = false, MishaTruth2 = false) ;
     Liar = sasha -> (SashaTruth1 = false, SashaTruth2 = false)),
    
    % Полуправдивый: ровно одно утверждение истинно
    (HalfTruth = kolya ->
        ((KolyaTruth1 = true, KolyaTruth2 = false) ; (KolyaTruth1 = false, KolyaTruth2 = true)) ;
     HalfTruth = misha ->
        ((MishaTruth1 = true, MishaTruth2 = false) ; (MishaTruth1 = false, MishaTruth2 = true)) ;
     HalfTruth = sasha ->
        ((SashaTruth1 = true, SashaTruth2 = false) ; (SashaTruth1 = false, SashaTruth2 = true))).

:- solve(Breaker, Truthful, Liar, HalfTruth),
   format('Кто разбил вазу: ~w~n', [Breaker]),
   format('Правдивый: ~w, Лжец: ~w, Полуправдивый: ~w~n', [Truthful, Liar, HalfTruth]),
   halt.
