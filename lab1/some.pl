mother(lena, olya).
mother(lena, kolya).
mother(lena, vika).
mother(natasha, sasha).

print_all(X, Y) :- 
    mother(X, Y),
    write(X), write(" есть мать "), write(Y), nl,
    fail.

print_one(X,Y) :-
    mother(X,Y),
    write(X), write(" есть мать "), write(Y), nl,
    !.

