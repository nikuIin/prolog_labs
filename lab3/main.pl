:- use_module(library(odbc)).

% Вспомогательные предикаты для форматирования вывода
format_person(FirstName, Patronymic, LastName) :-
    format('~w ~w ~w~n', [FirstName, Patronymic, LastName]).

format_person_with_year(FirstName, Patronymic, LastName, BirthYear) :-
    format('~w ~w ~w, год рождения: ~w~n', [FirstName, Patronymic, LastName, BirthYear]).

format_last_name(LastName) :-
    format('~w~n', [LastName]).

% Запрос 1: Найти всех близнецов
find_twins :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    findall([FirstName, Patronymic, LastName],
            odbc_query(Connection,
                       'SELECT first_name, patronymic, last_name FROM person WHERE is_twin = TRUE',
                       row(FirstName, Patronymic, LastName)),
            Twins),
    odbc_disconnect(Connection),
    (   Twins = [] ->
        write('Нет близнецов или запрос завершен.'), nl
    ;   foreach(member([FirstName, Patronymic, LastName], Twins),
                format_person(FirstName, Patronymic, LastName))
    ).

% Запрос 2: Найти всех детей, родившихся в 2010 году
find_children_by_year :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    findall([FirstName, Patronymic, LastName],
            odbc_query(Connection,
                       'SELECT p.first_name, p.patronymic, p.last_name
                        FROM person p
                        JOIN children c ON p.id = c.child_id
                        WHERE p.birth_year = 2010',
                       row(FirstName, Patronymic, LastName)),
            Children),
    odbc_disconnect(Connection),
    (   Children = [] ->
        write('Нет детей за указанный год или запрос завершен.'), nl
    ;   foreach(member([FirstName, Patronymic, LastName], Children),
                format_person(FirstName, Patronymic, LastName))
    ).

% Запрос 3: Найти всех работающих жен с доходом больше 85000
find_working_wives :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    findall([FirstName, Patronymic, LastName],
            odbc_query(Connection,
                       'SELECT p.first_name, p.patronymic, p.last_name
                        FROM person p
                        JOIN family f ON p.id = f.wife_id
                        WHERE p.gender = ''женский'' AND p.monthly_income > 85000',
                       row(FirstName, Patronymic, LastName)),
            Wives),
    odbc_disconnect(Connection),
    (   Wives = [] ->
        write('Нет подходящих жен или запрос завершен.'), nl
    ;   foreach(member([FirstName, Patronymic, LastName], Wives),
                format_person(FirstName, Patronymic, LastName))
    ).

% Запрос 4: Найти фамилии людей с 2 детьми
find_families_by_children_count :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    findall(LastName,
            odbc_query(Connection,
                       'SELECT p.last_name
                        FROM person p
                        JOIN family f ON p.id = f.husband_id OR p.id = f.wife_id
                        JOIN children c ON f.family_id = c.family_id
                        GROUP BY f.family_id, p.last_name
                        HAVING COUNT(c.child_id) = 2',
                       row(LastName)),
            Families),
    odbc_disconnect(Connection),
    (   Families = [] ->
        write('Нет семей с указанным числом детей или запрос завершен.'), nl
    ;   foreach(member(LastName, Families),
                format_last_name(LastName))
    ).

% Запрос 5: Найти самого старшего ребенка
find_oldest_child :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    (   odbc_query(Connection,
                   'SELECT p.first_name, p.patronymic, p.last_name, p.birth_year
                    FROM person p
                    JOIN children c ON p.id = c.child_id
                    ORDER BY p.birth_year ASC
                    LIMIT 1',
                   row(FirstName, Patronymic, LastName, BirthYear)) ->
        odbc_disconnect(Connection),
        format_person_with_year(FirstName, Patronymic, LastName, BirthYear)
    ;   odbc_disconnect(Connection),
        write('Нет детей или запрос завершен.'), nl
    ).

% Главный предикат для выполнения всех запросов
test :-
    write('=== Запрос 1: Все близнецы ==='), nl,
    find_twins,
    nl,
    write('=== Запрос 2: Дети, родившиеся в 2010 году ==='), nl,
    find_children_by_year,
    nl,
    write('=== Запрос 3: Работающие жены с доходом > 85000 ==='), nl,
    find_working_wives,
    nl,
    write('=== Запрос 4: Фамилии людей с 2 детьми ==='), nl,
    find_families_by_children_count,
    nl,
    write('=== Запрос 5: Самый старший ребенок ==='), nl,
    find_oldest_child.
