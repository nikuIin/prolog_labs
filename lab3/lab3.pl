% Факты о людях: person(Имя, Отчество, Фамилия, ГодРождения, Пол, Доход, Близнец)
:- dynamic person/7.

% Факты о семьях: family(Муж, Жена, СписокДетей)
:- dynamic family/3.

% Инициализация базы знаний
init_db :-
    % Люди (мужья, жены, дети)
    assertz(person('Иван', 'Петрович', 'Сидоров', 1980, 'мужской', 100000, false)),
    assertz(person('Мария', 'Ивановна', 'Сидорова', 1982, 'женский', 85000, false)),
    assertz(person('Алексей', 'Иванович', 'Сидоров', 2010, 'мужской', 0, true)),
    assertz(person('Анна', 'Ивановна', 'Сидорова', 2010, 'женский', 0, true)),
    assertz(person('Петр', 'Алексеевич', 'Иванов', 1975, 'мужской', 120000, false)),
    assertz(person('Елена', 'Сергеевна', 'Иванова', 1978, 'женский', 90000, false)),
    assertz(person('Сергей', 'Петрович', 'Иванов', 2005, 'мужской', 0, false)),
    assertz(person('Ольга', 'Петровна', 'Иванова', 2005, 'женский', 0, false)),
    assertz(person('Дмитрий', 'Васильевич', 'Кузнецов', 1985, 'мужской', 95000, false)),
    assertz(person('Светлана', 'Андреевна', 'Кузнецова', 1987, 'женский', 80000, false)),
    assertz(person('Михаил', 'Дмитриевич', 'Кузнецов', 2012, 'мужской', 0, false)),
    assertz(person('Николай', 'Сергеевич', 'Петров', 1990, 'мужской', 110000, false)),
    assertz(person('Татьяна', 'Николаевна', 'Петрова', 1992, 'женский', 87000, false)),
    assertz(person('Екатерина', 'Николаевна', 'Петрова', 2015, 'женский', 0, false)),
    assertz(person('Владимир', 'Игоревич', 'Смирнов', 1988, 'мужской', 105000, false)),
    assertz(person('Юлия', 'Владимировна', 'Смирнова', 1990, 'женский', 88000, false)),
    assertz(person('Артем', 'Владимирович', 'Смирнов', 2018, 'мужской', 0, false)),

    % Семьи
    assertz(family(
        person('Иван', 'Петрович', 'Сидоров', _, _, _, _),
        person('Мария', 'Ивановна', 'Сидорова', _, _, _, _),
        [person('Алексей', 'Иванович', 'Сидоров', _, _, _, _),
         person('Анна', 'Ивановна', 'Сидорова', _, _, _, _)]
    )),
    assertz(family(
        person('Петр', 'Алексеевич', 'Иванов', _, _, _, _),
        person('Елена', 'Сергеевна', 'Иванова', _, _, _, _),
        [person('Сергей', 'Петрович', 'Иванов', _, _, _, _),
         person('Ольга', 'Петровна', 'Иванова', _, _, _, _)]
    )),
    assertz(family(
        person('Дмитрий', 'Васильевич', 'Кузнецов', _, _, _, _),
        person('Светлана', 'Андреевна', 'Кузнецова', _, _, _, _),
        [person('Михаил', 'Дмитриевич', 'Кузнецов', _, _, _, _)]
    )),
    assertz(family(
        person('Николай', 'Сергеевич', 'Петров', _, _, _, _),
        person('Татьяна', 'Николаевна', 'Петрова', _, _, _, _),
        [person('Екатерина', 'Николаевна', 'Петрова', _, _, _, _)]
    )),
    assertz(family(
        person('Владимир', 'Игоревич', 'Смирнов', _, _, _, _),
        person('Юлия', 'Владимировна', 'Смирнова', _, _, _, _),
        [person('Артем', 'Владимирович', 'Смирнов', _, _, _, _)]
    )).

% Вспомогательные предикаты для вывода
format_person(person(FirstName, Patronymic, LastName, _, _, _, _)) :-
    format('~w ~w ~w~n', [FirstName, Patronymic, LastName]).

format_person_with_year(person(FirstName, Patronymic, LastName, BirthYear, _, _, _)) :-
    format('~w ~w ~w, год рождения: ~w~n', [FirstName, Patronymic, LastName, BirthYear]).

format_last_name(LastName) :-
    format('~w~n', [LastName]).

% Запрос 1: Найти всех близнецов
find_twins :-
    findall(person(FirstName, Patronymic, LastName, BirthYear, Gender, Income, true),
            person(FirstName, Patronymic, LastName, BirthYear, Gender, Income, true),
            Twins),
    (   Twins = [] ->
        write('Нет близнецов.'), nl
    ;   foreach(member(Person, Twins), format_person(Person))
    ).

% Запрос 2: Найти всех детей, родившихся в заданном году
find_children_by_year(Year) :-
    findall(person(FirstName, Patronymic, LastName, Year, Gender, 0, IsTwin),
            (family(_, _, Children),
             member(person(FirstName, Patronymic, LastName, Year, Gender, 0, IsTwin), Children)),
            Children),
    (   Children = [] ->
        format('Нет детей, родившихся в ~w году.~n', [Year])
    ;   foreach(member(Person, Children), format_person(Person))
    ).

% Запрос 3: Найти всех работающих жен с доходом больше заданной суммы
find_working_wives(IncomeThreshold) :-
    findall(person(FirstName, Patronymic, LastName, BirthYear, 'женский', Income, IsTwin),
            (family(_, person(FirstName, Patronymic, LastName, _, 'женский', _, _), _),
             person(FirstName, Patronymic, LastName, BirthYear, 'женский', Income, IsTwin),
             Income > IncomeThreshold),
            Wives),
    (   Wives = [] ->
        format('Нет жен с доходом больше ~w.~n', [IncomeThreshold])
    ;   foreach(member(Person, Wives), format_person(Person))
    ).

% Запрос 4: Найти фамилии людей с заданным числом детей
find_families_by_children_count(ChildrenCount) :-
    findall(LastName,
            (family(person(_, _, LastName, _, _, _, _), _, Children),
             length(Children, ChildrenCount)),
            HusbandLastNames),
    findall(LastName,
            (family(_, person(_, _, LastName, _, _, _, _), Children),
             length(Children, ChildrenCount)),
            WifeLastNames),
    append(HusbandLastNames, WifeLastNames, AllLastNames),
    sort(AllLastNames, UniqueLastNames),
    (   UniqueLastNames = [] ->
        format('Нет семей с ~w детьми.~n', [ChildrenCount])
    ;   foreach(member(LastName, UniqueLastNames), format_last_name(LastName))
    ).

% Запрос 5: Найти самого старшего ребенка
find_oldest_child :-
    findall(person(FirstName, Patronymic, LastName, BirthYear, Gender, 0, IsTwin),
            (family(_, _, Children),
             member(person(FirstName, Patronymic, LastName, _, _, _, _), Children),
             person(FirstName, Patronymic, LastName, BirthYear, Gender, 0, IsTwin),
             number(BirthYear)),
            AllChildren),
    (   AllChildren = [] ->
        write('Нет детей в базе данных.'), nl
    ;   sort(4, @=<, AllChildren, SortedChildren), % Сортировка по году рождения (4-е поле)
        [OldestChild|_] = SortedChildren,
        format_person_with_year(OldestChild)
    ).

% Тестовый предикат для выполнения всех запросов
test :-
    init_db,
    write('=== Запрос 1: Все близнецы ==='), nl,
    find_twins,
    nl,
    write('=== Запрос 2: Дети, родившиеся в 2010 году ==='), nl,
    find_children_by_year(2010),
    nl,
    write('=== Запрос 3: Работающие жены с доходом > 85000 ==='), nl,
    find_working_wives(85000),
    nl,
    write('=== Запрос 4: Фамилии людей с 2 детьми ==='), nl,
    find_families_by_children_count(2),
    nl,
    write('=== Запрос 5: Самый старший ребенок ==='), nl,
    find_oldest_child.
