:- use_module(library(pio)).
:- use_module(library(sgml)).
:- use_module(library(lists)).
:- use_module(library(error)).

:- dynamic person/7.
:- dynamic family/3.

% Логирование действий
log_action(Action) :-
    catch(
        (get_time(Time),
         format_time(atom(TimeStr), '%Y-%m-%d %H:%M:%S', Time),
         format(atom(LogEntry), '~w - ~w~n', [TimeStr, Action]),
         open('actions.log', append, Stream),
         write(Stream, LogEntry),
         close(Stream)),
        Error,
        (format('Ошибка логирования: ~w~n', [Error]), fail)
    ).

% Загрузка данных из XML
load_from_xml(File) :-
    log_action('Загрузка данных из XML-файла'),
    catch(
        (load_xml_file(File, XML),
         log_action('XML успешно загружен, структура: ~w'-[XML]),
         clear_db,
         parse_xml(XML)),
        Error,
        (format('Ошибка загрузки XML: ~w~n', [Error]), fail)
    ).

% Очистка базы данных
clear_db :-
    retractall(person(_, _, _, _, _, _, _)),
    retractall(family(_, _, _)).

% Парсинг XML
parse_xml([element(people, _, People)]) :-
    parse_people(People).
parse_xml([]) :-
    format('Предупреждение: XML пустой~n'), !.
parse_xml(Other) :-
    format('Ошибка: Некорректная структура XML: ~w~n', [Other]), fail.

% Парсинг людей и семей
parse_people([]).
parse_people([element(person, Attrs, _)|Rest]) :-
    (   memberchk(first_name=FirstName, Attrs),
        memberchk(patronymic=Patronymic, Attrs),
        memberchk(last_name=LastName, Attrs),
        memberchk(birth_year=BirthYearStr, Attrs),
        memberchk(gender=Gender, Attrs),
        memberchk(income=IncomeStr, Attrs),
        memberchk(is_twin=IsTwinStr, Attrs),
        atom_number(BirthYearStr, BirthYear),
        atom_number(IncomeStr, Income),
        (IsTwinStr = 'true' -> IsTwin = true ; IsTwin = false),
        assertz(person(FirstName, Patronymic, LastName, BirthYear, Gender, Income, IsTwin)),
        log_action('Добавлен человек: ~w ~w ~w'-[FirstName, Patronymic, LastName])
    ->  parse_people(Rest)
    ;   format('Ошибка парсинга person: ~w~n', [Attrs]),
        parse_people(Rest) % Continue parsing instead of failing
    ).
parse_people([element(family, _, Children)|Rest]) :-
    (   % Filter out non-element terms (e.g., commas)
        findall(Elem, (member(Elem, Children), Elem = element(_, _, _)), ValidChildren),
        member(element(husband, HAttrs, _), ValidChildren),
        member(element(wife, WAttrs, _), ValidChildren),
        member(element(children, _, ChildNodes), ValidChildren),
        memberchk(first_name=HFirst, HAttrs),
        memberchk(patronymic=HPatr, HAttrs),
        memberchk(last_name=HLast, HAttrs),
        memberchk(first_name=WFirst, WAttrs),
        memberchk(patronymic=WPatr, WAttrs),
        memberchk(last_name=WLast, WAttrs),
        parse_children(ChildNodes, ChildList),
        assertz(family(
            person(HFirst, HPatr, HLast, _, _, _, _),
            person(WFirst, WPatr, WLast, _, _, _, _),
            ChildList
        )),
        log_action('Добавлена семья: ~w ~w - ~w ~w'-[HFirst, HLast, WFirst, WLast])
    ->  parse_people(Rest)
    ;   format('Ошибка парсинга family, пропуск: ~w~n', [Children]),
        parse_people(Rest) % Continue parsing instead of failing
    ).
parse_people([Other|Rest]) :-
    parse_people(Rest).

% Парсинг детей
parse_children([], []).
parse_children([element(child, Attrs, _)|Rest], [person(FirstName, Patronymic, LastName, _, _, _, _)|ChildList]) :-
    (   memberchk(first_name=FirstName, Attrs),
        memberchk(patronymic=Patronymic, Attrs),
        memberchk(last_name=LastName, Attrs),
        log_action('Добавлен ребенок: ~w ~w ~w'-[FirstName, Patronymic, LastName])
    ->  parse_children(Rest, ChildList)
    ;   format('Ошибка парсинга child: ~w~n', [Attrs]),
        parse_children(Rest, ChildList) % Continue parsing
    ).
parse_children([Other|Rest], ChildList) :-
    parse_children(Rest, ChildList).

% Сохранение в XML
save_to_xml(File) :-
    log_action('Сохранение данных в XML-файл'),
    catch(
        (findall(element(person, [
                    first_name=FirstName, patronymic=Patronymic, last_name=LastName,
                    birth_year=BirthYear, gender=Gender, income=Income, is_twin=IsTwinStr
                ], []),
                (person(FirstName, Patronymic, LastName, BirthYear, Gender, Income, IsTwin),
                 (IsTwin = true -> IsTwinStr = 'true' ; IsTwinStr = 'false')),
                People),
         findall(element(family, [], [
                    element(husband, [first_name=HFirst, patronymic=HPatr, last_name=HLast], []),
                    element(wife, [first_name=WFirst, patronymic=WPatr, last_name=WLast], []),
                    element(children, [], ChildElements)
                ]),
                (family(person(HFirst, HPatr, HLast, _, _, _, _),
                        person(WFirst, WPatr, WLast, _, _, _, _),
                        Children),
                 children_to_xml(Children, ChildElements)),
                Families),
         append(People, Families, AllElements),
         XML = [element(people, [], AllElements)],
         open(File, write, Stream),
         xml_write(Stream, XML, [header(true)]),
         close(Stream)),
        Error,
        (format('Ошибка сохранения XML: ~w~n', [Error]), fail)
    ).

% Преобразование детей в XML
children_to_xml([], []).
children_to_xml([person(FirstName, Patronymic, LastName, _, _, _, _)|Rest],
                [element(child, [first_name=FirstName, patronymic=Patronymic, last_name=LastName], [])|ChildRest]) :-
    children_to_xml(Rest, ChildRest).

% Инициализация базы данных
init_db :-
    log_action('Инициализация базы данных из XML'),
    (   exists_file('family.xml') ->
        load_from_xml('family.xml')
    ;   % Если XML-файла нет, инициализируем базу вручную
        log_action('XML-файл не найден, инициализация базы данных вручную'),
        assertz(person('Иван', 'Петрович', 'Сидоров', 1980, 'мужской', 100000, false)),
        assertz(person('Мария', 'Ивановна', 'Сидорова', 1982, 'женский', 85000, false)),
        assertz(person('Алексей', 'Иванович', 'Сидоров', 2010, 'мужской', 0, true)),
        assertz(person('Анна', 'Ивановна', 'Сидорова', 2010, 'женский', 0, true)),
        assertz(person('Петр', 'Алексеевич', 'Иванов', 1975, 'мужской', 120000, false)),
        assertz(person('Елена', 'Сергеевна', 'Иванова', 1978, 'женский', 0, false)),
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
        assertz(family(
            person('Иван', 'Петрович', 'Сидоров', 1980, 'мужской', 100000, false),
            person('Мария', 'Ивановна', 'Сидорова', 1982, 'женский', 85000, false),
            [person('Алексей', 'Иванович', 'Сидоров', 2010, 'мужской', 0, true),
             person('Анна', 'Ивановна', 'Сидорова', 2010, 'женский', 0, true)]
        )),
        assertz(family(
            person('Петр', 'Алексеевич', 'Иванов', 1975, 'мужской', 120000, false),
            person('Елена', 'Сергеевна', 'Иванова', 1978, 'женский', 0, false),
            [person('Сергей', 'Петрович', 'Иванов', 2005, 'мужской', 0, false),
             person('Ольга', 'Петровна', 'Иванова', 2005, 'женский', 0, false)]
        )),
        assertz(family(
            person('Дмитрий', 'Васильевич', 'Кузнецов', 1985, 'мужской', 95000, false),
            person('Светлана', 'Андреевна', 'Кузнецова', 1987, 'женский', 80000, false),
            [person('Михаил', 'Дмитриевич', 'Кузнецов', 2012, 'мужской', 0, false)]
        )),
        assertz(family(
            person('Николай', 'Сергеевич', 'Петров', 1990, 'мужской', 110000, false),
            person('Татьяна', 'Николаевна', 'Петрова', 1992, 'женский', 87000, false),
            [person('Екатерина', 'Николаевна', 'Петрова', 2015, 'женский', 0, false)]
        )),
        assertz(family(
            person('Владимир', 'Игоревич', 'Смирнов', 1988, 'мужской', 105000, false),
            person('Юлия', 'Владимировна', 'Смирнова', 1990, 'женский', 88000, false),
            [person('Артем', 'Владимирович', 'Смирнов', 2018, 'мужской', 0, false)]
        ))
    ).

% Вспомогательные предикаты для вывода
format_person(person(FirstName, Patronymic, LastName, _, _, _, _)) :-
    format('~w ~w ~w~n', [FirstName, Patronymic, LastName]).

format_person_with_year(person(FirstName, Patronymic, LastName, BirthYear, _, _, _)) :-
    format('~w ~w ~w, год рождения: ~w~n', [FirstName, Patronymic, LastName, BirthYear]).

format_last_name(LastName) :-
    format('~w~n', [LastName]).

% Запрос 1: Найти всех людей, чей доход меньше заданного
find_people_with_income_less_than(MaxIncome) :-
    log_action('Запрос: Найти людей с доходом меньше заданной суммы'),
    findall(person(FirstName, Patronymic, LastName, BirthYear, Gender, Income, Twin),
            (person(FirstName, Patronymic, LastName, BirthYear, Gender, Income, Twin), Income < MaxIncome, Income > 0),
            People),
    (   People = [] ->
        format('Нет людей с доходом меньше ~w.~n', [MaxIncome])
    ;   format('Люди с доходом меньше ~w:~n', [MaxIncome]),
        foreach(member(Person, People), format_person_with_year(Person))
    ).

% Запрос 2: Найти всех детей, младше заданного возраста
find_children_younger_than(MaxAge) :-
    log_action('Запрос: Найти детей младше заданного возраста'),
    current_year(CurrentYear),
    BirthYearThreshold is CurrentYear - MaxAge,
    findall(person(FirstName, Patronymic, LastName, BirthYear, Gender, 0, Twin),
            (family(_, _, Children),
             member(person(FirstName, Patronymic, LastName, BirthYear, Gender, 0, Twin), Children),
             person(FirstName, Patronymic, LastName, BirthYear, Gender, 0, Twin),
             BirthYear >= BirthYearThreshold),
            Children),
    (   Children = [] ->
        format('Нет детей младше ~w лет.~n', [MaxAge])
    ;   format('Дети младше ~w лет:~n', [MaxAge]),
        foreach(member(Person, Children), format_person_with_year(Person))
    ).

% Предикат для получения текущего года
current_year(2024).

% Запрос 3: Найти всех неработающих жен, которые родились позже заданного года
find_non_working_wives_born_after(Year) :-
    log_action('Запрос: Найти неработающих жен, родившихся после заданного года'),
    findall(person(FirstName, Patronymic, LastName, BirthYear, 'женский', 0, Twin),
            (family(_, person(FirstName, Patronymic, LastName, BirthYear, 'женский', 0, Twin), _),
             person(FirstName, Patronymic, LastName, BirthYear, 'женский', 0, Twin),
             BirthYear > Year),
            Wives),
    (   Wives = [] ->
        format('Нет неработающих жен, родившихся после ~w года.~n', [Year])
    ;   format('Неработающие жены, родившиеся после ~w года:~n', [Year]),
        foreach(member(Wife, Wives), format_person_with_year(Wife))
    ).

% Запрос 4: Найти всех детей, у которых разница в возрасте родителей превышает заданную величину
find_children_with_parental_age_gap_greater_than(AgeGap) :-
    log_action('Запрос: Найти детей с разницей в возрасте родителей больше заданной величины'),
    findall(person(CFirst, CPatr, CLast, CBirthYear, CGender, 0, CTwin),
            (family(person(HFirst, HPatr, HLast, HBirthYear, _, _, _),
                    person(WFirst, WPatr, WLast, WBirthYear, _, _, _),
                    Children),
             member(person(CFirst, CPatr, CLast, CBirthYear, CGender, 0, CTwin), Children),
             person(HFirst, HPatr, HLast, HBirthYear, _, _, _),
             person(WFirst, WPatr, WLast, WBirthYear, _, _, _),
             abs(HBirthYear - WBirthYear) > AgeGap),
            ChildrenWithGap),
    (   ChildrenWithGap = [] ->
        format('Нет детей с разницей в возрасте родителей больше ~w лет.~n', [AgeGap])
    ;   format('Дети с разницей в возрасте родителей больше ~w лет:~n', [AgeGap]),
        list_to_set(ChildrenWithGap, UniqueChildrenWithGap),
        foreach(member(Person, UniqueChildrenWithGap), format_person_with_year(Person))
    ).

% Запрос 5: Вывод самого младшего ребенка
find_youngest_child :-
    log_action('Запрос: Найти самого младшего ребенка'),
    findall(person(FirstName, Patronymic, LastName, BirthYear, Gender, 0, IsTwin),
            (family(_, _, Children),
             member(person(FirstName, Patronymic, LastName, BirthYear, Gender, 0, IsTwin), Children),
             person(FirstName, Patronymic, LastName, BirthYear, Gender, 0, IsTwin)),
            AllChildren),
    (   AllChildren = [] ->
        write('Нет детей в базе данных.'), nl
    ;   sort(4, @>=, AllChildren, SortedChildren),
        [YoungestChild|_] = SortedChildren,
        format_person_with_year(YoungestChild)
    ).

% Главный предикат
main :-
    catch(
        (log_action('Запуск программы'),
         init_db,
         format('Запросы к базе данных:~n'),
         format('------------------------~n'),

         format('1. Люди с доходом меньше 90000:~n'),
         find_people_with_income_less_than(90000), nl,

         format('2. Дети младше 15 лет:~n'),
         find_children_younger_than(15), nl,

         format('3. Неработающие жены, родившиеся после 1975:~n'),
         find_non_working_wives_born_after(1975), nl,

         format('4. Дети с разницей в возрасте родителей больше 5 лет:~n'),
         find_children_with_parental_age_gap_greater_than(5), nl,

         format('5. Самый молодой ребенок в семье:~n'),
         find_youngest_child, nl,

         save_to_xml('family_updated.xml'),
         log_action('Программа завершена')),
        Error,
        (format('Ошибка в программе: ~w~n', [Error]), fail)
    ).

:- initialization(main).
