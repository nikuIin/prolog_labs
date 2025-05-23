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
        assertz(person(FirstName, Patronymic, LastName, BirthYear, Gender, Income, IsTwin))
    ->  parse_people(Rest)
    ;   format('Ошибка парсинга person: ~w~n', [Attrs]), fail
    ).
parse_people([element(family, _, Children)|Rest]) :-
    (   member(element(husband, HAttrs, _), Children),
        member(element(wife, WAttrs, _), Children),
        member(element(children, _, ChildNodes), Children),
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
        ))
    ->  parse_people(Rest)
    ;   format('Ошибка парсинга family: ~w~n', [Children]), fail
    ).
parse_people([_Element|Rest]) :-
    parse_people(Rest).

% Парсинг детей
parse_children([], []).
parse_children([element(child, Attrs, _)|Rest], [person(FirstName, Patronymic, LastName, _, _, _, _)|ChildList]) :-
    (   memberchk(first_name=FirstName, Attrs),
        memberchk(patronymic=Patronymic, Attrs),
        memberchk(last_name=LastName, Attrs)
    ->  parse_children(Rest, ChildList)
    ;   format('Ошибка парсинга child: ~w~n', [Attrs]), fail
    ).

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
    load_from_xml('family.xml').

% Вспомогательные предикаты для вывода
format_person(person(FirstName, Patronymic, LastName, _, _, _, _)) :-
    format('~w ~w ~w~n', [FirstName, Patronymic, LastName]).

format_person_with_year(person(FirstName, Patronymic, LastName, BirthYear, _, _, _)) :-
    format('~w ~w ~w, год рождения: ~w~n', [FirstName, Patronymic, LastName, BirthYear]).

format_last_name(LastName) :-
    format('~w~n', [LastName]).

% Запрос 1: Найти всех близнецов
find_twins :-
    log_action('Запрос: Найти всех близнецов'),
    findall(person(FirstName, Patronymic, LastName, BirthYear, Gender, Income, true),
            person(FirstName, Patronymic, LastName, BirthYear, Gender, Income, true),
            Twins),
    (   Twins = [] ->
        write('Нет близнецов.'), nl
    ;   foreach(member(Person, Twins), format_person(Person))
    ).

% Запрос 2: Найти всех детей, родившихся в заданном году
find_children_by_year(Year) :-
    log_action('Запрос: Найти детей, родившихся в заданном году'),
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
    log_action('Запрос: Найти работающих жен с доходом больше заданной суммы'),
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
    log_action('Запрос: Найти фамилии людей с заданным числом детей'),
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
    log_action('Запрос: Найти самого старшего ребенка'),
    findall(person(FirstName, Patronymic, LastName, BirthYear, Gender, 0, IsTwin),
            (family(_, _, Children),
             member(person(FirstName, Patronymic, LastName, _, _, _, _), Children),
             person(FirstName, Patronymic, LastName, BirthYear, Gender, 0, IsTwin),
             number(BirthYear)),
            AllChildren),
    (   AllChildren = [] ->
        write('Нет детей в базе данных.'), nl
    ;   sort(4, @=<, AllChildren, SortedChildren),
        [OldestChild|_] = SortedChildren,
        format_person_with_year(OldestChild)
    ).

% Тестовый предикат
test :-
    catch(
        (log_action('Запуск теста всех запросов'),
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
         find_oldest_child,
	 save_to_xml('family_updated.xml'),
         log_action('Тест завершен')),
        Error,
        (format('Ошибка в тесте: ~w~n', [Error]), fail)
    ).
