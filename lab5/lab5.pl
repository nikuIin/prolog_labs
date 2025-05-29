:- encoding(utf8).
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
parse_xml([element(family_data, _, People)]) :-
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
         XML = [element(family_data, [], AllElements)],
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

% Запрос 1: Найти всех людей, чей доход меньше заданного
find_low_income(IncomeThreshold) :-
    log_action('Запрос: Найти людей с доходом меньше заданного'),
    findall(person(FirstName, Patronymic, LastName, BirthYear, Gender, Income, IsTwin),
            (person(FirstName, Patronymic, LastName, BirthYear, Gender, Income, IsTwin),
             Income < IncomeThreshold),
            People),
    (   People = [] ->
        format('Нет людей с доходом меньше ~w.~n', [IncomeThreshold])
    ;   foreach(member(Person, People), format_person(Person))
    ).

% Запрос 2: Найти всех детей, младше заданного возраста
find_young_children(Age) :-
    log_action('Запрос: Найти детей младше заданного возраста'),
    CurrentYear is 2025,
    MinBirthYear is CurrentYear - Age,
    findall(person(FirstName, Patronymic, LastName, BirthYear, Gender, 0, IsTwin),
            (family(_, _, Children),
             member(person(FirstName, Patronymic, LastName, _, _, _, _), Children),
             person(FirstName, Patronymic, LastName, BirthYear, Gender, 0, IsTwin),
             BirthYear > MinBirthYear),
            Children),
    (   Children = [] ->
        format('Нет детей младше ~w лет.~n', [Age])
    ;   foreach(member(Person, Children), format_person_with_year(Person))
    ).

% Запрос 3: Найти всех неработающих жен, которые родились позже заданного года
find_unemployed_wives(BirthYear) :-
    log_action('Запрос: Найти неработающих жен, родившихся после заданного года'),
    findall(person(FirstName, Patronymic, LastName, BirthYearAfter, 'женский', 0, IsTwin),
            (family(_, person(FirstName, Patronymic, LastName, _, 'женский', _, _), _),
             person(FirstName, Patronymic, LastName, BirthYearAfter, 'женский', 0, IsTwin),
             BirthYearAfter > BirthYear),
            Wives),
    (   Wives = [] ->
        format('Нет неработающих жен, родившихся после ~w года.~n', [BirthYear])
    ;   foreach(member(Person, Wives), format_person_with_year(Person))
    ).

% Запрос 4: Найти всех детей, у которых разница в возрасте родителей превышает заданную величину
find_children_by_parent_age_diff(AgeDiff) :-
    log_action('Запрос: Найти детей с разницей в возрасте родителей больше заданной'),
    findall(person(CFirst, CPatr, CLast, CBirthYear, CGender, 0, CIsTwin),
            (family(person(_, _, _, HBirthYear, _, _, _),
                    person(_, _, _, WBirthYear, _, _, _),
                    Children),
             member(person(CFirst, CPatr, CLast, _, _, _, _), Children),
             person(CFirst, CPatr, CLast, CBirthYear, CGender, 0, CIsTwin),
             abs(HBirthYear - WBirthYear) > AgeDiff),
            Children),
    (   Children = [] ->
        format('Нет детей с разницей в возрасте родителей больше ~w лет.~n', [AgeDiff])
    ;   foreach(member(Person, Children), format_person_with_year(Person))
    ).

% Запрос 5: Подсчитать количество семей, у которых двое детей
count_families_with_two_children :-
    log_action('Запрос: Подсчитать количество семей с двумя детьми'),
    findall(_,
            (family(_, _, Children),
             length(Children, 2)),
            Families),
    length(Families, Count),
    format('Количество семей с двумя детьми: ~w~n', [Count]).

% Тестовый предикат
test :-
    catch(
        (log_action('Запуск теста всех запросов'),
         init_db,
         write('=== Запрос 1: Люди с доходом меньше 50000 ==='), nl,
         find_low_income(50000),
         nl,
         write('=== Запрос 2: Дети младше 15 лет ==='), nl,
         find_young_children(15),
         nl,
         write('=== Запрос 3: Неработающие жены, родившиеся после 1985 ==='), nl,
         find_unemployed_wives(1985),
         nl,
         write('=== Запрос 4: Дети с разницей возраста родителей больше 3 лет ==='), nl,
         find_children_by_parent_age_diff(3),
         nl,
         write('=== Запрос 5: Количество семей с двумя детьми ==='), nl,
         count_families_with_two_children,
         save_to_xml('family_updated.xml'),
         log_action('Тест завершен')),
        Error,
        (format('Ошибка в тесте: ~w~n', [Error]), fail)
    ).
