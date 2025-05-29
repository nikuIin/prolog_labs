:- encoding(utf8).
:- use_module(library(http/http_server)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(odbc)).

% Suppress predicate discontinuity warnings
:- discontiguous server/0.

% HTTP handlers
:- http_handler(root(.), home_page, []).
:- http_handler(root(init_db), init_db, [method(post)]).
:- http_handler(root(add_person), add_person_page, []).
:- http_handler(root(add_person_submit), add_person_submit, [method(post)]).
:- http_handler(root(update_person), update_person_page, []).
:- http_handler(root(update_person_submit), update_person_submit, [method(post)]).
:- http_handler(root(delete_person), delete_person, [method(post)]).
:- http_handler(root(add_family), add_family_page, []).
:- http_handler(root(add_family_submit), add_family_submit, [method(post)]).
:- http_handler(root(delete_family), delete_family, [method(post)]).
:- http_handler(root(add_child), add_child_page, []).
:- http_handler(root(add_child_submit), add_child_submit, [method(post)]).
:- http_handler(root(delete_child), delete_child, [method(post)]).
:- http_handler(root(query_low_income), query_low_income, [method(post)]).
:- http_handler(root(query_young_children), query_young_children, [method(post)]).
:- http_handler(root(query_unemployed_wives), query_unemployed_wives, [method(post)]).
:- http_handler(root(query_parent_age_diff), query_parent_age_diff, [method(post)]).
:- http_handler(root(query_two_children), query_two_children, [method(get)]).

% Start server
server :-
    http_server(http_dispatch, [port(8080)]).

% Initialize database
init_db(Request) :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    odbc_query(Connection, 'DROP TABLE IF EXISTS children CASCADE'),
    odbc_query(Connection, 'DROP TABLE IF EXISTS family CASCADE'),
    odbc_query(Connection, 'DROP TABLE IF EXISTS person CASCADE'),
    odbc_query(Connection,
               'CREATE TABLE person (
                    id SERIAL PRIMARY KEY,
                    first_name VARCHAR(50),
                    patronymic VARCHAR(50),
                    last_name VARCHAR(50),
                    birth_year INTEGER,
                    gender VARCHAR(10),
                    monthly_income INTEGER,
                    is_twin BOOLEAN
                )'),
    odbc_query(Connection,
               'CREATE TABLE family (
                    family_id SERIAL PRIMARY KEY,
                    husband_id INTEGER,
                    wife_id INTEGER,
                    FOREIGN KEY (husband_id) REFERENCES person(id),
                    FOREIGN KEY (wife_id) REFERENCES person(id)
                )'),
    odbc_query(Connection,
               'CREATE TABLE children (
                    family_id INTEGER,
                    child_id INTEGER,
                    FOREIGN KEY (family_id) REFERENCES family(family_id),
                    FOREIGN KEY (child_id) REFERENCES person(id),
                    PRIMARY KEY (family_id, child_id)
                )'),
    odbc_query(Connection,
               'INSERT INTO person (first_name, patronymic, last_name, birth_year, gender, monthly_income, is_twin) VALUES
                    (''Александр'', ''Иванович'', ''Иванов'', 1978, ''мужской'', 75000, false),
                    (''Наталья'', ''Сергеевна'', ''Иванова'', 1980, ''женский'', 70000, false),
                    (''Михаил'', ''Александрович'', ''Иванов'', 2012, ''мужской'', 0, true),
                    (''Екатерина'', ''Александровна'', ''Иванова'', 2012, ''женский'', 0, true),
                    (''Виктор'', ''Петрович'', ''Кузнецов'', 1970, ''мужской'', 80000, false),
                    (''Ирина'', ''Викторовна'', ''Кузнецова'', 1972, ''женский'', 85000, false),
                    (''Артём'', ''Викторович'', ''Кузнецов'', 2008, ''мужской'', 0, false),
                    (''Ольга'', ''Михайловна'', ''Смирнова'', 1987, ''женский'', 0, false),
                    (''Сергей'', ''Васильевич'', ''Смирнов'', 1985, ''мужской'', 90000, false),
                    (''Анастасия'', ''Сергеевна'', ''Смирнова'', 2016, ''женский'', 0, false)'),
    odbc_query(Connection,
               'INSERT INTO family (husband_id, wife_id) VALUES
                    (1, 2),
                    (5, 6),
                    (9, 8)'),
    odbc_query(Connection,
               'INSERT INTO children (family_id, child_id) VALUES
                    (1, 3),
                    (1, 4),
                    (2, 7),
                    (3, 10)'),
    odbc_disconnect(Connection),
    http_redirect(moved, '/', Request).

% Home page
home_page(_Request) :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    findall(tr([],
               [td([], ID),
                td([], FirstName),
                td([], Patronymic),
                td([], LastName),
                td([], BirthYear),
                td([], Gender),
                td([], Income),
                td([], IsTwin)]),
            odbc_query(Connection,
                       'SELECT id, first_name, patronymic, last_name, birth_year, gender, monthly_income, is_twin FROM person',
                       row(ID, FirstName, Patronymic, LastName, BirthYear, Gender, Income, IsTwin)),
            PersonRows),
    findall(tr([],
               [td([], FamilyID),
                td([], HusbandName),
                td([], WifeName)]),
            odbc_query(Connection,
                       'SELECT f.family_id, h.last_name AS husband, w.last_name AS wife
                        FROM family f
                        JOIN person h ON f.husband_id = h.id
                        JOIN person w ON f.wife_id = w.id',
                       row(FamilyID, HusbandName, WifeName)),
            FamilyRows),
    findall(tr([],
               [td([], FamilyID),
                td([], ChildID),
                td([], ChildName)]),
            odbc_query(Connection,
                       'SELECT c.family_id, c.child_id, p.last_name
                        FROM children c
                        JOIN person p ON c.child_id = p.id',
                       row(FamilyID, ChildID, ChildName)),
            ChildRows),
    odbc_disconnect(Connection),
    reply_html_page(
        [title('База данных семей'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-5 fw-bold mb-4 text-primary'], 'База данных семей'),
             div([class='card mb-4'],
                 [div([class='card-header bg-primary text-white'], 'Люди'),
                  div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'ID'),
                                         th([], 'Имя'),
                                         th([], 'Отчество'),
                                         th([], 'Фамилия'),
                                         th([], 'Год рождения'),
                                         th([], 'Пол'),
                                         th([], 'Зарплата'),
                                         th([], 'Близнец?')])]),
                              tbody([], PersonRows)])])]),
             div([class='card mb-4'],
                 [div([class='card-header bg-primary text-white'], 'Семьи'),
                  div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'ID семьи'),
                                         th([], 'Муж'),
                                         th([], 'Жена')])]),
                              tbody([], FamilyRows)])])]),
             div([class='card mb-4'],
                 [div([class='card-header bg-primary text-white'], 'Дети'),
                  div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'ID семьи'),
                                         th([], 'ID ребенка'),
                                         th([], 'Фамилия')])]),
                              tbody([], ChildRows)])])]),
             div([class='d-grid gap-2 d-md-flex justify-content-md-start mb-4'],
                 [form([action('/add_person'), method(get)],
                       button([type=submit, class='btn btn-primary'], 'Добавить человека')),
                  form([action('/update_person'), method(get)],
                       button([type=submit, class='btn btn-primary'], 'Изменить человека')),
                  form([action('/delete_person'), method(post), class='d-inline'],
                       [input([type=text, name=id, placeholder='ID человека', class='form-control d-inline-block w-auto me-2']),
                        button([type=submit, class='btn btn-danger'], 'Удалить человека')]),
                  form([action('/add_family'), method(get)],
                       button([type=submit, class='btn btn-primary'], 'Создать семью')),
                  form([action('/delete_family'), method(post), class='d-inline'],
                       [input([type=text, name=family_id, placeholder='ID семьи', class='form-control d-inline-block w-auto me-2']),
                        button([type=submit, class='btn btn-danger'], 'Удалить семью')]),
                  form([action('/add_child'), method(get)],
                       button([type=submit, class='btn btn-primary'], 'Добавить ребенка')),
                  form([action('/delete_child'), method(post), class='d-inline'],
                       [input([type=text, name=family_id, placeholder='ID семьи', class='form-control d-inline-block w-auto me-2']),
                        input([type=text, name=child_id, placeholder='ID ребенка', class='form-control d-inline-block w-auto me-2']),
                        button([type=submit, class='btn btn-danger'], 'Удалить ребенка')]),
                  form([action('/init_db'), method(post)],
                       button([type=submit, class='btn btn-warning'], 'Обновить БД'))]),
             div([class='d-grid gap-2 d-md-flex justify-content-md-start'],
                 [form([action('/query_low_income'), method(post), class='d-inline'],
                       [input([type=number, name=income, placeholder='Порог зарплаты', class='form-control d-inline-block w-auto me-2']),
                        button([type=submit, class='btn btn-success'], 'Список людей с зарплатой ниже порога')]),
                  form([action('/query_young_children'), method(post), class='d-inline'],
                       [input([type=number, name=age, placeholder='Максимальный возраст', class='form-control d-inline-block w-auto me-2']),
                        button([type=submit, class='btn btn-success'], 'Список детей меньшего возраста')]),
                  form([action('/query_two_children'), method(get)],
                       button([type=submit, class='btn btn-success'], 'Семьи с двумя детьми'))])
            ])).

% Add person page
add_person_page(_Request) :-
    reply_html_page(
        [title('Добавить человека'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Добавление пользователя'),
             div([class='card'],
                 [div([class='card-body'],
                      [form([action('/add_person_submit'), method(post), class='row g-3'],
                            [div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Имя'),
                                  input([type=text, name=first_name, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Отчество'),
                                  input([type=text, name=patronymic, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Фамилия'),
                                  input([type=text, name=last_name, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Год рождения'),
                                  input([type=number, name=birth_year, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Пол'),
                                  select([name=gender, class='form-select'],
                                         [option([value='мужской'], 'Мужчина'),
                                          option([value='женский'], 'Женщина')])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Зарплата'),
                                  input([type=number, name=monthly_income, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Близнец?'),
                                  select([name=is_twin, class='form-select'],
                                         [option([value='true'], 'Да'),
                                          option([value='false'], 'Нет')])]),
                             div([class='col-12'],
                                 [input([type=submit, value='Создать', class='btn btn-primary me-2']),
                                  a([href='/', class='btn btn-secondary'], 'Назад')])])])])
            ])).

% Add person submit
add_person_submit(Request) :-
    http_parameters(Request, [
        first_name(FirstName, []),
        patronymic(Patronymic, []),
        last_name(LastName, []),
        birth_year(BirthYearAtom, []),
        gender(Gender, []),
        monthly_income(IncomeAtom, []),
        is_twin(IsTwin, [])
    ]),
    atom_number(BirthYearAtom, BirthYear),
    atom_number(IncomeAtom, Income),
    (IsTwin = 'true' -> IsTwinBool = true ; IsTwinBool = false),
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    format(atom(Query),
           'INSERT INTO person (first_name, patronymic, last_name, birth_year, gender, monthly_income, is_twin) VALUES (''~w'', ''~w'', ''~w'', ~w, ''~w'', ~w, ~w)',
           [FirstName, Patronymic, LastName, BirthYear, Gender, Income, IsTwinBool]),
    odbc_query(Connection, Query),
    odbc_disconnect(Connection),
    http_redirect(moved, '/', Request).

% Update person page
update_person_page(_Request) :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    findall(option([value=ID], PersonName),
            odbc_query(Connection,
                       'SELECT id, CONCAT(first_name, '' '', last_name) AS name FROM person',
                       row(ID, PersonName)),
            PersonOptions),
    odbc_disconnect(Connection),
    reply_html_page(
        [title('Edit Individual'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Edit Individual'),
             div([class='card'],
                 [div([class='card-body'],
                      [form([action('/update_person_submit'), method(post), class='row g-3'],
                            [div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Выбрать пользователя'),
                                  select([name=id, class='form-select'], PersonOptions)]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Имя'),
                                  input([type=text, name=first_name, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Отчество'),
                                  input([type=text, name=patronymic, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Фамилия'),
                                  input([type=text, name=last_name, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Год рождения'),
                                  input([type=number, name=birth_year, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Пол'),
                                  select([name=gender, class='form-select'],
                                         [option([value='мужской'], 'Женщина'),
                                          option([value='женский'], 'Мужчина')])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Зарплата'),
                                  input([type=number, name=monthly_income, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Близнец?'),
                                  select([name=is_twin, class='form-select'],
                                         [option([value='true'], 'Да'),
                                          option([value='false'], 'Нет')])]),
                             div([class='col-12'],
                                 [input([type=submit, value='Обновить', class='btn btn-primary me-2']),
                                  a([href='/', class='btn btn-secondary'], 'Назад')])])])])
            ])).

% Update person submit
update_person_submit(Request) :-
    http_parameters(Request, [
        id(IDAtom, []),
        first_name(FirstName, []),
        patronymic(Patronymic, []),
        last_name(LastName, []),
        birth_year(BirthYearAtom, []),
        gender(Gender, []),
        monthly_income(IncomeAtom, []),
        is_twin(IsTwin, [])
    ]),
    atom_number(IDAtom, ID),
    atom_number(BirthYearAtom, BirthYear),
    atom_number(IncomeAtom, Income),
    (IsTwin = 'true' -> IsTwinBool = true ; IsTwinBool = false),
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    format(atom(Query),
           'UPDATE person SET first_name=''~w'', patronymic=''~w'', last_name=''~w'', birth_year=~w, gender=''~w'', monthly_income=~w, is_twin=~w WHERE id=~w',
           [FirstName, Patronymic, LastName, BirthYear, Gender, Income, IsTwinBool, ID]),
    odbc_query(Connection, Query),
    odbc_disconnect(Connection),
    http_redirect(moved, '/', Request).

% Delete person
delete_person(Request) :-
    http_parameters(Request, [id(IDAtom, [])]),
    atom_number(IDAtom, ID),
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    format(atom(Query), 'DELETE FROM person WHERE id=~w', [ID]),
    odbc_query(Connection, Query),
    odbc_disconnect(Connection),
    http_redirect(moved, '/', Request).

% Add family page
add_family_page(_Request) :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    findall(option([value=ID], Name),
            odbc_query(Connection,
                       'SELECT id, CONCAT(first_name, '' '', last_name) AS name FROM person WHERE gender=''мужской''',
                       row(ID, Name)),
            HusbandOptions),
    findall(option([value=ID], Name),
            odbc_query(Connection,
                       'SELECT id, CONCAT(first_name, '' '', last_name) AS name FROM person WHERE gender=''женский''',
                       row(ID, Name)),
            WifeOptions),
    odbc_disconnect(Connection),
    reply_html_page(
        [title('Добавление семьи'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Добавление семьи'),
             div([class='card'],
                 [div([class='card-body'],
                      [form([action('/add_family_submit'), method(post), class='row g-3'],
                            [div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Муж'),
                                  select([name=husband_id, class='form-select'], HusbandOptions)]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Жена'),
                                  select([name=wife_id, class='form-select'], WifeOptions)]),
                             div([class='col-12'],
                                 [input([type=submit, value='Создать', class='btn btn-primary me-2']),
                                  a([href='/', class='btn btn-secondary'], 'Назад')])])])])
            ])).

% Add family submit
add_family_submit(Request) :-
    http_parameters(Request, [husband_id(HusbandIDAtom, []), wife_id(WifeIDAtom, [])]),
    atom_number(HusbandIDAtom, HusbandID),
    atom_number(WifeIDAtom, WifeID),
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    format(atom(Query), 'INSERT INTO family (husband_id, wife_id) VALUES (~w, ~w)', [HusbandID, WifeID]),
    odbc_query(Connection, Query),
    odbc_disconnect(Connection),
    http_redirect(moved, '/', Request).

% Delete family
delete_family(Request) :-
    http_parameters(Request, [family_id(FamilyIDAtom, [])]),
    atom_number(FamilyIDAtom, FamilyID),
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    format(atom(Query), 'DELETE FROM family WHERE family_id=~w', [FamilyID]),
    odbc_query(Connection, Query),
    odbc_disconnect(Connection),
    http_redirect(moved, '/', Request).

% Add child page
add_child_page(_Request) :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    findall(option([value=FamilyID], FamilyID),
            odbc_query(Connection, 'SELECT family_id FROM family', row(FamilyID)),
            FamilyOptions),
    findall(option([value=ID], Name),
            odbc_query(Connection,
                       'SELECT id, CONCAT(first_name, '' '', last_name) AS name FROM person',
                       row(ID, Name)),
            ChildOptions),
    odbc_disconnect(Connection),
    reply_html_page(
        [title('Добавление ребенка'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Добавление ребенка'),
             div([class='card'],
                 [div([class='card-body'],
                      [form([action('/add_child_submit'), method(post), class='row g-3'],
                            [div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'ID семьи'),
                                  select([name=family_id, class='form-select'], FamilyOptions)]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Ребенок'),
                                  select([name=child_id, class='form-select'], ChildOptions)]),
                             div([class='col-12'],
                                 [input([type=submit, value='Добавить', class='btn btn-primary me-2']),
                                  a([href='/', class='btn btn-secondary'], 'Назад')])])])])
            ])).

% Add child submit
add_child_submit(Request) :-
    http_parameters(Request, [family_id(FamilyIDAtom, []), child_id(ChildIDAtom, [])]),
    atom_number(FamilyIDAtom, FamilyID),
    atom_number(ChildIDAtom, ChildID),
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    format(atom(Query), 'INSERT INTO children (family_id, child_id) VALUES (~w, ~w)', [FamilyID, ChildID]),
    odbc_query(Connection, Query),
    odbc_disconnect(Connection),
    http_redirect(moved, '/', Request).

% Delete child
delete_child(Request) :-
    http_parameters(Request, [family_id(FamilyIDAtom, []), child_id(ChildIDAtom, [])]),
    atom_number(FamilyIDAtom, FamilyID),
    atom_number(ChildIDAtom, ChildID),
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    format(atom(Query), 'DELETE FROM children WHERE family_id=~w AND child_id=~w', [FamilyID, ChildID]),
    odbc_query(Connection, Query),
    odbc_disconnect(Connection),
    http_redirect(moved, '/', Request).

% Query 1: Find people with income less than specified
query_low_income(Request) :-
    http_parameters(Request, [income(IncomeAtom, [])]),
    atom_number(IncomeAtom, Income),
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    format(atom(Query), 'SELECT first_name, patronymic, last_name, monthly_income FROM person WHERE monthly_income < ~w', [Income]),
    findall([FirstName, Patronymic, LastName, MonthlyIncome],
            odbc_query(Connection, Query, row(FirstName, Patronymic, LastName, MonthlyIncome)),
            People),
    odbc_disconnect(Connection),
    (People = [] -> format(atom(Message), 'Не нашлось людей с доходом ниже ~w рублей.', [Income]) ; Message = ''),
    findall(tr([],
               [td([], FirstName),
                td([], Patronymic),
                td([], LastName),
                td([], MonthlyIncome)]),
            member([FirstName, Patronymic, LastName, MonthlyIncome], People),
            Rows),
    reply_html_page(
        [title('Малый доход'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Люди с меньшим доходом'),
             p([class='text-muted'], Message),
             div([class='card'],
                 [div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'Имя'),
                                         th([], 'Отчество'),
                                         th([], 'Отество'),
                                         th([], 'Доход')])]),
                              tbody([], Rows)])])]),
             a([href='/', class='btn btn-secondary mt-3'], 'Назад')
            ])).

% Query 2: Find children younger than specified age
query_young_children(Request) :-
    http_parameters(Request, [age(AgeAtom, [])]),
    atom_number(AgeAtom, Age),
    CurrentYear is 2025,
    MinBirthYear is CurrentYear - Age,
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    format(atom(Query), 'SELECT p.first_name, p.patronymic, p.last_name, p.birth_year
                         FROM person p
                         JOIN children c ON p.id = c.child_id
                         WHERE p.birth_year > ~w', [MinBirthYear]),
    findall([FirstName, Patronymic, LastName, BirthYear],
            odbc_query(Connection, Query, row(FirstName, Patronymic, LastName, BirthYear)),
            Children),
    odbc_disconnect(Connection),
    (Children = [] -> format(atom(Message), 'No dependents younger than ~w found.', [Age]) ; Message = ''),
    findall(tr([],
               [td([], FirstName),
                td([], Patronymic),
                td([], LastName),
                td([], BirthYear)]),
            member([FirstName, Patronymic, LastName, BirthYear], Children),
            Rows),
    reply_html_page(
        [title('Young Дети'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Дети Younger Than Specified Age'),
             p([class='text-muted'], Message),
             div([class='card'],
                 [div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'Имя'),
                                         th([], 'Отчество'),
                                         th([], 'Фамилия'),
                                         th([], 'Год рождения')])]),
                              tbody([], Rows)])])]),
             a([href='/', class='btn btn-secondary mt-3'], 'Назад')
            ])).

% Query 3: Find unemployed wives born after specified year
query_unemployed_wives(Request) :-
    http_parameters(Request, [birth_year(BirthYearAtom, [])]),
    atom_number(BirthYearAtom, BirthYear),
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    format(atom(Query), 'SELECT p.first_name, p.patronymic, p.last_name, p.birth_year
                         FROM person p
                         JOIN family f ON p.id = f.wife_id
                         WHERE p.gender = ''женский'' AND p.monthly_income = 0 AND p.birth_year > ~w', [BirthYear]),
    findall([FirstName, Patronymic, LastName, BirthYear],
            odbc_query(Connection, Query, row(FirstName, Patronymic, LastName, BirthYear)),
            Wives),
    odbc_disconnect(Connection),
    (Wives = [] -> format(atom(Message), 'No non-working spouses born after ~w found.', [BirthYear]) ; Message = ''),
    findall(tr([],
               [td([], FirstName),
                td([], Patronymic),
                td([], LastName),
                td([], BirthYear)]),
            member([FirstName, Patronymic, LastName, BirthYear], Wives),
            Rows),
    reply_html_page(
        [title('Non-Working Spouses'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Non-Working Spouses Born After Specified Year'),
             p([class='text-muted'], Message),
             div([class='card'],
                 [div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'Имя'),
                                         th([], 'Отчество'),
                                         th([], 'Фамилия'),
                                         th([], 'Год рождения')])]),
                              tbody([], Rows)])])]),
             a([href='/', class='btn btn-secondary mt-3'], 'Назад')
            ])).

% Query 4: Find children with parents' age difference exceeding specified value
query_parent_age_diff(Request) :-
    http_parameters(Request, [age_diff(AgeDiffAtom, [])]),
    atom_number(AgeDiffAtom, AgeDiff),
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    format(atom(Query), 'SELECT p.first_name, p.patronymic, p.last_name, p.birth_year
                         FROM person p
                         JOIN children c ON p.id = c.child_id
                         JOIN family f ON c.family_id = f.family_id
                         JOIN person h ON f.husband_id = h.id
                         JOIN person w ON f.wife_id = w.id
                         WHERE ABS(h.birth_year - w.birth_year) > ~w', [AgeDiff]),
    findall([FirstName, Patronymic, LastName, BirthYear],
            odbc_query(Connection, Query, row(FirstName, Patronymic, LastName, BirthYear)),
            Children),
    odbc_disconnect(Connection),
    (Children = [] -> format(atom(Message), 'No dependents with parents'' age gap exceeding ~w found.', [AgeDiff]) ; Message = ''),
    findall(tr([],
               [td([], FirstName),
                td([], Patronymic),
                td([], LastName),
                td([], BirthYear)]),
            member([FirstName, Patronymic, LastName, BirthYear], Children),
            Rows),
    reply_html_page(
        [title('Дети с разницой в n лет с родителями'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Дети с разницой в возрасте c родителями'),
             p([class='text-muted'], Message),
             div([class='card'],
                 [div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'Имя'),
                                         th([], 'Отчество'),
                                         th([], 'Фамилия'),
                                         th([], 'Год рождения')])]),
                              tbody([], Rows)])])]),
             a([href='/', class='btn btn-secondary mt-3'], 'Обратно')
            ])).

% Query 5: Count families with exactly two children
query_two_children(_Request) :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    odbc_query(Connection,
               'SELECT COUNT(DISTINCT f.family_id) AS count
                FROM family f
                JOIN children c ON f.family_id = c.family_id
                GROUP BY f.family_id
                HAVING COUNT(c.child_id) = 2',
               row(Count)),
    odbc_disconnect(Connection),
    format(atom(Message), 'Количество семей с двумя детьми: ~w', [Count]),
    reply_html_page(
        [title('Количество семей с двумя детьми'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Количество семей с двумя детьми'),
             p([class='text-muted'], Message),
             a([href='/', class='btn btn-secondary mt-3'], 'Назад')
            ])).
