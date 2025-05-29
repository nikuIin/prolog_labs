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
                    (''John'', ''James'', ''Smith'', 1978, ''мужской'', 75000, false),
                    (''Emma'', ''Jane'', ''Smith'', 1980, ''женский'', 70000, false),
                    (''Liam'', ''John'', ''Smith'', 2012, ''мужской'', 0, true),
                    (''Olivia'', ''Emma'', ''Smith'', 2012, ''женский'', 0, true),
                    (''Michael'', ''David'', ''Brown'', 1970, ''мужской'', 80000, false),
                    (''Sophia'', ''Marie'', ''Brown'', 1972, ''женский'', 85000, false),
                    (''Ethan'', ''Michael'', ''Brown'', 2008, ''мужской'', 0, false),
                    (''Ava'', ''Louise'', ''Wilson'', 1987, ''женский'', 0, false),
                    (''William'', ''Thomas'', ''Wilson'', 1985, ''мужской'', 90000, false),
                    (''Isabella'', ''Ava'', ''Wilson'', 2016, ''женский'', 0, false)'),
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
        [title('Household Registry'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-5 fw-bold mb-4 text-primary'], 'Household Registry'),
             div([class='card mb-4'],
                 [div([class='card-header bg-primary text-white'], 'Individuals'),
                  div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'ID'),
                                         th([], 'First Name'),
                                         th([], 'Middle Name'),
                                         th([], 'Last Name'),
                                         th([], 'Birth Year'),
                                         th([], 'Gender'),
                                         th([], 'Income'),
                                         th([], 'Is Twin')])]),
                              tbody([], PersonRows)])])]),
             div([class='card mb-4'],
                 [div([class='card-header bg-primary text-white'], 'Households'),
                  div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'Household ID'),
                                         th([], 'Husband'),
                                         th([], 'Wife')])]),
                              tbody([], FamilyRows)])])]),
             div([class='card mb-4'],
                 [div([class='card-header bg-primary text-white'], 'Dependents'),
                  div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'Household ID'),
                                         th([], 'Child ID'),
                                         th([], 'Child Last Name')])]),
                              tbody([], ChildRows)])])]),
             div([class='d-grid gap-2 d-md-flex justify-content-md-start mb-4'],
                 [form([action('/add_person'), method(get)],
                       button([type=submit, class='btn btn-primary'], 'Add New Individual')),
                  form([action('/update_person'), method(get)],
                       button([type=submit, class='btn btn-primary'], 'Edit Individual')),
                  form([action('/delete_person'), method(post), class='d-inline'],
                       [input([type=text, name=id, placeholder='Enter ID', class='form-control d-inline-block w-auto me-2']),
                        button([type=submit, class='btn btn-danger'], 'Remove Individual')]),
                  form([action('/add_family'), method(get)],
                       button([type=submit, class='btn btn-primary'], 'Create Household')),
                  form([action('/delete_family'), method(post), class='d-inline'],
                       [input([type=text, name=family_id, placeholder='Household ID', class='form-control d-inline-block w-auto me-2']),
                        button([type=submit, class='btn btn-danger'], 'Dissolve Household')]),
                  form([action('/add_child'), method(get)],
                       button([type=submit, class='btn btn-primary'], 'Add Dependent')),
                  form([action('/delete_child'), method(post), class='d-inline'],
                       [input([type=text, name=family_id, placeholder='Household ID', class='form-control d-inline-block w-auto me-2']),
                        input([type=text, name=child_id, placeholder='Child ID', class='form-control d-inline-block w-auto me-2']),
                        button([type=submit, class='btn btn-danger'], 'Remove Dependent')]),
                  form([action('/init_db'), method(post)],
                       button([type=submit, class='btn btn-warning'], 'Reset Registry'))]),
             div([class='d-grid gap-2 d-md-flex justify-content-md-start'],
                 [form([action('/query_low_income'), method(post), class='d-inline'],
                       [input([type=number, name=income, placeholder='Income Threshold', class='form-control d-inline-block w-auto me-2']),
                        button([type=submit, class='btn btn-success'], 'List Low Earners')]),
                  form([action('/query_young_children'), method(post), class='d-inline'],
                       [input([type=number, name=age, placeholder='Max Age', class='form-control d-inline-block w-auto me-2']),
                        button([type=submit, class='btn btn-success'], 'List Young Dependents')]),
                  form([action('/query_unemployed_wives'), method(post), class='d-inline'],
                       [input([type=number, name=birth_year, placeholder='Birth Year After', class='form-control d-inline-block w-auto me-2']),
                        button([type=submit, class='btn btn-success'], 'List Non-Working Spouses')]),
                  form([action('/query_parent_age_diff'), method(post), class='d-inline'],
                       [input([type=number, name=age_diff, placeholder='Age Gap', class='form-control d-inline-block w-auto me-2']),
                        button([type=submit, class='btn btn-success'], 'List Dependents by Parent Age Gap')]),
                  form([action('/query_two_children'), method(get)],
                       button([type=submit, class='btn btn-success'], 'Count Households with Two Dependents'))])
            ])).

% Add person page
add_person_page(_Request) :-
    reply_html_page(
        [title('Add Individual'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Add New Individual'),
             div([class='card'],
                 [div([class='card-body'],
                      [form([action('/add_person_submit'), method(post), class='row g-3'],
                            [div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'First Name'),
                                  input([type=text, name=first_name, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Middle Name'),
                                  input([type=text, name=patronymic, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Last Name'),
                                  input([type=text, name=last_name, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Birth Year'),
                                  input([type=number, name=birth_year, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Gender'),
                                  select([name=gender, class='form-select'],
                                         [option([value='мужской'], 'Male'),
                                          option([value='женский'], 'Female')])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Monthly Income'),
                                  input([type=number, name=monthly_income, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Is Twin'),
                                  select([name=is_twin, class='form-select'],
                                         [option([value='true'], 'Yes'),
                                          option([value='false'], 'No')])]),
                             div([class='col-12'],
                                 [input([type=submit, value='Submit', class='btn btn-primary me-2']),
                                  a([href='/', class='btn btn-secondary'], 'Return')])])])])
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
                                 [label([class='form-label fw-bold'], 'Select Individual'),
                                  select([name=id, class='form-select'], PersonOptions)]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'First Name'),
                                  input([type=text, name=first_name, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Middle Name'),
                                  input([type=text, name=patronymic, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Last Name'),
                                  input([type=text, name=last_name, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Birth Year'),
                                  input([type=number, name=birth_year, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Gender'),
                                  select([name=gender, class='form-select'],
                                         [option([value='мужской'], 'Male'),
                                          option([value='женский'], 'Female')])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Monthly Income'),
                                  input([type=number, name=monthly_income, class='form-control'])]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Is Twin'),
                                  select([name=is_twin, class='form-select'],
                                         [option([value='true'], 'Yes'),
                                          option([value='false'], 'No')])]),
                             div([class='col-12'],
                                 [input([type=submit, value='Update', class='btn btn-primary me-2']),
                                  a([href='/', class='btn btn-secondary'], 'Return')])])])])
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
        [title('Create Household'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Create New Household'),
             div([class='card'],
                 [div([class='card-body'],
                      [form([action('/add_family_submit'), method(post), class='row g-3'],
                            [div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Husband'),
                                  select([name=husband_id, class='form-select'], HusbandOptions)]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Wife'),
                                  select([name=wife_id, class='form-select'], WifeOptions)]),
                             div([class='col-12'],
                                 [input([type=submit, value='Create', class='btn btn-primary me-2']),
                                  a([href='/', class='btn btn-secondary'], 'Return')])])])])
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
        [title('Add Dependent'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Add New Dependent'),
             div([class='card'],
                 [div([class='card-body'],
                      [form([action('/add_child_submit'), method(post), class='row g-3'],
                            [div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Household ID'),
                                  select([name=family_id, class='form-select'], FamilyOptions)]),
                             div([class='col-md-6'],
                                 [label([class='form-label fw-bold'], 'Child'),
                                  select([name=child_id, class='form-select'], ChildOptions)]),
                             div([class='col-12'],
                                 [input([type=submit, value='Add', class='btn btn-primary me-2']),
                                  a([href='/', class='btn btn-secondary'], 'Return')])])])])
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
    (People = [] -> format(atom(Message), 'No individuals with income below ~w found.', [Income]) ; Message = ''),
    findall(tr([],
               [td([], FirstName),
                td([], Patronymic),
                td([], LastName),
                td([], MonthlyIncome)]),
            member([FirstName, Patronymic, LastName, MonthlyIncome], People),
            Rows),
    reply_html_page(
        [title('Low Earners'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Individuals with Low Income'),
             p([class='text-muted'], Message),
             div([class='card'],
                 [div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'First Name'),
                                         th([], 'Middle Name'),
                                         th([], 'Last Name'),
                                         th([], 'Income')])]),
                              tbody([], Rows)])])]),
             a([href='/', class='btn btn-secondary mt-3'], 'Return')
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
        [title('Young Dependents'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Dependents Younger Than Specified Age'),
             p([class='text-muted'], Message),
             div([class='card'],
                 [div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'First Name'),
                                         th([], 'Middle Name'),
                                         th([], 'Last Name'),
                                         th([], 'Birth Year')])]),
                              tbody([], Rows)])])]),
             a([href='/', class='btn btn-secondary mt-3'], 'Return')
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
                                        [th([], 'First Name'),
                                         th([], 'Middle Name'),
                                         th([], 'Last Name'),
                                         th([], 'Birth Year')])]),
                              tbody([], Rows)])])]),
             a([href='/', class='btn btn-secondary mt-3'], 'Return')
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
        [title('Dependents by Parent Age Gap'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Dependents with Parents'' Age Gap Exceeding Specified Value'),
             p([class='text-muted'], Message),
             div([class='card'],
                 [div([class='card-body'],
                      [table([class='table table-striped table-hover'],
                             [thead([class='table-light'],
                                    [tr([],
                                        [th([], 'First Name'),
                                         th([], 'Middle Name'),
                                         th([], 'Last Name'),
                                         th([], 'Birth Year')])]),
                              tbody([], Rows)])])]),
             a([href='/', class='btn btn-secondary mt-3'], 'Return')
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
    format(atom(Message), 'Number of households with exactly two dependents: ~w', [Count]),
    reply_html_page(
        [title('Households with Two Dependents'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css'])],
        div([class='container mt-4'],
            [h1([class='display-6 fw-bold mb-4 text-primary'], 'Households with Exactly Two Dependents'),
             p([class='text-muted'], Message),
             a([href='/', class='btn btn-secondary mt-3'], 'Return')
            ])).
