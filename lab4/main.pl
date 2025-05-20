:- encoding(utf8).
:- use_module(library(http/http_server)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(odbc)).

% Устранение предупреждений о разрыве предикатов
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
:- http_handler(root(query_twins), query_twins, [method(get)]).
:- http_handler(root(query_children_2010), query_children_2010, [method(get)]).
:- http_handler(root(query_wives), query_wives, [method(get)]).
:- http_handler(root(query_two_children), query_two_children, [method(get)]).
:- http_handler(root(query_oldest_child), query_oldest_child, [method(get)]).

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
                    (''Иван'', ''Петрович'', ''Иванов'', 1980, ''мужской'', 90000, true),
                    (''Мария'', ''Ивановна'', ''Иванова'', 1982, ''женский'', 85000, false),
                    (''Анна'', ''Ивановна'', ''Иванова'', 2010, ''женский'', 0, true),
                    (''Алексей'', ''Иванович'', ''Иванов'', 2010, ''мужской'', 0, true),
                    (''Елена'', ''Сергеевна'', ''Петрова'', 1975, ''женский'', 95000, false),
                    (''Сергей'', ''Александрович'', ''Петров'', 1973, ''мужской'', 100000, false),
                    (''Михаил'', ''Сергеевич'', ''Петров'', 2005, ''мужской'', 0, false),
                    (''Ольга'', ''Дмитриевна'', ''Сидорова'', 1990, ''женский'', 88000, false),
                    (''Дмитрий'', ''Васильевич'', ''Сидоров'', 1988, ''мужской'', 92000, false),
                    (''Ксения'', ''Дмитриевна'', ''Сидорова'', 2015, ''женский'', 0, false)'),
    odbc_query(Connection,
               'INSERT INTO family (husband_id, wife_id) VALUES
                    (1, 2),
                    (6, 5),
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
    % Persons table
    findall(tr([class='border-b'],
               [td([class='px-4 py-2'], ID),
                td([class='px-4 py-2'], FirstName),
                td([class='px-4 py-2'], Patronymic),
                td([class='px-4 py-2'], LastName),
                td([class='px-4 py-2'], BirthYear),
                td([class='px-4 py-2'], Gender),
                td([class='px-4 py-2'], Income),
                td([class='px-4 py-2'], IsTwin)]),
            odbc_query(Connection,
                       'SELECT id, first_name, patronymic, last_name, birth_year, gender, monthly_income, is_twin FROM person',
                       row(ID, FirstName, Patronymic, LastName, BirthYear, Gender, Income, IsTwin)),
            PersonRows),
    % Families table
    findall(tr([class='border-b'],
               [td([class='px-4 py-2'], FamilyID),
                td([class='px-4 py-2'], HusbandName),
                td([class='px-4 py-2'], WifeName)]),
            odbc_query(Connection,
                       'SELECT f.family_id, h.last_name AS husband, w.last_name AS wife
                        FROM family f
                        JOIN person h ON f.husband_id = h.id
                        JOIN person w ON f.wife_id = w.id',
                       row(FamilyID, HusbandName, WifeName)),
            FamilyRows),
    % Children table
    findall(tr([class='border-b'],
               [td([class='px-4 py-2'], FamilyID),
                td([class='px-4 py-2'], ChildName)]),
            odbc_query(Connection,
                       'SELECT c.family_id, p.last_name
                        FROM children c
                        JOIN person p ON c.child_id = p.id',
                       row(FamilyID, ChildName)),
            ChildRows),
    odbc_disconnect(Connection),
    reply_html_page(
        [title('Family Database'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css'])],
        div([class='container mx-auto p-4'],
            [h1([class='text-3xl font-bold mb-4'], 'Family Database'),
             % Persons section
             h2([class='text-2xl font-semibold mt-6 mb-2'], 'Persons'),
             table([class='w-full border-collapse border border-gray-300'],
                   [tr([class='bg-gray-100'],
                        [th([class='px-4 py-2'], 'ID'),
                         th([class='px-4 py-2'], 'First Name'),
                         th([class='px-4 py-2'], 'Patronymic'),
                         th([class='px-4 py-2'], 'Last Name'),
                         th([class='px-4 py-2'], 'Birth Year'),
                         th([class='px-4 py-2'], 'Gender'),
                         th([class='px-4 py-2'], 'Income'),
                         th([class='px-4 py-2'], 'Is Twin')])|PersonRows]),
             % Families section
             h2([class='text-2xl font-semibold mt-6 mb-2'], 'Families'),
             table([class='w-full border-collapse border border-gray-300'],
                   [tr([class='bg-gray-100'],
                        [th([class='px-4 py-2'], 'Family ID'),
                         th([class='px-4 py-2'], 'Husband'),
                         th([class='px-4 py-2'], 'Wife')])|FamilyRows]),
             % Children section
             h2([class='text-2xl font-semibold mt-6 mb-2'], 'Children'),
             table([class='w-full border-collapse border border-gray-300'],
                   [tr([class='bg-gray-100'],
                        [th([class='px-4 py-2'], 'Family ID'),
                         th([class='px-4 py-2'], 'Child')])|ChildRows]),
             % Action buttons
             div([class='mt-6 flex flex-wrap gap-4'],
                 [form([action('/add_person'), method(get), class='inline-block'],
                       button([type=submit, class='bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600'], 'Add Person')),
                  form([action('/update_person'), method(get), class='inline-block'],
                       button([type=submit, class='bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600'], 'Update Person')),
                  form([action('/delete_person'), method(post), class='inline-block'],
                       [input([type=text, name=id, placeholder='Person ID', class='border p-2 mr-2']),
                        button([type=submit, class='bg-red-500 text-white px-4 py-2 rounded hover:bg-red-600'], 'Delete Person')]),
                  form([action('/add_family'), method(get), class='inline-block'],
                       button([type=submit, class='bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600'], 'Add Family')),
                  form([action('/delete_family'), method(post), class='inline-block'],
                       [input([type=text, name=family_id, placeholder='Family ID', class='border p-2 mr-2']),
                        button([type=submit, class='bg-red-500 text-white px-4 py-2 rounded hover:bg-red-600'], 'Delete Family')]),
                  form([action('/add_child'), method(get), class='inline-block'],
                       button([type=submit, class='bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600'], 'Add Child')),
                  form([action('/delete_child'), method(post), class='inline-block'],
                       [input([type=text, name=family_id, placeholder='Family ID', class='border p-2 mr-2']),
                        input([type=text, name=child_id, placeholder='Child ID', class='border p-2 mr-2']),
                        button([type=submit, class='bg-red-500 text-white px-4 py-2 rounded hover:bg-red-600'], 'Delete Child')]),
                  form([action('/init_db'), method(post), class='inline-block'],
                       button([type=submit, class='bg-yellow-500 text-white px-4 py-2 rounded hover:bg-yellow-600'], 'Reset Database'))]),
             % Query buttons
             div([class='mt-6 flex flex-wrap gap-4'],
                 [form([action('/query_twins'), method(get), class='inline-block'],
                       button([type=submit, class='bg-green-500 text-white px-4 py-2 rounded hover:bg-green-600'], 'Find Twins')),
                  form([action('/query_children_2010'), method(get), class='inline-block'],
                       button([type=submit, class='bg-green-500 text-white px-4 py-2 rounded hover:bg-green-600'], 'Children Born in 2010')),
                  form([action('/query_wives'), method(get), class='inline-block'],
                       button([type=submit, class='bg-green-500 text-white px-4 py-2 rounded hover:bg-green-600'], 'Working Wives > 85000')),
                  form([action('/query_two_children'), method(get), class='inline-block'],
                       button([type=submit, class='bg-green-500 text-white px-4 py-2 rounded hover:bg-green-600'], 'Families with 2 Children')),
                  form([action('/query_oldest_child'), method(get), class='inline-block'],
                       button([type=submit, class='bg-green-500 text-white px-4 py-2 rounded hover:bg-green-600'], 'Oldest Child'))])
            ])).

% Add person page
add_person_page(_Request) :-
    reply_html_page(
        [title('Add Person'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css'])],
        div([class='container mx-auto p-4'],
            [h1([class='text-2xl font-bold mb-4'], 'Add Person'),
             form([action('/add_person_submit'), method(post), class='space-y-4'],
                  [div([class='flex flex-col'],
                       [label([class='font-semibold'], 'First Name'),
                        input([type=text, name=first_name, class='border p-2 rounded'])]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Patronymic'),
                        input([type=text, name=patronymic, class='border p-2 rounded'])]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Last Name'),
                        input([type=text, name=last_name, class='border p-2 rounded'])]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Birth Year'),
                        input([type=number, name=birth_year, class='border p-2 rounded'])]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Gender'),
                        select([name=gender, class='border p-2 rounded'],
                               [option([value='мужской'], 'Male'),
                                option([value='женский'], 'Female')])])),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Monthly Income'),
                        input([type=number, name=monthly_income, class='border p-2 rounded'])]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Is Twin'),
                        select([name=is_twin, class='border p-2 rounded'],
                               [option([value='true'], 'Yes'),
                                option([value='false'], 'No')])])),
                   div([class='flex space-x-4'],
                       [input([type=submit, value='Add', class='bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600']),
                        a([href='/', class='bg-gray-500 text-white px-4 py-2 rounded hover:bg-gray-600'], 'Back')])])
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
        [title('Update Person'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css'])],
        div([class='container mx-auto p-4'],
            [h1([class='text-2xl font-bold mb-4'], 'Update Person'),
             form([action('/update_person_submit'), method(post), class='space-y-4'],
                  [div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Select Person'),
                        select([name=id, class='border p-2 rounded'], PersonOptions)]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'First Name'),
                        input([type=text, name=first_name, class='border p-2 rounded'])]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Patronymic'),
                        input([type=text, name=patronymic, class='border p-2 rounded'])]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Last Name'),
                        input([type=text, name=last_name, class='border p-2 rounded'])]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Birth Year'),
                        input([type=number, name=birth_year, class='border p-2 rounded'])]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Gender'),
                        select([name=gender, class='border p-2 rounded'],
                               [option([value='мужской'], 'Male'),
                                option([value='женский'], 'Female')])])),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Monthly Income'),
                        input([type=number, name=monthly_income, class='border p-2 rounded'])]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Is Twin'),
                        select([name=is_twin, class='border p-2 rounded'],
                               [option([value='true'], 'Yes'),
                                option([value='false'], 'No')])])),
                   div([class='flex space-x-4'],
                       [input([type=submit, value='Update', class='bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600']),
                        a([href='/', class='bg-gray-500 text-white px-4 py-2 rounded hover:bg-gray-600'], 'Back')])])
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
        [title('Add Family'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css'])],
        div([class='container mx-auto p-4'],
            [h1([class='text-2xl font-bold mb-4'], 'Add Family'),
             form([action('/add_family_submit'), method(post), class='space-y-4'],
                  [div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Husband'),
                        select([name=husband_id, class='border p-2 rounded'], HusbandOptions)]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Wife'),
                        select([name=wife_id, class='border p-2 rounded'], WifeOptions)]),
                   div([class='flex space-x-4'],
                       [input([type=submit, value='Add', class='bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600']),
                        a([href='/', class='bg-gray-500 text-white px-4 py-2 rounded hover:bg-gray-600'], 'Back')])])
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
        [title('Add Child'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css'])],
        div([class='container mx-auto p-4'],
            [h1([class='text-2xl font-bold mb-4'], 'Add Child'),
             form([action('/add_child_submit'), method(post), class='space-y-4'],
                  [div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Family ID'),
                        select([name=family_id, class='border p-2 rounded'], FamilyOptions)]),
                   div([class='flex flex-col'],
                       [label([class='font-semibold'], 'Child'),
                        select([name=child_id, class='border p-2 rounded'], ChildOptions)]),
                   div([class='flex space-x-4'],
                       [input([type=submit, value='Add', class='bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600']),
                        a([href='/', class='bg-gray-500 text-white px-4 py-2 rounded hover:bg-gray-600'], 'Back')])])
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

% Query 1: Find twins
query_twins(_Request) :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    findall([FirstName, Patronymic, LastName],
            odbc_query(Connection,
                       'SELECT first_name, patronymic, last_name FROM person WHERE is_twin = TRUE',
                       row(FirstName, Patronymic, LastName)),
            Twins),
    odbc_disconnect(Connection),
    (Twins = [] -> Message = 'No twins found.' ; Message = ''),
    findall(tr([class='border-b'],
               [td([class='px-4 py-2'], FirstName),
                td([class='px-4 py-2'], Patronymic),
                td([class='px-4 py-2'], LastName)]),
            member([FirstName, Patronymic, LastName], Twins),
            Rows),
    reply_html_page(
        [title('Find Twins'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css'])],
        div([class='container mx-auto p-4'],
            [h1([class='text-2xl font-bold mb-4'], 'Twins'),
             p([class='text-gray-600'], Message),
             table([class='w-full border-collapse border border-gray-300'],
                   [tr([class='bg-gray-100'],
                        [th([class='px-4 py-2'], 'First Name'),
                         th([class='px-4 py-2'], 'Patronymic'),
                         th([class='px-4 py-2'], 'Last Name')])|Rows]),
             a([href='/', class='bg-gray-500 text-white px-4 py-2 rounded hover:bg-gray-600 mt-4 inline-block'], 'Back')
            ])).

% Query 2: Find children born in 2010
query_children_2010(_Request) :-
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
    (Children = [] -> Message = 'No children born in 2010 found.' ; Message = ''),
    findall(tr([class='border-b'],
               [td([class='px-4 py-2'], FirstName),
                td([class='px-4 py-2'], Patronymic),
                td([class='px-4 py-2'], LastName)]),
            member([FirstName, Patronymic, LastName], Children),
            Rows),
    reply_html_page(
        [title('Children Born in 2010'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css'])],
        div([class='container mx-auto p-4'],
            [h1([class='text-2xl font-bold mb-4'], 'Children Born in 2010'),
             p([class='text-gray-600'], Message),
             table([class='w-full border-collapse border border-gray-300'],
                   [tr([class='bg-gray-100'],
                        [th([class='px-4 py-2'], 'First Name'),
                         th([class='px-4 py-2'], 'Patronymic'),
                         th([class='px-4 py-2'], 'Last Name')])|Rows]),
             a([href='/', class='bg-gray-500 text-white px-4 py-2 rounded hover:bg-gray-600 mt-4 inline-block'], 'Back')
            ])).

% Query 3: Find working wives with income > 85000
query_wives(_Request) :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    findall([FirstName, Patronymic, LastName],
            odbc_query(Connection,
                       'SELECT p.first_name, p.patronymic, p.last_name
                        FROM person p
                        JOIN family f ON p.id = f.wife_id
                        WHERE p.gender = ''женский'' AND p.monthly_income > 80000',
                       row(FirstName, Patronymic, LastName)),
            Wives),
    odbc_disconnect(Connection),
    (Wives = [] -> Message = 'No working wives with income > 85000 found.' ; Message = ''),
    findall(tr([class='border-b'],
               [td([class='px-4 py-2'], FirstName),
                td([class='px-4 py-2'], Patronymic),
                td([class='px-4 py-2'], LastName)]),
            member([FirstName, Patronymic, LastName], Wives),
            Rows),
    reply_html_page(
        [title('Working Wives'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css'])],
        div([class='container mx-auto p-4'],
            [h1([class='text-2xl font-bold mb-4'], 'Working Wives with Income > 85000'),
             p([class='text-gray-600'], Message),
             table([class='w-full border-collapse border border-gray-300'],
                   [tr([class='bg-gray-100'],
                        [th([class='px-4 py-2'], 'First Name'),
                         th([class='px-4 py-2'], 'Patronymic'),
                         th([class='px-4 py-2'], 'Last Name')])|Rows]),
             a([href='/', class='bg-gray-500 text-white px-4 py-2 rounded hover:bg-gray-600 mt-4 inline-block'], 'Back')
            ])).

% Query 4: Find families with 2 children
query_two_children(_Request) :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    findall([LastName],
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
    (Families = [] -> Message = 'No families with 2 children found.' ; Message = ''),
    findall(tr([class='border-b'],
               [td([class='px-4 py-2'], LastName)]),
            member([LastName], Families),
            Rows),
    reply_html_page(
        [title('Families with 2 Children'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css'])],
        div([class='container mx-auto p-4'],
            [h1([class='text-2xl font-bold mb-4'], 'Families with 2 Children'),
             p([class='text-gray-600'], Message),
             table([class='w-full border-collapse border border-gray-300'],
                   [tr([class='bg-gray-100'],
                        [th([class='px-4 py-2'], 'Last Name')])|Rows]),
             a([href='/', class='bg-gray-500 text-white px-4 py-2 rounded hover:bg-gray-600 mt-4 inline-block'], 'Back')
            ])).

% Query 5: Find oldest child
query_oldest_child(_Request) :-
    odbc_connect('SWI-Prolog Discourse', Connection, []),
    (odbc_query(Connection,
                'SELECT p.first_name, p.patronymic, p.last_name, p.birth_year
                 FROM person p
                 JOIN children c ON p.id = c.child_id
                 ORDER BY p.birth_year ASC
                 LIMIT 1',
                row(FirstName, Patronymic, LastName, BirthYear)) ->
        Rows = [tr([class='border-b'],
                   [td([class='px-4 py-2'], FirstName),
                    td([class='px-4 py-2'], Patronymic),
                    td([class='px-4 py-2'], LastName),
                    td([class='px-4 py-2'], BirthYear)]),
        Message = '' ;
        Rows = [], Message = 'No children found.'),
    odbc_disconnect(Connection),
    reply_html_page(
        [title('Oldest Child'),
         link([rel='stylesheet', href='https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css'])],
        div([class='container mx-auto p-4'],
            [h1([class='text-2xl font-bold mb-4'], 'Oldest Child'),
             p([class='text-gray-600'], Message),
             table([class='w-full border-collapse border border-gray-300'],
                   [tr([class='bg-gray-100'],
                        [th([class='px-4 py-2'], 'First Name'),
                         th([class='px-4 py-2'], 'Patronymic'),
                         th([class='px-4 py-2'], 'Last Name'),
                         th([class='px-4 py-2'], 'Birth Year')])|Rows]),
             a([href='/', class='bg-gray-500 text-white px-4 py-2 rounded hover:bg-gray-600 mt-4 inline-block'], 'Back')
            ])).
