

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

:- use_module(library(rdf)).
:- use_module('D:/SWEXSYS/shared_modules/rdf_db.pl').

:- dynamic get_left/1, get_right/1.

%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::%

%:- rdf_register_ns(kb, 'http://protege.stanford.edu/kb#').

% Clear working memory of all dynamic variables
% Start of with SWEXSYS knowing nothing
emptybin:-
        retractall(known(_)),
        retractall(used(_,_)),
        retractall(wastold(_,_,_)),
        retractall(end_answers(_)),
        retractall(copy(_)),
        retractall(lastindex(_)),
        retractall(used_rule(_)),
        retractall(derived_fact(_)).
chain_inference:-
        emptybin,
        assert(lastindex(0)),
        free(F),
        new(F, frame('SWEXSYS Backward and Forward Chaining Engine')),
        send(F, append, new(D, dialog)),
        
        send(F, append, new(@left, view)),
        %send(Left,request_geometry, 1, 1, 50, 100),
        
        send(F, append, new(@right, view)),
        send(@right,request_geometry, 1, 1, 55, 40),
        send(@right, resize),
        send(F, append, new(Dia, dialog)),
        send(Dia, height,200),
        send(Dia, width, 40),
        send(Dia, below, @right),
        send(@right, right, @left),
        send(@left, below, D),
        assert(get_left(@left)),
        assert(get_right(@right)),

        send(Dia, append, new(label(selection,' '))),

        
        send(D,  append, new(MB, menu_bar)),
        % attach pull down menus to above menu bar
        send(MB, append, new(File, popup('File'))),
        send(MB, append, new(Inference, popup('Inference'))),
        send(MB, append, new(Help, popup('Help'))),
        % attach sub menu items to pull down menus
        send_list(File, append,
                        [
                          menu_item('New forward Inference', message(@prolog, f_begin)),
                          menu_item('New backward Inference', message(@prolog, b_begin)),
                          menu_item('Save', message(@prolog, save_scenario)),
                          menu_item('Save As', message(@prolog, saveAs_scenario),end_group := @on),
                          menu_item('Quit', message(F, destroy))
                        ]),

        send_list(Inference, append,
                       [
                         menu_item('Load Files', message(@prolog, load_file)),
                         menu_item('Register Namespace', message(@prolog, reg_ns,@left)),
                         menu_item('Backward Chain', message(@prolog, get_goal_backward, @left)),
                         menu_item('Forward Chain', message(@prolog, derive_forward))
                       ]),

        send_list(Help, append,
                        [
                          menu_item('File -> New forward Inference', message(@prolog, helpFileNewScenario)),
                          menu_item('File -> New backward Inference', message(@prolog, helpFileNewScenario)),
                          menu_item('File -> Save', message(@prolog, helpFileSave)),
                          menu_item('File -> Save As', message(@prolog, helpFileSaveAs)),
                          menu_item('File -> Quit', message(@prolog, helpFileQuit),end_group := @on),

                          menu_item('Inference -> Load Files', message(@prolog, helpNewHypothesis)),
                          menu_item('Inference -> Backward Chain', message(@prolog, helpNewHypothesis),end_group := @on)
                        ]),
        send(F, open).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% New Inference
b_begin:-
       emptybin,
       get_left(View),
       get_right(R),
       send(R, clear),
       get_goal_backward(View).

f_begin:-
         emptybin,
         get_right(R),
         send(R, clear),
         derive_forward.

load_file:-

        new(Dia, dialog('Load Files')),
        send(Dia, height,210),
        send(Dia, width, 450),
        send(Dia, append, new(T1,text_item('RDF File'))),
        send(Dia, append, button('Browse', message(@prolog,browse,T1)),right),
        send(Dia, append, new(T2,text_item('Knowledge_base'))),
        send(Dia, append, button('Browse', message(@prolog,browse,T2)),right),
        send(Dia, append, new(T3,text_item('Helper Files'))),
        send(Dia, append, button('Browse', message(@prolog,browse,T3)),right),
        send(Dia, append, button('Enter', message(@prolog, pre_chaining,
                                                                        T1?selection,
                                                                        T2?selection,
                                                                        T3?selection,
                                                                        Dia)), below),
        send(Dia, append, button('Quit', message(Dia, destroy))),writeln('\n'),
        send(Dia, open).




pre_chaining(File_RDF,Rules, Helper, Dia):-
       % load the corresponding files
       rdf_load(File_RDF),
       consult(Helper),
       consult(Rules),
       send(Dia, destroy),
       get_left(View),
       send(View, format, '%s\n', 'Files Successfully loaded').

get_goal_backward(View):-
        consult('D:/SWEXSYS/INFERENCE/BACKWARD/BACKWARD.PL'),
        make_dialog(D, get_goal_backward),
        get(D, confirm_centered, Name),
        send(D, destroy),
        atom_to_term(Name, Q, _X),
        retractall(question(_)),
        assert(question(Q)),
        answeryes(Q, View).

derive_forward:-
        consult('D:/SWEXSYS/INFERENCE/FORWARD/FORWARD.PL'),
        derive.
        

derive:-
        markstatus(negative),
        new_derived_fact(Question, [], Answer),
        positive(Answer),
        markstatus(positive),
        assert(derived_fact(Question)),
        assert(known(Question)),
        term_to_atom(Question, Q),
        get_right(R_View),
        send(R_View, format, '%s\n', Q),
        %present_fact(Answer),
        derive
        ;
        get_left(View),
        send(View, format, '\n%s\n', 'ALL FACTS DERIVED').
        
answeryes(Question, View):-
        markstatus(negative),
        explore(Question, [], Answer),
        positive(Answer),
        markstatus(positive),
        assert(derived_fact(Question)),
        term_to_atom(Question, Q),
        get_right(R_View),
        send(R_View, format, '%s\n', Q),
        present(Answer, View),
        
        send(View, format, '\n%s\n', 'More Solutions? '),
        getreply(Reply),
        Reply = no
        ;
        nl, write('Finished').
        
reg_ns(V):-
        new(Dia,  dialog('Register Namespace')),
        send(Dia, height,100),
        send(Dia, width, 700),
        send(Dia, append, new(A1,text_item('Alias'))),
        send(A1, selection, kb),
        send(Dia, append, new(A2,text_item('URI')), right),
        send(A2, width, 50),
        send(A2, selection, 'http://protege.stanford.edu/kb#'),
        send(Dia, append, new(B1,text_item('Alias'))),
        send(B1, selection, rdf_ns),
        send(Dia, append, new(B2,text_item('URI')), right),
        send(B2, width, 50),
        send(B2, selection, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'),
        send(Dia, append, button('Enter', message(@prolog, start,
                                                           A1?selection,
                                                           A2?selection,
                                                           B1?selection,
                                                           B2?selection,
                                                           Dia,
                                                           V)), below),
        send(Dia, append, button('Quit', message(Dia, destroy))),writeln('\n'),
        send(Dia, open).


start(A1, A2, B1, B2, Dia, V):-
        rdf_register_ns(A1, A2),
        rdf_register_ns(B1, B2),
        emptybin,
        assert(lastindex(0)),
        send(Dia, destroy),
        send(V, format, '%s\n', 'Namespace(s) Registered').


browse(T) :-
        get(@finder, file, exists := @on, FileName),
        send(T,value,FileName).
          
