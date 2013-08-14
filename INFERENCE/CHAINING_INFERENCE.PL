/ Author: Omoju Miller /
/***********************************************************************                                                                             *
*       CHAINING_INFERENCE.PL
*
***********************************************************************/

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

:- use_module(library(rdf)).
:- use_module('../shared_modules/rdf_db.pl').
:- consult('../shared_modules/inference_utilities.pl').

:- dynamic get_left/1, get_right/1, my_alias/1.

/***********************************************************************

% Clear working memory of all dynamic variables
% Start of with SWEXSYS knowing nothing
*/
emptybin:-
        retractall(known(_)),
        retractall(used(_,_)),
        retractall(wastold(_,_,_)),
        retractall(end_answers(_)),
        retractall(copy(_)),
        retractall(lastindex(_)),
        retractall(used_rule(_)),
        retractall(derived_fact(_)).

/***********************************************************************                                                                             *
*       TOP FRAME
*
***********************************************************************/

chain_inference:-
        free(F),
        new(F, frame('SWEXSYS Backward and Forward Chaining Engine')),
        send(F, append, new(D, dialog)),
        send(F, append, new(@left, view)),
        send(F, append, new(@right, view)),
        send(@right,request_geometry, 1, 1, 55, 40),
        send(@right, resize),
        send(@right, below, @left),
        send(@left, below, D),
        assert(get_left(@left)),
        assert(get_right(@right)),
        
        send(D,  append, new(MB, menu_bar)),
        % attach pull down menus to above menu bar
        send(MB, append, new(File, popup('File'))),
        send(MB, append, new(Inference, popup('Inference'))),
        send(MB, append, new(Help, popup('Help'))),
        % attach sub menu items to pull down menus
        send_list(File, append,
                        [
                          menu_item('New Forward Inference', message(@prolog, f_begin)),
                          menu_item('New Backward Inference', message(@prolog, b_begin),end_group := @on),
                          menu_item('Load Report', message(@prolog, load, F)),
                          menu_item('Save', message(@prolog, save)),
                          menu_item('Save As', message(@prolog, saveAs),end_group := @on),
                          menu_item('Quit', message(F, destroy))
                        ]),

        send_list(Inference, append,
                       [
                         menu_item('Load Files', message(@prolog, load_file_chain)),
                         menu_item('Register Namespace', message(@prolog, reg_ns,@left)),
                         menu_item('Backward Chain', message(@prolog, b_begin)),
                         menu_item('Forward Chain', message(@prolog, derive_forward))
                       ]),

        send_list(Help, append,
                        [
                          menu_item('File -> New forward Inference', message(@prolog, helpFileNewScenario)),
                          menu_item('File -> New backward Inference', message(@prolog, helpFileNewScenario)),
                          menu_item('File -> Load Report', message(@prolog, helpFileLoad)),
                          menu_item('File -> Save', message(@prolog, helpFileSave)),
                          menu_item('File -> Save As', message(@prolog, helpFileSaveAs)),
                          menu_item('File -> Quit', message(@prolog, helpFileQuit),end_group := @on),

                          menu_item('Inference -> Load Files', message(@prolog, helpNewHypothesis)),
                          menu_item('Inference -> Register Namespace', message(@prolog, helpNewHypothesis)),
                          menu_item('Inference -> Backward Chain', message(@prolog, helpNewHypothesis)),
                          menu_item('Inference -> Forward Chain', message(@prolog, helpNewHypothesis),end_group := @on)
                        ]),
        send(F, open).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% New Inference
b_begin:-
        emptybin,
        assert(lastindex(0)),
        get_left(L),
        get_right(R),
        send(R, clear),
        send(L, clear),
        send(L, format, '%s\n', 'BACKWARD CHAINING INFERENCE QUESTIONS'),
        send(R, format, '%s\n', 'DERIVED FACTS'),
        get_goal_backward(L).

f_begin:-
         emptybin,
         assert(lastindex(0)),
         get_left(L),
         get_right(R),
         send(R, clear),
         send(L, clear),
         send(L, format, '%s\n', 'FORWARD CHAINING INFERENCE QUESTIONS'),
         send(R, format, '%s\n', 'DERIVED FACTS'),
         derive_forward.
         
load_file_chain:-

        new(Dia, dialog('Load Files')),
        send(Dia, height,210),
        send(Dia, width, 450),
        send(Dia, append, new(T1,text_item('RDF File'))),
        send(Dia, append, button('Browse', message(@prolog,browse,T1)),right),
        send(Dia, append, new(T4,text_item('RDFS File'))),
        send(Dia, append, button('Browse', message(@prolog,browse,T4)),right),
        send(Dia, append, new(T2,text_item('Knowledge_base'))),
        send(Dia, append, button('Browse', message(@prolog,browse,T2)),right),
        send(Dia, append, new(T3,text_item('Helper Files'))),
        send(Dia, append, button('Browse', message(@prolog,browse,T3)),right),
        send(Dia, append, button('Enter', message(@prolog, pre_chaining,
                                                                        T1?selection,
                                                                        T2?selection,
                                                                        T3?selection,
                                                                        T4?selection,
                                                                        Dia)), below),
        send(Dia, append, button('Quit', message(Dia, destroy))),writeln('\n'),
        send(Dia, open).



pre_chaining(File_RDF,Rules, Helper, File_RDFS, Dia):-
       % load the corresponding files

        statistics(runtime, [T0|_]),
       rdf_load(File_RDF),
       statistics(runtime, [T1|_]),
       FetchTimeInMsecs is T1 - T0,
       write('Load Time '), write(FetchTimeInMsecs), nl,

         time(rdf_load(File_RDFS)),
        
       consult(Helper),
       consult(Rules),
       send(Dia, destroy),
       get_left(View),
       send(View, format, '%s\n', 'Files Successfully loaded').

get_goal_backward(L):-
        consult('BACKWARD/BACKWARD.PL'),
        make_dialog(D, get_goal_backward),
        get(D, confirm_centered, Name),
        send(D, destroy),
        atom_to_term(Name, Q, _X),
        retractall(question(_)),
        assert(question(Q)),
        answeryes(Q, L).
        
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
        send(View, format, '\n%s', 'More Solutions? '),
        getreply(Reply),
        term_to_atom(Reply, R),
        send(View, format,'%s\n', R),
        (Reply = quit ->
          (
           send(View, format,'\n%s\n', 'FINISHED'),
           call(abort)
           )
          ;
          fail
        ).


answerno(Question, View):-
        markstatus(negative),
        explore(Question, [], Answer),
        negative(Answer),
        ssert(derived_fact(Question)),
        term_to_atom(Question, Q),
        get_right(R_View),
        send(R_View, format, '%s\n', Q),
        present(Answer, View),
        send(View, format, '\n%s', 'More negative Solutions? '),
        term_to_atom(Reply, R),
        send(View, format,'%s\n', R),
        getreply(Reply),
        Reply = no,
        send(View, format,'\n%s\n', 'FINISHED').
        
derive_forward:-
        consult('FORWARD/FORWARD.PL'),
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
        send(R_View, format, '%s', Q),
        %present_fact(Answer),
        derive
        ;
        get_left(View),
        send(View, format, '\n%s\n', 'ALL FACTS DERIVED').

        


        
reg_ns(V):-
        new(Dia,  dialog('Register Namespace')),
        send(Dia, height,100),
        send(Dia, width, 700),
        send(Dia, append, new(A1,text_item('Alias'))),
        send(A1, selection, kb),
        send(Dia, append, new(A2,text_item('URI')), right),
        send(A2, width, 50),
        send(Dia, append, new(B1,text_item('Alias'))),
        send(B1, selection, rdfs_ns),
        send(Dia, append, new(B2,text_item('URI')), right),
        send(B2, width, 50),
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
        assert(my_alias(A1)),
        rdf_register_ns(B1, B2),
        emptybin,
        send(Dia, destroy),
        send(V, format, '%s\n', 'Namespace(s) Registered').
        
:- chain_inference.

/***********************************************************************                                                                             *
*       END     CHAINING_INFERENCE.PL
*
***********************************************************************/
