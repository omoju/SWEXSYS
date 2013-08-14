% Author: Omoju Miller
/***********************************************************************                                                                             *
*                         XPCE_UNCERTAINTY.PL
*
***********************************************************************/


:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

:- use_module(library(rdf)).
:- use_module('../../shared_modules/rdf_db.pl').

:- dynamic get_left/1, get_right/1.

%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::%
% Clear working memory of all dynamic variables
% Start of with SWEXSYS knowing nothing

emptybin:-
        retractall(known(_,_)),
        retractall(used(_,_)),
        retractall(wastold(_,_,_)),
        retractall(end_answers(_)),
        retractall(copy(_)),
        retractall(lastindex(_)),
        retractall(used_rule(_)),
        retractall(derived_fact(_)).
/**********************************************************************
*
*                        TOP FRAME
*
***********************************************************************/


cert_inference:-
        free(F),
        new(F, frame('SWEXSYS CERTAINTY FACTOR MODULE')),
        send(F, append, new(D, dialog)),
        
        send(F, append, new(@left, view)),
        %send(Left,request_geometry, 1, 1, 50, 100),
        
        send(F, append, new(@right, view)),
        send(@right,request_geometry, 1, 1, 55, 40),
        send(@right, resize),
        send(F, append, new(Dia, dialog)),
        send(Dia, height,100),
        send(Dia, width, 40),
        send(Dia, below, @right),
        send(@right, below, @left),
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
                          menu_item('New Certainty Factor Model', message(@prolog, cf_begin),end_group := @on),
                          menu_item('Load Report', message(@prolog, load, F)),
                          menu_item('Save', message(@prolog, save)),
                          menu_item('Save As', message(@prolog, saveAs),end_group := @on),
                          menu_item('Quit', message(F, destroy))
                        ]),

        send_list(Inference, append,
                       [
                         menu_item('Load Files', message(@prolog, load_file)),
                         menu_item('Register Namespace', message(@prolog, reg_ns,@left)),
                         menu_item('Certainty Factor Model', message(@prolog, derive_certainty))
                       ]),

        send_list(Help, append,
                        [
                          menu_item('File -> New Certainty Factor', message(@prolog, helpFileNewScenario)),
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
% File Menus actions

% take the file name from the user using standard file input dialog box
% and display the file in the view of the frame
load(F) :-
        get(@finder, file, exists := @on, FileName),
        send(F, report, progress,'Loading %s ...', FileName),
        get(F, member, view, V),
        new(File, file(FileName)),
        send(V, load, File),
        send(F, report, done).

% save the contents of view
save :-
        get_left(L),
        send(L, save).

% take the file name from the user using standard file input dialog box
% and save the contents of view to the entered file name
saveAs :-
        get_left(L),
        get(@finder, file, @off, File),
        send(L, format, '\n%s\n', 'SWEXSYS INFERENCE ENGINE'),
        send(L, format, '%s\n', new(date)?string),
        send(L, save, File).
        
browse(T) :-
        get(@finder, file, exists := @on, FileName),
        send(T,value,FileName).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% New Inference
cf_begin:-
         emptybin,
         assert(lastindex(0)),
         get_left(L),
         get_right(R),
         send(R, clear),
         send(L, clear),
         derive_certainty.
         
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

        
derive_certainty:-
        emptybin,
        assert(lastindex(0)),
        get_left(L),
        get_right(R),
        send(R, clear),
        send(L, clear),
        send(L, format, '%s\n', 'CERTAINTY INFERENCE QUESTIONS'),
        send(R, format, '%s\n', 'DERIVED FACTS'),
        consult('FORWARD_CERTAINTY.PL'),
        derive_cert.

derive_cert:-
        get_left(L),
        get_right(R),
        new_derived_fact(FACT, [], _Answer, CF),
        !,
        assert(known(FACT, CF)),
        term_to_atom(FACT, F),
        term_to_atom(CF, C_F),
        send(R, format, '%s - %s\n', F, C_F),
        derive_cert
        ;
        send(L, format, '\n%s\n', 'NO MORE FACTS').
        
pre_chaining(File_RDF,Rules, Helper, Dia):-
       % load the corresponding files
       rdf_load(File_RDF),
       consult(Helper),
       consult(Rules),
       send(Dia, destroy),
       get_left(View),
       send(View, format, '%s\n', 'Files Successfully loaded').
       
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
        send(Dia, destroy),
        send(V, format, '%s\n', 'Namespace(s) Registered').
        
:- cert_inference.

/***********************************************************************                                                                             *
*                         END XPCE_UNCERTAINTY.PL
*
***********************************************************************/
