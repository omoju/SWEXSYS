%
% Title:    Program for GUI Implementation of Semantic Web Expert System Shell
% Author:   Amit Aggarwal
% CoAuthor: Omoju Miller
% Date :    Dec 5, 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% CoAuthor: Omoju Thomas
% Date :    Feb 23, 2004
% Extending the application to perform several inference strategies.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- require([ send_list/3]).

%Load File-loading library modules

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

%Load the XPCE library modules

:- use_module(library(pce)).
:- use_module(library(emacs_interface)).

%Load the HTML/RDF/SGML library modules

:- use_module(library('doc/load')).
:- use_module(doc(emit)).
:- use_module(doc(html)).
:- use_module(library(sgml)).
:- use_module(library(rdf)).
:- use_module('shared_modules/http_open.pl').
:- use_module('shared_modules/http_load.pl').
:- use_module('shared_modules/rdf_db.pl').
:- include('shared_modules/seek_past_headers.pl').

%Load the user defined modules




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Draw Main Frame Window along with view, menus and sub-menus

semantic:-
        free(F),
        % create the main frame with title 'Semantic Web Expert System Shell'
        new(F, frame('Semantic Web Expert System Shell')),
        % attach adialog box to above frame
        send(F, append, new(D, dialog)),
        % attach a view to above dialog box
        send(new(V, view), below, D),
        % attach a menu bar to dialog
        send(D,  append, new(MB, menu_bar)),
        % attach pull down menus to above menu bar
        send(MB, append, new(File, popup('File'))),
        send(MB, append, new(InferenceEngine, popup('Inference Engine'))),
        send(MB, append, new(Help, popup('Help'))),
        % attach sub menu items to pull down menus
        send_list(File, append,
                        [
                          menu_item('Load', message(@prolog, load, F)),
                          menu_item('Clear View', message(V, clear),end_group := @on),
                          menu_item('Save', message(@prolog, save, V)),
                          menu_item('Save As', message(@prolog, saveAs, V),end_group := @on),
                          menu_item('Quit', message(F, destroy))
                        ]),
                        
        send_list(InferenceEngine, append,
                       [
                         menu_item('Backward Chain', message(@prolog,backwardChain )),
                         menu_item('Forward Chain', message(@prolog,forwardChain )),
                         menu_item('Inexact Reasoning', message(@prolog,inexact_reason ),end_group := @on)
                       ]),

        send_list(Help, append,
                        [
                          menu_item('File -> Load', message(@prolog, helpFileLoad)),
                          menu_item('File -> Clear View', message(@prolog, helpFileClearView)),
                          menu_item('File -> Save', message(@prolog, helpFileSave)),
                          menu_item('File -> Save As', message(@prolog, helpFileSaveAs)),
                          menu_item('File -> Quit', message(@prolog, helpFileQuit),end_group := @on),

                          menu_item('KnowledgeBase -> Load Prolog KB', message(@prolog, helpKBLoad)),
                          menu_item('KnowledgeBase -> Import RuleML', message(@prolog, helpKBImport)),
                          menu_item('KnowledgeBase -> Export RuleML', message(@prolog,helpKBExport)),
                          menu_item('KnowledgeBase -> Load RDF from Crawl', message(@prolog,helpKBLoadRDF),end_group := @on),

                          menu_item('InferenceEngine -> Backward Chain', message(@prolog,helpIEBC)),
                          menu_item('InferenceEngine -> Forward Chain', message(@prolog,helpIEFC)),
                          menu_item('InferenceEngine -> Plausible Inference', message(@prolog,helpIEPI)),
                          menu_item('InferenceEngine -> Plausible IE Test', message(@prolog,helpIEPITest )),
                          menu_item('InferenceEngine -> Interpreter', message(@prolog,helpIEInterpreter ),end_group := @on),

                          menu_item('Database -> Load Prolog Facts', message(@prolog, helpDBLoadProlog)),
                          menu_item('Database -> Load ODBC', message(@prolog, helpDBLoadODBC )),
                          menu_item('Database -> Import RuleML', message(@prolog,helpDBImport)),
                          menu_item('Database -> Export RuleML', message(@prolog, helpDBExport),end_group := @on),

                          menu_item('RDFCrawler -> Execute', message(@prolog, helpRDFExecute)),
                          menu_item('RDFCrawler -> Test', message(@prolog, helpRDFTest),end_group := @on),

                          menu_item('Contents', message(@prolog, contents_help)),
                          menu_item('Readme', message(@prolog, read_me)),
                          menu_item('About', message(@display, inform,'Semantic Web Expert System Shell'))
                        ]),
         % display the frame
         send(F,open).


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
save(V) :-  
        send(V, save).

% take the file name from the user using standard file input dialog box
% and save the contents of view to the entered file name
saveAs(V) :-
        get(@finder, file, @off, File),
        send(V, save, File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Inference Engine Menus actions

forwardChain :- featureN.
inexact_reason  :- featureN.
interpreter :- featureN.


backwardChain:-
        new(Dia,  dialog('Load Backward Chaining Supporting Files')),
        send(Dia, height,210),
        send(Dia, width, 500),
        send(Dia, append, new(T1,text_item('RDF File'))),
        send(Dia, append, button('Browse', message(@prolog,browse,T1)),right),
        send(Dia, append, new(T2,text_item('Knowledge_base'))),
        send(Dia, append, button('Browse', message(@prolog,browse,T2)),right),
        send(Dia, append, new(T3,text_item('Helper Files'))),
        send(Dia, append, button('Browse', message(@prolog,browse,T3)),right),
        send(Dia, append, button('Enter', message(@prolog, pre_backward,
                                                                        T1?selection,
                                                                        T2?selection,
                                                                        T3?selection)), below),
        send(Dia, append, button('Quit', message(Dia, destroy))),writeln('\n'),
        send(Dia, open).


pre_backward(File_RDF,Rules, Helper):-
       % load the corresponding files
       rdf_load(File_RDF),
       consult(Helper),
       consult(Rules),
       consult('Inference/BACKWARD.pl').

get_goal:-
        make_dialog(D, get_goal),
        get(D, confirm_centered, Name),
        send(D, destroy),
        retractall(question(_)),
        assert(question(Name)).

dialog(get_goal,
       [ object        :=
           Get_goal,
         parts         :=
           [ Get_goal := dialog('Get Goal'),
             Name     := text_item('Goal'),
             Enter    := button('Enter'),
             Cancel   := button('Cancel')
           ],
         modifications :=
           [ Name   := [ length    := 26,
                         alignment := left
                       ],
             Enter  := [ alignment := right
                       ],
             Cancel := [ alignment := right
                       ]
           ],
         layout        :=
           [ below(Enter, Name),
             right(Cancel, Enter)
           ],
         behaviour     :=
           [ Enter  := [ message := message(Get_goal,
                                            return,
                                            Name?selection)
                       ],
             Cancel := [ message := message(Get_goal, return, @nil)
                       ]
           ]
       ]).



infer:-
                question(Question),
                (
                        answeryes(Question)
                        ;
                        answerno(Question)
                ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Help Menus actions

helpFileLoad      :- show('help/FileLoad.html').
helpFileClearView :- show('help/FileClearView.html').
helpFileSave      :- show('help/FileSave.html').
helpFileSaveAs    :- show('help/FileSaveAs.html').
helpFileQuit      :- show('help/FileQuit.html').
helpKBLoad        :-  featureN.
helpKBLoadRDF     :-  featureN.
helpKBImport      :- show('help/KBImport.html').
helpKBExport      :- show('help/KBExport.html').
helpIEBC          :-  featureN.
helpIEFC          :-  featureN.
helpIEInterpreter :-  featureN.
helpIEPI          :- show('help/IEPI.html').
helpIEPITest      :- show('help/IEPITest.html').
helpDBLoadProlog  :-  featureN.
helpDBLoadODBC    :-  featureN.
helpDBImport      :-  featureN.
helpDBExport      :-  featureN.
helpRDFExecute    :-  featureN.
helpRDFTest       :- show('help/RDFTest.html').
contents_help     :- show('help/Contents.html').
read_me           :-show('readme.html').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

featureN :-    new(Dia, dialog('Under Development')),
               send(Dia, height, 70),
               send(Dia, width,  300),
               send(Dia, append, new(_,text('This feature has not been implemented yet'))),
               send(Dia, append, button('Ok', message(Dia, destroy)),below),nl,
               send(Dia,open).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some Common modules called by other predicates

% This module creates a new view and displays the ASCII file passed to it as argument
open_output(File):-
               send(new(OView,view(File)),open),
               send(OView,load(File)),
               send(OView,height,10),
               send(OView,width,60).
               
% This module creates a new view and displays the HTML file passed to it as argument
show(HtmlFile) :-
        send(new(P, picture), open),
        send(P, display, new(PB, pbox), point(10,10)),
        send(P, resize_message, message(PB, width, @arg2?width - 20)),
        load_html_file(HtmlFile, Tokens),
        send(P, height, 200),
        send(P, width, 600),
        send(P, background, 'alice blue'),
        send(PB, show, Tokens).
        
% This module creates a list of files from the filename passed to it as argument In
% The file passed must contain absolute filenames on each line
% The list is stored in Array argument
read_file_to_list(In,Array) :-
                read_line_to_codes(In,Char),
                (Char = end_of_file
                 -> Array=[]
                ;
                (atom_chars(R,Char),
                Array = [R |T ]),
                read_file_to_list(input, T)
                ).

% This module takes input filename from user through standard file input dialog box
% and copies that absolute filename to the object passed as argument (text_item object)
browse(T) :-
        get(@finder, file, exists := @on, FileName),
        send(T,value,FileName).

obrowse(T) :-
        get(@finder, file, exists := @off, FileName),
        send(T,value,FileName).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Run the application 

:- semantic.
