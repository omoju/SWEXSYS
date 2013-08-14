% Author: Omoju Thomas
% Date: 7/5/2004

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                              %
%                          LOAD DATA FROM RDF                                  %
%                                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reg_ns:-
        new(Dia,  dialog('Register Namespace')),
        send(Dia, height,100),
        send(Dia, width, 700),
        send(Dia, append, new(A1,text_item('Alias'))),
        send(A1, selection, kb),
        send(Dia, append, new(A2,text_item('URI')), right),
        send(A2, width, 50),
        send(A2, selection, 'http://protege.stanford.edu/kb#'),
        send(Dia, append, new(B1,text_item('Alias'))),
        send(B1, selection, rdfs_ns),
        send(Dia, append, new(B2,text_item('URI')), right),
        send(B2, width, 50),
        send(B2, selection, 'http://www.w3.org/TR/1999/PR-rdf-schema-19990303#'),
        send(Dia, append, button('Enter', message(@prolog, start,
                                                           A1?selection,
                                                           A2?selection,
                                                           B1?selection,
                                                           B2?selection,
                                                           Dia)), below),
        send(Dia, append, button('Quit', message(Dia, destroy))),writeln('\n'),
        send(Dia, open).


start(A1, A2, B1, B2, Dia):-
        rdf_register_ns(A1, A2),
        rdf_register_ns(B1, B2),
        send(Dia, destroy),
        write('Namespace(s) Registered').

load_file:-

        new(Dia, dialog('Load Files')),
        send(Dia, height,210),
        send(Dia, width, 450),
        send(Dia, append, new(T1,text_item('RDF File'))),
        send(Dia, append, button('Browse', message(@prolog,browse,T1)),right),
        send(Dia, append, new(T2,text_item('RDFS File'))),
        send(Dia, append, button('Browse', message(@prolog,browse,T2)),right),
        send(Dia, append, button('Enter', message(@prolog, pre_chaining,
                                                                        T1?selection,
                                                                        T2?selection,
                                                                        Dia)), below),
        send(Dia, append, button('Quit', message(Dia, destroy))),writeln('\n'),
        send(Dia, open).

pre_chaining(File1, File2, Dia):-
       % load the corresponding files
       rdf_load(File1),
       rdf_load(File2),
       send(Dia, destroy),
       write('Files Successfully loaded').

browse(T) :-
        get(@finder, file, exists := @on, FileName),
        send(T,value,FileName).
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                              %
%                          HELP FILES METHODS                                  %
%                                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This module creates a new view and displays the HTML file passed to it as argument
show_file(HtmlFile) :-
        send(new(P, picture), open),
        send(P, display, new(PB, pbox), point(10,10)),
        send(P, resize_message, message(PB, width, @arg2?width - 20)),
        load_html_file(HtmlFile, Tokens),
        send(P, height, 200),
        send(P, width, 600),
        send(P, background, 'alice blue'),
        send(PB, show, Tokens).

featureN :-    new(Dia, dialog('Under Development')),
               send(Dia, height, 70),
               send(Dia, width,  300),
               send(Dia, append, new(_,text('This feature has not been implemented yet'))),
               send(Dia, append, button('Ok', message(Dia, destroy)),below),nl,
               send(Dia,open).

