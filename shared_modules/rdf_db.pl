/*  $Id: rdf_db.pl,v 1.1 2002/10/24 15:40:02 JanWielemaker Exp wwwrun $

    Developed in the MIA project
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(rdf_db,
	  [ rdf_db/1,			% -DB
	    rdf_db/2,			% -OldDB, +NewDB
	    rdf_clean/0,		% Clean current database
	    rdf_clean/1,		% +DB
	    rdf_subject/1,		% ?Subject
	    rdf_subject/2,		% ?Subject, ?DB
	    rdf/3,			% ?Subject, ?Predicate, ?Object
	    rdf/4,			% ?Subject, ?Predicate, ?Object, ?DB

	    rdf_assert/3,		% +Subject, +Predicate, +Object
	    rdf_assert/4,		% +Subject, +Predicate, +Object, +DB
	    rdf_retract/3,		% ?Subject, ?Predicate, ?Object
	    rdf_retract/4,		% ?Subject, ?Predicate, ?Object, +DB
	    rdf_retractall/3,		% ?Subject, ?Predicate, ?Object
	    rdf_retractall/4,		% ?Subject, ?Predicate, ?Object, +DB

	    rdf_gen_id/1,		% -Id
	    rdf_gen_id/2,		% +DB, -Id
	    rdf_gen_id/3,		% +DB, +Base, -Id

	    rdf_load/1,			% +File
	    rdf_load/2,			% +File, +DB
	    rdf_save/1,			% +File
	    rdf_save/2,			% +File, +DB

	    rdf_register_ns/2,		% +Alias, +URI

	    rdf_global_id/2		% +Id, -Atom
	  ]).
:- use_module(library(rdf)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simple Prolog memory-based storage module for RDF.

Database
--------

ns(Id, NameSpace)

rdf(Subject, Predicate, Object, DB)

rdf_db(DB)

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	rdf_db/1,			% Currently used database
	rdf_db/4,			% RDF Data
	rdf_instance_db/3,		% +Type, -Instance, +DB
	rdf_subject_db/2,		% +Resource, +DB	(Subject?)
	rdf_db_subject/2,		% +DB, +Resource
	ns/2.

rdf_db(user).				% the default DB

:- multifile
	rdf_db:ns/2.
:- dynamic
	rdf_db:ns/2.

ns(rdf,  'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
ns(rdfs, 'http://www.w3.org/TR/1999/PR-rdf-schema-19990303#').

%	rdf_register_ns(+Alias, +URI)
%
%	Register a namespace.  What to do if the Alias already
%	exists?  Throw a permission error?  Use both URI's as synonyms?

rdf_register_ns(Alias, URI) :-
	ns(Alias, URI), !.
rdf_register_ns(Alias, _) :-
	ns(Alias, _),
	throw(error(permission_error(register, namespace, Alias),
		    context(_, 'Already defined'))).
rdf_register_ns(Alias, URI) :-
	assert(ns(Alias, URI)).


%	rdf_global_id(?Id, ?GlobalId)
%
%	Convert between NS:Local and global atomic identifier.
%	To be completed.

rdf_global_id(NS:Local, Global) :- !,
	(   ns(NS, Full)
	*-> atom_concat(Full, Local, Global)
	;   atom_concat(NS, Local, Global)
	).
rdf_global_id(Global, Global).

%	rdf_db(-Old, +New)
%
%	Switch the view for the current database (`world').  Worlds are
%	identified by a general Prolog term.

rdf_db(Old, New) :-
	Old == New, !,
	rdf_db(Old).
rdf_db(Old, New) :-
	retract(rdf_db(Old)), !,
	assert(rdf_db(New)).

%	rdf_clean
%
%	Clean current database

rdf_clean :-
	rdf_db(DB) ->
	rdf_clean(DB).

rdf_clean(DB) :-
	(   retract(rdf_db_subject(DB, S)),
	    retractall(rdf_subject_db(S, DB)),
	    retractall(rdf_db(S,_,_,DB)),
	    fail
	;   true
	),
	retractall(rdf_instance_db(_,_,DB)).

%	rdf_subject(?Resource, [?DB])
%
%	Enumerate currently defined subjects.  Without this Prolog has
%	no good means to generate all defined subjects just once.

rdf_subject(Resource) :-
	rdf_db(DB) ->
	rdf_subject(Resource, DB).

rdf_subject(Subject, DB) :-
	globalise(Subject, S),
	(   ground(S)
	->  rdf_subject_db(S, DB)
	;   rdf_db_subject(DB, S)
	),
	post_globalise(Subject, S).

%	rdf(?Subject, ?Predicate, ?Object)
%
%	Query the RDF database.  This predicate can be used in all directions.
%       To allow for namespace reasoning, all fields may be written as
%	NS:Value

rdf(Subject, Predicate, Object) :-
	rdf_db(DB),
	rdf(Subject, Predicate, Object, DB).

rdf(Subject, Predicate, Object, DB) :-	% optimise rdf(X, rdf:type, Class)
	Predicate == rdf:type,
	var(Subject), !,
	S = Subject,
	globalise_object(Object, O),
	rdf_instance_db(O, S, DB),
	post_globalise(Subject, S),
	post_globalise(Object, O).
rdf(Subject, Predicate, Object, DB) :-
	globalise(Subject, S),
	globalise(Predicate, P),
	globalise_object(Object, O), 
	(   var(S), nonvar(DB)
	->  rdf_db_subject(DB, S)
	;   true
	),
	rdf_db(S, P, O, DB),
	post_globalise(Subject, S),
	post_globalise(Predicate, P),
	post_globalise(Object, O).
	
globalise(NSid:Name, Global) :-
	atom(NSid), atomic(Name), !,
	(   ns(NSid, NS)
	->  atom_concat(NS, Name, Global)
	;   atom_concat(NSid, Name, Global)
	).
globalise(Atom, Global) :-
	atom(Atom), !,
	Global = Atom.
globalise(V, _) :-
	var(V), !.
globalise(NS:V, _) :-
	var(NS) ; var(V).


globalise_object(Object, Global) :-
	(   nonvar(Object),
	    Object = literal(_)
	->  Global = Object
	;   globalise(Object, Global)
	).

post_globalise(Spec, _Global) :-
	ground(Spec), !.
post_globalise(Spec, literal(Literal)) :- !,
	Spec = literal(Literal).
post_globalise(Spec, Global) :-
	var(Spec), !,
	Spec = Global.
post_globalise(NSid:Name, Global) :- !,
	(   sub_atom(Global, _, _, A, #)
	->  sub_atom(Global, _, A, 0, Name),
	    sub_atom(Global, 0, _, A, NS),
	    (   ns(NSid, NS)
	    ->  true
	    ;   NSid = NS
	    )
	;   NSid = '',
	    Name = Global
	).
post_globalise(Global, Global).


		 /*******************************
		 *	      STORING		*
		 *******************************/

%	rdf_assert(+Subject, +Predicate, +Object)
%
%	Assert an RDF triple.  Can we assert the same triple twice?

rdf_assert(Subject, Predicate, Object) :-
	rdf_db(DB),
	rdf_assert(Subject, Predicate, Object, DB).

rdf_assert(Subject, Predicate, Object, DB) :-
	assert_globalise(Subject, S),
	assert_globalise(Predicate, P),
	assert_globalise(Object, O),
	(   rdf_db(S, P, O, DB)		% already got this?
	->  true
	;   assert(rdf_db(S, P, O, DB)),
	    (	rdf_subject_db(S, DB)
	    ->	true
	    ;	assert(rdf_subject_db(S, DB)),
		assert(rdf_db_subject(DB, S))
	    ),
	    (	rdf_global_id(rdf:type, P)
	    ->	assert(rdf_instance_db(O, S, DB))
	    ;	true
	    )
	).

assert_globalise(NS:Name, Global) :-
	atom(NS), atom(Name), !,
	(   ns(NS, Long)
	->  atom_concat(Long, Name, Global)
	;   atom_concat(NS, Name, Global)
	).
assert_globalise(Name, Name).

		 /*******************************
		 *	ANONYMOUS SUBJECTS	*
		 *******************************/

%	rdf_gen_id([+DB], -Id)
%
%	Generate a unique identifier for a subject.  Necessary for creating
%	a database of triples for saving it later.
%
%	TBD: Not sure whether this a reasonable interface.  What about
%	namespaces?  Use the type (subjects don't need to have a type).
%
%	TBD: Getting the first unique id might take long this way!

rdf_gen_id(Value) :-
	rdf_db(DB),
	rdf_gen_id(DB, Value).

rdf_gen_id(DB, Value) :-
	rdf_gen_id(DB, DB, Value).

rdf_gen_id(DB, Base, Value) :-
	repeat,
	gensym(Base, Value),
	\+ rdf_db(Value, _, _, DB), !.


		 /*******************************
		 *	     REMOVING		*
		 *******************************/

%	rdf_retract(?Subject, ?Predicate, ?Object, [+DB])
%	rdf_retractall(?Subject, ?Predicate, ?Object, [+DB])
%
%	Remove facts from the RDF database.  Note that this will only
%	work if the database contains unit-facts.  If you construct
%	an rdf database using rules (for example to define a view on
%	another database), you cannot use these predicates.

rdf_retract(Subject, Predicate, Object) :-
	rdf_db(DB),
	rdf_retract(Subject, Predicate, Object, DB).

rdf_retract(Subject, Predicate, Object, DB) :-
	globalise(Subject, S),
	globalise(Predicate, P),
	globalise_object(Object, O), 
	clause(rdf_db(S, P, O, DB), true, Ref),
	post_globalise(Subject, S),
	post_globalise(Predicate, P),
	post_globalise(Object, O),
	erase(Ref),
	(   rdf_db(S, _, _, DB)
	->  true
	;   retractall(rdf_subject_db(S, DB)),
	    retractall(rdf_db_subject(DB, S))
	),
	(   rdf_global_id(rdf:type, P)
	->  retractall(rdf_instance_db(O, S, DB))
	;   true
	).

rdf_retractall(Subject, Predicate, Object) :-
	rdf_db(DB),
	rdf_retractall(Subject, Predicate, Object, DB).

rdf_retractall(Subject, Predicate, Object, DB) :-
	(   rdf_retract(Subject, Predicate, Object, DB),
	    fail
	;   true
	).


		 /*******************************
		 *	    LOAD RDF    	*
		 *******************************/

%	rdf_load(+File)
%
%	Load RDF file into the current database.

rdf_load(File) :-
	rdf_db(DB),
	rdf_load(File, DB).

rdf_load(Spec, DB) :-
	(   Spec = '$stream'(_)
	->  File = Spec
	;   absolute_file_name(Spec,
			       [ access(read),
				 extensions([rdf,rdfs,''])
			       ], File)
	),
	statistics(cputime, CpuOld),
	load_rdf(File, Triples), !,
	statistics(cputime, CpuLoaded),
	assert_triples(Triples, DB),
	statistics(cputime, CpuNew),
	ParseTime is CpuLoaded - CpuOld,
	AssertTime is CpuNew - CpuLoaded,
	length(Triples, N),
	print_message(informational,
		      rdf(loaded(Spec, N, ParseTime, AssertTime))).


%	assert_triples(+Triples, +DB)
%
%	This predicate is complicated to speed-up the assertion of
%	new things to the DB.

assert_triples([], _).
assert_triples([rdf(S,P,O)|T], DB) :- !,
	rdf_assert(S, P, O, DB),
	assert_triples(T, DB).
assert_triples([H|_], _) :-
	throw(error(type_error(rdf_triple, H), _)).


		 /*******************************
		 *	     SAVE RDF		*
		 *******************************/

%	rdf_save(File)
%
%	Save RDF data to file

rdf_save(File) :-
	rdf_db(DB),
	rdf_save(File, DB).

rdf_save(File, DB) :-
	open(File, write, Out),
	rdf_save_header(Out, DB),
	(   rdf_subject(Subject, DB),
	    setof(Pred=Object, rdf_db(Subject, Pred, Object, DB), Atts),
	    rdf_save_subject(Out, Subject, Atts),
	    fail
	;   true
	),
	rdf_save_footer(Out),
	close(Out).

%	rdf_save_header(+Fd)
%
%	Save XML documentheader, doctype and open the RDF environment.
%	This predicate also sets up the namespace notation.

rdf_save_header(Out, DB) :-
	format(Out, '<?xml version=\'1.0\' encoding=\'ISO-8859-1\'?>~n', []),
	format(Out, '<!DOCTYPE rdf:RDF [', []),
	used_namespaces(NSList, DB),
	(   member(Id, NSList),
	    ens(Id, NS),
	    format(Out, '~N    <!ENTITY ~w \'~w\'>', [Id, NS]),
	    fail
	;   true
	),
	format(Out, '~N]>~n~n', []),
	format(Out, '<rdf:RDF', []),
	(   member(Id, NSList),
	    format(Out, '~N    xmlns:~w="&~w;"~n', [Id, Id]),
	    fail
	;   true
	),
	format(Out, '>~n', []).

ens(e, '').
ens(Id, NS) :-
	ns(Id, NS).

%	used_namespaces(-List)
%
%	Return the list of namespaces used in an RDF database.

:- dynamic
	used_ns/1.

used_namespaces(List, DB) :-
	setof(NS, Full^ens(NS, Full), NS0),
	used_ns(NS0, List, DB).

used_ns([], [], _).
used_ns([H|T0], [H|T], DB) :-
	used_ns(H, DB), !,
	used_ns(T0, T, DB).
used_ns([_|T0], T, DB) :-
	used_ns(T0, T, DB).

used_ns(e, _) :- !.			% for now just assume it
used_ns(NS, DB) :-
	ns(NS, Full),
	rdf_db(S,P,O,DB),
	(   sub_atom(S, 0, _, _, Full)
	;   sub_atom(P, 0, _, _, Full)
	;   atom(O),
	    sub_atom(O, 0, _, _, Full)
	), !.


rdf_save_footer(Out) :-
	format(Out, '</rdf:RDF>~n', []).

rdf_save_subject(Out, Subject, Atts) :-
	globalise(rdf:type, RdfType),
	select(RdfType=Type, Atts, Atts1), !,
	rdf_value(Subject, QSubject),
	rdf_id(Type, DefNS, TypeId),
	format(Out, '<~w rdf:about="~w"', [TypeId, QSubject]),
	save_attributes(Atts1, DefNS, Out, TypeId).
rdf_save_subject(Out, Subject, Atts) :-
	rdf_value(Subject, QSubject),
	format(Out, '<rdf:Description about="~w"', [QSubject]),
	save_attributes(Atts, rdf, Out, rdf:'Description').

%	save_attributes(+List, +DefNS, +Stream, Element)
%
%	Save the attributes.  Short literal attributes are saved in the
%	tag.  Others as the content of the description element.  The
%	begin tag has already been filled.

save_attributes(Atts, DefNS, Out, Element) :-
	split_attributes(Atts, InTag, InBody),
	save_attributes2(InTag, DefNS, tag, Out),
	(   InBody == []
	->  format(Out, '/>~n', [])
	;   format(Out, '>~n', []),
	    save_attributes2(InBody, _, body, Out),
	    format(Out, '~N</~w>~n', [Element])
	).

%	split_attributes(+Attributes, -Inline, -Body)
%
%	Split attributes for (literal) attributes to be used in the
%	begin-tag and ones that have to go into the body of the description.

split_attributes([], [], []).
split_attributes([H|TA], [H|TI], B) :-
	in_tag_attribute(H), !,
	split_attributes(TA, TI, B).
split_attributes([H|TA], I, [H|TB]) :-
	split_attributes(TA, I, TB).

in_tag_attribute(_=literal(Text)) :-
	atom_length(Text, Len),
	Len < 60.

%	save_attributes(+List, +DefNS, +TagOrBody, +Stream)
%
%	Save a list of attributes.

save_attributes2([], _, _, _).
save_attributes2([H|T], DefNS, Where, Out) :-
	save_attribute(Where, H, DefNS, Out),
	save_attributes2(T, DefNS, Where, Out).

save_attribute(tag, Name=literal(Value), DefNS, Out) :-
	rdf_local_id(Name, DefNS, NameText),
	xml_quote_attribute(Value, QVal),
	format(Out, '~N\t~w="~w"', [NameText, QVal]).
save_attribute(body, Name=literal(Value), _, Out) :- !,
	rdf_id(Name, _, NameText),
	xml_quote_cdata(Value, QVal),
	format(Out, '~N\t<~w>~w</~w>', [NameText, QVal, NameText]).
save_attribute(body, Name=Value, _, Out) :-
	rdf_value(Value, QVal),
	rdf_id(Name, _, NameText),
	format(Out, '~N\t<~w rdf:resource="~w"/>', [NameText, QVal]).

rdf_id(Id, NS, NS:Local) :-
	ns(NS, Full),
	Full \== '',
	atom_concat(Full, Local, Id), !.
rdf_id(Id, e, e:Id).


rdf_local_id(Id, NS, Local) :-
	ns(NS, Full),
	atom_concat(Full, Local, Id), !.
rdf_local_id(Id, _, Text) :-
	rdf_id(Id, _, Text).

rdf_value(V, Text) :-
	ns(NS, Full),
	atom_concat(Full, Local, V), !,
	concat_atom(['&', NS, (';'), Local], Text).
rdf_value(V, V).


		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(rdf(loaded(Spec, Triples, ParseTime, AssertTime))) -->
	{   atom(Spec)
	->  file_base_name(Spec, Base)
	;   Base = Spec
	},
	[ '\t\tParsed "~w" in ~2f sec; added ~D triples in ~2f sec'-
	  [Base, ParseTime, Triples, AssertTime]
	].

