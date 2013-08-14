:- module(prolog_http_load,
	  [
	  ]).
:- use_module(http_open).

%	prolog_load_file(+URL, +Options)
%	
%	Hook into load_files/2 that loads http:// files directly from
%	the web.

user:prolog_load_file(File, Options) :-
	'$strip_module'(File, Module, URL),
	atom(URL),
	(   sub_atom(URL, 0, _, _, 'http://')
	->  parse_url(URL, Parts0)
	;   prolog_load_context(file, Parent),
	    sub_atom(Parent, 0, _, _, 'http://'),
	    parse_url(URL, Parent, Parts0)
	),
	ensure_extension(Parts0, pl, Parts),
	parse_url(Id, Parts),
	http_open(Parts, In),
	load_files(Module:Id, [stream(In)|Options]),
	close(In).


%	ensure_extension(+Parts, +Ext, -PlParts)
%	
%	If the HTTP location is a plain path without extension, add the
%	.pl extension. This ensures extension-less files appearing in
%	file-loading directives are processed correctly.

ensure_extension(Parts0, Ext, Parts) :-
	select(path(Path0), Parts0, Rest),
	file_name_extension(_, '', Path0),
	\+ memberchk(search(_), Rest),
	\+ memberchk(fragment(_), Rest), !,
	file_name_extension(Path0, Ext, Path),
	Parts = [path(Path)|Rest].
ensure_extension(Parts, _, Parts).
