/*  $Id: xml_write.pl,v 1.1 2000/09/27 10:20:31 jan Exp $

    Developed in the MIA project
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(xml_write,
	  [ xml_quote/2			% +In, -Out
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Still very incomplete library to facilitate writing XML output.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	      QUOTING		*
		 *******************************/

%	xml_quote(+In, -Out)
%
%	Quote characters that need quoting for use in an XML attribute
%	value or element content.  If nothing is mapped, failure is
%	forced and the original is returned to minimise garbage production.

quote('\'', '&apos;').
quote('"',  '&quot;').
quote('&',  '&amp;').
quote('<',  '&lt;').
quote('>',  '&gt;').

xml_quote(In, Out) :-
	atom_chars(In, Chars),
	map_chars(Chars, Mapped),
	Mapped \== Chars, !,
	concat_atom(Mapped, Out).
xml_quote(In, In).

map_chars([], []).
map_chars([H|T], [H1|T1]) :-
	(   quote(H, H1)
	->  true
	;   H1 = H
	),
	map_chars(T, T1).

	
