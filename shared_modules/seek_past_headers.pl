/*This rule advances the file pointer of Stream past the xml headers in
 *the file, if applicable.  This rule assumes proper usage of XML syntax
 */
seek_past_headers(Stream) :-
	get(Stream, NextCode),
	char_code(Char, NextCode),
	get_char(Stream, SecondChar),
	Char=='<',
	SecondChar=='?',
	find_end_tag(Stream),
	seek_past_headers(Stream), !.

seek_past_headers(Stream) :-
	seek(Stream, -2, current, _).


/*This rule finds the end of the next xml tag in Stream
 */
find_end_tag(Stream) :-
	get_char(Stream, Char),
	Char=='>', !.

find_end_tag(Stream) :-
	find_end_tag(Stream).

