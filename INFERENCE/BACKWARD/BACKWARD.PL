/***********************************************************************                                                                             *
*                BACKWARD.PL
*
* Author: OMOJU Miller
* Date: 3/29/2004
*
* Simple RDF enabled backward chaining in prolog
*
* Supporting Files
* RULES.pl Rulebase
* COMMON_PROCEDURES.pl User-defined Operators.
*
*************************************************************************/



/***********************************************************************%
% Procedure
%
% explore(Goal, Trace, Answer)
%
%
% Finds Answer to a given Goal. Trace is a chain of ancestors goals and rules. 
%'explore' tends to find a positive answer to a question.
% Answer is 'false' only when all the possibilities have been investigated
% and they all resulted in 'false'
%
% Assume only one rule about each type of goal.
%
% Explore predicates for RDF related data.
***********************************************************************/
explore(A isa C, _, 'RDF Fact(s)' is true was 'found'):-
        findall((A,C),rdf(A, rdfs_ns:subClassOf, C),L),
        L \= [],
        present_facts(L).

explore(A isa C, _, A isa C is true was 'found as a RDF fact'):-
        my_alias(ALIAS),
        call(rdf(ALIAS:A, rdf:type, ALIAS:C)).
        
explore(rdf(Subject, Predicate, Object),_, rdf(Subject, Predicate, Object) is true was 'found as a fact'):-
        call(rdf(kb:Subject, Predicate, Object)).

explore(Goal, _, Goal is true was 'found as fact'):-
        fact-- Goal.


% Explore predicates for User defined data.

% If there is a rule in knowledge base of the form  "if Condition then Goal",
% then explore Condition and use the result to construct answer.
        
explore(Goal, Trace, Goal is TruthValue was 'derived by' Rule from Answer):-
        Rule--if Condition then Goal,
        explore(Condition, [Goal by Rule|Trace], Answer),
        truth(Answer, TruthValue).

% If Goal is of the form G1 and G2 then explore G1 and now:
% if G1 is false then Answer is "G is false", else explore G2 and
% appropriately combine answers to both G1 and G2 into Answer.

explore(Goal1 and Goal2, Trace, Answer):- !,
        explore(Goal1, Trace, Answer1),
        continue(Answer1, Goal1 and Goal2, Trace, Answer).

% If Goal is of the form G1 or G2 then explore G1 and now:
% if G1 is true then Answer is "G is true", or alternatively
% explore G2 and appropriately combine answers to both G1 and G2 into Answer.  

explore(Goal1 or Goal2, Trace, Answer):-
        exploreyes(Goal1, Trace, Answer)                                        % Positive answer to Goal1
        ;
        exploreyes(Goal2, Trace, Answer).                                       % Positive answer to Goal2

explore(Goal1 or Goal2, Trace, Answer1 and Answer2):- !,
        not(exploreyes(Goal1, Trace, _)),
        not(exploreyes(Goal2, Trace, _)),                                       % No postive answer
        explore(Goal1, Trace, Answer1),                                         % Answer1 must be negative
        explore(Goal2, Trace, Answer2).                                         % Answer2 must be negative

explore(Goal, _, Goal was 'a derived fact'):-
        derived_fact(Goal).

explore(Goal, Trace, Goal is Answer was told):-
        useranswer(Goal, Trace, Answer).                                        % User:supplied Answer

exploreyes(Goal, Trace, Answer):-
        explore(Goal, Trace, Answer),
        positive(Answer).


continue(Answer1, _ and Goal2, Trace, Answer):-
        positive(Answer1),
        explore(Goal2, Trace, Answer2),
        (
                positive(Answer2),
                Answer = Answer1 and Answer2
                ;
                negative(Answer2),
                Answer = Answer2
        ).

continue(Answer1, _ and _, _, _):-
        negative(Answer1).

present_facts([]).
present_facts([(A,C)|Rest]):-
   assert(known(rdf_fact(A,rdfs_ns:subClassOf,C))),
   get_right(R),
   send(R, format, '%s isa %s\n', A, C),
   present_facts(Rest).



