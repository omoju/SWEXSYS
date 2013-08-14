/***********************************************************************                                                                             *
*                FORWARD.PL
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

:- dynamic rdf_fact/1.



new_derived_fact('Batch RDF Facts', _, 'Batch RDF Facts'):-
        findall((A,C),rdf(A, rdfs_ns:subClassOf, C),L),
        present_facts(L).

        
new_derived_fact(Goal, Trace, Answer):-
        Rule--if Cond then Goal,
        not(used_rule(Rule)),
        not(known(Goal)),
        composed_fact(Cond, Trace, Answer),
        asserta(used_rule(Rule)).
/***********************************************************************
*
* The procedure composed_fact(Goal, Trace,Answer)
*
* Finds Answer to a given Goal. Trace is a chain of ancestors goals and rules.
*'composed_fact' tends to find a positive answer to a question.
* Answer is 'false' only when all the possibilities have been investigated
* and they all resulted in 'false'
*
* Assume only one rule about each type of goal.
*
*************************************************************************/

composed_fact(rdf(A, B, C), _, rdf(A, B, C) is true was 'found as fact'):-
        rdf(A, B, C).

composed_fact(Goal, _, Goal is true was 'found as fact'):-
        known(Goal).

composed_fact(Goal, Trace, Goal is TruthValue was 'derived by' Rule from Answer):-
        Rule--if Condition then Goal,
        not(used_rule(Rule)),
        var_bind(TheObj),
        Goal=..[_|[TheObj|_]],
        composed_fact(Condition, [Goal by Rule|Trace], Answer),
        assert(known(Goal)),
        assert(used_rule(Rule)),
        asserta(used(Condition, Goal)),
        % Get Window
        get_right(View),
        term_to_atom(Goal, G),
        send(View, format, '%s\n', G),
        %write('Rule fired '), write(Rule), nl,
        %write('DERIVED: '), write(Goal), nl,
        truth(Answer, TruthValue).
        
composed_fact(Goal1 and Goal2, Trace, Answer):-
        composed_fact(Goal1, Trace, Answer1),
        continue(Answer1, Goal1 and Goal2, Trace, Answer).
        
composed_fact(Goal1 or Goal2, Trace, Answer):-
        exploreyes(Goal1, Trace, Answer)                                        % Positive answer to Goal1
        ;
        exploreyes(Goal2, Trace, Answer).                                       % Positive answer to Goal2

composed_fact(Goal1 or Goal2, Trace, Answer1 and Answer2):- !,
        not(exploreyes(Goal1, Trace, _)),
        not(exploreyes(Goal2, Trace, _)),                                       % No postive answer
        composed_fact(Goal1, Trace, Answer1),                                   % Answer1 must be negative
        composed_fact(Goal2, Trace, Answer2).                                   % Answer2 must be negative
        
composed_fact(Goal, Trace, Goal is Answer was told):-
        useranswer(Goal, Trace, Answer).

exploreyes(Goal, Trace, Answer):-
        composed_fact(Goal, Trace, Answer),
        positive(Answer).
        
% Bind to the known variable that SWEXSYS is forward chaining on.
var_bind(TheObj):-
                  known(A), A=..[_|[TheObj|_]].
                  

continue(Answer1, _ and Goal2, Trace, Answer):-
        positive(Answer1),
        composed_fact(Goal2, Trace, Answer2),
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
   
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::%
%
% The procedure present(Answer) displays the final result of a
% consultation session and generates the 'how' explanation.
%
% Answer includes both an answer to the user's question, and a
% proof tree showing how this conclusion was reached.
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::%


%% Show which rule(s) was fired. this is basically the rule firing sequence.


ruleTrace:-
        used(CONDITION, GOAL),
        RULE--if CONDITION then GOAL,
        nl,
        write(RULE),
        printstring(" : "),
        printstring("if, "),
        write(CONDITION),
        nl, tab(6),
        printstring("then, "),
        write(GOAL),
        nl,
        fail.

ruleTrace.

printstring([]).
printstring([H|T]):-
        put(H), printstring(T).




