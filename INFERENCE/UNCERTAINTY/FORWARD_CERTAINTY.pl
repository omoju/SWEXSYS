% Author: OMOJU THOMAS
% Date: 3/29/2004

/************************************************************************
*
* Simple forward chaining with certainty factors in prolog
*
* Supporting Files
* RULES_CERTAINTY.pl Rulebase
* COMMON_PROCEDURES_CERTAINTY.pl User-defined Operators.
*
*************************************************************************/

new_derived_fact(Goal, [], Answer, CF):-
        Rule--if Cond then Goal with CFRule,
        not(used_rule(Rule)),
        not(known(Goal, _)),
        composed_fact(Cond, [], Answer, CFList),
        calculate_cf(CFList, CFRule, CF, Goal),
        asserta(used_rule(Rule)).
        
calculate_cf(CFList, CFRule, CF, Goal):-
        composite_hypotheses(CFList, CFMin),
        uncertain_evidence(CFRule, CFMin, CF),
        co_concluding_rules(Goal, CF, _).
        
        

/***********************************************************************                                                                             *
*
* The procedure composed_fact(Goal, Trace,Answer)
*                                   %
* Finds Answer to a given Goal. Trace is a chain of ancestors goals and rules.
*'composed_fact' tends to find a positive answer to a question.
* Answer is 'false' only when all the possibilities have been investigated
* and they all resulted in 'false'
*
* Assume only one rule about each type of goal.
*
***********************************************************************/



composed_fact(rdf(A, B, C), _, rdf(A, B, C) is true was 'found as fact', CF):-
        rdf(A, B, C),
        CF is 1.

composed_fact(Goal, _, Goal is true was 'found as fact', CF):-
        known(Goal, CF).

composed_fact(Goal1 and Goal2, Trace, Answer, CFList):-
        composed_fact(Goal1, Trace, Answer1, CF1),
        continue(Answer1,CF1, Goal1 and Goal2, Trace, Answer, CFList).
        
composed_fact(Goal1 or Goal2, Trace, Answer1 and Answer2, CFList):-
        composed_fact(Goal1, Trace, Answer1, CF1),
        composed_fact(Goal2, Trace, Answer2, CF2),
        flatten([CF1,CF2], CFRule),
        maximum(CFRule, CFRul),
        flatten([CFRul], CFList).

composed_fact(Goal, Trace, Goal is TruthValue was 'derived by' Rule from Answer, CFList):-
        Rule--if Condition then Goal with CFRule,
        not(used_rule(Rule)),
        var_bind(TheObj),Goal=..[_|[TheObj |_]],
        composed_fact(Condition, [Goal by Rule|Trace], Answer, CFNewList),
        calculate_cf(CFNewList, CFRule, CF, Goal),flatten([CF], CFList),
        assert(known(Goal,CF)),
        assert(used_rule(Rule)),
        asserta(used(Condition, Goal)),
        get_right(R),
        term_to_atom(Goal, G),
        term_to_atom(CF,C_F),
        send(R, format, '%s with cf- %s\n', G, C_F),
        truth(Answer, TruthValue).

composed_fact(Goal, Trace, Goal is Answer was told, CF):-
        useranswer(Goal, Trace, Answer, CF).


        
% Bind to the known variable that SWEXSYS is forward chaining on.
var_bind(TheObj):-
                  known(A,_), A=..[_|[TheObj|_]].
                  

continue(Answer1, Cf1, _ and Goal2, Trace, Answer1 and Answer2,CFNew):-
        positive(Answer1),
        composed_fact(Goal2, Trace, Answer2, Cf2),
        flatten([Cf1,Cf2], CFNew),
        positive(Answer2).

continue(Answer1,_,_ and _, _, _, _):-
        negative(Answer1).

% -----------------------------------------------------------  %
%
% -----------------------------------------------------------  %
%         Utilities for Calculating Certainty Factor
% -----------------------------------------------------------  %
%
% -----------------------------------------------------------  %

composite_hypotheses(CFList, CFMin):-
         minimum(CFList, CFMin).

uncertain_evidence(CFRule, CFMin, CF):-
         CF is CFMin * CFRule.

co_concluding_rules(P, CFNew, CFFact):-
        retract(known(P, CFOld)),
        case(CFNew,CFOld, CFFact).
co_concluding_rules(_, CF, CF).

case(CFnew,CFold,CFfact) :-
     CFnew > 0,
     CFold > 0,!,
     CFfact is CFold + CFnew - CFold * CFnew.
case(CFnew,CFold,CFfact) :-
     CFnew < 0,
     CFold < 0,!,
     CFfact is CFold + CFnew + CFold * CFnew.
case(CFnew,CFold,CFfact) :-
     Numerator is CFnew + CFold,
     (CFnew >= 0, AbsCFnew is CFnew ;
      AbsCFnew is -CFnew),
     (CFold >= 0, AbsCFold is CFold ;
      AbsCFold is -CFold),
     minimum([AbsCFnew,AbsCFold],Min),
     Denominator is 1 - Min,
     (Denominator > 0,
      CFfact is Numerator/Denominator ;
      nl,
      write('Contradictory information found!'),
      nl,!,
      fail).

minimum([A],A) :- !.
minimum([A,B|L],M) :-
     (A < B,
      minimum([A|L],M));
     (A >= B,
      minimum([B|L],M)),!.

maximum([1|_],1) :- !.
maximum([A],A) :- !.
maximum([A,B|L],M) :-
     (A > B,
      maximum([A|L],M));
     (A =< B,
      maximum([B|L],M)),!.

/***************************************************************************
*
* The procedure present(Answer) displays the final result of a
* consultation session and generates the 'how' explanation.
*
* Answer includes both an answer to the user's question, and a
* proof tree showing how this conclusion was reached.
*
****************************************************************************/


%% Show which rule(s) was fired. this is basically the rule firing sequence.


ruleTrace:-
        used(CONDITION, GOAL),
        RULE--if CONDITION then GOAL with CF,
        nl,
        write(RULE),write(' : '),write('if, '),write(CONDITION),
        nl, tab(6),
        write('then, '),write(GOAL),write(' with cf= '),write(CF),
        nl,
        fail.

ruleTrace.




