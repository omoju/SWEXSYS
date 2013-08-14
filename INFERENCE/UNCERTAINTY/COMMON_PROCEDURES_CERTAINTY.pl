% Author: Omoju Thomas
% Date: 2/23/2004
% Common Modules



:- dynamic known/2, used/2, used_rule/1, wastold/4, end_answers/1, copy/1, lastindex/1.
:- op(900, xfx, --).
:- op(880, xfx, then).
:- op(870, fx, if).
:- op(800, xfx, was).
:- op(600, xfx, from).
:- op(600, xfx, by).
:- op(550, xfy, or).
:- op(540, xfy, and).
:- op(530, xfy, with).
:- op(300, fx, 'derived by').
:- op(100, xfx, [has, gives, 'does not', eats, lays, isa, size, wings, color, flight]).
:- op(100, xf, [swims, flies]).

%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::%

getreply(Reply):-
        make_dialog(D, getreply),
        get(D, confirm_centered, Reply),
        %write(Reply),
        send(D, destroy).


% useranswer() is an implementation of querying the user for information.
% It queries the user when needed and also explains to the user why it is
% needed.

useranswer(Goal, Trace, Answer, Cf):-
        askable(Goal, _),
        freshcopy(Goal, Copy),                                  % Variables in Goal renamed.
        useranswer(Goal, Copy, Trace, Answer, 1, Cf).

% Do not ask the user about instantiated goals.

useranswer(Goal,_,_,_,N,_):-
        N > 1,
        instantiated(Goal), !,                                  % No variables in Goal
        fail.



% Don't ask questions about attributes that are not multivariate.

useranswer(Goal, _, _, _, _, _):-
        wastold(Temp, _,_,_),
        Temp=..[Verb|[Subject|[_]]],
        Goal=..[Verb|[Subject|[_]]],
        not( multivariate(Verb) ), !,
        fail.

% Is Goal implied true or false for all instatiations?

useranswer(Goal, Copy, _, Answer, _, Cf):-
        wastold(Copy, Answer,_,Cf),
        instance_of(Copy,Goal), !.                              % Answer to Goal implied

% Retrieve known solutions, indexed from N on, for Goal

useranswer(Goal,_,_,true,N, Cf):-
        wastold(Goal, true, M,Cf),
        M >= N.

% Has everything already been said about Goal?

useranswer(Goal, Copy, _, _, _, _):-
        end_answers(Copy),
        instance_of(Copy, Goal),                                % Everything was already said about Goal
        fail.

% Ask the user for more solutions

useranswer(Goal, _, Trace, Answer, N, Cf):-
       %testask(Goal, Trace, Answer, N).
        askuser(Goal, Trace, Answer, N, Cf).

askuser(Goal, Trace, Answer, N, Cf):-
        askable(Goal, ExternalFormat),
        format(Goal, ExternalFormat, Question, [], Variables),  % Get question format
        ask(Goal, Question, Variables, Trace, Answer, N, Cf).

ask(Goal, Question, Variables, Trace, Answer, N, Cf):-
          get_left(View),
        (
                Variables = [], !,
                send(View, format, '%s','Certainty Factor: ')
                ;
                send(View, format, '%s\n','Certainty Factor: ')
        ),
        term_to_atom(Question, Q),
        send(View, format, '%s', Q),
        getreply(Reply),
        send(View, format, ' = %s\n', Reply),
        atom_to_term(Reply, Cf, _),
        !,
        process(Cf, Goal, Question, Variables, Trace, Answer, N, Cf).
        
process(quit, _, _, _, _, _, _, _):-
        call(abort).
process(q, _, _, _, _, _, _, _):-
        call(abort).

process(why, Goal, Question, Variables, Trace, Answer, N, Cf):-
        showtrace(Trace),
        ask(Goal, Question, Variables, Trace, Answer, N, Cf).

process(Cf, Goal, _, Variables, Trace, true, _, Cf):-
        nextindex(Next),                                                        % Get new free index for was told
        Next1 is Next + 1,
        (
                askvars(Variables),
                assertz(wastold(Goal, true, Next, Cf))                          % Record solution
                ;
                freshcopy(Goal, Copy),                                          % Copy of Goal
                useranswer(Goal, Copy, Trace, _, Next1, Cf) % More answers?
        ).

process(no, Goal, _, _, _, no,_, Cf):-
        freshcopy(Goal, Copy),
        wastold(Copy, true, _, Cf), !,                                          % 'no' means: no more solutions
        assertz(end_answers(Goal)),                                             % Mark end of all answers
        fail
        ;
        nextindex(Next),                                                        % Next free index for wastold
        assertz(wastold(Goal, no, Next, Cf)).

format(Var, Name, Name, Vars, [Var/Name | Vars]):-
        var(Var), !.

format(Atom, Name, Atom, Vars, Vars):-
        atomic(Atom), !,
        atomic(Name).

format(Goal, Form, Question, Vars0, Vars):-
        Goal=..[Functor | Args1],
        Form=..[Functor | Forms],
        formatall(Args1, Forms, Args2, Vars0, Vars),
        Question=..[Functor | Args2].

formatall([], [], [], Vars, Vars).

formatall([X|XL], [F|FL], [Q|QL], Vars0, Vars):-
        formatall(XL, FL, QL, Vars0, Vars1),
        format(X, F, Q, Vars1, Vars).

askvars([]).
askvars([Variable/_Name | Variables]):-
        make_dialog(D, get_vars),
        get(D, confirm_centered, Variable),
        write(Variable),
        send(D, destroy),
        askvars(Variables).

showtrace([]):-
        % get xpce view
        get_left(View),
        send(View, format, '%\ns\n', 'This was your question').

showtrace([Goal by Rule| Trace]):-
        % get xpce view
        get_left(View),
        send(View, format, '%\ns\n','To investigate, by'),
        term_to_atom(Rule, R),
        term_to_atom(Goal, G),
        send(View, format, '%s , ',R),
        send(View, format, '%s , ',G),
        showtrace(Trace).

instantiated(Term):-
        numbervars(Term, 0, 0).                                                 % No variables in Term

% instance_of(T1, T2): instance of T1 is T2; that is,
% term T1 is more general than T2 or equally general as T2

instance_of(Term, Term1):-
        freshcopy(Term1, Term2),
        numbervars(Term2, 0, _), !,
        Term = Term2.

freshcopy(Term, FreshTerm):-
        asserta(copy(Term)),
        retract(copy(FreshTerm)), !.

nextindex(Next):-
        retract(lastindex(Last)), !,
        Next is Last + 1,
        assert(lastindex(Next)).

%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::%


truth(_ is TruthValue was _, TruthValue):- !.

% To compensate for joint positive answers with and as the conjuction.

truth(Answer1 and Answer2, TruthValue):-
        truth(Answer1, true),
        truth(Answer2, true), !,
        TruthValue = true
        ;
        TruthValue = false.

positive(Answer):-
        truth(Answer, true).

negative(Answer):-
        truth(Answer, false).

markstatus(positive):-
        retract(no_positive_answer_yet), !
        ;
        true.

markstatus(negative):-
        assert(no_positive_answer_yet).

getquestion(Question):-
        nl, write('Question, please: '), nl,
        read(Question).

%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::%
%                                                                                       %
% The procedure present(Answer) displays the final result of a                          %
% consultation session and generates the 'how' explanation.                             %
%                                                                                       %
% Answer includes both an answer to the user's question, and a                          %
% proof tree showing how this conclusion was reached.                                   %
%                                                                                       %
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::%

present(Ans, V):-
      showconclusion(Ans, V),
      send(V, format, '%s\n', 'Would you like to see how?'),
      getreply(Reply),
      Reply = yes,
      myshow(Ans, V).

showconclusion(Answer1 and Answer2, V) :- !,
        showconclusion(Answer1, V),
        send(V, format, '%s', ' and '),
        showconclusion(Answer2, V).

showconclusion(Conclusion was _, V):-
        term_to_atom(Conclusion, Con),
        send(V, format, '%s\n',Con).

% 'myshow' displays a complete solution tree

myshow(Solution, V):-
        send(V, format, '\n'),
        myshow(Solution,V,0), !.

myshow(Answer1 and Answer2, V, H):-
       % Indent by H
        myshow(Answer1, V, H),
        my_tab(H, V),
        send(V, format, '%s\n', 'and'),
        myshow(Answer2, V, H).

myshow(Answer was Found, V, H):-
        my_tab(H, V), writeans(Answer, V),
        nl, my_tab(H, V),
        send(V, format, '%s',' was '),
        show1(Found, V, H).

show1(Derived from Answer, V, H):- !,
        term_to_atom(Derived, D),
        send(V, format, '%s',D),
        send(V, format, '%s',' from '),
        nl, H1 is H + 4,
        myshow(Answer, V, H1).

show1(Found, V, _):-
        term_to_atom(Found, F),
        send(V, format, '%s\n', F).

writeans(Goal is true, V):-
        term_to_atom(Goal, G),
        send(V, format, '%s',G).

writeans(Answer, V):-
        term_to_atom(Answer, A),
        send(V, format, '%s',A).

my_tab(0, _).
my_tab(H, V):-
       send(V, format, '\t'),
       N is H - 1,
       my_tab(N, V).

dialog(getreply,
       [ object        :=
           Getreply,
         parts         :=
           [ Getreply := dialog('Get Certainty Factor'),
             Reply     := text_item('Certainty Factor [0, 1]'),
             Enter    := button('Enter'),
             Cancel   := button('Cancel')
           ],
         modifications :=
           [ Reply   := [ length    := 26,
                         alignment := left
                       ],
             Enter  := [ alignment := right
                       ],
             Cancel := [ alignment := right
                       ]
           ],
         layout        :=
           [ below(Enter, Reply),
             right(Cancel, Enter)
           ],
         behaviour     :=
           [ Enter  := [ message := message(Getreply,
                                            return,
                                            Reply?selection)
                       ],
             Cancel := [ message := message(Getreply, return, no)
                       ]
           ]
       ]).
dialog(get_vars,
       [ object        :=
           Get_vars,
         parts         :=
           [ Get_vars := dialog('Ask Variable'),
             Variable     := text_item('Variable Name'),
             Enter    := button('Enter'),
             Cancel   := button('Cancel')
           ],
         modifications :=
           [ Variable   := [ length    := 26,
                         alignment := left
                       ],
             Enter  := [ alignment := right
                       ],
             Cancel := [ alignment := right
                       ]
           ],
         layout        :=
           [ below(Enter, Variable),
             right(Cancel, Enter)
           ],
         behaviour     :=
           [ Enter  := [ message := message(Get_vars,
                                            return,
                                            Variable?selection)
                       ],
             Cancel := [ message := message(Get_vars, destroy)
                       ]
           ]
       ]).



