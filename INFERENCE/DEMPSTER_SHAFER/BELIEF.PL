:- dynamic bpa0 /3.
:- dynamic bel0 /3.
:- dynamic pos0 /3.
:- dynamic lastindex /1.
:- dynamic com /3.



%---------------------------------------------------
% state space, event
%---------------------------------------------------
event(E):-
   states(A),
   bag1(E,A,_N).

sevent(X):-
   event(X),
   sort(X,X).

all_event(S):-setof(E,event(E),S).

all_sevent(S):-setof(E,sevent(E),S).

cevent(E,X):-
   complementary_event(E,X).

complementary_event(E,X):-
   states(A),
   event(X),
   subtract(A,X,C),
   sevent(E),
   seteq(C,E).

state(S):- states(A),member(S,A).

%
% -----------------------------------------------------------  %
%   Utilities for list operations
% -----------------------------------------------------------  %

% equality for pair of set
% -----------------------------------------------------------  %
seteq(X,Y):-
   sort(X,Sort),
   sort(Y,Sort).
%
%
% descending/ascending natural number sequence less than N.
% -----------------------------------------------------------  %

dnum_seq([],N):-N<0,!.
dnum_seq([0],1).
dnum_seq([A|Q],N):-
   A is N - 1,
   length(Q,A),
   dnum_seq(Q,A).
anum_seq(Aseq,N):-dnum_seq(Dseq,N),sort(Dseq,Aseq).

dnum_seq1(Q,N):-
   M is N + 1,
   dnum_seq(Q0,M),
   subtract(Q0,[0],Q).
anum_seq1(Q,N):-
   M is N + 1,
   anum_seq(Q0,M),
   subtract(Q0,[0],Q).
%
% -----------------------------------------------------------  %
bag0([],_A,0).
bag0([C|B],A,N):-
   length([C|B],N),
   bag0(B,A,_N1),
   member(C,A).
zeros(Zero,N):-bag0(Zero,[0],N).
ones(One,N):-bag0(One,[1],N).

%
% bag1/3 : do not allow multiplicity
% -----------------------------------------------------------  %
bag1([],_A,0).
bag1([C|B],A,N1):-
  \+var(A),
  length(A,L),
  anum_seq(Q,L),
  member(N,Q),
  length(B,N),bag1(B,A,N),N1 is N + 1,
  member(C,A),\+member(C,B).

% a sequence of binary choice for a list:
%--------------------------------------------------
list_projection([],[],[]).
list_projection([X|Y],[_A|B],C):-
   X = 0,
   list_projection(Y,B,C).
list_projection([X|Y],[A|B],[A|C]):-
   X = 1,
   list_projection(Y,B,C).
%
% subset_of/3 : subset-enumeration
% -----------------------------------------------------------  %
subset_of(A,N,As):-
   length(As,L),
   length(D,L),
   list_projection(D,As,B),
   length(B,N),
   sort(B,A).
%
% complementary list projection
%--------------------------------------------------
% added: 10 Jan 2003.
c_list_projection(X,Y,Z):-
   complement(X,XC,_N),
   list_projection(XC,Y,Z).

complement(X,XC,N):-
   \+ (var(X),var(N)),
   bag0(X,[1,0],N),
   zeros(Zero,N),
   ones(One,N),
   replace(X,Zero,One,XC).

% -----------------------------------------------------------  %
% Arithmetic
% -----------------------------------------------------------  %
%
% evaluation of a nummerical value
% -----------------------------------------------------------  %
eval_number(X,X1):-
   X1 is X,
   number(X1).

%
% max,min
% -----------------------------------------------------------  %
max_of(X,[X]).
max_of(Z,[X|Y]):-
   max_of(Z1,Y),
   (X > Z1 -> Z=X; Z=Z1).
min_of(X,[X]).
min_of(Z,[X|Y]):-
   min_of(Z1,Y),
   (X < Z1 -> Z=X; Z=Z1).

% sum
% -----------------------------------------------------------  %
sum([],0).
sum([X|Members],Sum):-
   sum(Members,Sum1),
   Sum is Sum1 + X.

% added: 27 feb 2003.
sum_eq([],0,0).
sum_eq([X],X,X).
sum_eq([X|Members],Eq,Sum):-
   Members \= [],
   sum_eq(Members,Eq1,Sum1),
   Eq = Eq1 + X,
   Sum is Sum1 + X.


% -----------------------------------------------------------  %
%
% SWEXSYS IMPLEMENTATION OF DEMPSTER_SHAFER THEORY OF UNCERTAIN
% EVIDENCE
% -----------------------------------------------------------  %



% Dempster-Shafer Combination Function
mass_intersection(Z, X, Y, Mz, Mx, My):-
          event(X), X \=[],
          event(Y), Y \=[],
          intersection(X, Y, Z),
          Z \=[],
          Mz is Mx * My.

% -----------------------------------------------------------  %
%
%  belief function
%  Data_source: data source in scenario
%  E = event
%  Bel = strenght of belief
%
% -----------------------------------------------------------  %
belief(Data_source, E, Bel):-
        E \= [],
        length(E, L),
        findall(N-Mn,
          (
          event(N), N \=[],
          length(N,L2),
          L2 < L,
          subset(N, E),
          mass(Data_source,N,Mn)
          ),
          Lit),
        sum_list(Lit, M),
        mass(Data_source,E,Me),
        Bel is Me + M.

% -----------------------------------------------------------  %
%
%  plausibility function
%  Data_source: data source in scenario
%  E = event
%  Bel = strenght of belief
%
% -----------------------------------------------------------  %
plausibility(Data_Source, E, Pls):-
   event(E),
   \+ E = [],
   findall(M-Mm,
     (
       sevent(F),
       F \= [],
       mass(Data_Source, F, Mf),
       mass(Data_Source, E, Me),
       mass_intersection(M, F, E, Mm, Mf, Me),
       M \= []
     ),
   G),
   sum_list(G, Pls).

        
% -----------------------------------------------------------  %
%
%  Mass function
%  Data_source: data source in scenario
%  Ev = event
%  Mass = basic probability Assignment
%
% -----------------------------------------------------------  %
mass(Data_source, Ev, Mass):-
               bpa0(Data_source, Ev, Mass).
mass(_, _, 0).


   
sum_list([],0).
sum_list([_-Num|Rest], N):-
                       sum_list(Rest, N1),
                       N is Num + N1.

% -----------------------------------------------------------  %
%
%  Combination Function for Masses
%  Max: Maximum number of data sources
%
% -----------------------------------------------------------  %

combine(0).
combine(MAX):-

% Where N and Max are data sources
% Where Z, X, and Y are subsets of the theta
% Where Mz, Mx, and My are the masses of
% Z, X, and Y.

             N is MAX - 1,
             findall(
               Z-Mz,
                 (
                 bpa0(N, X, Mx),
                 bpa0(MAX, Y, My),
                 mass_intersection(Z, X, Y, Mz, Mx, My)
                 ),
               List),
             update_mass(List),
             result,
             retractall(com(_,_,_)),
             combine(N, result).


combine(0,result).
combine(MAX, result):-
% Where N and Max are data sources
% Where Z, X, and Y are subsets of the theta
% Where Mz, Mx, and My are the masses of
% Z, X, and Y.

             N is MAX - 1,
             findall(
               Z-Mz,
                 (
                 bpa0(N, X, Mx),
                 bpa0(result, Y, My),
                 mass_intersection(Z, X, Y, Mz, Mx, My)
                 ),
               List),
             update_mass(List),
             result,
             retractall(com(_,_,_)),
             combine(N, result).

% -----------------------------------------------------------  %
%
% -----------------------------------------------------------  %
%         Utilities for Combination Function
% -----------------------------------------------------------  %
%
% -----------------------------------------------------------  %

update_mass([E-M|Rest]):-
                (com(result, E, M2)->
                (
                retract(com(result, E, M2)),
                M3 is M + M2,
                assert(com(result,E,M3)),
                update_mass(Rest)
                ); fail).

update_mass([E-M|Rest]):-
                assert(com(result,E,M)),
                update_mass(Rest).
update_mass([]).

find_max_data_source(MAX):-
                        findall(N, bpa0(N,_,_), List), max_of(MAX, List).
                        
result:-
            findall(E-M, com(result, E, M), List),
            result(List).

result([]).
result([E-M|Rest]):-
                      (
                      evd(Name, bpa0(result, E, _), De),
                      bpa0(result, E,_)
                      ->
                      (
                        retract(evd(Name, bpa0(result, E, _), _)),
                        assert(evd(Name, bpa0(result, E, M),De)),
                        retract( bpa0(result, E,_)),
                        assert(bpa0(result, E, M)),
                        result(Rest)
                      ); fail).
result([E-M|Rest]):-
                     (
                      bpa0(result, E,_)
                      ->
                      (
                        evd(Name, bpa0(result, E, _), De),
                        assert(evd(Name, bpa0(result, E, M), De)),
                        retract( bpa0(result, E,_)),
                        assert(bpa0(result, E, M)),
                        result(Rest)
                      ); fail).
result([E-M|Rest]):-
                      evd(Name, bpa0(_, E, _), De),
                      assert(evd(Name, bpa0(result, E, M), De)),
                      assert(bpa0(result, E, M)),
                      result(Rest).


