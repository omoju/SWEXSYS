% Author: OMOJU  Miller
% Date: 3/29/2004
% A small knowledge base for identifying Objects.



rule1--if
                rdf(Object, kb:live, literal(at_sea)) and
                rdf(Object, kb:nostril, literal(external_tubular)) and
                rdf(Object, kb:bill, literal(hooked))
        then
                Object isa 'order_tubenose'.

rule2--if
                Object isa 'order_tubenose' and
                Object size large and
                Object wings long_narrow
        then
                Object isa family_albatross.

rule3--if
                Object isa bird and
                Object isa family_albatross and
                Object color white
        then
                Object isa laysan_albatross.

rule4--if
                Object isa bird and
                Object isa family_albatross and
                Object color dark
        then
                Object isa black_footed_albatross.

rule5--if
                Object isa bird and
                Object isa order_tubenose and
                Object size medium and
                Object flight flap_glide and
                Object color dark
        then
                Object isa fulmar.
rule6--if
                Object has hair
                or
                Object gives milk
        then
                Object isa mammal.

rule7--if
                Object has feathers
                or
                (Object flies and
                  Object lays eggs)
        then
                Object isa bird.

rule8--if
                Object isa mammal and
                (Object eats meat
                or
                Object has pointed_teeth and
                Object has claws and
                Object has forward_pointing_eyes)
        then
                Object isa carnivore.

rule9--if
                Object isa carnivore and
                Object has 'tawny colour' and
                Object has 'dark spots'
        then
                Object isa cheetah.

rule10--if
                Object isa carnivore and
                Object has 'tawny colour' and
                Object has 'black stripes'
        then
                Object isa tiger.

rule11--if
                Object isa bird and
                Object 'does not' fly and
                Object swims
        then
                Object isa penguin.






fact-- X isa animal :-
        member(X, [cheetah, tiger, penguin, bird]).
fact--_ and _ :- fail.
fact--_ or _ :- fail.
fact--_ was _ :- fail.
fact--_ from _ :- fail.
fact--_ gives _ :- fail.
fact--_ flies :- fail.
fact--_ swims :- fail.
fact--_ lays _ :- fail.
fact--_ eats _ :- fail.
fact--_ has _ :- fail.
fact--_ 'does not' _ :- fail.
fact--_ size _ :- fail.
fact--_ wings _ :- fail.
fact--_ color _ :- fail.
fact--_ flight _ :- fail.



askable(_ gives _, 'Object' gives 'What').
askable(_ flies, 'Object' flies).
askable(_ lays eggs, 'Object' lays eggs).
askable(_ eats _, 'Object' eats 'What').
askable(_ has _, 'Object' has 'Something').
askable(_ 'does not' _, 'Object' 'does not' 'DoSomething').
askable(_ swims, 'Object' swims).
askable(_ isa 'good flyer', 'Object' isa 'good flyer').
askable(_ size _, 'Object' size 'What').
askable(_ wings _, 'Object' wings 'How').
askable(_ color _, 'Object' color 'Is').
askable(_ flight _, 'Object' flight 'Is').

multivariate(eats).
multivariate(has).
multivariate('does not').


