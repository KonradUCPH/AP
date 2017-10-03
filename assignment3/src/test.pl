/* Imports by regular file import: "[tritbook], [test]."
 Run tests by "run_tests."

 The way to suppress Choicpoint warning: all(_ = [1]) */

g1([person(kara, [barry, clark]),
 person(bruce, [clark, oliver]),
 person(barry, [kara, oliver]),
 person(clark, [oliver, kara]),
 person(oliver, [kara])]).

g2([person(batman, [green_arrow, superman]),
 person(green_arrow, [supergirl]),
 person(supergirl, [flash, superman]),
 person(flash, [green_arrow, supergirl]),
 person(superman, [green_arrow, supergirl])]).

/*translation list between G1 and G2*/
a1([(kara,supergirl),
    (bruce,batman),
    (barry,flash),
    (clark,superman),
    (oliver,green_arrow)]).

a2([(1,a),(2,b),(3,c)]).

g3([person(a,[b,c]), person(x,[y,z])]).
g4([person(x,[z,y]), person(a,[c,b])]).
g5([person(1,[2,3]), person(4,[5,6])]).


:- begin_tests(twitbook).

test('like', all(_ = [1])) :-
    g1(G), likes(G,bruce,clark).
test('doesnt like', all(_ = [1])) :-
    g1(G), \+likes(G,bruce,kara).

test('disLike', all(_ = [1])) :-
    g1(G), dislikes(G,clark,bruce).
test('doesnt disLike', all(_ = [1])) :-
    g1(G), \+dislikes(G,bruce,clark).

test('getPerson', all(_ = [1])) :-
    g1(G), getPerson(G,clark,[oliver, kara]).
test('getPerson fail', all(_ = [1])) :-
    g1(G), \+getPerson(G,clark,[oliver, kara1]).

test('getPersonCompound', all(_ = [1])) :-
    g1(G), getPersonCompound(G,clark,person(clark, [oliver, kara])).
test('getPersonCompound fail', all(_ = [1])) :-
    g1(G), \+getPersonCompound(G,clark1,_).

test('pCompound', all(_ = [1])) :-
    pCompound(person(konrad, [luis, steffan]), konrad, [luis, steffan]).
test('pCompound fail when person is not a person', all(_ = [1])) :-
    \+pCompound(person1(konrad, [luis, steffan]), konrad, [luis, steffan]).
test('pCompound fail when person has other friends', all(_ = [1])) :-
    \+pCompound(person(konrad, [luis, steffan]), konrad, [steffan, luis]).

test('elem', all(_ = [1])) :-
    elem(k, [a,k]).
test('elem fail', all(_ = [1])) :-
    \+elem(k, [a,t]).

test('mySelect', all(_ = [1])) :-
    mySelect(k, [a,k], [a]).
test('mySelect fail', all(_ = [1])) :-
    \+mySelect(k, [a,k], [k]).

test('different', all(_ = [1])) :-
    g1(G), different(G, kara, bruce).
test('different fail for non existing person', all(_ = [1])) :-
    g1(G), \+different(G, kara, bruce1).
test('different fail', all(_ = [1])) :-
    g1(G), \+different(G, kara, kara).

test('popular', all(_ = [1])) :-
    g1(G), popular(G, kara).
test('not popular', all(_ = [1])) :-
    g1(G), \+popular(G, clark).

test('outcast', all(_ = [1])) :-
    g1(G), outcast(G, bruce).
test('not outcast', all(_ = [1])) :-
    g1(G), \+outcast(G, kara).

test('friendly', all(_ = [1])) :-
    g1(G), friendly(G, barry).
test('not friendly', all(_ = [1])) :-
    g1(G), \+friendly(G, kara).

test('hostile1', all(_ = [1])) :-
    g1(G), hostile(G, X), X = oliver.
test('hostile2', all(_ = [1])) :-
    g1(G), hostile(G, X), X = bruce.
test('not hostile', all(_ = [1])) :-
    g1(G), \+ hostile(G, kara).

test('admires1', [nondet]) :-
    g1(G), admires(G, X, Y),
    X = bruce, Y = barry.
test('admires2', all(_ = [1])) :-
    g1(G), admires(G, X, Y),
    X = oliver, Y = kara.
test('not admires1', all(_ = [1])) :-
    g1(G), \+ admires(G, barry, bruce).
test('not admires1', all(_ = [1])) :-
    g1(G), \+ admires(G, bruce, bruce).

test('indifferent1', all(_ = [1])) :-
    g1(G), indifferent(G, X, Y),
    X = oliver, Y = bruce.
test('indifferent2', all(_ = [1])) :-
    g1(G), indifferent(G, X, Y),
    X = clark, Y = bruce.
test('not indifferent1', all(_ = [1])) :-
    g1(G), \+ indifferent(G, kara, clark).
test('not indifferent2', all(_ = [1])) :-
    g1(G), \+ indifferent(G, bruce, bruce).


test('equal for graphs', all(_ = [1])) :-
    g3(G), g4(H), equalSP(G, H).
test('not equal for graphs', all(_ = [1])) :-
    g1(G), g4(H), \+ equalSP(G, H).

test('same_world 1', all(_ = [1])) :-
    g1(G), g2(H), a1(A), same_world(G, H, A).
test('same_world 2', all(_ = [1])) :-
    g1(G), g2(H), same_world(G, H, A),
    a1(B), A = B.
test('same_world 3', all(_ = [1])) :-
    g1(G), g2(H), same_world(G, H, A), 
    member((bruce,batman), A).
test('same_world 3', all(_ = [1])) :-
    g1(G), g2(H), same_world(G, H, A), 
    \+ member((bruce,supergirl), A).
test('not same_world1', all(_ = [1])) :-
    g1(G), g3(H), \+ same_world(G, H, _).
test('not same_world2', all(_ = [1])) :-
    g1(G), g2(H), a2(A), \+ same_world(G, H, A).

:- end_tests(twitbook).