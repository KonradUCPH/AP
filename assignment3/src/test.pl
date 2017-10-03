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

g3([person(a,[b,c]), person(x,[y,z])]).
g4([person(x,[z,y]), person(a,[c,b])]).
g5([person(1,[2,3]), person(4,[5,6])]).


:- begin_tests(twitbook).

g1([person(kara, [barry, clark]),
 person(bruce, [clark, oliver]),
 person(barry, [kara, oliver]),
 person(clark, [oliver, kara]),
 person(oliver, [kara])]).


test(like, all(_ = [1])) :-
    g1(G), likes(G,bruce,clark).

test('doesnt like', all(_ = [1])) :-
    g1(G), \+likes(G,bruce,kara).

test(disLike, all(_ = [1])) :-
    g1(G), dislikes(G,clark,bruce).

test('doesnt disLike', all(_ = [1])) :-
    g1(G), \+dislikes(G,bruce,clark).

test(getPersonAtom, all(_ = [1])) :-
    g1(G), getPerson(G,clark,[oliver, kara]).

test('getPersonAtom fail', all(_ = [1])) :-
    g1(G), \+getPerson(G,clark,[oliver, kara1]).

test(getPerson, all(_ = [1])) :-
    g1(G), getPersonCompound(G,clark,person(clark, [oliver, kara])).

test('getPerson fail', all(_ = [1])) :-
    g1(G), \+getPersonCompound(G,clark1,_).

test(pCompound, all(_ = [1])) :-
    pCompound(person(konrad, [luis, steffan]), konrad, [luis, steffan]).

test('pCompound fail when person is not a person', all(_ = [1])) :-
    \+pCompound(person1(konrad, [luis, steffan]), konrad, [luis, steffan]).

test('pCompound fail when person has other friends', all(_ = [1])) :-
    \+pCompound(person(konrad, [luis, steffan]), konrad, [steffan, luis]).

test(elem, all(_ = [1])) :-
    elem(k, [a,k]).

test('elem fail', all(_ = [1])) :-
    \+elem(k, [a,t]).

test(mySelect, all(_ = [1])) :-
    mySelect(k, [a,k], [a]).

test('mySelect fail', all(_ = [1])) :-
    \+mySelect(k, [a,k], [k]).

test(different, all(_ = [1])) :-
    g1(G), different(G, kara, bruce).

test('different fail for non existing person', all(_ = [1])) :-
    g1(G), \+different(G, kara, bruce1).

test('different fail', all(_ = [1])) :-
    g1(G), \+different(G, kara, kara).

test(popular, all(_ = [1])) :-
    g1(G), popular(G, kara).

test('not popular', all(_ = [1])) :-
    g1(G), \+popular(G, clark).

test(outcast, all(_ = [1])) :-
    g1(G), outcast(G, bruce).

test('not outcast', all(_ = [1])) :-
    g1(G), \+outcast(G, kara).

test(friendly, all(_ = [1])) :-
    g1(G), friendly(G, barry).

test('not friendly', all(_ = [1])) :-
    g1(G), \+friendly(G, kara).

test('equal for graphs', all(_ = [1])) :-
    g3(G), g4(H), equalSP(G, H).

test('not equal for graphs', all(_ = [1])) :-
    g1(G), g4(H), \+ equalSP(G, H).

:- end_tests(twitbook).