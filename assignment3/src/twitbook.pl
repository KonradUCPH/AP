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

a1([(kara,supergirl),
    (bruce,batman),
    (barry,flash),
    (clark,superman),
    (oliver,green_arrow)]).

/* gives acces to the atom of a Person.
    reads from a Graph g
    getPersonAtom(G, Name, Friends) */
getPersonAtom(G, Name, Friends) :-
    getPerson(G, Name, Person),
    pCompound(Person, Name, Friends).

/* reads a person compound from a graph G
    getPerson(G, Name, person(Name, Friends)) */
getPerson([person(Name,Friends)|_], Name, person(Name,Friends)).
getPerson([_|T], Name, Person) :- getPerson(T, Name, Person).

/* utillity function for converting between a person compound
    and its single atoms*/
pCompound(person(Name,Friends), Name, Friends).

/* is the given element in the List ?*/
myMember(X, [X|_]).
myMember(X, [_|T]) :- myMember(X, T).

/* is the given element in the List ?
    only works for names of people in the network*/
notMember(_, _, []).
notMember(G, X, [Y|T]) :-
    different(G, X, Y),
    notMember(G, X, T).

/* my implementation of select.
    removes the given argument from the list*/
mySelect(X, [X|XS], XS).
mySelect(X, [Y|XS], [Y|YS]) :-
    mySelect(X, XS, YS).

/* likes(Graph, Name, Name)*/
likes(G, X, Y) :-
    getPersonAtom(G, X, Friends),
    myMember(Y, Friends).

notLikes(G, X, Y) :-
    getPersonAtom(G, X, Friends),
    notMember(G, Y, Friends).


/* are X and Y different members of G?
    different(G, Name, Name)*/
different(G, Nx, Ny) :-
    getPerson(G, Nx, Px),
    getPerson(G, Ny, Py),
    mySelect(Px, G, G1),
    mySelect(Py, G1, _).

/* dislikes(Graph, Name, Name)*/
dislikes(G, X, Y) :-
    likes(G, Y, X), % Y likes X
    getPersonAtom(G, X, FriendsX), % get Friendlist of X
    notMember(G, Y, FriendsX). % all on Xs friendlist are different from Y

/* popular (Graph, Name) is liked back by everyone*/
popular(G, X) :-
    getPersonAtom(G, X, Friends),
    likedByEveryone(G, X, Friends).

/* checks if X is liked by everyone on the List*/
likedByEveryone(_, _, []).
likedByEveryone(G, X, [H|T]) :-
    likes(G, H, X),
    likedByEveryone(G, X, T).

/* outcast(Graph, Name) is not liked back by anyone*/
outcast(G, X) :-
    getPersonAtom(G, X, Friends),
    dislikedByEveryone(G, X, Friends).

/* Checks if X is disliked by everone on the list*/
dislikedByEveryone(_, _, []).
dislikedByEveryone(G, X, [H|T]) :-
    dislikes(G, H, X),
    dislikedByEveryone(G, X, T).

/*checks if x dislikes everone on the list*/
dislikesEveryone(_, _, []).
dislikesEveryone(G, X, [H|T]) :-
    dislikes(G, X, H),
    dislikesEveryone(G, X, T).



/* friendly(Graph, Name) is liked back by anyone*/
friendly(G, X) :-
    getPersonAtom(G, X, Friends),
    likedByEveryone(G, X, Friends).

/* generates a list of everyone who likes X
    likedBy(Graph, Graph, x, [list of people who like X])*/
likedBy(G, [P|T], X, [Name|XS]) :-
    pCompound(P, Name, Friends),
    myMember(X, Friends),
    likedBy(G, T, X, XS).
likedBy(G, [P|T], X, XS) :-
    pCompound(P, _, Friends),
    notMember(G, X, Friends),
    likedBy(G, T, X, XS).
likedBy(_, [], _, []).

/* True if X doesnt like anyone who likes him*/
hostile(G, X) :-
    likedBy(G, G, X, PeopleWhoLikeX),
    dislikesEveryone(G, X, PeopleWhoLikeX).

/* admires = transitive like*/
admires(G, X, Y) :-
    different(G, X, Y), % X and Y must be different
    admiresList(G, [X], Y, [], VS),
    myMember(Y, VS). % did we ever visit Y?

/* admires(Graph, List of current options, 
    Target, Visited List, updated visited list)
        ** no guarantee for efficiency ;)** */
admiresList(G, [X| _], Y, VS, [X, Y|VS]) :-
    likes(G, X, Y). % target found
admiresList(_, [X| _], _, VS, VS) :-
    myMember(X, VS). % X was already visited
admiresList(_, [], _, VS, VS).
admiresList(G, [X| XS], Y, VS, VS2) :-
    notLikes(G, X, Y), % target not found, continue serach and not X as visited
    notMember(G, X, VS), % and X not already visited
    getPersonAtom(G, X, FriendsX),
    admiresList(G, FriendsX, Y, [X | VS], VS1), % check friends of X
    admiresList(G, XS, Y, VS1, VS2). % continue decent of initial list

/* indifferent = opposite of admires. So if the 
    result is not in the list of visited persons, 
    X is indifferent to Y*/
indifferent(G, X, Y) :-
    different(G, X, Y), % X and Y must be different
    admiresList(G, [X], Y, [], VS),
    notMember(G, Y, VS). % did we ever visit Y if yes, X admires Y?

sameWorld(G, H, A) :-
    translate(G, G, A, G1),
    equal(G1, H).

/* translate(Graph g, Graph G, translation table, Translates Graph)*/
translate(_, [], _, []).
translate(G, [person(Name, Friends) | GS], A, 
  [person(TranslatedName, TranslatedFriends)|TNS]) :-
    translateName(G, Name, A, TranslatedName),
    translateNames(G, Friends, A, TranslatedFriends),
    translate(G, GS, A, TNS).

translateName(_, Name, [(Name, TranslatedName)| _], TranslatedName).
translateName(G, Name, [(Name1, _)| AS], TranslatedName) :-
    different(G, Name, Name1),
    translateName(G, Name, AS, TranslatedName).

translateNames(_, [], _, []).
translateNames(G, [Name|NS], A, [TranslatedName|TNS]) :-
    translateName(G, Name, A, TranslatedName),
    translateNames(G, NS, A, TNS).

/* Checks if two lists are equal*/
equal([],[]). %TODO: fix for persons and friend lists
equal([X|XS], YS) :-
    mySelect(X, YS, YS1),
    equal(XS, YS1).
