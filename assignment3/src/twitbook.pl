/*
    Assignment 3: Prolog
    Konrad Gnoinski, Steffen Czolbe
*/


/* gives acces to the atom of a Person.
    reads from a Graph g
    getPerson(G, Name, Friends) */
getPerson(G, Name, Friends) :-
    getPersonCompound(G, Name, Person),
    pCompound(Person, Name, Friends).

/* reads a person compound from a graph G
    getPersonCompound(G, Name, person(Name, Friends)) */
getPersonCompound([person(Name,Friends)|_], Name, person(Name,Friends)).
getPersonCompound([_|T], Name, Person) :- 
    getPersonCompound(T, Name, Person).

/* utillity function for converting between a person compound
    and its single atoms*/
pCompound(person(Name,Friends), Name, Friends).

/* is the given element in the List ?*/
elem(X, [X|_]).
elem(X, [_|T]) :- elem(X, T).

/* is the given element in the List ?
    only works for names of people in the network*/
notElem(_, _, []).
notElem(G, X, [Y|T]) :-
    different(G, X, Y),
    notElem(G, X, T).

/* is a List a subset of another?*/
subset([], _).
subset([X|XS],YS) :-
    elem(X, YS),
    subset(XS, YS).


/* my implementation of select.
    removes the given argument from the list*/
mySelect(X, [X|XS], XS).
mySelect(X, [Y|XS], [Y|YS]) :-
    mySelect(X, XS, YS).

/* likes(Graph, Name, Name)*/
likes(G, X, Y) :-
    getPerson(G, X, Friends),
    elem(Y, Friends).

notLikes(G, X, Y) :-
    getPerson(G, X, Friends),
    notElem(G, Y, Friends).

/* are X and Y different members of G?
    different(G, Name, Name)*/
different(G, Nx, Ny) :-
    getPersonCompound(G, Nx, Px),
    getPersonCompound(G, Ny, Py),
    mySelect(Px, G, G1),
    mySelect(Py, G1, _).

/* dislikes(Graph, Name, Name)*/
dislikes(G, X, Y) :-
    likes(G, Y, X), % Y likes X
    getPerson(G, X, FriendsX), % get Friendlist of X
    notElem(G, Y, FriendsX). % all on Xs friendlist are different from Y

/* popular (Graph, Name) is liked back by everyone*/
popular(G, X) :-
    getPerson(G, X, Friends),
    likedByEveryone(G, X, Friends).

/* outcast(Graph, Name) is not liked back by anyone*/
outcast(G, X) :-
    getPerson(G, X, Friends),
    dislikedByEveryone(G, X, Friends).


/* checks if X is liked by everyone on the List*/
likedByEveryone(_, _, []).
likedByEveryone(G, X, [H|T]) :-
    likes(G, H, X),
    likedByEveryone(G, X, T).
    
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

/* friendly(Graph, Name) is liking back anyone who likes her*/
friendly(G, X) :-
    likedBy(G, X, LikedBy),
    getPerson(G, X, Likes),
    subset(LikedBy, Likes).

/* True if X doesnt like anyone who likes him*/
hostile(G, X) :-
    getNames(G, NamesG),
    elem(X,NamesG),
    likedBy(G, X, PeopleWhoLikeX),
    dislikesEveryone(G, X, PeopleWhoLikeX).

/* generates a list of everyone who likes X
    likedBy(Graph, x, [list of people who like X])*/
likedBy(G, X, Peolple) :-
    likedBy(G, G, X, Peolple).

likedBy(G, [P|T], X, [Name|XS]) :-
    pCompound(P, Name, Friends),
    elem(X, Friends),
    likedBy(G, T, X, XS).
likedBy(G, [P|T], X, XS) :-
    pCompound(P, _, Friends),
    notElem(G, X, Friends),
    likedBy(G, T, X, XS).
likedBy(_, [], _, []).

/* admires = transitive like*/
admires(G, X, Y) :-
    different(G, X, Y), % X and Y must be different
    admiresList(G, [X], Y, [], VS),
    elem(Y, VS). % did we ever visit Y?

/* admires(Graph, List of current options, 
    Target, Visited List, updated visited list)
        ** no guarantee for efficiency ;)** */
admiresList(G, [X| _], Y, VS, [X, Y|VS]) :-
    likes(G, X, Y). % target found
admiresList(G, [X| T], Y, VS, VS1) :-
    elem(X, VS), % X was already visited
    admiresList(G, T, Y, VS, VS1).
admiresList(_, [], _, VS, VS).
admiresList(G, [X| XS], Y, VS, VS2) :-
    notLikes(G, X, Y), % target not found, continue serach and not X as visited
    notElem(G, X, VS), % and X not already visited
    getPerson(G, X, FriendsX),
    admiresList(G, FriendsX, Y, [X | VS], VS1), % check friends of X
    admiresList(G, XS, Y, VS1, VS2). % continue decent of initial list

/* indifferent = opposite of admires. So if the 
    result is not in the list of visited persons, 
    X is indifferent to Y*/
indifferent(G, X, Y) :-
    different(G, X, Y), % X and Y must be different
    admiresList(G, [X], Y, [], VS),
    notElem(G, Y, VS). % did we ever visit Y if yes, X admires Y?

/* checks if two Graphs G and H are the same word, just with different names.
    A contains the name translation list*/
same_world(G, H, A) :-
    getTranslationTable(G, H, A),
    translateGraph(G, A, TG),
    equalSP(TG, H).


/* Checks if two sets of persons are equal, ignoring order of persons and friends*/
equalSP([],[]).
equalSP([X|XS], YS) :-
    selectP(X, YS, YS1),
    equalSP(XS, YS1).

/*select for lists of the persond coumpound.
    takes permutation of friends into account*/
selectP(P, [X|XS], XS) :-
    equalP(P, X).
selectP(P, [X|XS], [X|YS]) :-
    selectP(P, XS, YS).

/* Equal of the person Compound, ignores order of friends*/
equalP(person(Nx, F1), person(Nx, F2)) :-
    equalS(F1, F2).

/*checks if two Sets are equal, ignoring order */
equalS([], []).
equalS([X|XS], YS) :-
    mySelect(X, YS, YS1),
    equalS(XS, YS1).

/* converts a graph G given the translation list A */
translateGraph([], _, []).
translateGraph([P|T], A, [P2|T2]) :-
    convertPerson(P, A, P2),
    translateGraph(T, A, T2).

/* converts a person given the translation list A */
convertPerson(person(Name, Friends), A, person(TName, TFriends)) :-
    translateName(Name, A, TName),
    translateFriends(Friends, A, TFriends).

/* converts a list of names (Friendslist) given the translation list A */
translateFriends([], _, []).
translateFriends([Name|T], A, [TName|TT]) :-
    translateName(Name, A, TName),
    translateFriends(T, A, TT).

/* converts a Name given the translation list A */
translateName(Name, [(Name, TName)|_], TName).
translateName(Name, [_|T], TName) :-
    translateName(Name, T, TName).

/* generates all possible translation tables A for the given graphs G and H*/
getTranslationTable(G, H, A) :- 
    getNames(G, NamesG), 
    getNames(H, NamesH),
    combinationMaker(NamesG, NamesH, A).

/* Collects all Names of the persons in Graph G */
getNames([], []).
getNames([person(Name,_)|T], [Name|NF]) :- getNames(T, NF).

/* creates all possible combinations of elements from the two lists G and H */
combinationMaker(LA, LB, A) :-
    permutation(LB, PLB),
    zip(LA, PLB, A).

/* generates all possible permutations of a given list */
permutation([], []).
permutation(List, [Element | Permutation]) :-
    mySelect(Element, List, Rest),
    permutation(Rest, Permutation).

/* Zips two lists */
zip([],[],[]).
zip([LAH|LAT], [LBH|LBT], [(LAH,LBH)|R]) :-
    zip(LAT, LBT, R).
    


