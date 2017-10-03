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
admiresList(_, [X| _], _, VS, VS) :-
    elem(X, VS). % X was already visited
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

same_world(G, H, A) :-
    translateGraph(G, H, A),
    translate(G, G, A, G1),
    equalSP(G1, H).

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

/* Checks if two sets of persons are equal, ignoring order*/
equalSP([],[]). %TODO: fix for persons and friend lists
equalSP([X|XS], YS) :-
    selectP(X, YS, YS1),
    equalSP(XS, YS1).

/*select for lists of the persond coumpound.
    takes permutation of friends into account*/
selectP(P, [X|XS], XS) :-
    equalP(P, X).
selectP(P, [X|XS], [X|YS]) :-
    selectP(P, XS, YS).

/* Equal of the person Compound*/
equalP(person(Nx, F1), person(Nx, F2)) :-
    equalS(F1, F2).

/*checks if two Sets are equal, ignoring order */
equalS([], []).
equalS([X|XS], YS) :-
    mySelect(X, YS, YS1),
    equalS(XS, YS1).

%sameWorld(G, H, A) :- 
%.
translateGraph(G, H, A) :- 
    getTheList(G, NamesG), 
    getTheList(H, NamesH),
    combinationMaker(NamesG, NamesH, A).

getTheList([], []).
getTheList([person(Name,_)|T], [Name|NF]) :- getTheList(T, NF).

combinationMaker(LA, LB, A) :-
    permutation(LA, PLA), 
    permutation(LB, PLB),
    zip(PLA, PLB, A).

permutation([], []).
permutation(List, [Element | Permutation]) :-
    mySelect(Element, List, Rest),
    permutation(Rest, Permutation).

zip([],[],[]).
zip([LAH|LAT], [LBH|LBT], [(LAH,LBH)|R]) :-
    zip(LAT, LBT, R).
    





