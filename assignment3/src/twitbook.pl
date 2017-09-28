g1([person(kara, [barry, clark]),
 person(bruce, [clark, oliver]),
 person(barry, [kara, oliver]),
 person(clark, [oliver, kara]),
 person(oliver, [kara])]).

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

/* my implementation of select.
    removes the given argument from the list*/
mySelect(X, [X|XS], XS).
mySelect(X, [Y|XS], [Y|YS]) :-
    mySelect(X, XS, YS).

/* likes(Graph, Name, Name)*/
likes(G, X, Y) :-
    getPersonAtom(G, X, Friends),
    myMember(Y, Friends).

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
    isNotOnFriendList(G, Y, FriendsX). % all on Xs friendlist are different from Y

/* Checks that the given name is not on the list*/
isNotOnFriendList(_, _, []).
isNotOnFriendList(G, Name, [H|T]) :-
    different(G, H, Name),
    isNotOnFriendList(G, Name, T).

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


/* friendly(Graph, Name) is liked back by anyone*/
friendly(G, X) :-
    getPersonAtom(G, X, Friends),
    likedByEveryone(G, X, Friends).

