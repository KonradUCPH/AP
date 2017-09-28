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

/* likes(Graph, Name, Name)*/
likes(G, X, Y) :-
    getPersonAtom(G, X, Friends),
    myMember(Y, Friends).

/* are X and Y different members of G?*/
different(G, X, Y) :-
    select(X, G, G1),
    select(Y, G1, _).


/* dislikes(Graph, Name, Name)*/
dislikes(G, X, Y) :-
    likes(G, Y, X), % Y likes X
    \+ likes(G, X, Y). % but X doesnt like Y
