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

:- end_tests(twitbook).

% Imports by regular file import
% Run tests by run_tests

% The way to suppress Choicpoint warning all(_ = [1])