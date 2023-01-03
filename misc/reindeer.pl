% A holiday puzzle, solved with CLP(FD)
% https://gist.github.com/Spuffynism/446c7c2d498477491d8137e8f234d4a9

:- use_module(library(clpfd)).

%% reindeer(?Name, ?ID)
% A mapping of reindeer name to an integer ID.
% IDs are integers between 1 and 9 inclusive.
reindeer(dasher, 1).
reindeer(dancer, 2).
reindeer(prancer, 3).
reindeer(vixen, 4).
reindeer(comet, 5).
reindeer(cupid, 6).
reindeer(donder, 7).
reindeer(blitzen, 8).
reindeer(rudolph, 9).

%% position(?N, +ReindeerIDs, ?Name)
% The reindeer with the given Name has position N in the list of ReindeerIDs.
position(N, ReindeerIDs, Name) :-
    reindeer(Name, ID),
    element(N, ReindeerIDs, ID).

%% solution(?Names)
% Gives a solution to the puzzle, as a list of reindeer names.
solution(Reindeer) :-
    % For human friendliness, we give the solution in terms of reindeer names,
    % but we compute it using CLP(FD) in terms of reindeer IDs.
    length(Reindeer, 9),
    length(ReindeerIDs, 9),
    ReindeerIDs ins 1..9,
    all_distinct(ReindeerIDs),

    % Get the reindeer positions as variables.
    position(Dasher, ReindeerIDs, dasher),
    position(Dancer, ReindeerIDs, dancer),
    position(Prancer, ReindeerIDs, prancer),
    position(Vixen, ReindeerIDs, vixen),
    position(Comet, ReindeerIDs, comet),
    position(Cupid, ReindeerIDs, cupid),
    position(Donder, ReindeerIDs, donder),
    position(Blitzen, ReindeerIDs, blitzen),
    position(Rudolph, ReindeerIDs, rudolph),

    % Vixen should be behind Rudolph, Prancer and Dasher,
    Rudolph #< Vixen, Prancer #< Vixen, Dasher #< Vixen,

    % whilst Vixen should be in front of Dancer and Comet.
    Vixen #< Dancer, Vixen #< Comet,

    % Dancer should be behind Donder, Blitzen and Rudolph.
    Donder #< Dancer, Blitzen #< Dancer, Comet #< Dancer,

    % Comet should be behind Cupid, Prancer and Rudolph.
    Cupid #< Comet, Prancer #< Comet, Rudolph #< Comet,

    % Donder should be behind Comet, Vixen, Dasher, Prancer and Cupid.
    Comet #< Donder, Vixen #< Donder, Dasher #< Donder, Prancer #< Donder, Cupid #< Donder,

    % Cupid should be in front of Comet, Blitzen, Vixen, Dancer and Rudolph.
    Cupid #< Comet, Cupid #< Blitzen, Cupid #< Vixen, Cupid #< Dancer, Cupid #< Rudolph,

    % Prancer should be in front of Blitzen, Donder and Cupid.
    Prancer #< Blitzen, Prancer #< Donder, Prancer #< Cupid,

    % Blitzen should be behind Cupid but in front of Dancer, Vixen and Donder.
    Cupid #< Blitzen, Blitzen #< Dancer, Blitzen #< Vixen, Blitzen #< Donder,

    % Rudolph should be behind Prancer but in front of Dasher, Dancer and Donder.
    Prancer #< Rudolph, Rudolph #< Dasher, Rudolph #< Dancer, Rudolph #< Donder,

    % Finally, Dasher should be behind Prancer but in front of Blitzen, Dancer and Vixen.
    Prancer #< Dasher, Dasher #< Blitzen, Dasher #< Dancer, Dasher #< Vixen,

    % Label the solution and map IDs to names.
    label(ReindeerIDs),
    maplist(reindeer, Reindeer, ReindeerIDs).
