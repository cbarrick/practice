%% The Game of Pig
%
% https://www.reddit.com/r/prolog/comments/gd7b5e/
%
% Each turn, a player repeatedly rolls a die until either a 1 is rolled or the
% player decides to "hold":
%
%     - If the player rolls a 1, they score nothing and it becomes the next
%       player's turn.
%     - If the player rolls any other number, it is added to their turn total
%       and the player's turn continues.
%     - If a player chooses to "hold", their turn total is added to their score,
%       and it becomes the next player's turn.
%
% The first player to score 100 or more points wins.


% Game Loop
% ---------------------------------------------------------------------------

%% play_pig(+Agents)
% Play a game of Pig to 100.
%
% Agents is a list of player agents, e.g. `[human(alice), human(bob)]` for a
% two player game among humans named Alice and Bob.
play_pig(Agents) :- play_pig(Agents, 100).

%% play_pig(+Agents, +Target)
% Play a game of Pig to a configurable target score.
play_pig(Agents, Target) :-
    length(Agents, N),
    length(Scores, N),
    maplist(=(0), Scores),
    format("current scores ~w\n", [Scores]),
    play_pig(Agents, N, Target, 0, Scores, []).

% The main loop.
play_pig(Agents, N, Target, Turn, Scores, History) :-
    I is Turn rem N,
    nth0(I, Agents, Agent),
    nth0(I, Scores, Score, OpponentScores),
    roll(Die),
    sum([Die | History], Total),
    format("player ~w rolls ~w (total: ~w)\n", [I, Die, Total]),

    repeat,
    get_action(Agent, Die, Score, History, OpponentScores, Target, Action),
    (Action = bust -> !,
        NextTurn is Turn + 1,
        format("player ~w busts\n", [I]),
        format("current scores ~w\n", [Scores]),
        play_pig(Agents, N, Target, NextTurn, Scores, [])

    ; Action = hold -> !,
        NewScore is Score + Total,
        nth0(I, NewScores, NewScore, OpponentScores),
        NextTurn is Turn + 1,
        format("player ~w holds\n", [I]),
        format("player ~w scores ~w\n", [I, Total]),
        format("current scores ~w\n", [NewScores]),
        (NewScore >= 100 ->
            format("player ~w wins!\n", [I])
        ;
            play_pig(Agents, N, Target, NextTurn, NewScores, [])
        )

    ; Action = roll -> !,
        play_pig(Agents, N, Target, Turn, Scores, [Die | History])

    ; Action = quit -> !,
        format("Bye!\n")

    ;
        format("invalid action: ~w\n", [Action]),
        format("valid actions are: [roll, hold, quit]\n"),
        false
    ).


% Helpers
% ---------------------------------------------------------------------------

%% get_action(+Agent, +Die, +Score, +History, +OpponentScores, +Target, -Action)
% Get the decision of an agent given some state. If the die is 1, the action
% will always be to bust.
%
% Arguments:
%     Agent: A goal to be called to get the agent's action.
%     Die: The value rolled on the die, a number 1 through 6.
%     Score: The agent's current score.
%     History: The list of rolls made this turn.
%     OpponentScores: The current socres of the opponents.
%     Target: The target score.
%     Action: The action to take, one of ['roll', 'hold', 'quit', 'bust'].
get_action(_, 1, _, _, _, _, bust) :- !.
get_action(Agent, Die, Score, History, OpponentScores, Target, Action) :-
    call(Agent, Die, Score, History, OpponentScores, Target, Action).


%% sum(+Numbers, -Value)
% Value is the sum of the list of Numbers.
sum(Numbers, Value) :-
    sum(Numbers, Value, 0).

sum([], V, V).
sum([H|T], V, X) :- Y is X + H, sum(T, V, Y).


%% roll(-Die)
% Roll a d6. Die is a random value from 1 through 6.
roll(Die) :- random(1, 7, Die).


% Agents
% ---------------------------------------------------------------------------

%% random_agent(+Die, +Score, +History, +OpponentScores, +Target, -Action)
% An agent that plays randomly.
random_agent(_, _, _, _, _, roll) :- maybe, !.
random_agent(_, _, _, _, _, hold).


%% simple_agent(+Limit, +Die, +Score, +History, +OpponentScores, +Target, -Action)
% An agent that rolls until the score for the current turn is `Limit` or
% greater. The agent would be passed to the `play_pig` predicate with the limit
% specified, e.g. `play_pig([human, simple_agent(20)])`.
simple_agent(_, Die, Score, History, _, Target, hold) :-
    sum([Die, Score | History], Total),
    Total >= Target,
    !.

simple_agent(Limit, Die, _, History, _, _, hold) :-
    sum([Die | History], Total),
    Total >= Limit,
    !.

simple_agent(_, _, _, _, _, _, roll).


%% human(+Die, +Score, +History, +OpponentScores, +Target, -Action)
% An anonymous human agent.
human(Die, Score, History, OpponentScores, Target, Action) :-
    human('human', Die, Score, History, OpponentScores, Target, Action).


%% human(+Name, +Die, +Score, +History, +OpponentScores, +Target, -Action)
% A named human agent.
human(Name, _, _, _, _, _, Action) :-
    format(string(Prompt), "(~w)>> ", [Name]),
    prompt1(Prompt),
    read_term(Term, []),
    (Term = end_of_file ->
        Action = quit
    ;
        Action = Term
    ).
