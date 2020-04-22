%% Maze Generator
%
% https://www.reddit.com/r/prolog/comments/g4tdav/
%
% A maze is a structure of the form `maze(Height, Width, Edges)` that gives the
% dimensions of the maze and a list of edges. The edges are represented as an
% ordset where elements take the form `[Y1,X1]-[Y2,X2]`. The nodes in an edge
% are given in order, e.g. the node `[Y1,X1]` will always be the top node for a
% vertical edge or the left node for a horizontal edge.


% Helpers
% ---------------------------------------------------------------------------

%% node(+Height, +Width, ?Node)
% Node is valid for a maze with the given Height and Width.
node(Height, Width, [Y, X]) :-
    between(1, Height, Y),
    between(1, Width, X).


%% mazechk(+Maze)
% Maze is a valid maze.
mazechk(Maze) :-
    Maze = maze(Height, Width, Edges),
    between(1, inf, Height),
    between(1, inf, Width),
    NumNodes is Height * Width,
    MaxEdges is NumNodes * 2 - Height - Width,
    between(0, MaxEdges, L),
    length(Edges, L),
    forall(member(Edge, Edges), (
        Edge = [Y1,X1]-[Y2,X2],
        node(Height, Width, [Y1,X1]),
        node(Height, Width, [Y2,X2]),
        (
            Y2 is Y1,
            X2 is X1 + 1,
            X2 =< Width
        ;
            Y2 is Y1 + 1,
            X2 is X1,
            Y2 =< Height
        )
    )),
    is_ordset(Edges),
    !.


% Display
% ---------------------------------------------------------------------------

%% print_maze(+Maze)
% Print an ASCII representation of a maze to the current output.
print_maze(Maze) :-
    print_maze__make_blocks(Maze, Blocks),
    print_maze__print_blocks(Blocks).

print_maze__make_blocks(maze(Height, Width, Edges), Blocks) :-
    findall(Row, (
        between(1, Height, Y),
        findall(Block, (
            between(1, Width, X),
            ([Y,X] = [1,1] ->
                Block = [
                    ['  ', '  ', '██'],
                    ['  ', '  ',   E ],
                    ['██',   S,  '██']
                ]
            ; [Y,X] = [Height,Width] ->
                Block = [
                    ['██',   N,  '██'],
                    [  W,  '  ', '  '],
                    ['██', '  ', '  ']
                ]
            ;
                Block = [
                    ['██',  N, '██'],
                    [  W,  '  ', E ],
                    ['██',  S, '██']
                ]
            ),
            Y0 is Y-1,
            Y1 is Y+1,
            X0 is X-1,
            X1 is X+1,
            (ord_memberchk([Y0,X]-[Y,X], Edges) -> N='  ' ; N='██'),
            (ord_memberchk([Y,X0]-[Y,X], Edges) -> W='  ' ; W='██'),
            (ord_memberchk([Y,X]-[Y,X1], Edges) -> E='  ' ; E='██'),
            (ord_memberchk([Y,X]-[Y1,X], Edges) -> S='  ' ; S='██')
        ), Row)
    ), Blocks).

print_maze__print_blocks(Blocks) :-
    forall(member(Row, Blocks), (
        forall(between(1, 3, I), (
            forall(member(Block, Row), (
                nth1(I, Block, BlockRow),
                forall(member(X, BlockRow), write(X))
            )),
            nl
        ))
    )).


% Fully Connected "Maze"
% ---------------------------------------------------------------------------

%% fully_connected_maze(?Height, ?Width, ?Maze)
% Generate a fully-connected maze.
fully_connected_maze(Height, Width, Maze) :-
    Maze = maze(Height, Width, Edges),
    findall(Edge, (
        Edge = [Y1,X1]-[Y2,X2],
        between(1, Height, Y1),
        between(1, Width, X1),
        (
            Y2 is Y1,
            X2 is X1 + 1,
            X2 =< Width
        ;
            Y2 is Y1 + 1,
            X2 is X1,
            Y2 =< Height
        )
    ), Edges),
    is_ordset(Edges).


% Randomized Depth-First Maze
% ---------------------------------------------------------------------------

%% dfs_maze(+Height, +Width, -Maze)
% Generate a maze using a randomized depth-first search.
dfs_maze(Height, Width, Maze) :-
    Maze = maze(Height, Width, Edges),
    findall(Node, node(Height, Width, Node), Unvisited),
    once(select([1,1], Unvisited, Unvisited_)),
    once(dfs_maze__loop(Height, Width, MEdges, Unvisited_, [[1,1]])),
    sort(MEdges, Edges),
    mazechk(Maze).

% The case where the top of the stack has an unvisited neighbor.
dfs_maze__loop(Height, Width, [Edge|MEdges], Unvisited, [[Y,X]|Stack]) :-
    % Find an unvisited neighbor.
    random_permutation([[0, -1], [-1, 0], [0, 1], [1, 0]], Deltas),
    member([Dy,Dx], Deltas),
    H is X + Dx,
    H =< Width,
    1 =< H,
    K is Y + Dy,
    K =< Height,
    1 =< K,
    select([K,H], Unvisited, Unvisited_),

    % Make the edge.
    sort([[Y,X],[K,H]], [[Y1,X1],[Y2,X2]]),
    Edge = [Y1,X1]-[Y2,X2],

    % Recurse.
    once(dfs_maze__loop(
        Height,
        Width,
        MEdges,
        Unvisited_,
        [[K,H],[Y,X]|Stack]
    )).

% No unvisited neighbors - pop the stack.
dfs_maze__loop(Height, Width, MEdges, Unvisited, [_|Stack]) :-
    Unvisited = [_|_],
    dfs_maze__loop(Height, Width, MEdges, Unvisited, Stack).

% All nodes are visited.
dfs_maze__loop(_, _, [], [], _).
