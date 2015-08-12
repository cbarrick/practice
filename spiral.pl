#!/usr/bin/env swipl -q -g main -t halt -s

% <module> Reason about square spirals
%
% This problem comes from r/dailyprogrammer. The problem is to correlate steps
% along a square spiral to their corresponding coordinates.
%
% https://www.reddit.com/r/dailyprogrammer/comments/3ggli3

:- use_module(library(dcg/basics)).
:- use_module(library(clp/clpfd)).


%! main
% Solves the challenge. This main predicate handles reading and printing.
% The real implementation uses a different coordinate system and starts
% counting steps at 0 rather than 1. This predicate handles the conversion
% between conventions so that the output matches the examples.
main :-
	% read and parse the initial line
	prompt1(''),
	read_line_to_codes(current_input, InitLine),
	phrase(integer(Size), InitLine),

	% read and parse the main line
	prompt1(''),
	read_line_to_codes(current_input, MainLine),
	(
		phrase((integer(X), blank, integet(Y)), MainLine),
		OutputType = step
	->true;
		phrase(integer(Step), MainLine),
		OutputType = coordinate
	),

	% convert input to match implementation
	% (rotate and translate coordinates, offset step)
	Step_ #= Step - 1,
	X_ #= Y - Size//2 - 1,
	Y_ #= X - Size//2 - 1,

	% do it
	square_spiral(Step_, [X_,Y_]),

	% print
	(OutputType = step -> format("~w\n", [Step]) ; format("(~w, ~w)", [X,Y])).


%! square_spiral(?N, ?Point)
% True when Point is the [X,Y] coordinate of the Nth step along a square spiral
% centered at [0,0]. The first step of the spiral is upwards, i.e.
% `square_spiral(0, [0,0])` and `square_spiral(1, [0,1])` are both true.
square_spiral(0, [0,0]).
square_spiral(Step, [X,Y]) :-
	% initial bounds
	Step in 1..sup,
	[X,Y] ins inf..sup,

	% Level indicates which of the concentric squares contains the point
	Level in 1..sup,
	Level #= max(abs(X), abs(Y)),

	% compute bounds of Step in terms of Level
	StepMin #= (2*Level - 1) ^ 2,
	StepMax #= StepMin + 8*Level - 1,
	StepMin #=< Step, Step #=< StepMax,

	% compute bounds of X and Y in terms of Level
	CoordMin #= -Level,
	CoordMax #= Level,
	CoordMin #=< X, X #=< CoordMax,
	CoordMin #=< Y, Y #=< CoordMax,

	% Side indicates which side of the level the point/step is on
	% 0 = top, 1 = left, 2 = bottom, 3 = right
	Side in 0..3,

	% correlate Step and Side
	LvlSize #= StepMax - StepMin + 1, % number of steps in the Level
	LvlStep #= Step - StepMin, % Step relative to the start of the Level
	(Side #= 0 #/\ 0 #=< LvlStep #/\ LvlStep #< LvlSize * 1//4) #\
	(Side #= 1 #/\ LvlSize * 1//4 #=< LvlStep #/\ LvlStep #< LvlSize * 2//4) #\
	(Side #= 2 #/\ LvlSize * 2//4 #=< LvlStep #/\ LvlStep #< LvlSize * 3//4) #\
	(Side #= 3 #/\ LvlSize * 3//4 #=< LvlStep #/\ LvlStep #< LvlSize),

	% correlate X, Y, and Side
	(Side #= 0 #/\ Y #= CoordMax #/\ CoordMin #=< X #/\ X #< CoordMax) #\
	(Side #= 1 #/\ X #= CoordMin #/\ CoordMin #=< Y #/\ Y #< CoordMax) #\
	(Side #= 2 #/\ Y #= CoordMin #/\ CoordMin #< X #/\ X #=< CoordMax) #\
	(Side #= 3 #/\ X #= CoordMax #/\ CoordMin #< Y #/\ Y #=< CoordMax),

	% correlate X, Y, and Step
	SideSize #= LvlSize // 4, % number of steps on the Side
	SideStep #= LvlStep - Side * SideSize, % LvlStep relative to the start of the Side
	(Side #= 0 #/\ X #= CoordMax - SideStep - 1) #\
	(Side #= 1 #/\ Y #= CoordMax - SideStep - 1) #\
	(Side #= 2 #/\ X #= CoordMin + SideStep + 1) #\
	(Side #= 3 #/\ Y #= CoordMin + SideStep + 1),

	% bind X, Y, and Step
	between(1, inf, Step),
	label([X,Y]).
