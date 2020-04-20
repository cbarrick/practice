#!/usr/bin/env swipl -q -g main -t halt -s

% <module> Add lists of fractions
%
% This module was written as a programming exercise posted to r/dailyprogrammer
% on Reddit. Prolog provides this kind of arithmetic already, but I was bored,
% so I wrote my own.

%! add(+Fractions, -Result)
% Adds a list of fractions.
add([X], Result) :- !, reduce(X, Result).
add([NA/DA,NB/DB|Fractions], Result) :-
	N is NA*DB + NB*DA,
	D is DB*DA,
	add([N/D|Fractions], Result).

%! reduce(+Fraction, -Reduced)
% Reduces a fraction to its normal form.
reduce(A/1, A) :- !.
reduce(A/B, C/D) :-
	gcd(A, B, Div),
	C is A / Div,
	D is B / Div.

%! gcd(+A, +B, -Div)
% Calculates the greatest common divisor of A and B using Euclid's algorithm.
gcd(A, 0, A) :- !.
gcd(A, B, Div) :-
	C is A rem B,
	gcd(B, C, Div).

% ===========================================================================
% Everything below this point is machinery for reading inputs

:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).

% Parser for fractions
fraction(N/D) --> integer(N), "/", integer(D).

% Reads lines from stdin matching the input format
read_fractions(Fracs) :-
	once(read_line_to_codes(user_input, CountStr)),
	phrase(integer(Count), CountStr),
	length(Fracs, Count),
	read_fractions_(Fracs).

read_fractions_([]) :- !.
read_fractions_([H|T]) :-
	once(read_line_to_codes(user_input, FracStr)),
	phrase(fraction(H), FracStr),
	read_fractions_(T).

% Read input format, add fractions, print answer, loop
main :-
	read_fractions(Fracs),
	(Fracs == [] -> halt ; true),
	add(Fracs, Result),
	format("~w\n", [Result]).
