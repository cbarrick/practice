#!/usr/bin/env swipl -q -g langford:main -t halt -s

% <module> Generate and test Langford strings
%
% This module was written as a programming exercise posted to r/dailyprogrammer
% on Reddit. The description of Langford strings given here was taken directly
% from the original Reddit post by u/XenophonOfAthens.
%
% The original post included three problems
% 1. Print all Langford strings of order 4
% 2. Print all Langford strings of order 8
% 3. Print the first 10 Langford strings of order 20
%
% The first two problems can be solved quickly by using depth-first search. The
% predicate `langford_dfs/2` implements this. The third problem is much harder.
%
% Depth-first search is too slow to find a Langford string of order 20. To
% handle high orders, use `langford_csp/2,3`. This predicate models the problem
% of finding Langford strings as a constraint satisfaction problem.
%
% The `_csp` variant is capable of generating Langford strings of order 20
% rather quickly (~17 seconds) when using the 'min' heuristic. However, the
% generated strings will not be in order. To generate strings in order, e.g. to
% solve problem #3, use the 'leftmost' heuristic. Be warned: the leftmost
% heuristic is _very_ slow to generate strings.
%
%
% ## Definition of a Langford string
%
% A "Langford string of order N" is defined as follows:
% - The length of the string is equal to 2*N
% - The string contains the the first N letters of the uppercase English
%   alphabet, with each letter appearing twice.
% - Each pair of letters contain X letters between them, with X being that
%   letter's position in the alphabet (that is, there is one letter between the
%   two A's, two letters between the two B's, three letters between the two C's,
%   etc)
%
% An example will make this clearer. These are the only two possible Langford
% strings of order 3:
% - BCABAC
% - CABACB
%
% Notice that for both strings, the A's have 1 letter between them, the B's have
% two letters between them, and the C's have three letters between them. As
% another example, this is a Langford string of order 7:
% - DFAGADCEFBCGBE
%
% It can be shown that Langford strings only exist when the order is a multiple
% of 4, or one less than a multiple of 4.
%
%
% ## Implementation Notes
%
% Langford strings are encoded as lists such that ±1=A, ±2=B, et cetera. Both
% positive and negative numbers occur exactly once, with the positive value
% always occuring before the matching negative value. Thus every value of a
% Langford string is distinct in [-Order, -1] ∪ [+1, +Order].
%
% @see https://www.reddit.com/r/dailyprogrammer/comments/3efbfh/20150724_challenge_224_hard_langford_strings/

:- module(langford, [
	write_langford/1, % write_langford(+String)
	langford_dfs/2,   % langford_dfs(?Order, ?String)
	langford_csp/2,   % langford_csp(?Order, ?String)
	langford_csp/3    % langford_csp(?Order, ?String, +Options)
]).

:- use_module(library(clpfd)).


%! write_langford(+String)
% Prints a langford string.
write_langford(String) :-
	write_langford_convert(String, Converted),
	format("~s", [Converted]).

write_langford_convert([], []).
write_langford_convert([A|String], [B|Converted]) :-
	B is abs(A) + 64,
	write_langford_convert(String, Converted).


% DEPTH-FIRST SEARCH
% ===========================================================================

%! langford_dfs(?Order, ?String)
% True when String is a langford string of a specific Order.
% This predicate solves the problem with depth-first search.
langford_dfs(Order, String) :-
	% validate arguments
	between(1, inf, Order),
	(0 =:= Order rem 4 ; 0 =:= (Order+1) rem 4),
	Size is 2 * Order,
	length(String, Size),

	% cut if we're not generating
	(forall(member(X, String), nonvar(X)) -> ! ; true),

	% depth-first search
	langford_dfs_(Order, String, []).


%! langford_dfs_(+Order, ?String)
% implements a depth-first search for langford strings
langford_dfs_(Order, [H|T], Seen) :-
	between(1, Order, H),
	\+ member(H, Seen),
	Opposite is -H,
	nth0(H, T, Opposite),
	langford_dfs_(Order, T, [H|Seen]).
langford_dfs_(Order, [H|T], Seen) :-
	nonvar(H),
	Opposite is -H,
	member(Opposite, Seen),
	langford_dfs_(Order, T, Seen).
langford_dfs_(_, [], _).


% CONSTRAINT SATISFACTION PROBLEM
% ===========================================================================

%! langford_csp(?Order, ?String)
% Equivalent to `langford_csp(Order, String, [min])`.
langford_csp(Order, String) :-
	langford_csp(Order, String, [min]).


%! langford_csp(?Order, ?String, +Options)
% True when String is a langford string of a specific Order.
% This predicate describes the problem as a constraint satisfaction problem.
% Options gets passed to clpfd:labeling/2. Typically, users need only concern
% themselves with two mutually exclusive options, 'leftmost' and 'min'.
% The leftmost heuristic will generate langford strings in alphabetical order,
% but this kind of search is very slow when generating strings of high orders.
% The min heuristic is much faster for this problem, but there is no guarentee
% of the order strings are generated.
langford_csp(Order, String, Options) :-
	% validate arguments
	between(1, inf, Order),
	(0 =:= Order rem 4 ; 0 =:= (Order+1) rem 4),
	L is 2 * Order,
	length(String, L),

	% set the domain
	% the list is encoded such that ±1=A, ±2=B, ...
	% and +X occurs before -X
	% and each value occurs exactly once
	Min is -Order,
	String ins (Min)..(-1) \/ 1..Order,
	all_distinct(String),

	% apply the main constraint
	langford_csp_v2_(Order, String),

	% The bonus problem specifies that we give the langford strings in order.
	% This forces us to use the `leftmost` heuristic.
	% The `min` heuristic is much faster, but does not yield strings in order.
	labeling(Options, String).


%! langford_csp_v1_(+Order, ?String)
% Applies the Langford string constraint by delegating to `element/3`.
langford_csp_v1_(Order, String) :-
	Order > 0,
	Opposite is -Order,
	element(A, String, Order),
	element(B, String, Opposite),
	B #= A + Order + 1,
	Next is Order - 1,
	langford_csp_v1_(Next, String).

langford_csp_v1_(0, _).


%! langford_csp_v2_(+Order, ?String)
% Applies the Langford string constraint by enumerating implications.
% This results in a faster search than delegating to `element/3`.
langford_csp_v2_(Order, String) :- langford_csp_v2_(Order, [], String).
langford_csp_v2_(Val, Prev, [X|Next]) :-
	Val > 0,

	% If X is Val, then the matching element to the right must equal -Val
	length(Next, N),
	(
		N > Val
	->
		nth0(Val, Next, NextMatch),
		X #= Val #==> NextMatch #= -Val,
		NextMatch #= Val #==> X #\= -Val
	;
		X #< Val
	),

	% If X is -Val, then the matching element to the left must equal Val
	length(Prev, P),
	(
		P > Val
	->
		nth0(Val, Prev, PrevMatch),
		X #= -Val #==> PrevMatch #= Val,
		PrevMatch #= -Val #==> X #\= Val
	;
		X #> -Val
	),

	langford_csp_v2_(Val, [X|Prev], Next).

langford_csp_v2_(Val, Prev, []) :-
	Val > 0,
	NextOrder is Val - 1,
	reverse(Prev, String),
	langford_csp_v2_(NextOrder, [], String).

langford_csp_v2_(0, _, _).


%! main
% Solves three problems:
% 1. Prints all langford strings of order 4
% 2. Prints all langford strings of order 8
% 3. Prints the first 10 langford strings of order 20
main :-
	format("# Problem 1: langford strings of order 4\n"),
	forall(langford_dfs(4, String), (
		write_langford(String), nl
	)),

	nl,
	format("# Problem 2: langford strings of order 8\n"),
	forall(langford_dfs(8, String), (
		write_langford(String), nl
	)),

	nl,
	format("# Bonus: langford strings of order 20\n"),
	format("(be patient, this is slow)\n"),
	forall(limit(10, langford_csp(20, String, [leftmost])), (
		write_langford(String), nl
	)).
