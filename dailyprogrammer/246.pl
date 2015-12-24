#!/usr/bin/env swipl -q -g main -t halt -s

% load the word list for the bonus
% the file defines word//0 that enumerates allowed words
:- consult("./words.pl").

% the grammar
decode_char(`a`) --> `1`.
decode_char(`b`) --> `2`.
decode_char(`c`) --> `3`.
decode_char(`d`) --> `4`.
decode_char(`e`) --> `5`.
decode_char(`f`) --> `6`.
decode_char(`g`) --> `7`.
decode_char(`h`) --> `8`.
decode_char(`i`) --> `9`.
decode_char(`j`) --> `10`.
decode_char(`k`) --> `11`.
decode_char(`l`) --> `12`.
decode_char(`m`) --> `13`.
decode_char(`n`) --> `14`.
decode_char(`o`) --> `15`.
decode_char(`p`) --> `16`.
decode_char(`q`) --> `17`.
decode_char(`r`) --> `18`.
decode_char(`s`) --> `19`.
decode_char(`t`) --> `20`.
decode_char(`u`) --> `21`.
decode_char(`v`) --> `22`.
decode_char(`w`) --> `23`.
decode_char(`x`) --> `24`.
decode_char(`y`) --> `25`.
decode_char(`z`) --> `26`.

decode_string([]) --> [].
decode_string([H|T]) --> decode_char([H]), decode_string(T).

% for the bonus, filter the string to only contain valid words
bonus --> [].
bonus --> word, bonus.

% the real code
main :-
	read_line_to_codes(user_input, Line),
	phrase(decode_string(Codes), Line),
	phrase(bonus, Codes),
	string_codes(Str, Codes),
	write(Str),
	nl,
	flush_output,
	fail.
main :- halt.
