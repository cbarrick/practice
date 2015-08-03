#!/usr/bin/env swipl -q -g main -t halt -s
main :- ','(write("#!/usr/bin/env swipl -q -g main -t halt -s"),','(nl,','(clause(main,A),','(write("main :- "),','(write_canonical(A),','(write("."),nl)))))).

% The above program is a quine, a program that outputs itself.
% When I wrote it, it looked like this:
%
% #!/usr/bin/env swipl -q -g main -t halt -s
% main :-
%     write("#!/usr/bin/env swipl -q -g main -t halt -s"),
%     nl,
%     clause(main, Body),
%     write("main :- "),
%     write_canonical(Body),
%     write("."),
%     nl.
%
% It's hard to make the computer generated code look like human written code
% so I just replaced the source with the output :P
