select_right(H, [H|T], T).
select_right(X, [_|T], R) :- select_right(X, T, R).

three_sum([A,B,C], Input) :-
    sort(Input, R1),
    select_right(A, R1, R2),
    select_right(B, R2, R3),
    select_right(C, R3, _),
    0 =:= A + B + C.
