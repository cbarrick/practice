def parens(n):
    assert 0 <= n
    assert n % 2 == 0

    if n == 0:
        yield ''
    elif n == 2:
        yield '()'
    else:
        for sub_soln in parens(n-2):
            yield f'{sub_soln}()'
            yield f'(){sub_soln}'
            yield f'({sub_soln})'
