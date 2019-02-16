'''Generating rhyme schemes.

In poetry, we have a convention for describing rhyme schemes. For
example, a four line poem may have a rhyme scheme of ``ABAB`` or
``ABBA`` etc. The idea is that we have one character for each line, and
lines described by the same character rhyme. The first character is
always ``A``, and each line that does not rhyme with any previous line
uses the next availiable letter. So ``B`` cannot be used unless ``A``
has been used for a previous line, ``C`` cannot be used until ``B`` has
been used, etc.

Our goal is to generate all possible rhyme schemes for a given number
of lines, ``n``. To simplify the logic, we use integers instead of
characters, where 0 corresponds to ``A``, 1 to ``B``, etc.
'''

def rhyme_v1(n, next=0):
    '''Naive solution.

    This solution has a runtime in ``O(2^n)`` to generate all solutions.

    Arguments:
        n (non-negative int):
            The number of lines in the poem.
        next (non-negative int):
            The first unused integer.

    Yields (tuple of int):
        An encoding of each possible rhyme scheme. This method never
        yields duplicates.
    '''
    if n <= 0:
        yield ()
        return

    for sub in rhyme_v1(n - 1, next):
        for i in range(next):
            yield (i,) + sub

    for sub in rhyme_v1(n - 1, next + 1):
        yield (next,) + sub


def rhyme_v2(n):
    '''An improved solution.

    This solution has a runtime in ``O(n^3)`` to generate all solutions.

    Arguments:
        n (non-negative int):
            The number of lines in the poem.

    Yields (tuple of int):
        An encoding of each possible rhyme scheme. This method never
        yields duplicates.
    '''
    start = tuple(0 for i in range(n))  # e.g. (0, 0, 0, 0) when n=4
    stop = tuple(i for i in range(n))   # e.g. (0, 1, 2, 3) when n=4

    # Given one rhyme scheme, return the next.
    # This has a worst case in ``O(n^2)``.
    def rhyme_next(prev):
        if len(prev) == 0: return ()
        m = max(prev[:-1])
        v = prev[-1] + 1
        if m + 1 < v:
            return rhyme_next(prev[:-1]) + (0,)
        else:
            return prev[:-1] + (v,)

    cur = start
    yield cur
    while cur != stop:
        cur = rhyme_next(cur)
        yield cur
