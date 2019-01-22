import sys
from functools import lru_cache


def difficulty(shape1, shape2):
    '''Compute the number of scalar mutiplications required to multiply two
    matricies of the given shapes.

    Arguments:
        shape1 (Tuple[int, int]):
            The shape of the left matrix.
        shape2 (Tuple[int, int]):
            The shape of the right matrix.

    Returns:
        cost (int):
            The number of scalar multiplications.
        shape (Tuple[int, int]):
            The shape of the resulting matrix.
    '''
    assert shape1[1] == shape2[0]
    n, m = shape1
    m, k = shape2
    cost = n * m * k
    shape = (n, k)
    return cost, shape


@lru_cache(maxsize=None)
def tree_difficulty(tree, shapes):
    '''Compute the number of scalar multiplications required to multiply some
    number matrices in a given order.

    Arguments:
        tree (Tuple or int):
            A binary tree describing the order of multiplications.
            The leaf nodes index the matrices being multiplied.
        shapes (Tuple[Tuple[int, int]]):
            The list of shapes for the matrices being multiplied.

    Returns:
        cost (int):
            The number of scalar multiplications.
        shape (Tuple[int, int]):
            The shape of the resulting matrix.
    '''
    if type(tree) is int:
        return 0, shapes[tree]

    lhs, rhs = tree
    lhs_cost, lhs_shape = tree_difficulty(lhs, shapes)
    rhs_cost, rhs_shape = tree_difficulty(rhs, shapes)

    cost, shape = difficulty(lhs_shape, rhs_shape)
    cost += lhs_cost + rhs_cost
    return cost, shape


@lru_cache(maxsize=None)
def matrix_chain(shapes, start=None, stop=None):
    '''Compute the optimal ordering of matrix multiplications.

    Arguments:
        shapes (Tuple[Tuple[int, int]]):
            A list of matrix shapes to be multiplied.
        start (int or None):
            Only consider the subproblem starting at this index.
        stop (int or None):
            Only consider the subproblem before this index.

    Returns:
        tree (Tuple or int):
            A binray tree describing the optimal order of multiplications.
            The leaf nodes index the matrices being multiplied.
        cost (int):
            The number of scalar multiplications.
    '''
    start = start or 0
    stop = stop or len(shapes)
    size = stop - start
    assert 0 < size
    assert type(size) is int

    if size == 1:
        tree = start
        cost = 0
        return tree, cost

    if size == 2:
        tree = (start, start+1)
        cost, _ = tree_difficulty(tree, shapes)
        return tree, cost

    best_tree = None
    best_cost = float('inf')
    for i in range(start+1, stop):
        lhs, lhs_cost = matrix_chain(shapes, start, i)
        rhs, rhs_cost = matrix_chain(shapes, i, stop)
        tree = (lhs, rhs)
        cost, _ = tree_difficulty(tree, shapes)
        if cost < best_cost:
            best_tree = tree
            best_cost = cost

    return best_tree, best_cost


def read_input(input=None):
    '''Read a matrix chain problem from some input stream.

    Arguments:
        input (IO[str] or None):
            An input stream for reading the problem.
            Defaults to `sys.stdin`.

    Returns:
        shapes (Tuple[Tuple[int, int], ...]):
            The shapes of the matrices to be multiplied.
    '''
    input = input or sys.stdin
    num_matrices = int(input.readline().strip())
    shapes = []
    for _ in range(num_matrices):
        n, m = input.readline().split()
        shape = (int(n), int(m))
        shapes.append(shape)
    shapes = tuple(shapes)
    return shapes


def main(input=None, output=None):
    '''Solve a matrix chain problem from some input stream and print the
    solution to some output stream.

    Arguments:
        input (IO[str] or None):
            An input stream for reading the problem.
            Defaults to `sys.stdin`.
        output (IO[str] or None):
            An output stream for printing the solution.
            Defaults to `sys.stdout`.
    '''
    input = input or sys.stdin
    output = output or sys.stdout
    shapes = read_input(input)
    tree, cost = matrix_chain(shapes)
    print(cost, file=output)
    print(tree, file=output)
    tree_difficulty.cache_clear()
    matrix_chain.cache_clear()


def generate_sample(n, output=None):
    '''Print a random matrix chain problem.

    The output is suitable for for `read_input`.

    Arguments:
        n (int):
            The number of matrices.
        output (IO[str] or None):
            An output stream for printing the problem.
            Defaults to `sys.stdout`.
    '''
    import numpy as np
    output = output or sys.stdout
    print(n, file=output)
    max_size = 2 ** 4  # A smaller size makes more interesting solutions.
    a = np.random.random_integers(max_size)
    for _ in range(n):
        b = np.random.random_integers(max_size)
        print(f'{a} {b}', file=output)
        a = b


def generate_files():
    '''Generate six files with random matrix chain problems.

    The files are named `'input_{n}.txt'` where `{n}` is the number of matrices.
    '''
    for i in range(2, 8):
        n = 2 ** i
        with open(f'input_{n}.txt', 'w') as f:
            generate_sample(n, f)


if __name__ == '__main__':
    main()
