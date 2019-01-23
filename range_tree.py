'''Efficient queries on a range.

Given a list of numbers, we want to find the maximum value in that list
between two indicies. We want to make multiple queries on the same
list, so we preprocess it into a tree for queries in ``O(log(n))``.
'''

class MaxTree:
    '''A tree for making max queries on ranges of a list.
    '''

    def __init__(self, val, lo, hi, left=None, right=None):
        '''Low-level constructor for a MaxTree.

        Arguments:
            val:
                The maximum value on the range.
            lo (int):
                The lower bound of the range, inclusive.
            hi (int):
                The upper bound of the range, exclusive.
            left (MaxTree or None):
                The left subtree.
            right (MaxTree or None):
                The right subtree.
        '''
        self.val = val
        self.lo = lo
        self.hi = hi
        self.left = left
        self.right = right

    @classmethod
    def from_list(cls, data):
        '''Construct a MaxTree from a list.

        This builds a balanced MaxTree from a list of data, bottom up.

        Arguments:
            data (list):
                The data to be built into a tree.
        '''
        assert 0 < len(data)

        # Convert each value into a single element tree.
        trees = [cls(val, i, i+1) for i, val in enumerate(data)]

        # Combine trees pairwise into a single balanced tree.
        while 1 < len(trees):
            evens = trees[0::2]
            odds = trees[1::2]
            trees.clear()
            for left, right in zip(evens, odds):
                val = max(left.val, right.val)
                lo = left.lo
                hi = right.hi
                tree = cls(val, lo, hi, left, right)
                trees.append(tree)
            if len(odds) < len(evens):
                trees.append(evens.pop())

        return trees[0]

    def __str__(self):
        '''A string representation of the tree for debugging.'''
        if self.hi - self.lo == 1:
            return str(self.val)
        else:
            return f"({self.left}, {self.right})"

    def max_on_range(self, i, j):
        '''Get the maximum value of the data betweem two indices.

        The query has a runtime in ``O(log(n))``.

        Arguments:
            i (int):
                The lower bound of the range, inclusive.
            j (int):
                The upper bound of the range, exclusive.

        Returns:
            The maximum value on the range.
        '''
        if not self.left:
            assert not self.right
            return self.val

        elif i <= self.lo and self.hi <= j:
            return self.val

        elif j <= self.right.lo:
            return self.left.max_on_range(i, j)

        elif self.left.hi <= i:
            return self.right.max_on_range(i, j)

        else:
            a = self.left.max_on_range(i, j)
            b = self.right.max_on_range(i, j)
            return max(a, b)
