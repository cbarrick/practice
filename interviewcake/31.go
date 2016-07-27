// Interviewcake #31:
// Write a recursive function that returns all permutations of a string.

package main

import "fmt"

// Perm returns all byte-wise permutations of s. Duplicates are not removed.
// The computation performs a constant number of heap allocations (5).
func Perm(s string) []string {
	bytes := []byte(s)
	res, _ := perm(bytes, len(bytes))
	strings := make([]string, len(res))
	for i := range strings {
		strings[i] = string(res[i])
	}
	return strings
}

// perm implements the recursive loop of Perm.
// s is the string being permuted.
// n is the length of the final string.
// res is the heap upon which the permutations are built.
// i is the index into res of the next empty slice.
func perm(s []byte, n int) (res [][]byte, i int) {
	// In the base case, there is 1 permutation of a string of length 1.
	// We know that the total number of permutations is n! and we know that each
	// permutation will have a length of n. So we perform all of the allocation
	// for the results here and pass that space back up the stack.
	if len(s) == 1 {
		nf := Fac(n)
		heap := make([]byte, n*nf)
		res = make([][]byte, nf)
		for j := range res {
			res[j] = heap[j*n : j*n : j*n+n] // len = 0, cap = n
		}
		res[0] = append(res[0], s[0])
		return res, 1
	}

	// In the recursive case, we first get the permutation of the string less
	// the first byte. We reuse the slice returned from the lower level to store
	// the results of this level. For each of the sub-permutations, we insert a
	// new permutation that includes the first byte at each possible position.
	// In the case of the final position, we simply append to the existing
	// sub-permutation.
	res, i = perm(s[1:], n)
	c := s[0]
	for j := range res[:i] {
		for k := range res[j] {
			res[i] = append(res[i], res[j][:k]...)
			res[i] = append(res[i], c)
			res[i] = append(res[i], res[j][k:]...)
			i++
		}
		res[j] = append(res[j], c)
	}
	return res, i
}

// Fac computes the factorial of n
func Fac(n int) int {
	m := n - 1
	for 0 < m {
		n *= m
		m--
	}
	return n
}

func main() {
	p := Perm("cats")
	fmt.Println(p)
}
