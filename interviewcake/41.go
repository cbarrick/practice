package main

import (
	"fmt"
	"math/rand"
)

const n = 31

func main() {
	for i := int64(0); i < 10; i++ {
		rand.Seed(i)

		// setup the problem:
		var ints [n + 1]int
		copy(ints[:], rand.Perm(n))
		dupe := ints[rand.Intn(n)]
		ints[n] = dupe
		i := rand.Intn(n)
		ints[i], ints[n] = ints[n], ints[i]

		// test the implementation
		if findDup(ints) != dupe {
			panic("It doesn't work :(")
		}
	}
	fmt.Println("It works!")
}

// FindDup finds the duplicate in a slice of size n+1 containing a permutation
// of [0,n) and one diplicate. It does this in linear time and constant space.
func findDup(ints [n + 1]int) (dup int) {
	var i, last int
	for {
		switch {
		case ints[i] == i:
			last = ints[i]
			i++
			continue

		case ints[i] == last:
			return last

		default:
			last = ints[i]
			ints[i], ints[ints[i]] = ints[ints[i]], ints[i]
			continue
		}
	}
}
