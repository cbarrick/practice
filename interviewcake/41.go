package main

import (
	"fmt"
	"math/rand"
)

func main() {
	for i := int64(0); i < 256; i++ {
		rand.Seed(i)

		// setup the problem:
		const n = 63
		var ints = make([]int, n+1)
		copy(ints[:n], rand.Perm(n))
		dup := rand.Intn(n)
		i := rand.Intn(n)
		ints[n] = dup
		ints[i], ints[n] = ints[n], ints[i]

		// test the implementation:
		if findDup(ints) != dup {
			fmt.Println("It doesn't work :(")
			panic("failed")
		}
	}
	fmt.Println("It works!")
}

// FindDup finds the duplicate in a slice of size n+1 containing a permutation
// of [0,n) and one diplicate. It does this in linear time and constant space.
func findDup(ints []int) (dup int) {
	var start, end, cycle int

	start = len(ints) - 1
	for _ = range ints {
		start = ints[start]
	}
	end = ints[start]
	cycle++
	for {
		end = ints[end]
		cycle++
		if start == end {
			break
		}
	}

	start = len(ints) - 1
	end = start
	for i := 0; i < cycle; i++ {
		end = ints[end]
	}
	for start != end {
		start = ints[start]
		end = ints[end]
	}
	return start
}
