package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

// Returns the endpoints of the longest substring that starts and ends with the
// same byte and contain no pairs of identical bytes in between.
//
// This is a naive implementation that runs in quadratic time.
func widestPair_naive(str []byte) (soln [2]int) {
	var len int
	for i := range str {
		x := str[i]
		seen := make(map[byte]bool)
		for j := range str[i+1:] {
			y := str[i+j+1]
			if x == y {
				if len < j+1 {
					soln = [2]int{i, i + j + 1}
					len = j + 1
				}
			}
			if seen[y] {
				break
			}
			seen[y] = true
		}
	}
	return soln
}

// Returns the endpoints of the longest substring that starts and ends with the
// same byte and contain no pairs of identical bytes in between.
//
// This implementation runs in linear time.
func widestPair(str []byte) (left, right int) {

	// keeps the value of the three most recent visits of each byte
	vs := make(map[byte][3]int)

	// pushes a new position for `b` into `vs`
	visit := func(b byte, val int) [3]int {
		pos, ok := vs[b]
		if ok {
			pos = [3]int{pos[1], pos[2], val}
		} else {
			pos = [3]int{val, val, val}
		}
		vs[b] = pos
		return pos
	}

	// returns true when none of the most recent position pairs
	// are strictly between l and r
	valid := func(l, r int) bool {
		bad := false
		for _, val := range vs {
			if l < val[1] && val[2] < r && val[1] != val[2] {
				bad = true
				break
			}
		}
		return !bad
	}

	// find the widest pair of characters
	// that do not surround another pair of characters.
	for i := range str {
		b := str[i]
		pos := visit(b, i)
		if valid(pos[0], pos[2]) && right-left < pos[2]-pos[0] {
			left, right = pos[0], pos[2]
		} else if valid(pos[1], pos[2]) && right-left < pos[2]-pos[1] {
			left, right = pos[1], pos[2]
		}
	}
	return left, right
}

// Appends the byte at `i` of `str` to `str` and removes the bytes at `i` and `j`
func process(str []byte, i, j int) []byte {
	b := str[i]
	str = append(str[:i], str[i+1:]...)
	str = append(str[:j-1], str[j:]...)
	str = append(str, b)
	return str
}

// Strips the first underscore and all following bytes.
func strip(str []byte) []byte {
	for i := range str {
		if str[i] == '_' {
			return str[:i]
		}
	}
	return str
}

// Solves challenge #252 from r/dailyprogrammer
func main() {

	// Read input
	var buf []byte
	stdin := bufio.NewReader(os.Stdin)
	for {
		line, err := stdin.ReadBytes('\n')
		if err == io.EOF {
			break
		}
		n := len(line)
		buf = append(buf, line[:n-1]...)
	}

	// Foo the bars
	for {
		left, right := widestPair(buf)
		if left == right {
			break
		}
		buf = process(buf, left, right)
	}
	buf = strip(buf)

	// Print the result
	fmt.Printf("%s\n", buf)
}
